module Network.Riak.Solr
  ( -- * Search
    search
    -- * Re-exports
  , module Solr.Query
  , module Solr.Query.Lucene
  ) where

import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Maybe (catMaybes)
import Data.Monoid (Endo(..))
import Data.Semigroup
import Data.Sequence ((|>))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Network.Riak.Connection (exchange)
import Network.Riak.Protocol.SearchQueryRequest (SearchQueryRequest)
import Network.Riak.Types (Connection)
import Network.Riak.Types.Internal (Index, SearchResult)
import Prelude.Compat
import Solr.Query
import Solr.Query.Lucene

import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.Lazy as LText (Text)
import qualified Data.Text.Lazy.Builder as Builder (toLazyText)
import qualified Data.Text.Lazy.Encoding as LText (encodeUtf8)
import qualified Network.Riak.Protocol.SearchQueryRequest as Req
import qualified Network.Riak.Response as Resp (search)
import qualified Text.ProtocolBuffers as Proto
import qualified Solr.Query as Solr
import qualified Solr.Query.Internal as Solr
import qualified Solr.Query.Lucene as Solr
import qualified Solr.Query.Lucene.Internal as Solr

search
  :: Connection -> Index -> [Solr.Param] -> Solr.LocalParams Solr.LuceneQuery
  -> Solr.LuceneQuery -> IO SearchResult
search conn index params locals query = Resp.search <$> exchange conn request
  where
    request :: SearchQueryRequest
    request = appEndo (foldMap (Endo . f) params <> g locals)
      (Proto.defaultValue
        { Req.q = b2lbs (Solr.coerceQuery query)
        , Req.index = index
        })
      where
        f :: Solr.Param -> SearchQueryRequest -> SearchQueryRequest
        f = \case
          Solr.ParamFl s       -> appendFl s
          -- I don't believe riak supports filter queries
          Solr.ParamFq _ _     -> id
          Solr.ParamRows n     -> setRows n
          Solr.ParamSortAsc s  -> setSort (s <> " asc")
          Solr.ParamSortDesc s -> setSort (s <> " desc")
          Solr.ParamStart n    -> setStart n

        g :: Solr.LocalParams Solr.LuceneQuery -> Endo SearchQueryRequest
        g Solr.LuceneParams{Solr.paramDf, Solr.paramQOp} =
          mconcat (catMaybes
            [ (Endo . setDf) <$> paramDf
            , (\case
                Solr.QOpAnd -> Endo (setOp "AND")
                Solr.QOpOr  -> Endo (setOp "OR"))
              <$> paramQOp
            ])

setDf :: Text -> SearchQueryRequest -> SearchQueryRequest
setDf s q = q { Req.df = Just (t2lbs s) }

appendFl :: Text -> SearchQueryRequest -> SearchQueryRequest
appendFl s q = q { Req.fl = Req.fl q |> t2lbs s }

setOp :: ByteString -> SearchQueryRequest -> SearchQueryRequest
setOp o q = q { Req.op = Just o }

setRows :: Int -> SearchQueryRequest -> SearchQueryRequest
setRows n q = q { Req.rows = Just (fromIntegral n) }

setSort :: Text -> SearchQueryRequest -> SearchQueryRequest
setSort s q = q { Req.sort = Just (t2lbs s) }

setStart :: Int -> SearchQueryRequest -> SearchQueryRequest
setStart n q = q { Req.start = Just (fromIntegral n) }

t2lbs :: Text -> ByteString
t2lbs = fromStrict . Text.encodeUtf8

lt2lbs :: LText.Text -> ByteString
lt2lbs = LText.encodeUtf8

b2lbs :: Builder -> ByteString
b2lbs = lt2lbs . Builder.toLazyText
