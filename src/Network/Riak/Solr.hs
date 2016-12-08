module Network.Riak.Solr
  ( -- * Search
    search
    -- * Re-exports
  , Query
  , FilterQuery
  , Expr
  , module Solr.Expr.Class
  , module Solr.LocalParam
  , module Solr.Param
  , module Solr.Query.Class
  ) where

import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Monoid (Endo(..))
import Data.Semigroup
import Data.Sequence ((|>))
import Data.Text (Text)
import Network.Riak.Connection (exchange)
import Network.Riak.Protocol.SearchQueryRequest (SearchQueryRequest)
import Network.Riak.Types (Connection)
import Network.Riak.Types.Internal (Index, SearchResult)
import Prelude.Compat
import Solr.Param
import Solr.LocalParam
import Solr.LocalParam.Internal
import Solr.Expr.Class
import Solr.Query
import Solr.Query.Class

import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.Lazy as LText (Text)
import qualified Data.Text.Lazy.Encoding as LText (encodeUtf8)
import qualified Network.Riak.Protocol.SearchQueryRequest as Req
import qualified Network.Riak.Response as Resp (search)
import qualified Text.ProtocolBuffers as Proto

search
  :: Connection -> Index -> [Param] -> [LocalParam Query] -> Query Expr
  -> IO SearchResult
search conn index params locals query = Resp.search <$> exchange conn request
  where
    request :: SearchQueryRequest
    request = appEndo (foldMap f params <> foldMap g locals)
      (Proto.defaultValue
        { Req.q = lt2lbs (compile [] [] query)
        , Req.index = index
        })
      where
        f :: Param -> Endo SearchQueryRequest
        f = \case
          ParamFl s    -> Endo (appendFl s)
          ParamFq _ _  -> mempty -- I don't believe riak supports filter queries
          ParamRows n  -> Endo (setRows n)
          ParamStart n -> Endo (setStart n)

        g :: LocalParam Query -> Endo SearchQueryRequest
        g = \case
          LocalParamDf s  -> Endo (setDf s)
          LocalParamOpAnd -> Endo (setOp "AND")
          LocalParamOpOr  -> Endo (setOp "OR")

          -- These correspond to the instances that Query is missing, so it
          -- isn't likely that we end up here (orphan instances...); just return
          -- mempty anyway.
          LocalParamCache _ -> mempty
          LocalParamCost  _ -> mempty

setDf :: Text -> SearchQueryRequest -> SearchQueryRequest
setDf s q = q { Req.df = Just (t2lbs s) }

appendFl :: Text -> SearchQueryRequest -> SearchQueryRequest
appendFl s q = q { Req.fl = Req.fl q |> t2lbs s }

setOp :: ByteString -> SearchQueryRequest -> SearchQueryRequest
setOp o q = q { Req.op = Just o }

setRows :: Int -> SearchQueryRequest -> SearchQueryRequest
setRows n q = q { Req.rows = Just (fromIntegral n) }

setStart :: Int -> SearchQueryRequest -> SearchQueryRequest
setStart n q = q { Req.start = Just (fromIntegral n) }

t2lbs :: Text -> ByteString
t2lbs = fromStrict . Text.encodeUtf8

lt2lbs :: LText.Text -> ByteString
lt2lbs = LText.encodeUtf8
