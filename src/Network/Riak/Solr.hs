module Network.Riak.Solr
  ( -- * Search
    search,
    searchRequest,

    -- * Re-exports
    module Solr.Query,
    module Solr.Query.Lucene,
  )
where

import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import qualified Data.Riak.Proto as Proto
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.Lazy as LText (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder (toLazyText)
import qualified Data.Text.Lazy.Encoding as LText (encodeUtf8)
import Network.Riak.Connection (exchange)
import qualified Network.Riak.Response as Resp (search)
import Network.Riak.Types.Internal hiding (MessageTag (..))
import Prelude.Compat
import RIO (trace, (.~), (^.))
import RIO.Prelude (decodeUtf8Lenient)
import Solr.Query
import qualified Solr.Query as Solr
import qualified Solr.Query.Internal as Solr
import Solr.Query.Lucene
import qualified Solr.Query.Lucene as Solr
import qualified Solr.Query.Lucene.Internal as Solr

search ::
  Connection ->
  Index ->
  [Solr.Param] ->
  Solr.LocalParams Solr.LuceneQuery ->
  Solr.LuceneQuery ->
  IO SearchResult
search conn index params locals query =
  Resp.search <$> exchange conn (searchRequest index params locals query)

searchRequest ::
  Index ->
  [Solr.Param] ->
  Solr.LocalParams Solr.LuceneQuery ->
  Solr.LuceneQuery ->
  Proto.RpbSearchQueryReq
searchRequest index params locals query =
  trace (decodeUtf8Lenient (toStrict $ b2lbs (Solr.coerceQuery query))) $
    appEndo
      (foldMap (Endo . f) params <> g locals)
      ( Proto.defMessage
          & Proto.q .~ (toStrict $ b2lbs (Solr.coerceQuery query))
          & Proto.index .~ index
      )
  where
    f :: Solr.Param -> Proto.RpbSearchQueryReq -> Proto.RpbSearchQueryReq
    f = \case
      Solr.ParamFl s -> appendFl s
      -- I don't believe riak supports filter queries
      Solr.ParamFq _ _ -> id
      Solr.ParamRows n -> setRows n
      Solr.ParamSort [] -> id
      Solr.ParamSort ss -> setSort (toSortString ss)
      Solr.ParamStart n -> setStart n

    g :: Solr.LocalParams Solr.LuceneQuery -> Endo Proto.RpbSearchQueryReq
    g Solr.LuceneParams {Solr.paramDf, Solr.paramQOp} =
      mconcat
        ( catMaybes
            [ (Endo . setDf) <$> paramDf,
              ( \case
                  Solr.QOpAnd -> Endo (setOp "AND")
                  Solr.QOpOr -> Endo (setOp "OR")
              )
                <$> paramQOp
            ]
        )

toSortString :: [(Text, SortWay)] -> Text
toSortString =
  mconcat . intersperse "," . map f
  where
    f :: (Text, SortWay) -> Text
    f (s, Asc) = s <> " asc"
    f (s, Desc) = s <> " desc"

setDf :: Text -> Proto.RpbSearchQueryReq -> Proto.RpbSearchQueryReq
setDf s q = q & Proto.df .~ (toStrict $ t2lbs s)

appendFl :: Text -> Proto.RpbSearchQueryReq -> Proto.RpbSearchQueryReq
appendFl s q = q & Proto.fl .~ ((q ^. Proto.fl) ++ [toStrict . t2lbs $ s])

--q { Req.fl = Req.fl q |> t2lbs s }

setOp :: ByteString -> Proto.RpbSearchQueryReq -> Proto.RpbSearchQueryReq
setOp o q = q & Proto.op .~ (toStrict o)

setRows :: Int -> Proto.RpbSearchQueryReq -> Proto.RpbSearchQueryReq
setRows n q = q & Proto.rows .~ (fromIntegral n)

setSort :: Text -> Proto.RpbSearchQueryReq -> Proto.RpbSearchQueryReq
setSort s q = q & Proto.sort .~ (toStrict (t2lbs s))

setStart :: Int -> Proto.RpbSearchQueryReq -> Proto.RpbSearchQueryReq
setStart n q = q & Proto.start .~ (fromIntegral n)

t2lbs :: Text -> ByteString
t2lbs = fromStrict . Text.encodeUtf8

lt2lbs :: LText.Text -> ByteString
lt2lbs = LText.encodeUtf8

b2lbs :: Builder -> ByteString
b2lbs = lt2lbs . Builder.toLazyText
