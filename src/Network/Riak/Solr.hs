module Network.Riak.Solr
  ( -- * Search
    search
    -- * Re-exports
  , Query
  , Expr
  , Param
  , paramDefaultField
  , paramOpAnd
  , paramOpOr
  , paramRows
  , paramStart
  , module Solr.Query.Class
  , module Solr.Expr.Class
  ) where

import Data.ByteString.Lazy                     (ByteString, fromStrict)
import Data.Monoid                              (Endo(..))
import Data.Semigroup
import Data.Text                                (Text)
import Network.Riak.Connection                  (exchange)
import Network.Riak.Protocol.SearchQueryRequest (SearchQueryRequest)
import Network.Riak.Types                       (Connection)
import Network.Riak.Types.Internal              (Index, SearchResult)
import Prelude                                  hiding (filter)
import Solr.Param.Internal
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
  :: Connection -> Index -> [Param Query] -> Query Expr
  -> IO SearchResult
search conn index params query = Resp.search <$> exchange conn request
  where
    request :: SearchQueryRequest
    request = appEndo (foldMap step params)
      (Proto.defaultValue
        { Req.q = lt2lbs (compile [] query)
        , Req.index = index
        })
      where
        step :: Param Query -> Endo SearchQueryRequest
        step = \case
          ParamDefaultField s -> Endo (setDefaultField s)
          ParamOpAnd          -> Endo (setOp "AND")
          ParamOpOr           -> Endo (setOp "OR")
          ParamRows         n -> Endo (setRows n)
          ParamStart        n -> Endo (setStart n)

          -- These correspond to the instances that Query is missing, so it
          -- isn't likely that we end up here (orphan instances...); just return
          -- mempty anyway.
          ParamCache _ -> mempty
          ParamCost  _ -> mempty

setDefaultField :: Text -> SearchQueryRequest -> SearchQueryRequest
setDefaultField s q = q { Req.df = Just (t2lbs s) }

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
