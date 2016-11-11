module Network.Riak.Solr
  ( -- * Search
    search
    -- * Re-exports
  , Param
  , paramDefaultField
  , paramOpAnd
  , paramOpOr
  , paramRows
  , paramStart
  , module Solr.Query.Class
  , module Solr.Expr.Class
  ) where

import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Coerce (coerce)
import Data.Semigroup
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word
import Network.Riak.Protocol.SearchQueryRequest
  (SearchQueryRequest(SearchQueryRequest))
import Network.Riak.Protocol.SearchQueryResponse (SearchQueryResponse)
import Prelude hiding (filter)
import Solr.Param.Internal
import Solr.Expr.Class
import Solr.Query
import Solr.Query.Class

import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.Lazy as LText (Text)
import qualified Data.Text.Lazy.Encoding as LText (encodeUtf8)
import qualified Network.Riak.Connection as Riak
import qualified Network.Riak.Protocol.SearchQueryRequest as Req
import qualified Network.Riak.Types as Riak
import qualified Network.Riak.Types.Internal as Riak

data SolrQueryOpts = SolrQueryOpts
  { rows    :: Option (Sum Word32)
  , start   :: Option (Min Word32)
  , sort    :: Option (Last ByteString)
  , filter  :: Option (Last ByteString)
  , df      :: Option (Last ByteString)
  , op      :: Option (Last ByteString)
  , fl      :: Seq ByteString
  , presort :: Option (Last ByteString)
  }

instance Semigroup SolrQueryOpts where
  SolrQueryOpts a b c d e f g h <> SolrQueryOpts a' b' c' d' e' f' g' h' =
    SolrQueryOpts (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f')
      (g <> g') (h <> h')

instance Monoid SolrQueryOpts where
  mempty = SolrQueryOpts mempty mempty mempty mempty mempty mempty mempty mempty
  mappend = (<>)

setDefaultField :: Text -> SolrQueryOpts
setDefaultField s = mempty { df = pure (pure (t2lbs s)) }

setOp :: ByteString -> SolrQueryOpts
setOp o = mempty { op = pure (pure o) }

setRows :: Int -> SolrQueryOpts
setRows n = mempty { rows = pure (pure (fromIntegral n)) }

setStart :: Int -> SolrQueryOpts
setStart n = mempty { start = pure (pure (fromIntegral n)) }

search
  :: Riak.Connection -> Riak.Index -> [Param SolrQuery] -> SolrQuery SolrExpr
  -> IO SearchQueryResponse
search conn index params query = Riak.exchange conn request
  where
    request :: SearchQueryRequest
    request = construct (foldMap mkOpt params)
      where
        mkOpt :: Param SolrQuery -> SolrQueryOpts
        mkOpt = \case
          ParamDefaultField s -> setDefaultField s
          ParamOpAnd          -> setOp "AND"
          ParamOpOr           -> setOp "OR"
          ParamRows         n -> setRows n
          ParamStart        n -> setStart n

          -- These correspond to the instances that SolrQuery is missing, so it
          -- isn't likely that we end up here (orphan instances...); just return
          -- mempty anyway.
          ParamCache _ -> mempty
          ParamCost  _ -> mempty

    construct :: SolrQueryOpts -> SearchQueryRequest
    construct opts = SearchQueryRequest
      { Req.q       = lt2lbs (compileSolrQuery [] query)
      , Req.index   = index
      , Req.rows    = coerce (rows opts)
      , Req.start   = coerce (start opts)
      , Req.sort    = coerce (sort opts)
      , Req.filter  = coerce (filter opts)
      , Req.df      = coerce (df opts)
      , Req.op      = coerce (op opts)
      , Req.fl      = fl opts
      , Req.presort = coerce (presort opts)
      }

t2lbs :: Text -> ByteString
t2lbs = fromStrict . Text.encodeUtf8

lt2lbs :: LText.Text -> ByteString
lt2lbs = LText.encodeUtf8
