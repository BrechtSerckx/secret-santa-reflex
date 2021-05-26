{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Servant.Server
  ( module SS
  , IsStatusCode(..)
  ) where

import "servant-server" Servant.Server         as SS
import "servant-server" Servant.Server.StaticFiles
                                               as SS

import           Control.Lens
import qualified Data.Text                     as T

import qualified Servant.API                   as S
import qualified Servant.API.TypeLevel         as S
import qualified Servant.Foreign               as SF
import qualified Servant.Foreign.Internal      as SF

instance
  ( SF.HasForeignType lang ftype as
  , SF.ReflectMethod method
  , S.Elem SF.JSON list
  )
  => SF.HasForeign (lang::k) ftype (S.UVerb method list as) where
  type Foreign ftype (S.UVerb method list as) = SF.Req ftype
  foreignFor lang Proxy Proxy req =
    req
      & (SF.reqFuncName . SF._FunctionName %~ (methodLC :))
      & (SF.reqMethod .~ method)
      & (SF.reqReturnType ?~ retType)
   where
    retType  = SF.typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy as)
    method   = SF.reflectMethod (Proxy :: Proxy method)
    methodLC = T.toLower $ decodeUtf8 method

class IsStatusCode status where
  errorConstructor :: Proxy status -> SS.ServerError

instance IsStatusCode 300 where
  errorConstructor Proxy = SS.err300
instance IsStatusCode 301 where
  errorConstructor Proxy = SS.err301
instance IsStatusCode 302 where
  errorConstructor Proxy = SS.err302
instance IsStatusCode 303 where
  errorConstructor Proxy = SS.err303
instance IsStatusCode 304 where
  errorConstructor Proxy = SS.err304
instance IsStatusCode 305 where
  errorConstructor Proxy = SS.err305
instance IsStatusCode 307 where
  errorConstructor Proxy = SS.err307
instance IsStatusCode 400 where
  errorConstructor Proxy = SS.err400
instance IsStatusCode 401 where
  errorConstructor Proxy = SS.err401
instance IsStatusCode 402 where
  errorConstructor Proxy = SS.err402
instance IsStatusCode 403 where
  errorConstructor Proxy = SS.err403
instance IsStatusCode 404 where
  errorConstructor Proxy = SS.err404
instance IsStatusCode 405 where
  errorConstructor Proxy = SS.err405
instance IsStatusCode 406 where
  errorConstructor Proxy = SS.err406
instance IsStatusCode 407 where
  errorConstructor Proxy = SS.err407
instance IsStatusCode 409 where
  errorConstructor Proxy = SS.err409
instance IsStatusCode 410 where
  errorConstructor Proxy = SS.err410
instance IsStatusCode 411 where
  errorConstructor Proxy = SS.err411
instance IsStatusCode 412 where
  errorConstructor Proxy = SS.err412
instance IsStatusCode 413 where
  errorConstructor Proxy = SS.err413
instance IsStatusCode 414 where
  errorConstructor Proxy = SS.err414
instance IsStatusCode 415 where
  errorConstructor Proxy = SS.err415
instance IsStatusCode 416 where
  errorConstructor Proxy = SS.err416
instance IsStatusCode 417 where
  errorConstructor Proxy = SS.err417
instance IsStatusCode 418 where
  errorConstructor Proxy = SS.err418
instance IsStatusCode 422 where
  errorConstructor Proxy = SS.err422
instance IsStatusCode 500 where
  errorConstructor Proxy = SS.err500
instance IsStatusCode 501 where
  errorConstructor Proxy = SS.err501
instance IsStatusCode 502 where
  errorConstructor Proxy = SS.err502
instance IsStatusCode 503 where
  errorConstructor Proxy = SS.err503
instance IsStatusCode 504 where
  errorConstructor Proxy = SS.err504
instance IsStatusCode 505 where
  errorConstructor Proxy = SS.err505
