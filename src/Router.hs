module Router
  ( Endpoint
  , getEp, headEp, postEp, putEp, deleteEp, optionsEp, patchEp
  , childEp, childEps
  , dispatch
  )
where

import Control.Applicative
import Data.Text (Text)
import qualified Network.Wai as Wai
import qualified Network.Wai.Trans as Wai
import qualified Network.HTTP.Types as HTTP


data Endpoint' a = Endpoint'
  { epGet :: Maybe a
  , epHead :: Maybe a
  , epPost :: Maybe a
  , epPut :: Maybe a
  , epDelete :: Maybe a
  , epOptions :: Maybe a
  , epPatch :: Maybe a
  , epGetChild :: Maybe (Text -> Endpoint' a)
  }


instance Functor Endpoint' where
    fmap f ep = ep
      { epGet = f <$> epGet ep
      , epHead = f <$> epHead ep
      , epPost = f <$> epPost ep
      , epPut = f <$> epPut ep
      , epDelete = f <$> epDelete ep
      , epOptions = f <$> epOptions ep
      , epPatch = f <$> epPatch ep
      , epGetChild = (fmap . fmap . fmap) f (epGetChild ep)}


instance Applicative Endpoint' where
    pure a = Endpoint'
      { epGet = Just a
      , epHead = Just a
      , epPost = Just a
      , epPut = Just a
      , epDelete = Just a
      , epOptions = Just a
      , epPatch = Just a
      , epGetChild = Nothing}
    ep1 <*> ep2 = Endpoint'
      { epGet = epGet ep1 <*> epGet ep2
      , epHead = epHead ep1 <*> epHead ep2
      , epPost = epPost ep1 <*> epPost ep2
      , epPut = epPut ep1 <*> epPut ep2
      , epDelete = epDelete ep1 <*> epDelete ep2
      , epOptions = epOptions ep1 <*> epOptions ep2
      , epPatch = epPatch ep1 <*> epPatch ep2
      , epGetChild = Nothing}


instance Alternative Endpoint' where
    empty = Endpoint'
      { epGet = Nothing
      , epHead = Nothing
      , epPost = Nothing
      , epPut = Nothing
      , epDelete = Nothing
      , epOptions = Nothing
      , epPatch = Nothing
      , epGetChild = Nothing}
    ep1 <|> ep2 = Endpoint'
      { epGet = epGet ep1 <|> epGet ep2
      , epHead = epHead ep1 <|> epHead ep2
      , epPost = epPost ep1 <|> epPost ep2
      , epPut = epPut ep1 <|> epPut ep2
      , epDelete = epDelete ep1 <|> epDelete ep2
      , epOptions = epOptions ep1 <|> epOptions ep2
      , epPatch = epPatch ep1 <|> epPatch ep2
      , epGetChild = epGetChild ep1 <|> epGetChild ep2}


type Endpoint m = Endpoint' (Wai.ApplicationT m)


emptyResponse :: HTTP.Status -> Wai.Response
emptyResponse s = Wai.responseBuilder s [] mempty

methodNotAllowed :: (Monad m) => Wai.ApplicationT m
methodNotAllowed _ sendResponse = sendResponse $ emptyResponse HTTP.status405

notFound :: (Monad m) => Wai.ApplicationT m
notFound _ sendResponse = sendResponse $ emptyResponse HTTP.status404


getEp :: (Monad m) => Wai.ApplicationT m -> Endpoint m
getEp a = empty { epGet = Just a }

headEp :: (Monad m) => Wai.ApplicationT m -> Endpoint m
headEp a = empty { epHead = Just a }

postEp :: (Monad m) => Wai.ApplicationT m -> Endpoint m
postEp a = empty { epPost = Just a }

putEp :: (Monad m) => Wai.ApplicationT m -> Endpoint m
putEp a = empty { epPut = Just a }

deleteEp :: (Monad m) => Wai.ApplicationT m -> Endpoint m
deleteEp a = empty { epDelete = Just a }

optionsEp :: (Monad m) => Wai.ApplicationT m -> Endpoint m
optionsEp a = empty { epOptions = Just a }

patchEp :: (Monad m) => Wai.ApplicationT m -> Endpoint m
patchEp a = empty { epPatch = Just a }

childEp :: (Monad m) => (Text -> Endpoint m) -> Endpoint m
childEp f = empty { epGetChild = Just f }

childEps :: (Monad m) => [(Text, Endpoint m)] -> Endpoint m
childEps eps = empty
  { epGetChild = Just $ \t -> maybe notFoundEp id $ lookup t eps }


getEpApp :: (Monad m) => HTTP.Method -> Endpoint m -> Maybe (Wai.ApplicationT m)
getEpApp method ep
  | method == HTTP.methodGet = epGet ep
  | method == HTTP.methodPost = epPost ep
  | method == HTTP.methodDelete = epDelete ep
  | method == HTTP.methodPut = epPut ep
  | method == HTTP.methodHead = epHead ep
  | method == HTTP.methodOptions = epOptions ep
  | method == HTTP.methodPatch = epPatch ep
  | otherwise = pure methodNotAllowed


notFoundEp :: (Monad m) => Endpoint m
notFoundEp = pure notFound


type Path = [Text]

dispatch :: (Monad m) => Endpoint m -> Wai.ApplicationT m
dispatch ep req = handler (Wai.pathInfo req) ep req
  where
    handler :: (Monad m) => Path -> Endpoint m -> Wai.ApplicationT m
    handler [] ep' =
      maybe methodNotAllowed id (getEpApp (Wai.requestMethod req) ep')
    handler (name:names) ep' =
      maybe notFound (\f -> (handler names $ f name)) (epGetChild ep')
