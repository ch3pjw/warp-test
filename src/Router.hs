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


type Endpoint = Endpoint' Wai.Application


emptyResponse :: HTTP.Status -> Wai.Response
emptyResponse s = Wai.responseBuilder s [] mempty

methodNotAllowed :: Wai.Application
methodNotAllowed _ sendResponse = sendResponse $ emptyResponse HTTP.status405

notFound :: Wai.Application
notFound _ sendResponse = sendResponse $ emptyResponse HTTP.status404


getEp :: Wai.Application -> Endpoint
getEp a = empty { epGet = Just a }

headEp :: Wai.Application -> Endpoint
headEp a = empty { epHead = Just a }

postEp :: Wai.Application -> Endpoint
postEp a = empty { epPost = Just a }

putEp :: Wai.Application -> Endpoint
putEp a = empty { epPut = Just a }

deleteEp :: Wai.Application -> Endpoint
deleteEp a = empty { epDelete = Just a }

optionsEp :: Wai.Application -> Endpoint
optionsEp a = empty { epOptions = Just a }

patchEp :: Wai.Application -> Endpoint
patchEp a = empty { epPatch = Just a }

childEp :: (Text -> Endpoint) -> Endpoint
childEp f = empty { epGetChild = Just f }

childEps :: [(Text, Endpoint)] -> Endpoint
childEps eps = empty
  { epGetChild = Just $ \t -> maybe notFoundEp id $ lookup t eps }


getEpApp :: HTTP.Method -> Endpoint -> Maybe Wai.Application
getEpApp method ep
  | method == HTTP.methodGet = epGet ep
  | method == HTTP.methodPost = epPost ep
  | method == HTTP.methodDelete = epDelete ep
  | method == HTTP.methodPut = epPut ep
  | method == HTTP.methodHead = epHead ep
  | method == HTTP.methodOptions = epOptions ep
  | method == HTTP.methodPatch = epPatch ep
  | otherwise = pure methodNotAllowed


notFoundEp :: Endpoint
notFoundEp = pure notFound


type Path = [Text]

dispatch :: Endpoint -> Wai.Application
dispatch ep req = handler (Wai.pathInfo req) ep req
  where
    handler :: Path -> Endpoint -> Wai.Application
    handler [] ep' =
      maybe methodNotAllowed id (getEpApp (Wai.requestMethod req) ep')
    handler (name:names) ep' =
      maybe notFound (\f -> (handler names $ f name)) (epGetChild ep')
