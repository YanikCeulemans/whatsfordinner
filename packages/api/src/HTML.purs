module Api.HTML where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff as Aff
import HTTPurple.Body (class Body)
import HTTPurple.Headers (mkRequestHeaders)
import Node.Encoding as Encoding
import Node.HTTP.OutgoingMessage as OutgoingMessage
import Node.HTTP.ServerResponse as ServerResponse
import Node.Stream as Stream

data HTML
  = Node String (Array String) (Array HTML)
  | Content String

renderHTML :: HTML -> String
renderHTML html' = "<!DOCTYPE html>" <> help html'
  where
  help =
    case _ of
      Node tag attrs children ->
        Array.fold
          [ "<"
          , tag
          , case attrs of
              [] -> ""
              _ -> " " <> Array.intercalate " " attrs
          , ">"
          , Array.fold $ help <$> children
          , "</"
          , tag
          , ">"
          ]
      Content text' -> text'

instance Body HTML where
  defaultHeaders html' =
    pure $ mkRequestHeaders
      [ Tuple "Content-Type" "text/html"
      , Tuple "Content-Length" $ show $ String.length rendered
      ]
    where
    rendered = renderHTML html'
  write html' response = Aff.makeAff \done -> do
    let
      stream = OutgoingMessage.toWriteable $ ServerResponse.toOutgoingMessage
        response
    void
      $ Stream.writeString' stream Encoding.UTF8 rendered
      $ const
      $ Stream.end' stream
      $ const
      $ done
      $ Right unit
    pure Aff.nonCanceler
    where
    rendered = renderHTML html'

html :: Array String -> Array HTML -> HTML
html = Node "html"

head :: Array String -> Array HTML -> HTML
head = Node "head"

body :: Array String -> Array HTML -> HTML
body = Node "body"

button :: Array String -> Array HTML -> HTML
button = Node "button"

text :: String -> HTML
text = Content

script :: Array String -> Array HTML -> HTML
script = Node "script"

ul :: Array String -> Array HTML -> HTML
ul = Node "ul"

li :: Array String -> Array HTML -> HTML
li = Node "li"
