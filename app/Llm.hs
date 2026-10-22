{-# LANGUAGE OverloadedStrings #-}

-- | The Ollama client: one call to POST /api/chat, with the reply
-- constrained by a JSON Schema (structured output).
module Llm
  ( Config (..)
  , Message (..)
  , Reply (..)
  , chat
  , parseReply
  , replySchema
  ) where

import           Control.Exception         (try)
import           Data.Aeson
import           Data.Aeson.Encoding       (encodingToLazyByteString, pair)
import           Data.Aeson.Types          (Parser, parseEither)
import qualified Data.ByteString.Lazy      as BL
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Text.Encoding.Error  (lenientDecode)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)

import           Engine                    (Action (..))

data Config = Config
  { cfgManager :: Manager
  , cfgModel   :: Text
  , cfgThink   :: Bool
  }

data Message = Message
  { msgRole    :: Text   -- ^ "system", "user" or "assistant"
  , msgContent :: Text
  }

instance ToJSON Message where
  toJSON (Message r c) = object ["role" .= r, "content" .= c]

-- | What the LLM sends back: its brief reasoning (never shown to the
-- player), an optional action and optional story text.
data Reply = Reply
  { replyReasoning :: Text
  , replyAction    :: Maybe Action
  , replyText      :: Text      -- ^ empty means no text
  }

ollamaUrl :: String
ollamaUrl = "http://localhost:11434/api/chat"

-- | The JSON Schema Ollama uses to constrain the model's output.
--
-- It is built as an 'Encoding' rather than a 'Value' because the order of
-- the properties matters: the model writes them in schema order, and we
-- want it to reason first, then pick an action, then write the story.
-- (An aeson 'Value' would sort the keys alphabetically.)
--
-- All fields are required so a small model can't drop them; "optional" is
-- expressed as action "none" and text "". The maxLength on "reasoning" is
-- a hard cap if Ollama's grammar enforces it, and harmless if it doesn't.
replySchema :: Encoding
replySchema = pairs $
     "type" .= ("object" :: Text)
  <> pair "properties" (pairs $
          pair "reasoning" (pairs $
               "type"      .= ("string" :: Text)
            <> "maxLength" .= (400 :: Int))
       <> pair "action" (pairs $
               "type" .= ("object" :: Text)
            <> pair "properties" (pairs $
                    pair "action" (pairs $
                         "type" .= ("string" :: Text)
                      <> "enum" .= (["none", "go", "take", "drop", "reveal", "unlock"] :: [Text]))
                 <> pair "id" (pairs $ "type" .= ("string" :: Text)))
            <> "required" .= (["action", "id"] :: [Text]))
       <> pair "text" (pairs $ "type" .= ("string" :: Text)))
  <> "required" .= (["reasoning", "action", "text"] :: [Text])

-- | Send the whole conversation so far and return the assistant's raw reply.
chat :: Config -> [Message] -> IO (Either Text Text)
chat cfg msgs = do
  req0 <- parseRequest ollamaUrl
  let req = req0
        { method          = "POST"
        , requestHeaders  = [("Content-Type", "application/json")]
        , requestBody     = RequestBodyLBS (encodingToLazyByteString body)
        , responseTimeout = responseTimeoutNone   -- local models can be slow
        }
  result <- try (httpLbs req (cfgManager cfg))
            :: IO (Either HttpException (Response BL.ByteString))
  pure $ case result of
    Left e -> Left $
      "could not reach Ollama at " <> T.pack ollamaUrl <> " (is it running?): " <> T.pack (show e)
    Right resp
      | code /= 200 -> Left $
          "Ollama returned HTTP " <> T.pack (show code) <> ": " <> decodeLenient (responseBody resp)
      | otherwise -> case eitherDecode (responseBody resp) >>= parseEither content of
          Left err -> Left ("unexpected response from Ollama: " <> T.pack err)
          Right t  -> Right t
      where code = statusCode (responseStatus resp)
  where
    body = pairs $
         "model"      .= cfgModel cfg
      <> "messages"   .= msgs
      <> "stream"     .= False
      <> "think"      .= cfgThink cfg
      <> pair "format" replySchema
      <> "keep_alive" .= ("30m" :: Text)
      <> "options"    .= object
           [ "num_ctx"     .= (8192 :: Int)
           , "temperature" .= (0.6 :: Double)
           ]

    content :: Value -> Parser Text
    content = withObject "response" $ \o -> do
      m <- o .: "message"
      withObject "message" (.: "content") m

    decodeLenient = TE.decodeUtf8With lenientDecode . BL.toStrict

-- | Turn the model's JSON reply into a 'Reply'.
parseReply :: Text -> Either String Reply
parseReply raw = eitherDecodeStrict (TE.encodeUtf8 raw) >>= parseEither reply
  where
    reply = withObject "reply" $ \o -> do
      r <- o .:? "reasoning" .!= ""
      a <- o .: "action"
      t <- o .:? "text" .!= ""
      act <- withObject "action" (\ao -> do
               n <- ao .: "action"
               i <- ao .:? "id" .!= ""
               toAction (T.toLower n) (T.strip i)) a
      pure (Reply (T.strip r) act (T.strip t))

    toAction :: Text -> Text -> Parser (Maybe Action)
    toAction n i = case n of
      "none"   -> pure Nothing
      "go"     -> pure (Just (Go i))
      "take"   -> pure (Just (Take i))
      "drop"   -> pure (Just (Drop i))
      "reveal" -> pure (Just (Reveal i))
      "unlock" -> pure (Just (Unlock i))
      _        -> fail ("unknown action: " ++ T.unpack n)
