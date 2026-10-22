{-# LANGUAGE OverloadedStrings #-}

-- | The game loop. Each player command is one "session": a short
-- multi-turn conversation with the LLM that ends when the LLM replies
-- without an action.
module Main (main) where

import           Control.Monad       (unless, when)
import           Data.Aeson.Text     (encodeToLazyText)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy      as TL
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           System.Environment  (getArgs)
import           System.Exit         (exitFailure)
import           System.IO

import           Engine
import           Llm
import           Prompt
import           World

data Env = Env
  { envConfig :: Config
  , envDebug  :: Bool
  }

modelName :: Text
modelName = "qwen3.5:4b"

-- | Safety valve: the most actions the LLM may chain for one command.
maxActions :: Int
maxActions = 4

main :: IO ()
main = do
  args <- getArgs
  let known = ["--debug", "--think"]
      bad   = filter (`notElem` known) args
  unless (null bad) $ do
    hPutStrLn stderr ("unknown argument(s): " ++ unwords bad)
    hPutStrLn stderr "usage: adventure [--debug] [--think]"
    exitFailure

  hSetBuffering stdout NoBuffering
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  mgr <- newManager defaultManagerSettings
  let env = Env
        { envConfig = Config mgr modelName ("--think" `elem` args)
        , envDebug  = "--debug" `elem` args
        }

  debugLog env "system prompt" systemPrompt

  -- The first command is implicit, so the story opens with the room.
  gs <- runSession env initialWorld "look"
  loop env gs

loop :: Env -> GameState -> IO ()
loop env gs = do
  TIO.putStr "\n> "
  eof <- isEOF
  if eof
    then TIO.putStrLn ""
    else do
      cmd <- T.strip <$> TIO.getLine
      if T.null cmd
        then loop env gs
        else if T.toLower cmd `elem` ["quit", "exit"]
          then TIO.putStrLn "\nThe book closes softly."
          else do
            TIO.putStrLn ""
            gs' <- runSession env gs cmd
            loop env gs'

-- | One session: the player's command, then any number of
-- action → engine result turns, until the LLM stops asking for actions.
runSession :: Env -> GameState -> Text -> IO GameState
runSession env gs0 cmd0 =
  step 0 gs0 [ Message "system" systemPrompt
             , Message "user" (commandMessage gs0 cmd) ]
  where

    cmd = case cmd0 of
        "n" -> "go north"
        "s" -> "go south"
        "w" -> "go west"
        "e" -> "go east"
        x -> x
  
    step :: Int -> GameState -> [Message] -> IO GameState
    step n gs msgs = do
      -- Show the message just added (the system prompt was shown at startup).
      debugLog env "engine -> LLM" (msgContent (last msgs))
      result <- chat (envConfig env) msgs
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("[error] " <> err)
          pure gs
        Right raw -> do
          debugLog env "LLM -> engine" raw
          case parseReply raw of
            Left err -> do
              TIO.hPutStrLn stderr ("[error] could not understand the narrator's reply: " <> T.pack err)
              pure gs
            Right (Reply mAction txt) -> do
              unless (T.null txt) (putWrapped txt)
              case mAction of
                Nothing -> pure gs
                Just action
                  | n >= maxActions -> do
                      debugLog env "engine" "too many actions for one command; ending the session"
                      pure gs
                  | otherwise -> do
                      let (outcome, gs') = runAction action gs
                      debugLog env "engine"
                        (T.pack (show action) <> " => " <> TL.toStrict (encodeToLazyText outcome))
                      step (n + 1) gs'
                        (msgs ++ [ Message "assistant" raw
                                 , Message "user" (resultMessage outcome gs') ])

debugLog :: Env -> Text -> Text -> IO ()
debugLog env label body =
  when (envDebug env) $
    TIO.hPutStrLn stderr ("\n[debug] " <> label <> ":\n" <> body)

-- | Print story text wrapped to a classic 72 columns.
putWrapped :: Text -> IO ()
putWrapped = TIO.putStrLn . T.intercalate "\n" . concatMap (wrapLine 72) . T.lines

wrapLine :: Int -> Text -> [Text]
wrapLine width line = case T.words line of
  []       -> [""]
  (w : ws) -> go w ws
  where
    go cur [] = [cur]
    go cur (x : xs)
      | T.length cur + 1 + T.length x > width = cur : go x xs
      | otherwise                             = go (cur <> " " <> x) xs
