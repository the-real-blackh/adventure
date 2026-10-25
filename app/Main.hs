{-# LANGUAGE OverloadedStrings #-}

-- | The game loop. Each player command is one "session": a short
-- multi-turn conversation with the LLM that ends when the LLM replies
-- without an action.
module Main (main) where

import           Control.Monad       (unless, when)
import           Data.Maybe          (fromMaybe)
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
import           State
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

-- | Where the game state is saved after every command.
saveFile :: FilePath
saveFile = "adventure.save"

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

  -- Carry on from the saved game if there is one.
  gs0 <- fromMaybe initialState <$> loadState saveFile

  -- The first command is implicit, so the story opens with the room.
  (gs, said) <- runSession env gs0 "" "look"
  loop env gs said

-- | The game loop. 'prev' is the story text the narrator wrote in
-- response to the previous command, passed along so the narrator can
-- resolve words like "it" and "there".
loop :: Env -> GameState -> Text -> IO ()
loop env gs prev = do
  TIO.putStr "\n> "
  eof <- isEOF
  if eof
    then TIO.putStrLn ""
    else do
      cmd <- T.strip <$> TIO.getLine
      if T.toLower cmd `elem` ["quit", "exit"]
          then TIO.putStrLn "\nThe book closes softly."
          else do
            TIO.putStrLn ""
            (gs', said) <- runSession env gs prev cmd
            saveState saveFile gs'
            -- If this session produced no text (e.g. an error), keep the
            -- older reply so the narrator still has something to refer to.
            loop env gs' (if T.null said then prev else said)

-- | One session: the player's command, then any number of
-- action → engine result turns, until the LLM stops asking for actions.
-- Returns the new state and all story text printed during the session.
runSession :: Env -> GameState -> Text -> Text -> IO (GameState, Text)
runSession env gs0 prev cmd0 = do
  (gs, texts) <- step 0 gs0 [] [ Message "system" systemPrompt
                               , Message "user" (commandMessage gs0 prev cmd) ]
  pure (gs, T.intercalate "\n\n" texts)
  where

    cmd = case cmd0 of
        "n" -> "go north"
        "s" -> "go south"
        "w" -> "go west"
        "e" -> "go east"
        x -> x
  
    -- 'said' collects the story text printed so far, newest first.
    step :: Int -> GameState -> [Text] -> [Message] -> IO (GameState, [Text])
    step n gs said msgs = do
      -- Show the message just added (the system prompt was shown at startup).
      debugLog env "engine -> LLM" (msgContent (last msgs))
      result <- chat (envConfig env) msgs
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("[error] " <> err)
          done gs said
        Right raw -> do
          debugLog env "LLM -> engine" raw
          case parseReply raw of
            Left err -> do
              TIO.hPutStrLn stderr ("[error] could not understand the narrator's reply: " <> T.pack err)
              done gs said
            Right (Reply _ mAction txt) -> do
              unless (T.null txt) (putWrapped txt)
              let said' = if T.null txt then said else txt : said
              case mAction of
                Nothing -> done gs said'
                Just action
                  | n >= maxActions -> do
                      debugLog env "engine" "too many actions for one command; ending the session"
                      done gs said'
                  | otherwise -> do
                      let (outcome, gs') = runCommand action gs
                      debugLog env "engine"
                        (T.pack (show action) <> " => " <> TL.toStrict (encodeToLazyText outcome))
                      step (n + 1) gs' said'
                        (msgs ++ [ Message "assistant" raw
                                 , Message "user" (resultMessage outcome gs') ])

    done gs said = pure (gs, reverse said)

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
