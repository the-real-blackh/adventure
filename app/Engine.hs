{-# LANGUAGE OverloadedStrings #-}

-- | The game engine: applies actions requested by the LLM and enforces
-- the rules. The LLM proposes, the engine disposes.
module Engine
  ( Action (..)
  , Outcome (..)
  , runAction
  ) where

import           Control.Monad.Reader (runReader)
import           Data.Aeson           (ToJSON (..), object, (.=))
import           Data.List            (find)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           State
import           World

data Action
  = Go Text        -- ^ direction
  | Take ItemId
  | Drop ItemId
  | Reveal ItemId
  | Unlock Text    -- ^ exit id
  deriving (Show)

data Outcome = Outcome
  { outSuccess     :: Bool
  , outDescription :: Maybe Text
  }

instance ToJSON Outcome where
  toJSON (Outcome s d) =
    object (("success" .= s) : maybe [] (\t -> ["description" .= t]) d)

runAction :: Action -> GameState -> (Outcome, GameState)
runAction action gs = flip runReader gs $ do
  here    <- roomItems (gsLocation gs)
  carried <- inventory
  exits   <- roomExits =<< currentRoom
  case action of

    Go dir -> case find (\exit -> exitDirection exit == norm dir || exitId exit == Just (norm dir)) exits of
      Nothing -> pure $ failWith ("there is no way to go " <> dir <> " from here")
      Just e -> do
        t <- exitTraversable e
        if t
          then (\to -> succeed Nothing gs { gsLocation = to }) <$> exitTo e
          else failWith <$> exitBlockedText e

    Take iid -> do
      let notHere
            | any (matches iid) carried = failWith "the player is already carrying that"
            | otherwise                 = failWith "there is nothing like that here"
      case findItem iid here of
        Nothing -> pure notHere
        Just it -> do
          v <- itemVisible it
          if not v then pure notHere else do
            p <- itemPortability it
            pure $ case p of
              Fixed why -> failWith why
              Portable  -> succeed Nothing (moveItem (itemId it) Carried gs)

    Drop iid -> pure $ case findItem iid carried of
      Nothing -> failWith "the player is not carrying that"
      Just it -> succeed Nothing (moveItem (itemId it) (InRoom (gsLocation gs)) gs)

    Reveal iid -> case findItem iid here of
      Nothing -> pure $ failWith "there is nothing like that to be found here"
      Just it -> do
        v <- itemVisible it
        if v
          then pure $ failWith "that is already in plain sight"
          else do
            d <- itemRevealText it
            pure $ succeed d (moveItem (itemId it) (InRoom (gsLocation gs)) gs)

    Unlock eid -> case find (maybe False ((== norm eid) . norm) . exitId) exits of
      Nothing -> pure $ failWith "there is nothing like that to unlock here"
      Just e -> do
        t   <- exitTraversable e
        req <- exitRequires e
        case (req, exitId e) of
          _ | t -> pure $ failWith "it is already unlocked"
          (Just need, Just x)
            | any (matches need) carried -> do
                d <- exitUnlockText e
                pure $ succeed d (moveItem need (InLock x) gs)
            | otherwise -> pure $ failWith ("the player does not have the " <> need)
          _ -> pure $ failWith "it cannot be unlocked"

  where
    succeed d gs' = (Outcome True d, gs')
    failWith d    = (Outcome False (Just d), gs)

-- Helpers ------------------------------------------------------------------

norm :: Text -> Text
norm = T.toLower . T.strip

matches :: ItemId -> Item -> Bool
matches iid it = norm (itemId it) == norm iid

findItem :: ItemId -> [Item] -> Maybe Item
findItem iid = find (matches iid)
