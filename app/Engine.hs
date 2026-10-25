{-# LANGUAGE OverloadedStrings #-}

-- | The game engine: applies actions requested by the LLM and enforces
-- the rules. The LLM proposes, the engine disposes.
module Engine
  ( Action (..)
  , Outcome (..)
  , runAction
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.List  (find)
import           Data.Text  (Text)
import qualified Data.Text  as T

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
runAction action gs = case action of

  Go dir -> case find (\exit -> exitDirection exit == norm dir || exitId exit == Just (norm dir)) exits of
    Nothing -> failWith ("there is no way to go " <> dir <> " from here")
    Just e
      | not (exitTraversable gs e) -> failWith (exitBlockedText e)
      | otherwise                  -> succeed Nothing gs { gsLocation = exitTo e }

  Take iid -> case findItem iid here of
    Just it | itemVisible gs it -> case itemPortability it of
      Fixed why -> failWith why
      Portable  -> succeed Nothing (moveItem (itemId it) Carried gs)
    _ | any (matches iid) carried -> failWith "the player is already carrying that"
      | otherwise                 -> failWith "there is nothing like that here"

  Drop iid -> case findItem iid carried of
    Nothing -> failWith "the player is not carrying that"
    Just it -> succeed Nothing (moveItem (itemId it) (InRoom (gsLocation gs)) gs)

  Reveal iid -> case findItem iid here of
    Nothing -> failWith "there is nothing like that to be found here"
    Just it
      | itemVisible gs it -> failWith "that is already in plain sight"
      | otherwise         -> succeed (itemRevealText it) (moveItem (itemId it) (InRoom (gsLocation gs)) gs)

  Unlock eid -> case find (maybe False ((== norm eid) . norm) . exitId) exits of
    Nothing -> failWith "there is nothing like that to unlock here"
    Just e
      | exitTraversable gs e -> failWith "it is already unlocked"
      | Just need <- exitRequires e, Just x <- exitId e ->
          if any (matches need) carried
            then succeed (exitUnlockText e) (moveItem need (InLock x) gs)
            else failWith ("the player does not have the " <> need)
      | otherwise -> failWith "it cannot be unlocked"

  where
    here    = roomItems (gsLocation gs) gs
    carried = inventory gs
    exits   = roomExits (currentRoom gs)

    succeed d gs' = (Outcome True d, gs')
    failWith d    = (Outcome False (Just d), gs)

-- Helpers ------------------------------------------------------------------

norm :: Text -> Text
norm = T.toLower . T.strip

matches :: ItemId -> Item -> Bool
matches iid it = norm (itemId it) == norm iid

findItem :: ItemId -> [Item] -> Maybe Item
findItem iid = find (matches iid)
