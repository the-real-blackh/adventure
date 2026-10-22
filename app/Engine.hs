{-# LANGUAGE OverloadedStrings #-}

-- | The game engine: applies actions requested by the LLM and enforces
-- the rules. The LLM proposes, the engine disposes.
module Engine
  ( Action (..)
  , Outcome (..)
  , runAction
  ) where

import           Data.Aeson      (ToJSON (..), object, (.=))
import           Data.List       (find)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T

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

  Go dir -> case find ((== norm dir) . norm . exitDirection) exits of
    Nothing -> failWith ("there is no way to go " <> dir <> " from here")
    Just e
      | not (exitTraversable e) -> failWith (exitBlockedText e)
      | otherwise               -> succeed Nothing gs { gsLocation = exitTo e }

  Take iid -> case findItem iid items of
    Just it | itemVisible it -> case itemPortability it of
      Fixed why -> failWith why
      Portable  -> succeed Nothing $
        updateRoom (removeItem it) gs { gsInventory = gsInventory gs ++ [it] }
    _ | any (matches iid) (gsInventory gs) -> failWith "the player is already carrying that"
      | otherwise                          -> failWith "there is nothing like that here"

  Drop iid -> case findItem iid (gsInventory gs) of
    Nothing -> failWith "the player is not carrying that"
    Just it -> succeed Nothing $
      updateRoom (\r -> r { roomItems = roomItems r ++ [it { itemVisible = True }] })
        gs { gsInventory = filter (not . sameItem it) (gsInventory gs) }

  Reveal iid -> case findItem iid items of
    Nothing -> failWith "there is nothing like that to be found here"
    Just it
      | itemVisible it -> failWith "that is already in plain sight"
      | otherwise      -> succeed (itemRevealText it) (updateRoom (setVisible it) gs)

  Unlock eid -> case find (maybe False ((== norm eid) . norm) . exitId) exits of
    Nothing -> failWith "there is nothing like that to unlock here"
    Just e
      | exitTraversable e -> failWith "it is already unlocked"
      | Just need <- exitRequires e
      , not (any (matches need) (gsInventory gs))
                          -> failWith ("the player does not have the " <> need)
      | otherwise         -> succeed (exitUnlockText e) (updateRoom (unlockExit e) gs)

  where
    room  = currentRoom gs
    items = roomItems room
    exits = roomExits room

    succeed d gs' = (Outcome True d, gs')
    failWith d    = (Outcome False (Just d), gs)

-- Helpers ------------------------------------------------------------------

norm :: Text -> Text
norm = T.toLower . T.strip

matches :: ItemId -> Item -> Bool
matches iid it = norm (itemId it) == norm iid

findItem :: ItemId -> [Item] -> Maybe Item
findItem iid = find (matches iid)

sameItem :: Item -> Item -> Bool
sameItem a b = itemId a == itemId b

-- | Modify the room the player is currently in.
updateRoom :: (Room -> Room) -> GameState -> GameState
updateRoom f gs = gs { gsRooms = Map.adjust f (gsLocation gs) (gsRooms gs) }

removeItem :: Item -> Room -> Room
removeItem it r = r { roomItems = filter (not . sameItem it) (roomItems r) }

setVisible :: Item -> Room -> Room
setVisible it r = r { roomItems = map reveal (roomItems r) }
  where reveal i = if sameItem it i then i { itemVisible = True } else i

unlockExit :: Exit -> Room -> Room
unlockExit e r = r { roomExits = map open (roomExits r) }
  where open x = if exitId x == exitId e then x { exitTraversable = True } else x
