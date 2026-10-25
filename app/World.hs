{-# LANGUAGE OverloadedStrings #-}

-- | The game logic: the rooms, items and exits, and how they behave
-- given the current "State".
--
-- Everything here is owned by the game engine. The LLM only ever sees a
-- JSON view of it (see "Prompt"), and some fields — such as which item
-- unlocks a door — are engine-only and never shown to the LLM.
module World
  ( RoomId
  , ItemId
  , ExitId
  , Portability (..)
  , Item (..)
  , Exit (..)
  , Room (..)
  , currentRoom
  , roomItems
  , inventory
  , itemVisible
  , exitTraversable
  , initialState
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T

import           State

-- | Whether an item can be picked up. 'Fixed' carries the reason it can't.
data Portability = Portable | Fixed Text
  deriving (Show)

data Item = Item
  { itemId          :: ItemId
  , itemDescription :: Text
  , itemRules       :: GameState -> Maybe Text  -- ^ Guidance for the LLM about this item
  , itemPortability :: Portability -- ^ Engine-only
  , itemRevealText  :: Maybe Text  -- ^ Engine-only: description returned on a successful reveal
  }

data Exit = Exit
  { exitId          :: Maybe ExitId -- ^ Needed for exits that can be unlocked, e.g. "door"
  , exitDirection   :: Text
  , exitTo          :: RoomId
  , exitRules       :: GameState -> Maybe Text  -- ^ Guidance for the LLM about this exit
  , exitRequires    :: Maybe ItemId -- ^ Engine-only: item that unlocks it; Nothing means always open
  , exitBlockedText :: Text        -- ^ Engine-only: description when the way is blocked
  , exitUnlockText  :: Maybe Text  -- ^ Engine-only: description on a successful unlock
  }

data Room = Room
  { roomId          :: RoomId
  , roomDescription :: Text
  , roomExits       :: [Exit]
  }

currentRoom :: GameState -> Room
currentRoom gs = case Map.lookup (gsLocation gs) rooms of
  Just r  -> r
  Nothing -> error ("world error: unknown room " ++ T.unpack (gsLocation gs))

-- | The items in a room, including hidden ones beneath items in the room.
roomItems :: RoomId -> GameState -> [Item]
roomItems rid gs = itemsWhere inRoom gs
  where
    inRoom (InRoom r)  = r == rid
    inRoom (Under iid) = maybe False inRoom (placeOf iid gs)
    inRoom _           = False

inventory :: GameState -> [Item]
inventory = itemsWhere (== Carried)

itemsWhere :: (Place -> Bool) -> GameState -> [Item]
itemsWhere p gs =
  [ it | (iid, pl) <- Map.toList (gsItems gs), p pl, Just it <- [Map.lookup iid items] ]

-- | False: the LLM knows about it but must not mention it.
itemVisible :: GameState -> Item -> Bool
itemVisible gs it = case placeOf (itemId it) gs of
  Just (Under _) -> False
  _              -> True

-- | An exit can be used if it needs no key, or its key is in its lock.
exitTraversable :: GameState -> Exit -> Bool
exitTraversable gs e = case (exitRequires e, exitId e) of
  (Nothing,   _)       -> True
  (Just need, Just x)  -> placeOf need gs == Just (InLock x)
  (Just _,    Nothing) -> False

-- | An exit that is always open.
openExit :: Text -> RoomId -> Exit
openExit dir to = Exit
  { exitId          = Nothing
  , exitDirection   = dir
  , exitTo          = to
  , exitRules       = const Nothing
  , exitRequires    = Nothing
  , exitBlockedText = ""
  , exitUnlockText  = Nothing
  }

initialState :: GameState
initialState = GameState
  { gsLocation = "path"
  , gsItems    = Map.fromList
      [ ("rock", InRoom "river")
      , ("key",  Under "rock")
      ]
  }

rooms :: Map RoomId Room
rooms = Map.fromList [ (roomId r, r) | r <- [path, river, grassy, garden] ]
  where
    path = Room
      { roomId          = "path"
      , roomDescription = "A gravel garden path, surrounded on every side by a wide, neatly kept lawn."
      , roomExits       = [openExit "north" "river"]
      }

    river = Room
      { roomId          = "river"
      , roomDescription = "The bank of a slow, clear river. The ground here is soft and muddy."
      , roomExits       = [openExit "south" "path", openExit "north" "grassy area"]
      }

    grassy = Room
      { roomId          = "grassy area"
      , roomDescription = "A grassy area with an old stone wall, thickly covered in ivy, blocks the way. Set into the wall is a weathered wooden door."
      , roomExits       = [door, openExit "south" "river"]
      }

    garden = Room
      { roomId          = "garden"
      , roomDescription = "A beautiful garden, overflowing with flowers of every colour."
      , roomExits       = [openExit "south" "grassy area"]
      }

    door = Exit
      { exitId          = Just "door"
      , exitDirection   = "north"
      , exitTo          = "garden"
      , exitRules       = \gs ->
          if exitTraversable gs door
              then Just "You can go through the door."
              else Just "A locked wooden door in the wall. The player can unlock this door if they have the key. They must explicitly say to unlock it."
      , exitRequires    = Just "key"
      , exitBlockedText = "the door is locked"
      , exitUnlockText  = Just "the key turns stiffly in the lock and the door swings open"
      }

items :: Map ItemId Item
items = Map.fromList [ (itemId i, i) | i <- [rock, key] ]
  where
    rock = Item
      { itemId          = "rock"
      , itemDescription = "a small mossy rock by the river"
      , itemRules       = const $ Just "It is half-buried in the mud. The player can lift it or look under it."
      , itemPortability = Fixed "the rock is half-buried in the mud and won't come free"
      , itemRevealText  = Nothing
      }

    key = Item
      { itemId          = "key"
      , itemDescription = "an old brass key"
      , itemRules       = \gs ->
          if itemVisible gs key
              then Nothing
              else Just "Reveal if the player looks under, lifts, moves or turns over the rock."
      , itemPortability = Portable
      , itemRevealText  = Just "the player found a key glinting in the mud under the rock"
      }
