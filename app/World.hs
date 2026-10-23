{-# LANGUAGE OverloadedStrings #-}

-- | The game world: its data types and the initial state.
--
-- Everything here is owned by the game engine. The LLM only ever sees a
-- JSON view of it (see "Prompt"), and some fields — such as which item
-- unlocks a door — are engine-only and never shown to the LLM.
module World
  ( RoomId
  , ItemId
  , Portability (..)
  , Item (..)
  , Exit (..)
  , Room (..)
  , GameState (..)
  , currentRoom
  , initialWorld
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T

type RoomId = Text
type ItemId = Text

-- | Whether an item can be picked up. 'Fixed' carries the reason it can't.
data Portability = Portable | Fixed Text
  deriving (Show)

data Item = Item
  { itemId          :: ItemId
  , itemDescription :: Text
  , itemVisible     :: Bool        -- ^ False: the LLM knows about it but must not mention it
  , itemRules       :: Item -> Maybe Text  -- ^ Guidance for the LLM about this item
  , itemPortability :: Portability -- ^ Engine-only
  , itemRevealText  :: Maybe Text  -- ^ Engine-only: description returned on a successful reveal
  }

data Exit = Exit
  { exitId          :: Maybe Text  -- ^ Needed for exits that can be unlocked, e.g. "door"
  , exitDirection   :: Text
  , exitTo          :: RoomId
  , exitTraversable :: Bool
  , exitRules       :: Exit -> Maybe Text  -- ^ Guidance for the LLM about this exit
  , exitRequires    :: Maybe ItemId -- ^ Engine-only: item needed in the inventory to unlock
  , exitBlockedText :: Text        -- ^ Engine-only: description when the way is blocked
  , exitUnlockText  :: Maybe Text  -- ^ Engine-only: description on a successful unlock
  }

data Room = Room
  { roomId          :: RoomId
  , roomDescription :: Text
  , roomItems       :: [Item]
  , roomExits       :: [Exit]
  }

data GameState = GameState
  { gsLocation  :: RoomId
  , gsRooms     :: Map RoomId Room
  , gsInventory :: [Item]
  }

currentRoom :: GameState -> Room
currentRoom gs = case Map.lookup (gsLocation gs) (gsRooms gs) of
  Just r  -> r
  Nothing -> error ("world error: unknown room " ++ T.unpack (gsLocation gs))

-- | An exit that is always open.
openExit :: Text -> RoomId -> Exit
openExit dir to = Exit
  { exitId          = Nothing
  , exitDirection   = dir
  , exitTo          = to
  , exitTraversable = True
  , exitRules       = const Nothing
  , exitRequires    = Nothing
  , exitBlockedText = ""
  , exitUnlockText  = Nothing
  }

initialWorld :: GameState
initialWorld = GameState
  { gsLocation  = "path"
  , gsRooms     = Map.fromList [ (roomId r, r) | r <- [path, river, grassy, garden] ]
  , gsInventory = []
  }
  where
    path = Room
      { roomId          = "path"
      , roomDescription = "A gravel garden path, surrounded on every side by a wide, neatly kept lawn."
      , roomItems       = []
      , roomExits       = [openExit "north" "river"]
      }

    river = Room
      { roomId          = "river"
      , roomDescription = "The bank of a slow, clear river. The ground here is soft and muddy."
      , roomItems       = [rock, key]
      , roomExits       = [openExit "south" "path", openExit "north" "grassy area"]
      }

    grassy = Room
      { roomId          = "grassy area"
      , roomDescription = "A grassy area with an old stone wall, thickly covered in ivy, blocks the way. Set into the wall is a weathered wooden door."
      , roomItems       = []
      , roomExits       = [door, openExit "south" "river"]
      }

    garden = Room
      { roomId          = "garden"
      , roomDescription = "A beautiful garden, overflowing with flowers of every colour."
      , roomItems       = []
      , roomExits       = [openExit "south" "grassy area"]
      }

    rock = Item
      { itemId          = "rock"
      , itemDescription = "a small mossy rock by the river"
      , itemVisible     = True
      , itemRules       = const $ Just "It is half-buried in the mud. The player can lift it or look under it."
      , itemPortability = Fixed "the rock is half-buried in the mud and won't come free"
      , itemRevealText  = Nothing
      }

    key = Item
      { itemId          = "key"
      , itemDescription = "an old iron key"
      , itemVisible     = False
      , itemRules       = \it ->
          if itemVisible it
              then Nothing
              else Just "Reveal if the player looks under, lifts, moves or turns over the rock."
      , itemPortability = Portable
      , itemRevealText  = Just "the player found a key glinting in the mud under the rock"
      }

    door = Exit
      { exitId          = Just "door"
      , exitDirection   = "north"
      , exitTo          = "garden"
      , exitTraversable = False
      , exitRules       = \e ->
          if exitTraversable e
              then Just "You can go through the door."
              else Just "A locked wooden door in the wall. The player can unlock this door if they have the key. They must explicitly say to unlock it."
      , exitRequires    = Just "key"
      , exitBlockedText = "the door is locked"
      , exitUnlockText  = Just "the key turns stiffly in the lock and the door swings open"
      }
