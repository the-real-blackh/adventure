{-# LANGUAGE OverloadedStrings #-}

-- | The game logic: the rooms, items and exits, and how they behave
-- given the current "State". Everything other than ids is computed in the
-- 'Logic' monad, so it can depend on anything in the game state.
--
-- Everything here is owned by the game engine. The LLM only ever sees a
-- JSON view of it (see "Prompt"), and some fields — such as which item
-- unlocks a door — are engine-only and never shown to the LLM.
module World
  ( RoomId
  , ItemId
  , ExitId
  , Logic
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

import           Control.Monad.Reader (Reader, asks)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text            as T

import           State

-- | A computation that can read the game state.
type Logic = Reader GameState

-- | Whether an item can be picked up. 'Fixed' carries the reason it can't.
data Portability = Portable | Fixed Text
  deriving (Show)

data Item = Item
  { itemId          :: ItemId
  , itemDescription :: Logic Text
  , itemRules       :: Logic (Maybe Text)  -- ^ Guidance for the LLM about this item
  , itemPortability :: Logic Portability   -- ^ Engine-only
  , itemRevealText  :: Logic (Maybe Text)  -- ^ Engine-only: description returned on a successful reveal
  }

data Exit = Exit
  { exitId          :: Maybe ExitId        -- ^ Needed for exits that can be unlocked, e.g. "door"
  , exitDirection   :: Text
  , exitTo          :: Logic RoomId
  , exitRules       :: Logic (Maybe Text)  -- ^ Guidance for the LLM about this exit
  , exitRequires    :: Logic (Maybe ItemId) -- ^ Engine-only: item that unlocks it; Nothing means always open
  , exitBlockedText :: Logic Text          -- ^ Engine-only: description when the way is blocked
  , exitUnlockText  :: Logic (Maybe Text)  -- ^ Engine-only: description on a successful unlock
  }

data Room = Room
  { roomId          :: RoomId
  , roomDescription :: Logic Text
  , roomExits       :: Logic [Exit]
  }

currentRoom :: Logic Room
currentRoom = do
  loc <- asks gsLocation
  case Map.lookup loc rooms of
    Just r  -> pure r
    Nothing -> error ("world error: unknown room " ++ T.unpack loc)

-- | The items in a room, including hidden ones beneath items in the room.
roomItems :: RoomId -> Logic [Item]
roomItems rid = do
  gs <- asks id
  let inRoom (InRoom r)  = r == rid
      inRoom (Under iid) = maybe False inRoom (placeOf iid gs)
      inRoom _           = False
  itemsWhere inRoom

inventory :: Logic [Item]
inventory = itemsWhere (== Carried)

itemsWhere :: (Place -> Bool) -> Logic [Item]
itemsWhere p = do
  places <- asks gsItems
  pure [ it | (iid, pl) <- Map.toList places, p pl, Just it <- [Map.lookup iid items] ]

-- | False: the LLM knows about it but must not mention it.
itemVisible :: Item -> Logic Bool
itemVisible it = do
  pl <- asks (placeOf (itemId it))
  pure $ case pl of
    Just (Under _) -> False
    _              -> True

-- | An exit can be used if it needs no key, or its key is in its lock.
exitTraversable :: Exit -> Logic Bool
exitTraversable e = do
  req <- exitRequires e
  case (req, exitId e) of
    (Nothing,   _)       -> pure True
    (Just need, Just x)  -> (== Just (InLock x)) <$> asks (placeOf need)
    (Just _,    Nothing) -> pure False

-- | An exit that is always open.
openExit :: Text -> Room -> Exit
openExit dir to = Exit
  { exitId          = Nothing
  , exitDirection   = dir
  , exitTo          = pure (roomId to)
  , exitRules       = pure Nothing
  , exitRequires    = pure Nothing
  , exitBlockedText = pure ""
  , exitUnlockText  = pure Nothing
  }

initialState :: GameState
initialState = GameState
  { gsLocation = roomId path
  , gsItems    = Map.fromList
      [ (itemId rock, InRoom (roomId river))
      , (itemId key,  Under (itemId rock))
      ]
  }

rooms :: Map RoomId Room
rooms = Map.fromList [ (roomId r, r) | r <- [path, river, grassy, garden] ]

items :: Map ItemId Item
items = Map.fromList [ (itemId i, i) | i <- [rock, key] ]

-- Rooms, each followed by the items that start in it ------------------------

path :: Room
path = Room
  { roomId          = "path"
  , roomDescription = pure "A gravel garden path, surrounded on every side by a wide, neatly kept lawn."
  , roomExits       = pure [openExit "north" river]
  }

river :: Room
river = Room
  { roomId          = "river"
  , roomDescription = pure "The bank of a slow, clear river. The ground here is soft and muddy."
  , roomExits       = pure [openExit "south" path, openExit "north" grassy]
  }

rock :: Item
rock = Item
  { itemId          = "rock"
  , itemDescription = pure "a small mossy rock by the river"
  , itemRules       = pure $ Just "It is half-buried in the mud. The player can lift it or look under it."
  , itemPortability = pure $ Fixed "the rock is half-buried in the mud and won't come free"
  , itemRevealText  = pure Nothing
  }

key :: Item
key = Item
  { itemId          = "key"
  , itemDescription = pure "an old brass key"
  , itemRules       = do
      v <- itemVisible key
      if v
          then pure Nothing
          else pure $ Just "Reveal if the player looks under, lifts, moves or turns over the rock."
  , itemPortability = pure Portable
  , itemRevealText  = pure (Just "the player found a key glinting in the mud under the rock")
  }

grassy :: Room
grassy = Room
  { roomId          = "grassy area"
  , roomDescription = pure "A grassy area with an old stone wall, thickly covered in ivy, blocks the way. Set into the wall is a weathered wooden door."
  , roomExits       = pure [door, openExit "south" river]
  }

door :: Exit
door = Exit
  { exitId          = Just "door"
  , exitDirection   = "north"
  , exitTo          = pure (roomId garden)
  , exitRules       = do
      t <- exitTraversable door
      if t
          then pure $ Just "You can go through the door."
          else pure $ Just "A locked wooden door in the wall. The player can unlock this door if they have the key. They must explicitly say to unlock it."
  , exitRequires    = pure (Just (itemId key))
  , exitBlockedText = pure "the door is locked"
  , exitUnlockText  = pure (Just "the key turns stiffly in the lock and the door swings open")
  }

garden :: Room
garden = Room
  { roomId          = "garden"
  , roomDescription = pure "A beautiful garden, overflowing with flowers of every colour."
  , roomExits       = pure [openExit "south" grassy]
  }
