{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The game logic: the rooms, items and exits, and how they behave
-- given the current "State". Everything other than ids is computed in the
-- 'Logic' monad, so it can depend on anything in the game state. Things
-- that change the game state are written in the 'Action' monad.
--
-- Everything here is owned by the game engine. The LLM only ever sees a
-- JSON view of it (see "Prompt"), and some fields — such as which item
-- unlocks a door — are engine-only and never shown to the LLM.
module World
  ( RoomId
  , ItemId
  , ExitId
  , Logic
  , Action
  , ReadState (..)
  , runAction
  , logic
  , setFlag
  , clearFlag
  , getFlag
  , moveTo
  , reveal
  , Portability (..)
  , Item (..)
  , Exit (..)
  , Room (..)
  , currentRoom
  , roomItems
  , inventory
  , carrying
  , isIn
  , here
  , itemVisible
  , exitTraversable
  , initialState
  ) where

import           Control.Monad.Reader (Reader, ask, asks, runReader)
import           Control.Monad.State  (State, get, gets, put, runState)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (isNothing)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T

import           State

-- | A computation that can read the game state.
type Logic = Reader GameState

-- | A computation that can change the game state. The 'Bool' starts as
-- False and becomes True if anything in the game state has changed.
type Action = State (GameState, Bool)

-- | Run an action, giving its result, the new game state, and whether
-- anything changed.
runAction :: Action a -> GameState -> (a, GameState, Bool)
runAction act gs = let (a, (gs', changed)) = runState act (gs, False) in (a, gs', changed)

-- | Monads that can read the game state.
class Monad m => ReadState m where
  readState :: m GameState

instance ReadState (Reader GameState) where
  readState = ask

instance ReadState Action where
  readState = gets fst

-- | Use game logic from within an action.
logic :: Logic a -> Action a
logic l = gets (runReader l . fst)

modifyState :: (GameState -> GameState) -> Action ()
modifyState f = do
  (gs, changed) <- get
  let gs' = f gs
  put (gs', changed || gs' /= gs)

setFlag :: Text -> Action ()
setFlag k = modifyState (\gs -> gs { gsFlags = Set.insert k (gsFlags gs) })

clearFlag :: Text -> Action ()
clearFlag k = modifyState (\gs -> gs { gsFlags = Set.delete k (gsFlags gs) })

-- | Move an item to a new place.
moveTo :: Item -> Place -> Action ()
moveTo it p = modifyState (moveItem (itemId it) p)

-- | Reveal a hidden item, putting it in plain sight in the player's current
-- room. Returns False, changing nothing, if it was already visible.
reveal :: Item -> Action Bool
reveal it = do
  v <- logic $ itemVisible it
  if v then pure False else do
    moveTo it . InRoom =<< gets (gsLocation . fst)
    pure True

getFlag :: ReadState m => Text -> m Bool
getFlag k = Set.member k . gsFlags <$> readState

-- | Whether an item can be picked up. 'Fixed' carries the reason it can't.
data Portability = Portable | Fixed Text
  deriving (Show)

data Item = Item
  { itemId          :: ItemId
  , itemDescription :: Logic Text
  , itemRules       :: Logic (Maybe Text)  -- ^ Guidance for the LLM about this item
  , itemPortability :: Logic Portability   -- ^ Engine-only
  , itemReveal      :: Action (Maybe Text) -- ^ Engine-only: the reveal command; succeeds if it changes the game state
  }

data Exit = Exit
  { exitId          :: Maybe ExitId        -- ^ Needed for exits that can be unlocked, e.g. "door"
  , exitDirection   :: Text
  , exitTo          :: Logic RoomId
  , exitRules       :: Logic (Maybe Text)  -- ^ Guidance for the LLM about this exit
  , exitBlocked     :: Logic (Maybe Text)  -- ^ Engine-only: Just a description if the way is blocked; Nothing means open
  , exitUnlock      :: Action (Maybe Text) -- ^ Engine-only: the unlock command; succeeds if it changes the game state
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

-- | Whether the player is carrying the item.
carrying :: Item -> Logic Bool
carrying it = (== Just Carried) <$> asks (placeOf (itemId it))

-- | Whether the item is in the room, including hidden beneath another item.
isIn :: Item -> Room -> Logic Bool
isIn it r = any ((== itemId it) . itemId) <$> roomItems (roomId r)

-- | Whether the item is in the player's current room.
here :: Item -> Logic Bool
here it = isIn it =<< currentRoom

-- | False: the LLM knows about it but must not mention it.
itemVisible :: Item -> Logic Bool
itemVisible it = do
  pl <- asks (placeOf (itemId it))
  pure $ case pl of
    Just (Under _) -> False
    _              -> True

-- | An exit can be used if it isn't blocked.
exitTraversable :: Exit -> Logic Bool
exitTraversable e = isNothing <$> exitBlocked e

-- | An exit that is always open.
openExit :: Text -> Room -> Exit
openExit dir to = Exit
  { exitId          = Nothing
  , exitDirection   = dir
  , exitTo          = pure (roomId to)
  , exitRules       = pure Nothing
  , exitBlocked     = pure Nothing
  , exitUnlock      = pure Nothing
  }

initialState :: GameState
initialState = GameState
  { gsLocation = roomId path
  , gsItems    = Map.fromList
      [ (itemId rock, InRoom (roomId river))
      , (itemId key,  Under (itemId rock))
      ]
  , gsFlags    = Set.empty
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
  , itemReveal      = pure (Just "that is already in plain sight")
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
  , itemReveal      = do
      revealed <- reveal key
      pure $ Just $ if revealed
        then "the player found a key glinting in the mud under the rock"
        else "that is already in plain sight"
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
  , exitBlocked     = do
      u <- getFlag "door unlocked"
      pure $ if u then Nothing else Just "the door is locked"
  , exitUnlock      = do
      u <- getFlag "door unlocked"
      if u then pure (Just "it is already unlocked") else do
        hasKey <- logic $ carrying key
        if hasKey
          then do
            setFlag "door unlocked"
            pure (Just "the key turns stiffly in the lock and the door swings open")
          else pure (Just "the player does not have the key")
  }

garden :: Room
garden = Room
  { roomId          = "garden"
  , roomDescription = pure "A beautiful garden, overflowing with flowers of every colour."
  , roomExits       = pure [openExit "south" grassy]
  }
