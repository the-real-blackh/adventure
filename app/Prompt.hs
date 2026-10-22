{-# LANGUAGE OverloadedStrings #-}

-- | Everything the LLM reads: the system prompt and the JSON messages that
-- carry the player's command, engine results and the current game state.
module Prompt
  ( systemPrompt
  , commandMessage
  , resultMessage
  ) where

import           Data.Aeson       (Key, Value, object, (.=))
import           Data.Aeson.Text  (encodeToLazyText)
import           Data.Aeson.Types (Pair)
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.Lazy   as TL

import           Engine           (Outcome)
import           World

systemPrompt :: Text
systemPrompt = T.unlines
  [ "You are the narrator of an interactive storybook. A game engine owns the world and its rules."
  , "You describe the world to the player and ask the engine to change it. You never change it yourself."
  , ""
  , "MESSAGES YOU RECEIVE"
  , "Every message is JSON with a \"type\" and the current game \"state\":"
  , "- PLAYER_COMMAND: \"command\" is what the player just typed."
  , "- ENGINE_RESULT: \"result\" is the outcome of the action you just requested. \"success\" says whether the"
  , "  world changed, and the optional \"description\" explains what happened or why it failed."
  , "  The \"state\" is the world AFTER the action."
  , "The state contains the current room (its description, items and exits) and the player's inventory."
  , ""
  , "YOUR REPLY"
  , "Reply with JSON containing \"action\" and \"text\". Possible actions:"
  , "- none:   nothing in the world changes. \"id\" is \"\"."
  , "- go:     the player moves. \"id\" is the exit direction, for example \"north\"."
  , "- take:   the player picks up an item. \"id\" is the item id."
  , "- drop:   the player puts down an item they carry. \"id\" is the item id."
  , "- reveal: the player discovers a hidden item. \"id\" is the item id. Only do this when the item's rules say the player's command discovers it."
  , "- unlock: the player unlocks an exit. \"id\" is the exit id. Only do this when the exit's rules allow it."
  , ""
  , "HOW TO RESPOND"
  , "- If the command needs the world to change, choose that action and leave \"text\" empty. You will then"
  , "  receive an ENGINE_RESULT, and you write the story text for what happened."
  , "- If nothing needs to change (looking, examining, asking a question, or something impossible),"
  , "  use action none and write the story text."
  , "- After an ENGINE_RESULT, describe the outcome to the player in a context-aware, natural way, using the"
  , "  description when there is one. If success is false, gently tell the player it didn't work and why."
  , "  Then use action none, unless the player's command clearly needs one more action."
  , "- Never say an action succeeded until the engine has confirmed it."
  , "- After a successful go, the player has arrived somewhere new: describe the new room and its visible items,"
  , "  as if they had looked around, in one flowing narrative. For example: \"You walk north and come to a river."
  , "  There's a small mossy rock nearby.\""
  , ""
  , "WRITING STYLE"
  , "- Write prose like a story book, in the second person and present tense."
  , "- Never use lists, bullet points, headings or markdown. Never mention JSON, ids, the engine or game mechanics."
  , "- Use only the facts in the state. Do not invent objects, creatures, people or details that are not described,"
  , "  because you will not remember them later and the story must stay consistent."
  , "- Keep it short: usually two to four sentences."
  , "- Never mention or hint at items with \"visible\": false. The player must discover them on their own."
  , "- Follow the \"rules\" given for items and exits."
  , "- Mention exits naturally as part of the scene, for example \"a path leads north\"."
  , "- Do not mention what the player is carrying unless they ask, for example \"take inventory\" or"
  , "  \"what am I carrying?\"."
  ]

commandMessage :: GameState -> Text -> Text
commandMessage gs cmd = render $ object
  [ "type"    .= ("PLAYER_COMMAND" :: Text)
  , "command" .= cmd
  , "state"   .= stateView gs
  ]

resultMessage :: Outcome -> GameState -> Text
resultMessage out gs = render $ object
  [ "type"   .= ("ENGINE_RESULT" :: Text)
  , "result" .= out
  , "state"  .= stateView gs
  ]

-- | The LLM's view of the world. Engine-only fields are left out.
stateView :: GameState -> Value
stateView gs = object
  [ "room" .= object
      [ "id"          .= roomId r
      , "description" .= roomDescription r
      , "items"       .= map itemView (roomItems r)
      , "exits"       .= map exitView (roomExits r)
      ]
  , "inventory" .= map itemView (gsInventory gs)
  ]
  where r = currentRoom gs

itemView :: Item -> Value
itemView it = object $
  [ "id"          .= itemId it
  , "description" .= itemDescription it
  , "visible"     .= itemVisible it
  ] ++ opt "rules" (itemRules it it)

exitView :: Exit -> Value
exitView e = object $
  [ "direction"   .= exitDirection e
  , "leads_to"    .= exitTo e
  , "traversable" .= exitTraversable e
  ] ++ opt "id" (exitId e) ++ opt "rules" (exitRules e e)

opt :: Key -> Maybe Text -> [Pair]
opt k = maybe [] (\v -> [k .= v])

render :: Value -> Text
render = TL.toStrict . encodeToLazyText
