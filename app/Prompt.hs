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
import           State
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
  , "A PLAYER_COMMAND may also include \"your_previous_reply\": the story text YOU wrote to the player in"
  , "response to their previous command. Use it only to understand what the player is referring to, for"
  , "example \"it\", \"that\", \"there\" or \"the thing you mentioned\". Do not repeat it. If it disagrees with"
  , "the state, the state is correct, because the world may have changed since."
  , ""
  , "YOUR REPLY"
  , "Reply with JSON containing \"reasoning\", \"text\" and \"action\", in that order."
  , "\"reasoning\" is private notes the player never sees. Write up to ten sentences to check what the"
  , "player means, which action fits (or why none does), and an analysis of what system prompt rules are relevant especially IMPORTANT ones."
  , "Don't write the story there."
  , "Possible actions:"
  , "- none:   nothing in the world changes. \"id\" is \"\"."
  , "- go:     the player moves. \"id\" is the exit direction, for example \"north\"."
  , "- take:   the player picks up an item. \"id\" is the item id."
  , "- drop:   the player puts down an item they carry. \"id\" is the item id."
  , "- reveal: the player discovers a hidden item. \"id\" is the item id. Only do this when the item's rules say the player's command discovers it."
  , "- unlock: the player unlocks an exit. \"id\" is the exit id. Only do this when the exit's rules allow it."
  , ""
  , "SYNONYMS"
  , "- \"get item\" means \"take item\""
  , ""
  , "HOW TO RESPOND"
  , "- If the command needs the world to change, choose that action. IMPORTANT: The text should be empty or describe an attempt and not assume it succeeded. You will then"
  , "  receive an ENGINE_RESULT. Do up to three actions to fulfill the user command. End with no action and the story text for what happened."
  , "- If nothing needs to change (looking, examining, asking a question, or something impossible),"
  , "  use action none and write the story text."
  , "- After an ENGINE_RESULT, describe the outcome to the player in a context-aware, natural way, using the"
  , "  description when there is one. If success is false, gently tell the player it didn't work and why."
  , "  Then use action none, unless the player's command clearly needs one more action."
  , "- Never say an action succeeded until the engine has confirmed it."
  , "- After a successful go, the player has arrived somewhere new: describe the new room and its visible items and exits,"
  , "  as if they had looked around, in one flowing narrative. For example: \"You walk north and come to a river."
  , "  There's a small mossy rock nearby. Paths lead to the north and south.\""
  , "- If the command is empty, give a creative descrition of something unimportant happening inspired by the room description."
  , "- If there is no way to achieve the command, say so in a quirky way."
  , ""
  , "WRITING STYLE"
  , "- Write prose like a story book, in the second person and present tense."
  , "- Never use lists, bullet points, headings or markdown. Never mention JSON, ids, the engine or game mechanics."
  , "- Use only the facts in the state. Do not invent objects, creatures, people or details that are not described,"
  , "  because you will not remember them later and the story must stay consistent."
  , "- Keep it short: usually two to four sentences."
  , "- IMPORTANT: Never mention or hint at items with \"visible\": false. The player must discover them on their own."
  , "- Follow the \"rules\" given for items and exits."
  , "- Mention exits naturally as part of the scene, for example \"a path leads north\"."
  , "- Do not mention what the player is carrying unless they ask, for example \"take inventory\" or"
  , "  \"what am I carrying?\"."
  ]

-- | The player's command. 'prev' is the story text the narrator wrote for
-- the previous command; it is left out when empty (at the very start).
commandMessage :: GameState -> Text -> Text -> Text
commandMessage gs prev cmd = render $ object $
  [ "type"    .= ("PLAYER_COMMAND" :: Text)
  , "command" .= cmd
  , "state"   .= stateView gs
  ] ++ (if T.null prev then [] else ["your_previous_reply" .= prev])

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
      , "items"       .= map (itemView gs) (roomItems (roomId r) gs)
      , "exits"       .= map (exitView gs) (roomExits r)
      ]
  , "inventory" .= map (itemView gs) (inventory gs)
  ]
  where r = currentRoom gs

itemView :: GameState -> Item -> Value
itemView gs it = object $
  [ "id"          .= itemId it
  , "description" .= itemDescription it
  , "visible"     .= itemVisible gs it
  ] ++ opt "rules" (itemRules it gs)

exitView :: GameState -> Exit -> Value
exitView gs e = object $
  [ "direction"   .= exitDirection e
  , "leads_to"    .= exitTo e
  , "traversable" .= exitTraversable gs e
  ] ++ opt "id" (exitId e) ++ opt "rules" (exitRules e gs)

opt :: Key -> Maybe Text -> [Pair]
opt k = maybe [] (\v -> [k .= v])

render :: Value -> Text
render = TL.toStrict . encodeToLazyText
