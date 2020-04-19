module State.Internals where

import           Data.Validation                ( Validation(Failure, Success) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Text.Regex.TDFA                ( (=~) )
import           Database.PostgreSQL.Simple     ( FromRow )

-- |The scryfall card id
newtype CardId = CardId String deriving (Eq, Show, Ord)
newtype DeckId = DeckId Int deriving (Eq, Show, Ord)
newtype PlayerId = PlayerId Int deriving (Eq, Show, Ord)

data Card = Card { scryfallId :: CardId, qty :: Int, cardDeckId :: DeckId } deriving (Eq, Show)
data Deck = Deck { deckId :: DeckId, deckName :: String, dateModified :: Int, deckPlayerId :: PlayerId } deriving (Eq, Show)
data Player = Player {
  playerId :: PlayerId
  , playerName :: String
  , email :: String
  , active :: Bool
  } deriving (Eq, Show)

data State = State {
  players :: Map PlayerId Player
  , decks :: Map DeckId Deck
  , cards :: Map CardId Card
}

mkCardId :: String -> Validation [String] CardId
mkCardId str
  | str =~ uuidRegex = Success (CardId str)
  | otherwise = Failure
    [str ++ " must be a UUID to make a card Id, as it's from Scryfall"]
 where
  uuidRegex
    = "^[0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}$"

mkCard :: String -> Int -> Int -> Validation [String] Card
mkCard id qty deckId = pure Card <*> mkCardId id <*> qty' <*> pure
  (DeckId deckId)
 where
  qty'
    | qty > 0 = Success qty
    | otherwise = Failure
      [ "Quantity "
        ++ show qty
        ++ " of card id "
        ++ id
        ++ " must be greater than zero"
      ]

mkDeck :: Int -> String -> Int -> Int -> Validation [String] Deck
mkDeck id name modified playerId =
  pure Deck <*> pure (DeckId id) <*> name' <*> pure modified <*> pure
    (PlayerId playerId)
 where
  name'
    | name == "" = Failure
      ["Deck with id " ++ show id ++ "Cannot have an empty name"]
    | otherwise = Success name

newtype DB a = DB a




-- addCard :: State -> Int -> DeckId -> CardId -> Validation [String] State
-- addCard state qty deckId cardId =
--   fmap addCard $ pure Card <*> cardId' <*> qty' <*> deckId'
--  where
--   cardId' = case Map.lookup cardId (cards state) of
--     Just _  -> Failure ["This card Id already exists"]
--     Nothing -> Success cardId

--   deckId' = case Map.lookup deckId (decks state) of
--     Just _  -> Success deckId
--     Nothing -> Failure ["The deck Id does not exist"]

--   addCard card =
--     state { cards = Map.insert (scryfallId card) card (cards state) }

--newtype SudoActions = ResetMeta PlayerId PlayerId


