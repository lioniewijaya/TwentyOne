{-# OPTIONS_GHC -Wno-unused-matches #-}

module Player where
import Parser.Parser      -- This is the source for the parser from the course notes
import Cards                        -- Finally, the generic card type(s)
import TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import TwentyOne.Rules    -- Rules of the game
import Parser.Instances
import Data.Char
import Data.List
import Data.Maybe

-- *** Main function ***

-- | This function is called once it's your turn, keeps getting called until turn ends.
playCard :: PlayFunc
-- + Case 1: Start of the game, always Bid minimum amount 
playCard _ pp pinf pid Nothing hand =
    (Bid minBid, memoryStr (Memory minBid (showAction (Bid minBid)) 0 0 0 pinf))

-- + Case 2: In the middle of the game
playCard maybeCard pp pinf pid (Just memory) hand =
    case maybeCard of
    -- + Case 2.1: Start of a hand, Bid amount according to True Count
    Nothing -> decision (Bid newBid) newBid
        where
            newBid = bidAmount newCount newNumCards pp pid
    -- + Case 2.2: In the middle of a hand
    Just card -> decision finalAction lastBid
        where
            action
                -- + Case 2.2.1: Get Insurance
                | canIns && willIns = Insurance (lastBid `div` 2)
                -- + Case 2.2.2: Get actions other than Bid and Insurance
                | otherwise = playOtherThanBidAndInsurance card hand lastMemory
            canIns = getRank card == Ace && head lastActions == 'B' &&
                    lastBid `div` 2 <= pointFromId pp pid
            willIns = worthInsurance newNumFaces newNumCards
            needInitBid = action `notElem` [Stand,Hit,Insurance (lastBid `div` 2)]
            notEnoughPts = lastBid > pointFromId pp pid
            -- + Case 2.2.3: Recheck if action is possible, else Stand
            finalAction = if needInitBid && notEnoughPts then Stand else action
    where
        decision action bid = (action,newMemory action bid)
        lastMemory = getResult (parse parseMemory memory)
        lastBid = initBid lastMemory
        lastActions = lastTwoActions lastMemory
        newActions = flip updtAction lastActions
        newCards = allNewCards (lastPlayerInfo lastMemory) pinf
        newCount = updtCount (count lastMemory) newCards
        newNumCards = updtNumCards (numCards lastMemory) newCards
        newNumFaces = updtNumFaces (numFaces lastMemory) newCards
        newMemory action bid = memoryStr ( Memory
            bid (newActions action) newCount newNumCards newNumFaces pinf)

-- ************************
-- *** Helper functions ***
-- ************************

-- | Get actions other than Bid and Insurance
playOtherThanBidAndInsurance :: Card -> Hand -> Memory -> Action
playOtherThanBidAndInsurance card hand memory
    -- + Case 2.2.2.1: Proceed DoubleDown with Hit and Stand
    | checkIfDD lastActions && hitAfterDD lastActions = Stand
    | checkIfDD lastActions = Hit
    -- + Case 2.2.2.2: Has a pair of cards, play pair strategy 
    | length hand == 2 && sameRank hand = playPair card hand memory
    -- + Case 2.2.2.3: Has an Ace out of two cards, play soft hand strategy
    | length hand == 2 && hasAce hand = playSoftHand card hand memory
    -- + Case 2.2.2.4: Has no Ace out of two cards, play hard hand strategy
    | length hand == 2 = playHardHand card hand memory
    -- + Case 2.2.2.5: More than two cards
    | length hand > 2 && handCalc hand <= 11 = Hit
    | otherwise = Stand
    where
        lastActions = lastTwoActions memory

-- | Get a suitable action when player has a pair of cards 
playPair :: Card -> Hand -> Memory -> Action
playPair card hand memory
    | playerCard == 10 = Stand
    | playerCard == 8 = Split bid
    | playerBetween 2 3 && dealerBetween 2 7 = Split bid
    | playerCard == 4 && dealerBetween 5 6 = Split bid
    | playerCard == 5 && dealerBetween 2 9 = DoubleDown bid
    | playerCard == 6 && dealerBetween 2 6 = Split bid
    | playerCard == 7 && dealerBetween 2 7 = Split bid
    | playerCard == 9 && (dealerBetween 2 6 || dealerBetween 8 9) = Split bid
    | playerCard == 9 && dealerCard `elem` [1,7,10]= Stand
    | otherwise = Hit
    where
        playerCard = toPoints (head hand)
        dealerCard = toPoints card
        playerBetween a b = valBetween a playerCard b
        dealerBetween a b = valBetween a dealerCard b
        bid = initBid memory

-- | Get a suitable action when player has one Ace in one out of two cards 
playSoftHand :: Card -> Hand -> Memory -> Action
playSoftHand card hand memory
    | playerNonAceCard >= 8 = Stand
    | playerNonAceCard == 7 && (dealerCard `elem` [2,7,8]) = Stand
    | playerBetween 6 7 && dealerBetween 3 6 = DoubleDown bid
    | playerBetween 4 5 && dealerBetween 4 6 = DoubleDown bid
    | playerBetween 2 3 && dealerBetween 5 6 = DoubleDown bid
    | otherwise = Hit
    where
        playerNonAceCard = toPoints (nonAce hand)
        dealerCard = toPoints card
        playerBetween a b = valBetween a playerNonAceCard b
        dealerBetween a b = valBetween a dealerCard b
        bid = initBid memory

-- | Get a suitable action when player has no Ace out of two cards 
playHardHand :: Card -> Hand -> Memory -> Action
playHardHand card hand memory
    | playerCards >= 17 = Stand
    | playerBetween 13 16 && dealerBetween 2 6 = Stand
    | playerCards == 12 && dealerBetween 4 6 = Stand
    | playerCards == 11 && dealerBetween 2 10 = DoubleDown bid
    | playerCards == 10 && dealerBetween 2 9 = DoubleDown bid
    | playerCards == 9 && dealerBetween 3 6 = DoubleDown bid
    | otherwise = Hit
    where
        playerCards = handCalc hand
        dealerCard = toPoints card
        playerBetween a b = valBetween a playerCards b
        dealerBetween a b = valBetween a dealerCard b
        bid = initBid memory

-- | Get unique elements in a list
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x:unique (filter (x /=) xs)

-- | Compare if an orderable is between two orderables
valBetween :: Ord a => a -> a -> a -> Bool
valBetween a b c = a <= b &&  b <= c

-- | Check if list contains the same value 
allSame :: (Eq a) => [a] -> Bool
allSame l = all (== head l) (tail l)

-- | Check if hand contains cards with the same rank 
sameRank :: Hand -> Bool
sameRank = allSame . (getRank <$>)

-- | Check if hand contains Ace
hasAce :: Hand -> Bool
hasAce = foldr ((||) . (Ace ==) . getRank) False

-- | Get the first non Ace card in hand
nonAce :: Hand -> Card
nonAce [] = error "Error, ensure hand has Ace"
nonAce (card:cards) = if getRank card /= Ace then card else nonAce cards

-- | Check if card is a face card
isFace :: Card -> Bool
isFace = (`elem` [Jack, Queen, King]) . getRank

-- | Get all player info based on ID
infoFromId :: PlayerId -> [PlayerInfo] -> [PlayerInfo]
infoFromId = filter . (. getId) . (==)

-- | Combine all hands from a list of player info
allHandsFromInfo :: [PlayerInfo] -> [Card]
allHandsFromInfo = (=<<) playerInfoHand

-- | Get new cards from a player
playerNewCards :: [PlayerInfo] -> [PlayerInfo] -> PlayerId -> [Card]
playerNewCards previous current pid = currentCards \\ previousCards
    where
        previousCards = allHandsFromInfo(infoFromId pid previous)
        currentCards = allHandsFromInfo(infoFromId pid current)

-- | Get new cards from all players
allNewCards :: [PlayerInfo] -> [PlayerInfo] -> [Card]
allNewCards previous current = newCards
    where
        pids = unique (getId <$> current)
        newCards =  pids >>= playerNewCards previous current

-- | Get count value of a card
countValue :: Card -> Int
countValue card
    | rank `elem` [Two,Three,Four,Five,Six] = 1
    | rank `elem` [Ten,Ace,Jack,Queen,King] = -1
    | otherwise = 0
    where
        rank = getRank card

-- | Calculate true count based on running count and remaining deck 
trueCount :: Int -> Int -> Int
trueCount count cards_used = if anyZero then 0 else count `div` remaining_deck
    where
        anyZero = count == 0 || remaining_deck == 0
        remaining_deck = ((numDecks*52)-cards_used) `div` 52

-- | Get ideal bid based on true count
bidByTC :: Int -> Int
bidByTC tc
    | tc < 2 = betUnit
    | tc == 3 = betUnit*3
    | tc == 4 = betUnit*5
    | tc == 5 = betUnit*7
    | tc == 6 = betUnit*9
    | otherwise = maxBid

-- | Get value for one unit of bet
betUnit :: Int
betUnit = if maxBid `div` 1000 > minBid then maxBid `div` 1000 else minBid

-- | Get current point of a player based on ID
pointFromId :: [PlayerPoints] -> PlayerId -> Points
pointFromId [] pid = error "Error, ensure player is in the game"
pointFromId (pp:pps) pid
    | _playerPointsId pp == pid = _playerPoints pp
    | otherwise = pointFromId pps pid

-- | Check if it is worth taking an insurance, only take when probability of
-- face cards in remaining cards is equal or more than 50%
worthInsurance :: Int -> Int -> Bool
worthInsurance numFaces numCards = remFaces < remCards && faceProb >= 50
    where
        faceProb = remFaces*100 `div` remCards
        remFaces = 9*numDecks - numFaces
        remCards = 52*numDecks - numCards

-- *** Parser functions, most are derived from FIT2102 Tutorial Week 11 and 12 ***

-- | Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser ~ Refer to Tut W11
list1 :: Parser a -> Parser [a]
list1 p = p >>= (\p' -> list p >>= (\p''-> pure (p':p'')))

-- | Return a parser that continues producing a list of values from the given 
-- parser ~ Refer to Tut W11
list :: Parser a -> Parser [a]
list = (||| pure mempty) . list1

-- | Return a parser that produces a character but fails if input is empty or
--  character does not satisfy predicate ~ Refer to Tut W11
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = character >>=
    (\c -> if not (f c) then unexpectedCharParser c else pure c)

-- | Return a parser that produces any character but fails if the input is empty
-- or the produced character is equal to the given character ~ Refer to Tut W11
isNot :: Char -> Parser Char
isNot = satisfy . (/=)

-- | Return a parser that produces any character but fails if the input is empty
-- or the produced character is not a space ~ Refer to Tut W11
space :: Parser Char
space = satisfy isSpace

-- | Return a parser that parses zero or more spaces ~ Refer to Tut W11
spaces :: Parser String
spaces = list space

-- | Return a parser that produces a character between '0' and '9' but fails if
-- the input is empty or the produced character is not a digit ~ Refer to Tut W11
digit :: Parser Char
digit = satisfy isDigit

-- | Return a parser that parses zero or more digits ~ Refer to Tut W11
digits :: Parser String
digits = list digit

-- | Return a parser that parses negative digits
negativeDigits :: Parser String
negativeDigits = is '-' >>= (\neg -> (:) neg <$> digits)

-- | Applies the given parser, then parses 0 or more spaces, then produces the result 
-- of the original parser ~ Refer to Tut W11
tok :: Parser a -> Parser a
tok p = p >>= (\r -> spaces >> pure r)

-- | Parses the given char followed by 0 or more spaces ~ Refer to Tut W11
charTok :: Char -> Parser Char
charTok = tok . is

-- |  Parses a comma ',' followed by 0 or more spaces ~ Refer to Tut W11
commaTok :: Parser Char
commaTok = charTok ','

-- | Write a function that produces a non-empty list of values coming off the
-- given parser (which must succeed at least once), separated by the second
-- given parser ~ Refer to Tut W11
sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p s = do
    first' <- p
    rest <- list (s >> p)
    pure (first':rest)

-- | Write a function that produces a list of values coming off the given
-- parser, separated by the second given parser ~ Refer to Tut W11
sepby :: Parser a -> Parser s -> Parser [a]
sepby x y = sepby1 x y ||| pure []

-- | Write a function that parses any character, but fails if it is in the
-- given string ~ Refer to Tut W11
noneof :: String -> Parser Char
noneof = satisfy . flip notElem

-- | Write a function that applies the first parser, runs the third parser
-- keeping the result, then runs the second parser and produces the obtained
-- result ~ Refer to Tut W12
between :: Parser o -> Parser c -> Parser a -> Parser a
between p1 p2 p3 = do
    _ <- p1
    x <- p3
    _ <- p2
    return x

-- | Write a function that applies the given parser in between the two given
-- characters ~ Refer to Tut W12
betweenCharTok :: Char -> Char -> Parser a -> Parser a
betweenCharTok start end = between (charTok start) (charTok end)

-- | Write a parser that parses between the two given characters, separated by
-- a comma character ',' ~ Refer to Tut W12
betweenSepbyComma :: Char -> Char -> Parser a -> Parser [a]
betweenSepbyComma start end p = betweenCharTok start end (sepby p commaTok)

-- | Return a parser that produces the given special character ~ Refer to Tut W12
specialChar :: Parser Char
specialChar = character >>= (\c -> case toSpecialCharacter c of
    Just x -> pure $ fromSpecialCharacter x
    Nothing -> failed $ UnexpectedChar c)

-- | Parse a special character, has to start with @\\@ ~ Refer to Tut W12
special :: Parser Char
special = is '\\' >> specialChar

-- | Data for special character  ~ Refer to Tut W12
data SpecialCharacter = BackSpace | FormFeed | NewLine | CarriageReturn
    | Tab | VerticalTab | SingleQuote | DoubleQuote | Backslash
    deriving (Eq, Ord, Show)

-- NOTE: This is not inverse to 'toSpecialCharacter' ~ Refer to Tut W12
fromSpecialCharacter :: SpecialCharacter -> Char
fromSpecialCharacter BackSpace      = chr 0x08
fromSpecialCharacter FormFeed       = chr 0x0C
fromSpecialCharacter NewLine        = '\n'
fromSpecialCharacter CarriageReturn = '\r'
fromSpecialCharacter Tab            = '\t'
fromSpecialCharacter VerticalTab    = '\v'
fromSpecialCharacter SingleQuote    = '\''
fromSpecialCharacter DoubleQuote    = '"'
fromSpecialCharacter Backslash      = '\\'

-- NOTE: This is not inverse to 'fromSpecialCharacter' ~ Refer to Tut W12
toSpecialCharacter :: Char -> Maybe SpecialCharacter
toSpecialCharacter c =
    let table = [('b', BackSpace),('f', FormFeed),
                ('n', NewLine),('r', CarriageReturn),
                ('t', Tab),('v', VerticalTab),
                ('\'', SingleQuote),('"' , DoubleQuote),
                ('\\', Backslash)]
    in snd <$> find_char ((==) c . fst) table
    where
    find_char _ [] = Nothing
    find_char p (x: xs)
        | p x = Just x
        | otherwise = find p xs

-- *** Main parsers and memory related functions ***

-- | Memory stored from previous turn
data Memory = Memory {
    initBid :: Int,                 -- player's bid at start of each hand
    lastTwoActions :: String,       -- player's last two actions
    count :: Int,                   -- running count
    numCards :: Int,                -- number of cards used
    numFaces :: Int,                -- number of face cards used
    lastPlayerInfo :: [PlayerInfo]  -- last players info 
}

-- | Get result from a parse
getResult :: ParseResult p -> p
getResult (Result _ res) = res
getResult _ = error "Error, invalid parse result"

-- | Parse suit character
parseSuit :: Parser Suit
parseSuit = (is 'S' >> pure Spade) ||| (is 'C' >> pure Club) |||
            (is 'D' >> pure Diamond) ||| (is 'H' >> pure Heart)

-- | Parse rank character
parseRank :: Parser Rank
parseRank = (is 'A' >> pure Ace) ||| (is '2' >> pure Two) |||
            (is '3' >> pure Three) |||(is '4' >> pure Four) |||
            (is '5' >> pure Five) ||| (is '6' >> pure Six) |||
            (is '7' >> pure Seven) ||| (is '8' >> pure Eight) |||
            (is '9' >> pure Nine) ||| (is 'T' >> pure Ten) |||
            (is 'J' >> pure Jack) ||| (is 'Q' >> pure Queen) |||
            (is 'K' >> pure King)

-- | Parse card made of rank and suit
parseCard :: Parser Card
parseCard = parseSuit >>= (\s -> parseRank >>= (pure . Card s))

-- | Parse hand containing cards
parseHand :: Parser Hand
parseHand = betweenSepbyComma '[' ']' parseCard

-- | Parse player ID
parseID :: Parser String
parseID = between (is '"') (charTok '"') (list (special  ||| noneof "\"\\"))

-- | Parse a single player info
parseAPlayerInfo :: Parser PlayerInfo
parseAPlayerInfo = parseID >>= (\pid -> PlayerInfo pid <$> parseHand)

-- | Parse a list of player info
parsePlayerInfo :: Parser [PlayerInfo]
parsePlayerInfo = betweenSepbyComma '[' ']' parseAPlayerInfo

-- | Show action as a single character
showAction :: Action -> String
showAction action = case action of
    Hit -> "H"
    Stand -> "S"
    Bid _ -> "B"
    DoubleDown _ -> "D"
    Split _ -> "Z"
    Insurance _ -> "I"

-- | Parse action as single character
parseAction :: Parser Char
parseAction = (is 'H' >> pure 'H') ||| (is 'S' >> pure 'S') |||
        (is 'B' >> pure 'B') ||| (is 'D' >> pure 'D') |||
        (is 'Z' >> pure 'Z') ||| (is 'I' >> pure 'I')

-- | Parse contatenated actions
parseActions :: Parser String
parseActions = list1 parseAction

-- | Read memory into Memory structure
parseMemory :: Parser Memory
parseMemory = do
    bidString <- digits
    let bid = read bidString
    _ <- is ';'
    lastTwoActions <- parseActions
    _ <- is ';'
    countString <- negativeDigits ||| digits
    let count = read countString
    _ <- is ';'
    numCardsString <- digits
    let numCards = read numCardsString
    _ <- is ';'
    numFacesString <- digits
    let numFaces = read numFacesString
    _ <- is ';'
    Memory bid lastTwoActions count numCards numFaces <$> parsePlayerInfo

-- | Get bid based on available points while considering true count
bidAmount :: Int -> Int -> [PlayerPoints] -> PlayerId -> Int
bidAmount count lastNumCards pp pid
    | idealBid <= pointFromId pp pid && idealBid >= minBid = idealBid
    | minBid <= pointFromId pp pid = minBid
    | otherwise = pointFromId pp pid
    where
        idealBid = bidByTC (trueCount count lastNumCards)

-- | Check if Double Down is in the last two actions
checkIfDD :: String -> Bool
checkIfDD = ('D' `elem`)

-- | Check if player has Hit after Double Down
hitAfterDD :: String -> Bool
hitAfterDD = ('H' ==) . head

-- | Update action to include the latest two actions only
updtAction :: Action -> String -> String
updtAction action actionMemory
    | length actionMemory == 2 = showAction action ++ take 1 actionMemory
    | otherwise = showAction action ++ actionMemory

-- | Update running count
updtCount :: Int -> Hand -> Int
updtCount = foldr ((+) . countValue)

-- | Update number of cards used
updtNumCards :: Int -> Hand -> Int
updtNumCards lastNumCards newCards = newNumCards `mod` (52*numDecks)
    where
        newNumCards = lastNumCards + length newCards 

-- | Update number of face cards used
updtNumFaces :: Int -> Hand -> Int
updtNumFaces numFaces newCards = newNumFaces `mod` (9*numDecks)
    where
        newNumFaces = length (filter isFace newCards) + numFaces

-- | Make memory string from Memory data type
memoryStr :: Memory -> String
memoryStr memory = show(initBid memory) ++ ";" ++
                    lastTwoActions memory ++ ";" ++
                    show(count memory) ++ ";" ++
                    show(numCards memory) ++ ";" ++
                    show(numFaces memory) ++ ";" ++
                    show(lastPlayerInfo memory)