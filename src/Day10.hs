{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Day10 where


import Text.ParserCombinators.Parsec
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Either (fromRight)

data Bot = Bot { chip1 :: Maybe Int
               , chip2 :: Maybe Int
               , lowTo :: Receiver
               , highTo :: Receiver
               } deriving (Show)

type Bots = IntMap Bot
type Outputs = IntMap Int

data Instruction = BotInstruction Int Receiver Receiver | ValueInstruction Int Int
    deriving (Show)

data Receiver = BotReceiver Int | OutputReceiver Int
    deriving (Show)

solve :: IO ()
solve = do
    input <- parseInput <$> readFile "inputs/input10.txt"

    -- Part 1
    let runnedBots = part1 input
    let sol1 = fst $ head $ IntMap.toList $ IntMap.filter (\bot -> chip1 bot == Just 17 && chip2 bot == Just 61) runnedBots

    -- Part 2
    let outputs = getOutputs runnedBots
    let sol2 = outputs IntMap.! 0 * outputs IntMap.! 1 * outputs IntMap.! 2


    -- Print solutions
    putStrLn $ "Part 1 sol: " ++ show sol1
    putStrLn $ "Part 2 sol: " ++ show sol2

-- Part 1

part1 :: Either ParseError [Instruction] -> Bots
part1 (Left err) = error $ show err
part1 (Right instructions) = runBots $ runInstructions instructions

runInstructions :: [Instruction] -> Bots
runInstructions instructions = runInstructions' instructions IntMap.empty

runInstructions' :: [Instruction] -> Bots -> Bots
runInstructions' [] bots = bots
runInstructions' (i:is) bots = case i of
    (BotInstruction botId low high) -> runInstructions' is (IntMap.alter (addLowHigh low high) botId bots)
    (ValueInstruction value botId) -> runInstructions' is (IntMap.alter (addChip (BotReceiver value)) botId bots)
    where
        addLowHigh low high (Just (Bot val1 val2 _ _)) = Just $ Bot val1 val2 low high
        addLowHigh low high Nothing = Just $ Bot Nothing Nothing low high

addChip :: Receiver -> Maybe Bot -> Maybe Bot
addChip (BotReceiver value) (Just (Bot Nothing Nothing low high)) = Just $ Bot (Just value) Nothing low high
addChip (BotReceiver value) (Just (Bot (Just v) Nothing low high)) = Just $ Bot (Just $ min value v) (Just $ max value v) low high
addChip (BotReceiver value) (Just (Bot Nothing (Just v) low high)) = Just $ Bot (Just $ min value v) (Just $ max value v) low high
addChip _ (Just (Bot (Just _) (Just _) _ _)) = error "Bot already has two chips"
addChip (BotReceiver value) Nothing = Just $ Bot (Just value) Nothing (BotReceiver (-1)) (BotReceiver (-1))
addChip (OutputReceiver _) bot = Nothing

runBots :: Bots -> Bots
runBots bots = runBots' (IntMap.toList bots) bots IntMap.empty

runBots' :: [(Int, Bot)] -> Bots -> Bots -> Bots
runBots' [] checkBots completedBots = if IntMap.null checkBots then completedBots else runBots' (IntMap.toList checkBots) checkBots completedBots
runBots' ((botId, bot):bots) checkBots completedBots = case (chip1 bot, chip2 bot) of
    (Just low, Just high) -> runBots' bots (IntMap.delete botId  $ IntMap.update (addChip' low) (getId $ lowTo bot) $ IntMap.update(addChip' high) (getId $ highTo bot) checkBots) (IntMap.insert botId bot completedBots)
    _ -> runBots' bots checkBots completedBots
    where
        addChip' val (Bot Nothing Nothing low high) = Just $ Bot (Just val) Nothing low high
        addChip' val (Bot (Just v) Nothing low high) = Just $ Bot (Just $ min val v) (Just $ max val v) low high
        addChip' val (Bot Nothing (Just v) low high) = Just $ Bot (Just $ min val v) (Just $ max val v) low high
        addChip' _ (Bot (Just _) (Just _) _ _) = error "Bot already has two chips"
        getId (BotReceiver id) = id
        getId (OutputReceiver _) = -1

-- Part 2

getOutputs :: Bots -> Outputs
getOutputs bots = getOutputs' (IntMap.toList bots) IntMap.empty

getOutputs' :: [(Int, Bot)] -> Outputs -> Outputs
getOutputs' [] outputs = outputs
getOutputs' ((botId, bot):bots) outputs = case (chip1 bot, chip2 bot) of
    (Just low, Just high) -> getOutputs' bots (insertIfOutput low (lowTo bot) $ insertIfOutput high (highTo bot) outputs)
    _ -> error "Not processed bots"
    where
        insertIfOutput val (OutputReceiver id) outputs = IntMap.insert id val outputs
        insertIfOutput _ _ outputs = outputs

-- Parsing

parseInput :: String -> Either ParseError [Instruction]
parseInput = parse parseDay10 "(unknown)"

parseDay10 :: Parser [Instruction]
parseDay10 = endBy line eol

eol :: Parser Char
eol = char '\n'

line :: Parser Instruction
line = try botInstruction <|> valueInstruction

botInstruction :: Parser Instruction
botInstruction = do
    string "bot "
    botId <- many1 digit
    string " gives low to "
    low <- parseOutput
    string " and high to "
    high <- parseOutput
    return $ BotInstruction (read botId) low high

parseOutput :: Parser Receiver
parseOutput = try botReceiver <|> outputReceiver

botReceiver :: Parser Receiver
botReceiver = do
    string "bot "
    botId <- many1 digit
    return $ BotReceiver (read botId)

outputReceiver :: Parser Receiver
outputReceiver = do
    string "output "
    outputId <- many1 digit
    return $ OutputReceiver (read outputId)

valueInstruction :: Parser Instruction
valueInstruction = do
    string "value "
    value <- many1 digit
    string " goes to bot "
    botId <- many1 digit
    return $ ValueInstruction (read value) (read botId)

