module Brainfuck (brainfuck) where

import Data.Char (ord, chr)
import Data.Array
import qualified Data.Sequence as S (fromList, index, length, Seq, update, drop, reverse)
data State = State {state :: S.Seq Char, pos :: Int} deriving (Show)

newState :: String -> State
newState input = State (S.fromList input) 0
dataState :: State
dataState = State (S.fromList . take 5000 . repeat $ '\NUL') 0
getPosition :: State -> Int
getPosition (State _ n) = n;
getData :: State -> S.Seq Char
getData (State n _) = n;
getCurrent :: State -> Char
getCurrent (State state pos) = S.index state pos
setCurrent :: State -> Char -> State
setCurrent (State state pos) new = State (replace state pos new) pos
offsetStateData :: State -> Int -> State
offsetStateData (State state pos) offset = State (replace state pos (shift (S.index state pos) offset)) pos
	where shift char offset = chr (ord char + offset)
offsetStatePointer :: State -> Int -> State
offsetStatePointer (State state pos) offset = State state (pos + offset)

indexOutOfRange :: State -> Bool
indexOutOfRange (State state pos) = pos >= S.length state

brainfuck :: String -> String -> String
brainfuck input var = getOut . until (indexOutOfRange . getInput) bf $ (newState input, dataState, "", newState var)
	where getOut (_, _, out, _) = reverse out;
		  getStateData (_, state, _, _) = getData state;
		  getInput (input, _, _, _) = input

bf :: (State, State, String, State) -> (State, State, String, State)
bf (input, state, output, var)
	| command == '+' = (newInput, plus state, output, var)
	| command == '-' = (newInput, minus state, output, var)
	| command == '>' = (newInput, rightShift state, output, var)
	| command == '<' = (newInput, leftShift state, output, var)
	| command == '.' = (newInput, state, getCurrent state:output, var)
	| command == ',' = (newInput, setCurrent state (getCurrent var), output, rightShift var)
	| command == '[' = if getCurrent state == '\NUL' then (toCloseBracket input, state, output, var)
							else (newInput, state, output, var)
	| command == ']' = if getCurrent state /= '\NUL' then (toOpenBracket input, state, output, var)
							else (newInput, state, output, var)
	| otherwise = (newInput, state, output, var)
	where 	command = getCurrent input;
			newInput = offsetStatePointer input 1;

plus :: State -> State
plus state = offsetStateData state 1
minus :: State -> State
minus state = offsetStateData state (-1)
rightShift :: State -> State
rightShift state = offsetStatePointer state 1
leftShift :: State -> State
leftShift state = offsetStatePointer state (-1)

toCloseBracket :: State -> State
toCloseBracket (State dat pos) = State dat (toCloseBracket' (S.drop pos dat) pos 0)
	where toCloseBracket' :: S.Seq Char -> Int -> Int -> Int;
		  toCloseBracket' state cPos openBrackets 
		  	| S.index state 0 == '[' = toCloseBracket' (S.drop 1 state) (cPos+1) (openBrackets+1);

		  toCloseBracket' state cPos openBrackets
		  	| S.index state 0 == ']' = if openBrackets > 1 then
		  			toCloseBracket' (S.drop 1 state) (cPos+1) (openBrackets-1)
		  		else
		  			cPos + 1;

		  toCloseBracket' state cPos openBrackets = toCloseBracket' (S.drop 1 state) (cPos+1) openBrackets;

toOpenBracket :: State -> State
toOpenBracket (State dat pos) = State dat (toOpenBracket' (S.drop ((S.length dat - pos) - 1) . S.reverse $ dat) pos 0)
	where toOpenBracket' :: S.Seq Char -> Int -> Int -> Int;
		  toOpenBracket' state cPos openBrackets 
		  	| S.index state 0 == ']' = toOpenBracket' (S.drop 1 state) (cPos-1) (openBrackets+1);

		  toOpenBracket' state cPos openBrackets
		  	| S.index state 0 == '[' = if openBrackets > 1 then 
		  			toOpenBracket' (S.drop 1 state) (cPos-1) (openBrackets-1)
		  		else
		  			cPos +  1;
		  toOpenBracket' state cPos openBrackets = toOpenBracket' (S.drop 1 state) (cPos-1) openBrackets;

replace :: S.Seq a -> Int -> a -> S.Seq a
replace seq pos new = S.update pos new seq