import Brainfuck
import System.Directory (doesFileExist)
import System.IO

getInput :: IO(String)
getInput = do 
	inp <- getLine
	fileExists <- doesFileExist inp
	if fileExists then
		readFile inp
	else
		return inp


main = do
	putStrLn "Program [FileName|Code]: "
	program <- getInput
	putStrLn "Input: "
	input <- getInput
	putStrLn "Running..."
	putStrLn $ brainfuck program input