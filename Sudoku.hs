

import System.IO
import Control.Monad
import Data.Char
import Data.List

import Helper

main = forever $ do
    putStrLn "To start playing, use 'start PATH', where PATH points to a saved game."
    command <- getLine
    execute command

execute :: String -> IO ()
execute cmd = 
    if cmd == "start"
      then do game cmd
      else do putStrLn "wrong command: change another one"

game :: String -> IO ()
game cmd = do
  file <- readFile "map.txt"
  let row_original = words file
      row_map = take (9) row_original
      row_state_original = drop (9) row_original
      pos = get_state row_map row_state_original
      map_sudo = concat $ get_drawn_sudo pos
  putStrLn (unlines map_sudo)
  move pos row_map 


move :: [[Coord]] -> [String] -> IO ()
move positions map = do
  putStrLn "Enter the next move in row: col: number:\n "
  command <- getLine
  if isInfixOf "save" command
    then save_sudo ((words command) !! 1) map positions
    else do
      let args = words command
          x = digitToInt $ (args !! 0) !! 0
          y = digitToInt $ (args !! 1) !! 0
          val = (args !! 2) !! 0
          pos = set_state x y val positions
          sudo_new = concat $ get_drawn_sudo pos
      if check_move_valid x y val pos == True
        then do
          if check_game_over pos == True then do
            putStrLn "Congratulations, you finished the game!\n"
          else do
            putStrLn"New Board:\n"
            putStrLn (unlines sudo_new)
            --putStrLn"New Board:\n"
            move pos map
        else do
          putStrLn "Sorry, there is a conflict existing in your board; please reenter:\n"
          move positions map
