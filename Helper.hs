--818/344/367/482



module Helper
  where
type Block = [String]
type Value = [String]
type Coord = (Int, Int, Char, Char)   

----------------getter methods to get the row in current/moving states
current_pos :: [[Coord]] -> Block
current_pos pos = map (\row -> get_row_state row) pos

get_row_state :: [Coord] -> String
get_row_state row = map (\(x, y, z, val) -> val) row

get_drawn_sudo :: [[Coord]] -> [Value]
get_drawn_sudo x = draw_basic ++ (map (\row -> [draw_row_zone row x, draw_row_bar row x]) x)

get_row_zone :: Coord -> [[Coord]] -> String
get_row_zone (x, y, z, v) c
  | x == 8 = "  " ++ [v] ++ "  "
  | get_zone (x + 1) y c /= z = "  " ++ [v] ++ " |"
  | otherwise = "  " ++ [v] ++ "  "

get_row_bar :: Coord -> [[Coord]] -> String
get_row_bar (x, y, z, v) c
  | y == 8 = "-----"
  | x == 8 && get_zone x (y + 1) c == z  = "     "
  | x == 8 && get_zone x (y + 1) c /= z = "-----"
  | get_zone x (y + 1) c /= z = "-----"
  | get_zone (x + 1) y c/= z = "    |"
  | otherwise = "     "

get_state  :: Block -> Value -> [[Coord]]
get_state row_map row_state = map (\(y, row) -> get_state_row y row row_state) $ zip[0..] row_map

get_state_row :: Int -> String -> Block -> [Coord]
get_state_row y row state = map (\(x, zone) -> (x, y, zone, ((state !! y) !! x))) $ zip [0..] row

get_zone :: Int -> Int -> [[Coord]] -> Char
get_zone x y c = zone
        where
            (_,_,zone,_)=((c !! y) !! x)

-----------draw-------------            
draw_basic :: [Block]
draw_basic = [["|---------------------------------------------|"]]

draw_row_bar :: [Coord] -> [[Coord]] -> String
draw_row_bar row x = "|" ++ (concat $ map (\coord -> get_row_bar coord x) row) ++ "|"

draw_row_zone :: [Coord] -> [[Coord]] -> String
draw_row_zone row x =  "|" ++ (concat $ (map (\coord -> get_row_zone coord x) row)) ++ "|"            
set_state :: Int -> Int -> Char -> [[Coord]] -> [[Coord]] 
set_state x y val pos = map (\row -> set_state_row x y val row) pos


------------set value---------------
set_state_row :: Int -> Int -> Char -> [Coord] -> [Coord]
set_state_row x y val row = map (\pos -> set_state_new x y val pos) row

set_state_new :: Int -> Int -> Char -> Coord -> Coord
set_state_new x y val pos@(px,py,pz,pv)
  | x == px && y == py = (x, y, pz, val)
  | otherwise = pos
 
--------------check if the entered command is valid in row/col/zone--------------- 
check_row_valid :: Char -> [Coord] -> Bool
check_row_valid val row =  (length (filter (\(x, y, zone, value) -> value == val) row)) < 2

check_col_valid :: Char -> [Coord] -> Bool
check_col_valid val column =  (length (filter (\(x, y, zone, value) -> value == val) column)) < 2

check_zone_valid :: Char -> [Coord] -> Bool
check_zone_valid val zone =  (length (filter (\(x, y, zone, value) -> value == val) zone)) < 2

filter_zone :: [Coord] -> Char -> [Coord]
filter_zone row targetZone  = filter (\(x, y, zone, val) -> zone == targetZone) row

check_move_valid :: Int -> Int -> Char -> [[Coord]] -> Bool
check_move_valid x y val pos = (check_col_valid val column) && (check_row_valid val row) && (check_zone_valid val zone)
  where row = pos !! y
        zone = concat $ map (\row ->  (filter_zone row (get_zone x y pos)) ) pos
        column = map (\row -> row !! x) pos

check_game_over :: [[Coord]] -> Bool
check_game_over pos = (length (concat $ map (\row -> (filter (\(x, y, zone, value) -> value == '.') row)) pos)) == 0 
--------------save input----------------
save_sudo :: String -> Block -> [[Coord]] -> IO ()
save_sudo path row pos = writeFile path (unlines $ row ++ (current_pos pos))
