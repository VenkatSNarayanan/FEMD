module Dungeon where
    data Tile = Wall | Floor deriving(Show) -- Tile could be Wall or Floor
    data Direction = Up | Down | None deriving(Eq, Show) -- Direction could be Up or Down
    data RoomData = RoomData {up_left_x :: Int, up_left_y :: Int, down_right_x :: Int, down_right_y :: Int, x_centre :: Int, y_centre :: Int} -- Data type to store information about a given Room

    instance Show RoomData where
       show RoomData {up_left_x = x1, up_left_y = y1, down_right_x = x2, down_right_y = y2, x_centre = xc, y_centre = yc} = "[(" ++ show x1 ++ "," ++ show y1 ++ ");(" ++ show x2 ++ "," ++ show y2 ++ ");(" ++ show xc ++ "," ++ show yc ++ ")]\n"

    data PathData = PathData {start_x :: Int, start_y :: Int, end_x :: Int, end_y :: Int, bend_x :: Int, bend_y :: Int, bend_dir :: Direction} -- Data type to store information about a given Path between Rooms
    
    instance Show PathData where
       show PathData {start_x = x1, start_y = y1, end_x = x2, end_y = y2, bend_x = xb, bend_y = yb, bend_dir = direct} = "(" ++ show x1 ++ "," ++ show y1 ++ ") to (" ++ show x2 ++ "," ++ show y2 ++ ")with bend " ++ show direct ++ "at (" ++ show xb ++ "," ++ show yb ++ ")\n"

    place_rooms :: Int -> Int -> Int -> [Int] -> ([Int], [RoomData]) -- Driver Function to build rooms for a given set of parameters
    place_rooms lngth brdth dnsty rndmSq = (place_rooms_impl (lngth, brdth, dnsty, rndmSq, []))

    build_room :: Int -> Int -> Int -> Int -> RoomData -- Function to assign one room object
    build_room x1 y1 x2 y2 =
                if ((x1 <= x2) && (y1 <= y2)) then
                    RoomData {up_left_x = x1, up_left_y = y1, down_right_x = x2, down_right_y = y2, x_centre = ((x1+x2) `div` 2), y_centre = ((y1+y2) `div` 2)}
                else if ((x1 <= x2) && (y1 > y2)) then
                    RoomData {up_left_x = x1, up_left_y = y2, down_right_x = x2, down_right_y = y1, x_centre = ((x1+x2) `div` 2), y_centre = ((y1+y2) `div` 2)}
                else if ((x1 > x2) && (y1 <= y2)) then
                    RoomData {up_left_x = x2, up_left_y = y1, down_right_x = x1, down_right_y = y2, x_centre = ((x1+x2) `div` 2), y_centre = ((y1+y2) `div` 2)}
                else
                    RoomData {up_left_x = x2, up_left_y = y2, down_right_x = x1, down_right_y = y1, x_centre = ((x1+x2) `div` 2), y_centre = ((y1+y2) `div` 2)}

    new_build_room :: Int -> Int -> Int -> Int -> Int -> Int -> RoomData -- Function to build one new room
    new_build_room x y dx dy lngth brdth =
            let
            ulx = x `rem` (lngth - 3)
            uly = y `rem` (brdth - 3)
            rdx = dx `rem` lngth
            rdy = dy `rem` lngth
            ndx = max 3 (min (lngth - ulx - 1) rdx)
            ndy = max 3 (min (brdth - uly - 1) rdy)
            drx = min (ulx + ndx) lngth - 1
            dry = min (uly + ndy) brdth - 1
        in
            build_room ulx uly drx dry

    check_overlap :: RoomData -> RoomData -> Bool -- Checks if a pair of rooms can coexist
    check_overlap room1 room2 =
        if (((up_left_x room1) > (down_right_x room2)) || ((up_left_x room2) > (down_right_x room1))) then
                True
            else if (((up_left_y room1) > (down_right_y room2)) || ((up_left_y room2) > (down_right_y room1))) then
                True
            else
                False

    place_rooms_impl :: (Int, Int, Int, [Int], [RoomData]) -> ([Int], [RoomData]) -- Inner function to place a new room if possible in a list
    place_rooms_impl (lngth, brdth, dnsty, rndmSq, exist_rooms) =
        if (dnsty == 0) then 
            (rndmSq, exist_rooms)
        else
            let 
            genRand = take 4 rndmSq
            newrandSeq = drop 4 rndmSq
            num1 = head (genRand)
            num2 = head (tail genRand)
            num3 = head (tail (tail genRand))
            num4 = head (tail (tail (tail genRand)))
            genRoom = new_build_room num1 num2 num3 num4 lngth brdth
            roomCheck = foldr (&&) True (map (check_overlap genRoom) exist_rooms)
            in
            if roomCheck then 
               place_rooms_impl (lngth, brdth, dnsty - 1, newrandSeq, (genRoom:exist_rooms))
            else place_rooms_impl (lngth, brdth, dnsty - 1, newrandSeq ,exist_rooms)
            
    sort_all_rooms :: [RoomData] -> [RoomData] -- Function to sort all rooms by virtue of the abscissa of the centroid
    sort_all_rooms [] = []
    sort_all_rooms (x:xs) = (sort_all_rooms (left_part)) ++ (x : sort_all_rooms (right_part)) where
                    left_part = [room | room <- xs, (x_centre room) <= (x_centre x)]
                    right_part = [room | room <- xs, (x_centre room) > (x_centre x)]
         
    make_path :: (RoomData, RoomData) -> PathData -- Function to build a path between 2 rooms
    make_path (room1, room2) 
                           | (x_centre room1 == x_centre room2) = if y_centre room1 > y_centre room2 then
                                                                     PathData {start_x = x_centre room1, start_y = down_right_y room1, end_x = x_centre room2, end_y = up_left_y room2, bend_x = x_centre room1, bend_y = down_right_y room1, bend_dir = Down}
                                                                  else 
                                                                     PathData {start_x = x_centre room1, start_y = up_left_y room1, end_x = x_centre room2, end_y = down_right_y room2, bend_x = x_centre room1, bend_y = up_left_y room1, bend_dir = Up}
                           | (y_centre room1 == y_centre room2) = PathData {start_x = down_right_x room1, start_y = y_centre room1, end_x = up_left_x room2, end_y = y_centre room2, bend_x = (0-1), bend_y = (0-1), bend_dir = None}
                           | otherwise = if y_centre room2 < y_centre room1 then
                                            PathData {start_x = down_right_x room1, start_y = y_centre room1, end_x = x_centre room2, end_y = down_right_y room2, bend_x = x_centre room2, bend_y = y_centre room1, bend_dir = Down}
                                         else 
                                            PathData {start_x = down_right_x room1, start_y = y_centre room1, end_x = x_centre room2, end_y = down_right_y room2, bend_x = x_centre room2, bend_y = y_centre room1, bend_dir = Up}
                                            
    make_all_paths_1 :: [RoomData] -> [PathData]
    make_all_paths list_of_rooms = 
                           let 
                           sorted_rooms = sort_all_rooms list_of_rooms
                           zipped_rooms = zip (sorted_rooms) (tail(sorted_rooms))
                           in make_all_paths_impl zipped_rooms

    make_all_paths_impl :: [(RoomData, RoomData)] -> [PathData]
    make_all_paths_impl [] = []
    make_all_paths_impl zipped_rooms = make_path(head(zipped_rooms)) : make_all_paths_impl(tail(zipped_rooms))
