module Dungeon where
    import Control.Monad
    import qualified Data.Sequence as Data_Seq

    -- Tile can be one of Floor, Room and Path
    data Tile = Floor | Room | Path | Exit deriving(Eq)

    -- How each tile should be displayed
    instance Show Tile where
       show Floor = " "
       show Room = "."
       show Path = "."
       show Exit = "X"

    -- Directions. These are meant for traversing the Rooms (internally). Could be one of North, South, East or None
    data Direction = North | South | East | None deriving(Eq, Show)

    -- RoomData type. All rooms are rectangular. This Data Type has the top left and bottom right coordinates with the centroid of the
    -- Room
    data RoomData = RoomData {up_left_x :: Int, up_left_y :: Int, down_right_x :: Int, down_right_y :: Int, x_centre :: Int, y_centre :: Int}

    -- Display each room (again for internal purposes)
    instance Show RoomData where
       show RoomData {up_left_x = x1, up_left_y = y1, down_right_x = x2, down_right_y = y2, x_centre = xc, y_centre = yc} = "[(" ++ show x1 ++ "," ++ show y1 ++ ");(" ++ show x2 ++ "," ++ show y2 ++ ");(" ++ show xc ++ "," ++ show yc ++ ")]\n"

    -- PathData type. All paths are straight lines.
    -- There might be bends in the path. These bends are right-angled
    -- The path is characterized by the starting location and direction, ending location and 1 bend if any.
    data PathData = PathData {start_x :: Int, start_y :: Int, start_dir :: Direction, end_x :: Int, end_y :: Int, bend_x :: Int, bend_y :: Int, bend_dir :: Direction}

    -- Display each path (again for internal purposes)
    instance Show PathData where
       show PathData {start_x = x1, start_y = y1, start_dir = s_direct, end_x = x2, end_y = y2, bend_x = xb, bend_y = yb, bend_dir = b_direct} = "(" ++ show x1 ++ "," ++ show y1 ++ ") to (" ++ show x2 ++ "," ++ show y2 ++ ") with bend " ++ show b_direct ++ " at (" ++ show xb ++ "," ++ show yb ++ ") starting " ++ show s_direct ++ "\n"

    -- Driver function to build rooms for a given set of parameters.
    -- Parameters include length and breadth of the floor.
    place_rooms :: Int -> Int -> Int -> [Int] -> ([Int], [RoomData])
    place_rooms lngth brdth dnsty rndmSq = (place_rooms_impl (lngth, brdth, dnsty, rndmSq, []))

    -- Function to assign one room object
    build_room :: Int -> Int -> Int -> Int -> RoomData
    build_room x1 y1 x2 y2 =
                if ((x1 <= x2) && (y1 <= y2)) then
                    RoomData {up_left_x = x1, up_left_y = y1, down_right_x = x2, down_right_y = y2, x_centre = (((x1+x2) `div` 2) + ((x1+x2) `rem` 2)), y_centre = (((y1+y2) `div` 2) + ((y1+y2) `rem` 2))}
                else if ((x1 <= x2) && (y1 > y2)) then
                    RoomData {up_left_x = x1, up_left_y = y2, down_right_x = x2, down_right_y = y1, x_centre = (((x1+x2) `div` 2) + ((x1+x2) `rem` 2)), y_centre = (((y1+y2) `div` 2) + ((y1+y2) `rem` 2))}
                else if ((x1 > x2) && (y1 <= y2)) then
                    RoomData {up_left_x = x2, up_left_y = y1, down_right_x = x1, down_right_y = y2, x_centre = (((x1+x2) `div` 2) + ((x1+x2) `rem` 2)) , y_centre = (((y1+y2) `div` 2) + ((y1+y2) `rem` 2))}
                else
                    RoomData {up_left_x = x2, up_left_y = y2, down_right_x = x1, down_right_y = y1, x_centre = (((x1+x2) `div` 2) + ((x1+x2) `rem` 2)), y_centre = (((y1+y2) `div` 2) + ((y1+y2) `rem` 2))}

    -- This function feeds the values for a room to build_room
    new_build_room :: Int -> Int -> Int -> Int -> Int -> Int -> RoomData
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

    -- Checks if a pair of rooms can co-exist
    check_overlap :: RoomData -> RoomData -> Bool
    check_overlap room1 room2 =
        if (((up_left_x room1) > (down_right_x room2)) || ((up_left_x room2) > (down_right_x room1))) then
                True
            else if (((up_left_y room1) > (down_right_y room2)) || ((up_left_y room2) > (down_right_y room1))) then
                True
            else
                False

    -- Implicit function to place a new room if possible based on current parameters in a list
    place_rooms_impl :: (Int, Int, Int, [Int], [RoomData]) -> ([Int], [RoomData])
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

    -- Function to sort all rooms by virtue of the absicca of the centroid of each room
    sort_all_rooms :: [RoomData] -> [RoomData]
    sort_all_rooms [] = []
    sort_all_rooms (x:xs) = (sort_all_rooms (left_part)) ++ (x : sort_all_rooms (right_part)) where
                    left_part = [room | room <- xs, (x_centre room) <= (x_centre x)]
                    right_part = [room | room <- xs, (x_centre room) > (x_centre x)]


    -- Function to build a path between any two rooms
    make_path :: (RoomData, RoomData) -> PathData
    make_path (room1, room2)
                           | (x_centre(room1) == x_centre(room2)) = if (y_centre(room1) < y_centre(room2)) then
                                                                       PathData {start_x = x_centre(room1), start_y = down_right_y(room1), start_dir = East, end_x = x_centre(room2), end_y = up_left_y(room2), bend_x = x_centre(room1), bend_y = down_right_y(room1), bend_dir = South}
                                                                    else
                                                                       PathData {start_x = x_centre(room1), start_y = down_right_y(room1), start_dir = East, end_x = x_centre(room2), end_y = up_left_y(room2), bend_x = x_centre(room1), bend_y = down_right_y(room1), bend_dir = North}

                           | (y_centre(room1) == y_centre(room2)) = PathData {start_x = down_right_x(room1), start_y = y_centre(room1), start_dir = East, end_x = up_left_x(room2), end_y = y_centre(room2), bend_x = (0-1), bend_y = (0-1), bend_dir = None}

                           | (y_centre(room1) > y_centre(room2)) = if (down_right_y(room2) < y_centre(room1)) then
                                                                      PathData {start_x = down_right_x(room1), start_y = y_centre(room1), start_dir = East, end_x = x_centre(room2), end_y = down_right_y(room2), bend_x = x_centre(room2), bend_y = y_centre(room1), bend_dir = North}

                                                                   else
                                                                      if (up_left_y(room1) > y_centre(room2)) then
                                                                         PathData {start_x = x_centre(room1), start_y = up_left_y(room1), start_dir = North, end_x = up_left_x(room2), end_y = y_centre(room2), bend_x = x_centre(room1), bend_y = y_centre(room2), bend_dir = East}

                                                                      else
                                                                         PathData {start_x = down_right_x(room1), start_y = ((down_right_y(room2) + down_right_y(room1)) `div` 2), start_dir = East, end_x = x_centre(room2), end_y = down_right_y(room2), bend_x = x_centre(room2), bend_y = ((down_right_y(room2) + down_right_y(room1)) `div` 2), bend_dir = North}

                           | (y_centre(room2) > y_centre(room1)) = if (up_left_y(room2) > y_centre(room1)) then
                                                                      PathData {start_x = up_left_x(room1), start_y = y_centre(room1), start_dir = East, end_x = x_centre(room2), end_y = up_left_y(room2), bend_x = x_centre(room2), bend_y = y_centre(room1), bend_dir = South}

                                                                   else
                                                                      if (down_right_y(room1) < y_centre(room2)) then
                                                                         PathData {start_x = x_centre(room1), start_y = down_right_y(room1), start_dir = South, end_x = down_right_x(room2), end_y = y_centre(room2), bend_x = x_centre(room1), bend_y = y_centre(room2), bend_dir = East}

                                                                      else
                                                                         PathData {start_x = up_left_x(room1), start_y = ((up_left_y(room2) + up_left_y(room1)) `div` 2), start_dir = East, end_x = x_centre(room2), end_y = up_left_y(room2), bend_x = x_centre(room2), bend_y = ((up_left_y(room2) + up_left_y(room1)) `div` 2), bend_dir = South}

    -- Driver function to build paths between all rooms
    make_all_paths :: [RoomData] -> [PathData]
    make_all_paths list_of_rooms =
                           let
                           sorted_rooms = sort_all_rooms list_of_rooms
                           zipped_rooms = zip (sorted_rooms) (tail(sorted_rooms))
                           in make_all_paths_impl zipped_rooms

    -- Implicit function to build paths
    make_all_paths_impl :: [(RoomData, RoomData)] -> [PathData]
    make_all_paths_impl [] = []
    make_all_paths_impl zipped_rooms = make_path(head(zipped_rooms)) : make_all_paths_impl(tail(zipped_rooms))

    -- Functions to "make" a displayable floor object
    -- This floor object is a list of lists of Datatype Tile

    -- Makes a row of a floor
    floor_row_init :: Int -> Tile -> [Tile]
    floor_row_init 0 _ = []
    floor_row_init lngth tiletype = tiletype : (floor_row_init (lngth - 1) tiletype)

    -- Driver function to initiate a floor
    floor_init :: Int -> Int -> Tile -> [[Tile]]
    floor_init _ 0 _ = []
    floor_init lngth brdth tiletype = (floor_row_init lngth tiletype) : (floor_init (lngth) (brdth - 1) tiletype)

    -- Implicit function to modify the entry of a row in a floor
    floor_row_mod :: Int -> Int -> [Tile] -> Tile -> [Tile]
    floor_row_mod start end row new_tile = (take start row) ++ (floor_row_init (end - start + 1) new_tile) ++ (drop (end + 1) row)

    -- Implicit function to modify a set of rows in a floor
    floor_rows_mod :: Int -> Int -> [[Tile]] -> Tile -> [[Tile]]
    floor_rows_mod _ _ [] _ = []
    floor_rows_mod start end cur_floor new_tile = (floor_row_mod start end (head(cur_floor)) new_tile) : floor_rows_mod start end (tail(cur_floor)) new_tile

    -- Driver function to "make" my floor object based on the Rooms I have
    floor_build :: [RoomData] -> [[Tile]] -> Tile -> [[Tile]]
    floor_build [] cur_floor _ = cur_floor
    floor_build my_rooms cur_floor new_tile = floor_build (tail(my_rooms)) (floor_room_mod (head(my_rooms)) cur_floor new_tile) new_tile

    -- Implicit function to modify the floor based on the parameters of one room
    floor_room_mod :: RoomData -> [[Tile]] -> Tile -> [[Tile]]
    floor_room_mod my_room cur_floor new_tile = let
                                                leave_before = up_left_y my_room
                                                leave_after = down_right_y my_room
                                                (above_part, unchange_after) = splitAt (leave_after+1) cur_floor
                                                (unchange_before, change_floor) = splitAt (leave_before) above_part
                                                start_x = up_left_x my_room
                                                end_x = down_right_x my_room
                                                in
                                                unchange_before ++ (floor_rows_mod start_x end_x change_floor new_tile) ++ unchange_after

    -- Function to display the floor.
    floor_display :: Data_Seq.Seq (Data_Seq.Seq Tile) -> Data_Seq.Seq (Data_Seq.Seq String)
    floor_display tiles = Data_Seq.mapWithIndex (\b a -> Data_Seq.mapWithIndex (\b a -> show a) a) tiles

    -- Function to convert a set of paths into a set of rooms.
    -- Paths can be considered to be a 1D room
    convert_paths_to_rooms :: [PathData] -> [RoomData]
    convert_paths_to_rooms [] = []
    convert_paths_to_rooms my_paths = convert_path_to_room (head(my_paths)) ++ convert_paths_to_rooms (tail(my_paths))

    -- Implicit function to convert a path into a list of 1 or 2 rooms depending upon whether a bend exists or not
    convert_path_to_room :: PathData -> [RoomData]
    convert_path_to_room cur_path = if (bend_dir(cur_path) == None) then
                                       let
                                       room1 = build_room (start_x(cur_path)) (start_y(cur_path)) (end_x(cur_path)) (end_y(cur_path))
                                       in
                                       (room1 : [])
                                    else
                                       let
                                       room1 = build_room (start_x(cur_path)) (start_y(cur_path)) (bend_x(cur_path)) (bend_y(cur_path))
                                       room2 = build_room (end_x(cur_path)) (end_y(cur_path)) (bend_x(cur_path)) (bend_y(cur_path))
                                       in
                                       (room1 : (room2 : []))

    -- Driver function to embed paths into the floor
    path_build :: [PathData] -> [[Tile]] -> [[Tile]]
    path_build my_paths cur_floor = let
                                    all_paths_are_rooms = convert_paths_to_rooms my_paths
                                    in
                                    floor_build all_paths_are_rooms cur_floor Room

    -- Function to get the entry point on a floor
    get_entry_point :: [RoomData] -> (Int, Int)
    get_entry_point list_of_rooms = let
                                    slor = sort_all_rooms list_of_rooms
                                    x = up_left_x(head(slor))
                                    y = down_right_y(head(slor))
                                    in
                                    (x,y)

    -- Function to get the exit point on a floor
    get_exit_point :: [RoomData] -> (Int, Int)
    get_exit_point list_of_rooms = let
                                   slor = sort_all_rooms list_of_rooms
                                   x = down_right_x(last(slor))
                                   y = up_left_y(last(slor))
                                   in
                                   (x, y)

    -- Function to convert the List of Lists to Sequence of Sequences
    conv_floor_to_seq :: [[Tile]] -> Data_Seq.Seq (Data_Seq.Seq Tile)
    conv_floor_to_seq [] = Data_Seq.empty
    conv_floor_to_seq cur_floor = (Data_Seq.<|) (Data_Seq.fromList (head(cur_floor))) (conv_floor_to_seq (tail(cur_floor)))
