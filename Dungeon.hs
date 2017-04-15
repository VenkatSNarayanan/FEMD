{-# LANGUAGE TemplateHaskell#-}
module Dungeon where
    import Control.Monad
    import Control.Lens
    import qualified Data.Sequence as Data_Seq
    import qualified Data.Map.Strict as Map
    import Entity

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
    data RoomData = RoomData {_up_left_x :: Int, _up_left_y :: Int, _down_right_x :: Int, _down_right_y :: Int, _x_centre :: Int, _y_centre :: Int}
    makeLenses ''RoomData

    -- PathData type. All paths are straight lines.
    -- There might be bends in the path. These bends are right-angled
    -- The path is characterized by the starting location and direction, ending location and 1 bend if any.
    data PathData = PathData {_start_x :: Int, _start_y :: Int, _start_dir :: Direction, _end_x :: Int, _end_y :: Int, _bend_x :: Int, _bend_y :: Int, _bend_dir :: Direction}
    makeLenses ''PathData

    -- Driver function to build rooms for a given set of parameters.
    -- Parameters include length and breadth of the floor.
    place_rooms :: Int -> Int -> Int -> [Int] -> ([Int], [RoomData])
    place_rooms lngth brdth dnsty rndmSq = (place_rooms_impl (lngth, brdth, dnsty, rndmSq, []))

    -- Function to assign one room object
    build_room :: Int -> Int -> Int -> Int -> RoomData
    build_room x1 y1 x2 y2 =
                if ((x1 <= x2) && (y1 <= y2)) then
                    RoomData {_up_left_x = x1, _up_left_y = y1, _down_right_x = x2, _down_right_y = y2, _x_centre = (((x1+x2) `div` 2) + ((x1+x2) `rem` 2)), _y_centre = (((y1+y2) `div` 2) + ((y1+y2) `rem` 2))}
                else if ((x1 <= x2) && (y1 > y2)) then
                    RoomData {_up_left_x = x1, _up_left_y = y2, _down_right_x = x2, _down_right_y = y1, _x_centre = (((x1+x2) `div` 2) + ((x1+x2) `rem` 2)), _y_centre = (((y1+y2) `div` 2) + ((y1+y2) `rem` 2))}
                else if ((x1 > x2) && (y1 <= y2)) then
                    RoomData {_up_left_x = x2, _up_left_y = y1, _down_right_x = x1, _down_right_y = y2, _x_centre = (((x1+x2) `div` 2) + ((x1+x2) `rem` 2)) , _y_centre = (((y1+y2) `div` 2) + ((y1+y2) `rem` 2))}
                else
                    RoomData {_up_left_x = x2, _up_left_y = y2, _down_right_x = x1, _down_right_y = y1, _x_centre = (((x1+x2) `div` 2) + ((x1+x2) `rem` 2)), _y_centre = (((y1+y2) `div` 2) + ((y1+y2) `rem` 2))}

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
        if (((room1 ^. up_left_x) > (room2 ^. down_right_x)) || ((room2 ^. up_left_x) > (room1 ^. down_right_x))) then
                True
            else if (((room1 ^. up_left_y) > (room2 ^. down_right_y)) || ((room2 ^. up_left_y) > (room1 ^. down_right_y))) then
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
                    left_part = [room | room <- xs, (room ^. x_centre) <= (x ^. x_centre)]
                    right_part = [room | room <- xs, (room ^. x_centre) > (x ^. x_centre)]


    -- Function to build a path between any two rooms
    make_path :: (RoomData, RoomData) -> PathData
    make_path (room1, room2)
                           | (room1 ^. x_centre == room2 ^. x_centre) = if (room1 ^. y_centre < room2 ^. y_centre) then
                                                                       PathData {_start_x = room1 ^. x_centre, _start_y = room1 ^. down_right_y, _start_dir = East, _end_x = room2 ^. x_centre, _end_y = room2 ^. up_left_y, _bend_x = room1 ^. x_centre, _bend_y = room1 ^. down_right_y, _bend_dir = South}
                                                                    else
                                                                       PathData {_start_x = room1 ^. x_centre, _start_y = room1 ^. down_right_y, _start_dir = East, _end_x = room2 ^. x_centre, _end_y = room2 ^. up_left_y, _bend_x = room1 ^. x_centre, _bend_y = room1 ^. down_right_y, _bend_dir = North}

                           | (room1 ^. y_centre == room2 ^. y_centre) = PathData {_start_x = room1 ^. down_right_x, _start_y = room1 ^. y_centre, _start_dir = East, _end_x = room2 ^. up_left_x, _end_y = room2 ^. y_centre, _bend_x = (0-1), _bend_y = (0-1), _bend_dir = None}

                           | (room1 ^. y_centre > room2 ^. y_centre) = if (room2 ^. down_right_y < room1 ^. y_centre) then
                                                                      PathData {_start_x = room1 ^. down_right_x, _start_y = room1 ^. y_centre, _start_dir = East, _end_x = room2 ^. x_centre, _end_y = room2 ^. down_right_y, _bend_x = room2 ^. x_centre, _bend_y = room1 ^. y_centre, _bend_dir = North}

                                                                   else
                                                                      if (room1 ^. up_left_y > room2 ^. y_centre) then
                                                                         PathData {_start_x = room1 ^. x_centre, _start_y = room1 ^. up_left_y, _start_dir = North, _end_x = room2 ^. up_left_x, _end_y = room2 ^. y_centre, _bend_x = room1 ^. x_centre, _bend_y = room2 ^. y_centre, _bend_dir = East}

                                                                      else
                                                                         PathData {_start_x = room1 ^. down_right_x, _start_y = ((room2 ^. down_right_y + room1 ^. down_right_y) `div` 2), _start_dir = East, _end_x = room2 ^. x_centre, _end_y = room2 ^. down_right_y, _bend_x = room2 ^. x_centre, _bend_y = ((room2 ^. down_right_y + room1 ^. down_right_y) `div` 2), _bend_dir = North}

                           | (room2 ^. y_centre > room1 ^. y_centre) = if (room2 ^. up_left_y > room1 ^. y_centre) then
                                                                      PathData {_start_x = room1 ^. up_left_x, _start_y = room1 ^. y_centre, _start_dir = East, _end_x = room2 ^. x_centre, _end_y = room2 ^. up_left_y, _bend_x = room2 ^. x_centre, _bend_y = room1 ^. y_centre, _bend_dir = South}

                                                                   else
                                                                      if (room1 ^. down_right_y < room2 ^. y_centre) then
                                                                         PathData {_start_x = room1 ^. x_centre, _start_y = room1 ^. down_right_y, _start_dir = South, _end_x = room2 ^. down_right_x, _end_y = room2 ^. y_centre, _bend_x = room1 ^. x_centre, _bend_y = room2 ^. y_centre, _bend_dir = East}

                                                                      else
                                                                         PathData {_start_x = room1 ^. up_left_x, _start_y = ((room2 ^. up_left_y + room1 ^. up_left_y) `div` 2), _start_dir = East, _end_x = room2 ^. x_centre, _end_y = room2 ^. up_left_y, _bend_x = room2 ^. x_centre, _bend_y = ((room2 ^. up_left_y + room1 ^. up_left_y) `div` 2), _bend_dir = South}

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
                                                leave_before = my_room ^. up_left_y
                                                leave_after = my_room ^. down_right_y
                                                (above_part, unchange_after) = splitAt (leave_after+1) cur_floor
                                                (unchange_before, change_floor) = splitAt (leave_before) above_part
                                                start_x = my_room ^. up_left_x
                                                end_x = my_room ^. down_right_x
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
    convert_path_to_room cur_path = if (cur_path ^. bend_dir == None) then
                                       let
                                       room1 = build_room (cur_path ^. start_x) (cur_path ^. start_y) (cur_path ^. end_x) (cur_path ^. end_y)
                                       in
                                       (room1 : [])
                                    else
                                       let
                                       room1 = build_room (cur_path ^. start_x) (cur_path ^. start_y) (cur_path ^. bend_x) (cur_path ^. bend_y)
                                       room2 = build_room (cur_path ^. end_x) (cur_path ^. end_y) (cur_path ^. bend_x) (cur_path ^. bend_y)
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
                                    x = (head(slor)) ^. up_left_x
                                    y = (head(slor)) ^. down_right_y
                                    in
                                    (x,y)

    -- Function to get the exit point on a floor
    get_exit_point :: [RoomData] -> (Int, Int)
    get_exit_point list_of_rooms = let
                                   slor = sort_all_rooms list_of_rooms
                                   x = (last(slor)) ^. down_right_x
                                   y = (last(slor)) ^. up_left_y
                                   in
                                   (x, y)

    -- Function to convert the List of Lists to Sequence of Sequences
    conv_floor_to_seq :: [[Tile]] -> Data_Seq.Seq (Data_Seq.Seq Tile)
    conv_floor_to_seq [] = Data_Seq.empty
    conv_floor_to_seq cur_floor = (Data_Seq.<|) (Data_Seq.fromList (head(cur_floor))) (conv_floor_to_seq (tail(cur_floor)))

    make_decision :: [Int] -> Int -> Bool
    make_decision randoms percentage = let
                                       rand_100 = map (`rem` 100) randoms
                                       new_randoms = filter (> (100 - percentage)) rand_100
                                       in
                                       if (length(new_randoms)) > 0 then
                                          True
                                       else
                                          False

    get_potions :: [RoomData] -> Int -> [Int] -> ([(Int, Int)], [Int])
    get_potions list_of_rooms dnsty rands = get_potions_impl list_of_rooms dnsty rands []

    get_potions_impl :: [RoomData] -> Int -> [Int] -> [(Int, Int)] -> ([(Int, Int)], [Int])
    get_potions_impl _ 0 rands p_list = (p_list, rands)
    get_potions_impl list_of_rooms dnsty rands p_list = let
                                            room_select = (head(rands)) `rem` (length(list_of_rooms))
                                            selected_room = list_of_rooms!!(room_select)
                                            select_x = (selected_room ^. up_left_x) + (rands!!1 `rem` ((selected_room ^. down_right_x) - (selected_room ^. up_left_x)))
                                            select_y = (selected_room ^. up_left_y) + (rands!!2 `rem` ((selected_room ^. down_right_y) - (selected_room ^. up_left_y)))
                                            in
                                            if (make_decision (take 10 (drop 4 rands)) 10) then
                                               get_potions_impl (list_of_rooms) (dnsty-1) (drop 13 rands) ((select_x, select_y) : p_list)
                                            else
                                               get_potions_impl (list_of_rooms) (dnsty-1) (drop 13 rands) (p_list)

    get_potion_map :: [(Int, Int)] -> Map.Map (Int, Int) Potion
    get_potion_map p_list = Map.fromList (zip (p_list) (repeat(Potion{_remain=1})))

    get_weapons :: [RoomData] -> Int -> [Int] -> Map.Map (Int, Int) Potion -> ([(Int, Int)], [Int])
    get_weapons list_of_rooms dnsty rands p_map = get_weapons_impl list_of_rooms dnsty rands [] p_map

    get_weapons_impl :: [RoomData] -> Int -> [Int] -> [(Int, Int)] -> Map.Map (Int, Int) Potion -> ([(Int, Int)], [Int])
    get_weapons_impl _ 0 rands c_list p_map = (c_list, rands)
    get_weapons_impl list_of_rooms dnsty rands c_list p_map = let
                                                      room_select = (head(rands)) `rem` (length(list_of_rooms))
                                                      selected_room = list_of_rooms!!(room_select)
                                                      select_x = (selected_room ^. up_left_x) + (rands!!1 `rem` ((selected_room ^. down_right_x) - (selected_room ^. up_left_x)))
                                                      select_y = (selected_room ^. up_left_y) + (rands!!2 `rem` ((selected_room ^. down_right_y) - (selected_room ^. up_left_y)))
                                                      in
                                                      if (make_decision (take 10 (drop 4 rands)) 5) then
                                                         if Map.member (select_x, select_y) (p_map) then
                                                            get_weapons_impl (list_of_rooms) (dnsty-1) (drop 13 rands) (c_list) (p_map)
                                                         else
                                                            get_weapons_impl (list_of_rooms) (dnsty-1) (drop 13 rands) ((select_x, select_y) : c_list) (p_map)
                                                      else
                                                         get_weapons_impl (list_of_rooms) (dnsty-1) (drop 13 rands) (c_list) (p_map)

    get_weapon_map :: [(Int, Int)] -> Map.Map (Int, Int) Weapon
    get_weapon_map w_list = Map.fromList (zip (w_list) (repeat(Weapon{_charges=15, _weight=2, _might=2, _hit=70, _crit=0, _minrange=1, _maxrange=1})))
