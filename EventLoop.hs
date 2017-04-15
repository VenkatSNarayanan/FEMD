{-# LANGUAGE TemplateHaskell#-}
module EventLoop where
    import Entity
    import Dungeon
    import System.Random
    import System.IO
    import Control.Monad
    import Data.Maybe
    import qualified Data.Sequence as Data_Seq
    import qualified Data.Map.Strict as Map
    import System.Process
    import Control.Lens
    import Control.Exception

    -- DungeonMap datatype : Contains information about the floor (a Sequence of Sequence of Tile (in Dungeon.hs)), the rooms and paths connecting the rooms, entry and exit points on the map, the statistics of the player, monsters present on the map, locations of potions and weapons strewn on the floor 
    -- Also contains auxiliary information about the current floor you are on, the random number sequence.
    data DungeonMap = DungeonMap { _dung_floor :: Data_Seq.Seq (Data_Seq.Seq Tile), _room_data :: [RoomData], _path_data :: [PathData], _entry_point :: (Int, Int), _exit_point :: (Int, Int), _player :: Character, _monsters :: Map.Map (Int,Int) Character, _randomnums :: [Int], _floor_number :: Int, _potions :: Map.Map (Int, Int) Potion, _weapons :: Map.Map (Int, Int) Weapon}
    makeLenses ''DungeonMap

    -- Contains the input types to distibguish between inputs
    data Inputs = Quit | MLeft | MDown | MUp | MRight | Quaff | WieldLeft | WieldDown | WieldUp | WieldRight | Wait | Whatever
    
    -- Contains the direction types
    data LDirection = LLeft | LRight | LUp | LDown deriving(Eq)

    -- new_eventloop is called from the Main module only
    new_eventloop val pchar_init = do
                                   let rand_n = randomRs (0, maxBound::Int) (mkStdGen val)
                                   eventloop rand_n pchar_init val

    -- Implicit eventloop that is called recursively on every new floor until game quits
    -- eventloop generates a floor, places rooms, paths, potions and weapons
    -- Also identifies the entry and exit points and maps these info to a DungeonMap object
    eventloop rand_n pchar_init val = do
                           let (n, lor) = place_rooms 25 25 10 rand_n
                           let entry = get_entry_point lor
                           let exit = get_exit_point lor
                           let exit_room = build_room (fst exit) (snd exit) (fst exit) (snd exit)
                           let all_p = make_all_paths lor
                           let floor_1 = floor_init 25 25 Floor
                           let floor_2 = floor_build lor floor_1 Room
                           let floor_3 = floor_build (exit_room:[]) floor_2 Exit
                           let floor_4 = path_build all_p floor_3
                           let (p_list, new_n) = get_potions lor 10 n
                           let p_map = get_potion_map p_list
                           let (w_list, newer_n) = get_weapons lor 10 new_n p_map
                           let w_map = get_weapon_map w_list
                           game_loop (DungeonMap (conv_floor_to_seq floor_4) lor all_p entry exit pchar_init Map.empty new_n val p_map w_map)

    -- The actual gameloop. If entry and exit points coincide, then go to new floor. Otherwise stay on same floor and play!!
    game_loop dung_map
                   |(dung_map ^. entry_point) == (dung_map ^. exit_point) = eventloop (dung_map ^. randomnums) (dung_map ^. player) ((dung_map ^. floor_number) + 1)
                   | otherwise = do
                            let new_dung_map = make_monster dung_map
                            system "clear"
                            putStrLn (show(dung_map ^. player))
                            handle_display new_dung_map
                            input <- get_input
                            case input of
                              Quit -> handle_exit
                              MUp -> validate_and_move_up new_dung_map
                              MRight -> validate_and_move_right new_dung_map
                              MLeft -> validate_and_move_left new_dung_map
                              MDown -> validate_and_move_down new_dung_map
                              Quaff -> handle_quaff new_dung_map
                              WieldUp -> handle_wield_up dung_map
                              WieldDown -> handle_wield_down dung_map
                              WieldLeft -> handle_wield_left dung_map
                              WieldRight -> handle_wield_right dung_map
                              Wait -> move_monsters new_dung_map
                              Whatever -> game_loop new_dung_map

    -- The input handler. Acts as an intermediary to input from player back to gameloop
    get_input = do
                what_to_do <- getChar
                case what_to_do of
                     'x' -> return Quit
                     'h' -> return MLeft
                     'j' -> return MDown
                     'k' -> return MUp
                     'l' -> return MRight
                     'q' -> return Quaff
                     'w' -> return WieldUp
                     'a' -> return WieldLeft
                     's' -> return WieldDown
                     'd' -> return WieldRight
                     'm' -> return Wait
                     _   -> return Whatever

    -- Handle display: Prints the contents of the map. This is called before every move, so that the player knows what is done/to be done.
    handle_display dung_map = do
                              let floor_map = floor_display (dung_map ^. dung_floor)
                              let base_map = Data_Seq.update (snd $ dung_map ^. entry_point) (Data_Seq.update (fst $ dung_map ^. entry_point) "@" (Data_Seq.index floor_map (snd $ dung_map ^. entry_point))) floor_map
                              let monst_map = Map.foldrWithKey (\(row,col) _ basemap -> Data_Seq.update (col) (Data_Seq.update (row) "T" (Data_Seq.index basemap (col))) basemap) base_map (dung_map ^. monsters)
                              let p_map = Map.foldrWithKey (\(row,col) _ basemap -> Data_Seq.update (col) (Data_Seq.update (row) "P" (Data_Seq.index basemap (col))) basemap) monst_map (dung_map ^. potions)
                              let w_map = Map.foldrWithKey (\(row,col) _ basemap -> Data_Seq.update (col) (Data_Seq.update (row) "w" (Data_Seq.index basemap (col))) basemap) p_map (dung_map ^. weapons)
                              putStrLn(Data_Seq.foldrWithIndex (\_ a b -> (Data_Seq.foldrWithIndex (\_ a b -> a++b) "" a)++"\n"++b) "\n" w_map)

    -- Just exit
    handle_exit = do
                  putStrLn "GGWP!!\n"
                  return ()

    -- Move functions {move_up, move_down, move_left, move_right} : Move the player from position (x,y) to {(x,y-1),(x,y+1),(x-1,y),(x+1,y)} respectively
    -- If he steps on a tile that has a potion/weapon, then the potion/weapon is added to his inventory
    move_up dung_map = do
                       let new_x = (fst(dung_map ^. entry_point))
                       let new_y = (snd(dung_map ^. entry_point)) - 1
                       if (Map.member (new_x, new_y) (dung_map ^. potions)) then
                          do
                          let player_mod = (dung_map ^. player) & items %~ digPotion
                          move_monsters (dung_map & entry_point .~ (new_x, new_y) & player .~ player_mod & potions %~ (Map.delete (new_x, new_y)))
                       else if (Map.member (new_x, new_y) (dung_map ^. weapons)) then
                          do
                          let new_weapon_1 = (Map.lookup (new_x, new_y) (dung_map ^. weapons))
                          let new_weapon = assert (isJust(new_weapon_1)) (fromJust(new_weapon_1))
                          let new_item_set = (dung_map ^. player ^. items) ++ [(WeaponTag (new_weapon))]
                          let player_mod = dung_map ^. player & items .~ new_item_set
                          move_monsters (dung_map & entry_point .~ (new_x,new_y) & player .~ player_mod & weapons %~ (Map.delete (new_x, new_y)))
                       else
                          do
                          move_monsters (dung_map & entry_point .~ (new_x,new_y))

    move_down dung_map = do
                         let new_x = (fst(dung_map ^. entry_point))
                         let new_y = (snd(dung_map ^. entry_point)) + 1
                         if (Map.member (new_x, new_y) (dung_map ^. potions)) then
                            do
                            let player_mod = (dung_map ^. player) & items %~ digPotion
                            move_monsters (dung_map & entry_point .~ (new_x, new_y) & player .~ player_mod & potions %~ (Map.delete (new_x, new_y)))
                         else if (Map.member (new_x, new_y) (dung_map ^. weapons)) then
                            do
                            let new_weapon_1 = (Map.lookup (new_x, new_y) (dung_map ^. weapons))
                            let new_weapon = assert (isJust(new_weapon_1)) (fromJust(new_weapon_1))
                            let new_item_set = (dung_map ^. player ^. items) ++ [(WeaponTag (new_weapon))]
                            let player_mod = dung_map ^. player & items .~ new_item_set
                            move_monsters (dung_map & entry_point .~ (new_x,new_y) & player .~ player_mod & weapons %~ (Map.delete (new_x, new_y)))
                         else
                            do
                            move_monsters (dung_map & entry_point .~ (new_x,new_y))

    move_left dung_map = do
                         let new_x = (fst(dung_map ^. entry_point)) - 1
                         let new_y = (snd(dung_map ^. entry_point))
                         if (Map.member (new_x, new_y) (dung_map ^. potions)) then
                            do
                            let player_mod = (dung_map ^. player) & items %~ digPotion
                            move_monsters (dung_map & entry_point .~ (new_x, new_y) & player .~ player_mod & potions %~ (Map.delete (new_x, new_y)))
                         else if (Map.member (new_x, new_y) (dung_map ^. weapons)) then
                            do
                            let new_weapon_1 = (Map.lookup (new_x, new_y) (dung_map ^. weapons))
                            let new_weapon = assert (isJust(new_weapon_1)) (fromJust(new_weapon_1))
                            let new_item_set = (dung_map ^. player ^. items) ++ [(WeaponTag (new_weapon))]
                            let player_mod = dung_map ^. player & items .~ new_item_set
                            move_monsters (dung_map & entry_point .~ (new_x,new_y) & player .~ player_mod & weapons %~ (Map.delete (new_x, new_y)))
                         else
                            do
                            move_monsters (dung_map & entry_point .~ (new_x,new_y))

    move_right dung_map = do
                          let new_x = (fst(dung_map ^. entry_point)) + 1
                          let new_y = (snd(dung_map ^. entry_point))
                          if (Map.member (new_x, new_y) (dung_map ^. potions)) then
                             do
                             let player_mod = (dung_map ^. player) & items %~ digPotion
                             move_monsters (dung_map & entry_point .~ (new_x, new_y) & player .~ player_mod & potions %~ (Map.delete (new_x, new_y)))
                          else if (Map.member (new_x, new_y) (dung_map ^. weapons)) then
                             do
                             let new_weapon_1 = (Map.lookup (new_x, new_y) (dung_map ^. weapons))
                             let new_weapon = assert (isJust(new_weapon_1)) (fromJust(new_weapon_1))
                             let new_item_set = (dung_map ^. player ^. items) ++ [(WeaponTag (new_weapon))]
                             let player_mod = dung_map ^. player & items .~ new_item_set
                             move_monsters (dung_map & entry_point .~ (new_x,new_y) & player .~ player_mod & weapons %~ (Map.delete (new_x, new_y)))
                          else
                             do
                             move_monsters (dung_map & entry_point .~ (new_x,new_y))

    -- Validate move. Defined for each of the aforementioned move functions
    -- Checks if the tile-to-be-moved-to is not occupied by a monster or is a Floor
    validate_and_move_up dung_map = do
                                    let cur_floor = (dung_map ^. dung_floor)
                                    let y = -1 + (snd(dung_map ^. entry_point))
                                    let x = (fst(dung_map ^. entry_point))
                                    if (Data_Seq.index (Data_Seq.index cur_floor y) x) == Floor || isJust(Map.lookup (x,y) (dung_map ^. monsters)) then
                                       game_loop dung_map
                                    else
                                       move_up dung_map

    validate_and_move_down dung_map = do
                                      let cur_floor = (dung_map ^. dung_floor)
                                      let y = 1 + (snd(dung_map ^. entry_point))
                                      let x = (fst(dung_map ^. entry_point))
                                      if (Data_Seq.index (Data_Seq.index cur_floor y) x) == Floor || isJust(Map.lookup (x,y) (dung_map ^. monsters)) then
                                         game_loop dung_map
                                      else
                                         move_down dung_map

    validate_and_move_left dung_map = do
                                      let cur_floor = (dung_map ^. dung_floor)
                                      let y = (snd(dung_map ^. entry_point))
                                      let x = -1 + (fst(dung_map ^. entry_point))
                                      if (Data_Seq.index (Data_Seq.index cur_floor y) x) == Floor || isJust(Map.lookup (x,y) (dung_map ^. monsters)) then
                                         game_loop dung_map
                                      else
                                         move_left dung_map

    validate_and_move_right dung_map = do
                                       let cur_floor = (dung_map ^. dung_floor)
                                       let y = (snd(dung_map ^. entry_point))
                                       let x = 1 + (fst(dung_map ^. entry_point))
                                       if (Data_Seq.index (Data_Seq.index cur_floor y) x) == Floor || isJust(Map.lookup (x,y) (dung_map ^. monsters)) then
                                          game_loop dung_map
                                       else
                                          move_right dung_map

    -- Function to move monsters across the map depending on Player's location.
    move_monsters dung_map = move_monsters_impl dung_map (map (\(a,b) -> a) (Map.toList ((dung_map ^. monsters))))

    -- Implicitly used function, which also provides a provision for the monsters to attack if within range
    move_monsters_impl :: DungeonMap -> [(Int,Int)] -> IO ()
    move_monsters_impl dung_map [] = game_loop dung_map
    move_monsters_impl dung_map (coord_head:coord_tail) =
        do
            let playpos = dung_map ^. entry_point
            let head_monster = assert (isJust(Map.lookup coord_head (dung_map ^. monsters))) (fromJust(Map.lookup coord_head (dung_map ^. monsters)))
            let monster_weapon = getWeapon (head_monster ^. items)
            let monst_dist = (abs((fst playpos) - (fst coord_head))) + (abs((snd playpos) - (snd coord_head)))
            let can_attack = fromMaybe False (monster_weapon >>= (\w -> Just (inRange monst_dist (w ^. minrange) (w ^. maxrange))))
            let curr_floor = dung_map ^. dung_floor
            let (monst,playerpost,randSeq) = if can_attack then
                                                (combat head_monster (dung_map ^. player) monst_dist (map (`rem` 100) (dung_map ^. randomnums)))
                                        else
                                            (Just head_monster,Just (dung_map ^. player),(dung_map ^. randomnums))
            let newpos = (if (can_attack) then
                            coord_head
                        else if ((fst coord_head) < (fst playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (snd coord_head)) (1+fst coord_head))==Room && isNothing(Map.lookup ((fst coord_head)+1,(snd coord_head)) (dung_map ^. monsters))) then
                            ((fst coord_head) + 1,snd coord_head)
                        else if ((fst coord_head) > (fst playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (snd coord_head)) (-1+fst coord_head))==Room && isNothing(Map.lookup ((fst coord_head)-1,(snd coord_head)) (dung_map ^. monsters))) then
                            ((fst coord_head) - 1,snd coord_head)
                        else if ((snd coord_head) < (snd playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (1+snd coord_head)) (fst coord_head))==Room && isNothing(Map.lookup ((fst coord_head),(snd coord_head)+1) (dung_map ^. monsters))) then
                            (fst coord_head,(snd coord_head) + 1)
                        else if ((snd coord_head) > (snd playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (-1+snd coord_head)) (fst coord_head))==Room && isNothing(Map.lookup ((fst coord_head),(snd coord_head)-1) (dung_map ^. monsters))) then
                            (fst coord_head,(snd coord_head) - 1)
                        else
                            coord_head)
            if isNothing(playerpost) then
                putStrLn "Sorry, you got KOed!\n"
            else if isNothing(monst) then
                do
                --putStrLn"The monster died!\n"
                (move_monsters_impl (dung_map & player .~ (assert (isJust(playerpost)) (fromJust(playerpost))) & monsters %~ (Map.delete coord_head) & randomnums .~ randSeq) coord_tail)
            else
                do
              --  putStrLn "Nobody died!\n"
                (move_monsters_impl (dung_map & player .~ (assert (isJust(playerpost)) (fromJust(playerpost))) & monsters %~ (\m -> (Map.insert newpos (assert(isJust(monst)) (fromJust(monst))) (Map.delete coord_head m))) & randomnums .~ randSeq) coord_tail)

    -- Function to select coordinates to spawn new monsters
    select_coords :: [RoomData] -> [Int] -> (Int, Int)
    select_coords list_of_rooms rands = let
                                         select_room = (rands!!0 `rem` (length(list_of_rooms)))
                                         cur_room = (list_of_rooms)!!select_room
                                         x = rands!!1 `rem` ((cur_room ^. down_right_x) - (cur_room ^. up_left_x))
                                         y = rands!!2 `rem` ((cur_room ^. down_right_y) - (cur_room ^. up_left_y))
                                         select_x_coord = (cur_room ^. up_left_x) + x
                                         select_y_coord = (cur_room ^. up_left_y) + y
                                         in
                                         (select_x_coord, select_y_coord)

    -- Function to generate monsters
    make_monster :: DungeonMap -> DungeonMap
    make_monster dung_map = if (make_decision (take 10 (dung_map ^. randomnums)) 3) then
                               let
                               mons_coords = select_coords (dung_map ^. room_data) (take 3 (tail(dung_map ^. randomnums)))
                               new_mons = Character {_stats = Stats{_hp=20, _strength=5, _skill=5, _speed=10, _luck=5, _defense=5}, _items=[WeaponTag Weapon{_charges=46, _weight=5, _might=5, _hit=90, _crit=0, _minrange=1, _maxrange=1}], _status=Status{_currhp=20, _condition=Healthy}}
                               in
                               dung_map & monsters %~ (Map.insert mons_coords new_mons) & randomnums %~ (drop 4)
                            else
                               dung_map & randomnums %~ tail

    -- Function below will return the closest monster to you within range, if there is a monster nearby
    get_monster :: LDirection -> DungeonMap -> (Character, (Int, Int))
    get_monster direction dung_map = let
                                    player_pos = (dung_map ^. entry_point)
                                    on_left = ((fst(player_pos)) - 1, (snd(player_pos)))
                                    on_right = ((fst(player_pos)) + 1, (snd(player_pos)))
                                    on_top = ((fst(player_pos)), (snd(player_pos))-1)
                                    on_bottom = ((fst(player_pos)), (snd(player_pos))+1)
                                    in
                                    if direction == LUp then
                                       assert (isJust(Map.lookup (on_top) (dung_map ^. monsters))) ((fromJust (Map.lookup (on_top) (dung_map ^. monsters))), (on_top))
                                    else if direction == LDown then
                                       assert(isJust(Map.lookup (on_bottom) (dung_map ^. monsters))) ((fromJust (Map.lookup (on_bottom) (dung_map ^. monsters))), (on_bottom))
                                    else if direction == LLeft then
                                       assert(isJust(Map.lookup (on_left) (dung_map ^. monsters))) ((fromJust (Map.lookup (on_left) (dung_map ^. monsters))), (on_left))
                                    else
                                       assert(isJust(Map.lookup (on_right) (dung_map ^. monsters))) ((fromJust (Map.lookup (on_right) (dung_map ^. monsters))), (on_right))

    -- Checks if there is a monster nearby
    check_if_monster_nearby :: LDirection -> DungeonMap -> Bool
    check_if_monster_nearby direction dung_map = let
                                                player_pos = (dung_map ^. entry_point)
                                                on_left = ((fst(player_pos)) - 1, (snd(player_pos)))
                                                on_right = ((fst(player_pos)) + 1, (snd(player_pos)))
                                                on_top = ((fst(player_pos)), (snd(player_pos))-1)
                                                on_bottom = ((fst(player_pos)), (snd(player_pos))+1)
                                                in
                                                if direction == LUp then
                                                   (Map.member (on_top) (dung_map ^. monsters))
                                                else if direction == LDown then
                                                   (Map.member (on_bottom) (dung_map ^. monsters))
                                                else if direction == LLeft then
                                                   (Map.member (on_left) (dung_map ^. monsters))
                                                else
                                                   (Map.member (on_right) (dung_map ^. monsters))

    -- Wield functions: Enable the player to attack first if a monster is within range.
    -- WieldUp allows player to attack monsters in front
    -- WieldDown allows player to attack monsters behind
    -- WieldLeft allows player to attack monsters to the left
    -- WieldRight allows player to attack monsters to the right
    handle_wield_up dung_map
                      | check_if_monster_nearby LUp dung_map = do
                                                                       let (head_monst, coord_head) = get_monster LUp dung_map
                                                                       let playpos = (dung_map ^. entry_point)
                                                                       let monst_dist = (abs((fst playpos) - (fst coord_head))) + (abs((snd playpos) - (snd coord_head)))
                                                                       let (playerpost, monst, randSeq) = (combat (dung_map ^. player) (head_monst) monst_dist (map (`rem` 100) (dung_map ^. randomnums)))
                                                                       let newpos = coord_head
                                                                       if isNothing(monst) then
                                                                          do
            --                                                              putStrLn "The Monster died!!\n"
                                                                          (move_monsters (dung_map & player .~ (assert(isJust(playerpost)) (fromJust(playerpost)))  & monsters %~ (Map.delete coord_head) & randomnums .~ randSeq))
                                                                       else if isNothing(playerpost) then
                                                                          do
                                                                           putStrLn "Sorry, you get KOed!"
                                                                       else
                                                                          do
          --                                                                putStrLn "Nobody died..\n"
                                                                          (move_monsters (dung_map & player .~ (assert(isJust(playerpost)) (fromJust(playerpost))) & monsters %~ (\m -> (Map.insert newpos (assert(isJust(monst)) (fromJust(monst))) (Map.delete coord_head m))) & randomnums .~ randSeq ))
                      | otherwise = move_monsters dung_map

    handle_wield_down dung_map
                        | check_if_monster_nearby LDown dung_map = do
                                                                    let (head_monst, coord_head) = get_monster LDown dung_map
                                                                    let playpos = (dung_map ^. entry_point)
                                                                    let monst_dist = (abs((fst playpos) - (fst coord_head))) + (abs((snd playpos) - (snd coord_head)))
                                                                    let (playerpost, monst, randSeq) = (combat (dung_map ^. player) (head_monst) monst_dist (map (`rem` 100) (dung_map ^. randomnums)))
                                                                    let newpos = coord_head
                                                                    if isNothing(monst) then
                                                                       do
        --                                                               putStrLn "The Monster died!!\n"
                                                                       (move_monsters (dung_map & player .~ (assert(isJust(playerpost)) (fromJust(playerpost))) & monsters %~ (Map.delete coord_head) & randomnums .~ randSeq))
                                                                    else if isNothing(playerpost) then
                                                                       do
                                                                         putStrLn "Sorry, you get KOed!"
                                                                    else
                                                                       do
      --                                                                 putStrLn "Nobody died..\n"
                                                                       (move_monsters (dung_map & player .~ (assert(isJust(playerpost)) (fromJust(playerpost))) & monsters %~ (\m -> (Map.insert newpos (assert(isJust(monst)) (fromJust(monst))) (Map.delete coord_head m))) & randomnums .~ randSeq ))
                        | otherwise = move_monsters dung_map

    handle_wield_left dung_map
                        | check_if_monster_nearby LLeft dung_map = do
                                                                    let (head_monst, coord_head) = get_monster LLeft dung_map
                                                                    let playpos = (dung_map ^. entry_point)
                                                                    let monst_dist = (abs((fst playpos) - (fst coord_head))) + (abs((snd playpos) - (snd coord_head)))
                                                                    let (playerpost, monst, randSeq) = (combat (dung_map ^. player) (head_monst) monst_dist (map (`rem` 100) (dung_map ^. randomnums)))
                                                                    let newpos = coord_head
                                                                    if isNothing(monst) then
                                                                       do
        --                                                               putStrLn "The Monster died!!\n"
                                                                       (move_monsters (dung_map & player .~ (assert(isJust(playerpost)) (fromJust(playerpost))) & monsters %~ (Map.delete coord_head) & randomnums .~ randSeq))
                                                                    else if isNothing(playerpost) then
                                                                       do
                                                                         putStrLn "Sorry, you get KOed!"
                                                                    else
                                                                       do
      --                                                                 putStrLn "Nobody died..\n"
                                                                       (move_monsters (dung_map & player .~ (assert(isJust(playerpost)) (fromJust(playerpost))) & monsters %~ (\m -> (Map.insert newpos (assert(isJust(monst)) (fromJust(monst))) (Map.delete coord_head m))) & randomnums .~ randSeq ))
                        | otherwise = move_monsters dung_map

    handle_wield_right dung_map
                         | check_if_monster_nearby LRight dung_map = do
                                                                      let (head_monst, coord_head) = get_monster LRight dung_map
                                                                      let playpos = (dung_map ^. entry_point)
                                                                      let monst_dist = (abs((fst playpos) - (fst coord_head))) + (abs((snd playpos) - (snd coord_head)))
                                                                      let (playerpost, monst, randSeq) = (combat (dung_map ^. player) (head_monst) monst_dist (map (`rem` 100) (dung_map ^. randomnums)))
                                                                      let newpos = coord_head
                                                                      if isNothing(monst) then
                                                                         do
          --                                                               putStrLn "The Monster died!!\n"
                                                                         (move_monsters (dung_map & player .~ (assert(isJust(playerpost)) (fromJust(playerpost))) & monsters %~ (Map.delete coord_head) & randomnums .~ randSeq))
                                                                      else if isNothing(playerpost) then
                                                                         do
                                                                           putStrLn "Sorry, you get KOed!"
                                                                      else
                                                                         do
        --                                                                 putStrLn "Nobody died..\n"
                                                                         (move_monsters (dung_map & player .~ (assert(isJust(playerpost)) (fromJust(playerpost))) & monsters %~ (\m -> (Map.insert newpos (assert(isJust(monst)) (fromJust(monst))) (Map.delete coord_head m))) & randomnums .~ randSeq ))
                         | otherwise = move_monsters dung_map

    -- If the player quaffs, then a part of his health is restored and the number of potions he has reduces.
    handle_quaff dung_map = let
                            cur_remain = (fromJust (getPotion (dung_map ^. player ^. items))) ^. remain
                            in
                            if cur_remain > 0 then
                               let
                               new_health = min (10 + (dung_map ^. player ^. status ^. currhp)) (dung_map ^. player ^. stats ^. hp)
                               player_mod = dung_map ^. player & items %~ usePotion & (status . currhp) .~ new_health
                               in
                               game_loop (dung_map & player .~ player_mod & randomnums %~ (drop 4))
                            else
                               game_loop (dung_map & randomnums %~ (drop 4))
