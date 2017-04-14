module EventLoop where
    import Entity
    import Dungeon
    import System.Random
    import System.IO
    import Control.Monad
    import Data.List
    import Data.Maybe
    import qualified Data.Sequence as Data_Seq
    import Debug.Trace
    import qualified Data.Map.Strict as Map
    import System.Process
    import qualified Data.Foldable as Data_Fold

    data DungeonMap = DungeonMap { dung_floor :: Data_Seq.Seq (Data_Seq.Seq Tile), room_data :: [RoomData], path_data :: [PathData], entry_point :: (Int, Int), exit_point :: (Int, Int), player :: Character, monsters :: Map.Map (Int,Int) Character, randomnums :: [Int], floor_number :: Int, potions :: Map.Map (Int, Int) Potion, weapons :: Map.Map (Int, Int) Weapon}
    data Inputs = Quit | MLeft | MDown | MUp | MRight | Quaff | WieldLeft | WieldDown | WieldUp | WieldRight | Wait | Whatever

    eventloop val pchar_init = do
                           let rand_n = randomRs (0, maxBound::Int) (mkStdGen (2*val))
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
    
    game_loop dung_map
                   |(entry_point(dung_map)) == (exit_point(dung_map)) = eventloop ((floor_number(dung_map))+1) (player(dung_map))
                   | otherwise = do
                            let new_dung_map = make_monster dung_map
               --          system "clear"
                            putStrLn (show(player(dung_map)))
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

    handle_display dung_map = do
                              let floor_map = floor_display (dung_floor(dung_map))
                              let base_map = Data_Seq.update (snd $ entry_point dung_map) (Data_Seq.update (fst $ entry_point dung_map) "@" (Data_Seq.index floor_map (snd $ entry_point dung_map))) floor_map
                              let monst_map = Map.foldrWithKey (\(row,col) _ basemap -> Data_Seq.update (col) (Data_Seq.update (row) "T" (Data_Seq.index basemap (col))) basemap) base_map (monsters dung_map)
                              let p_map = Map.foldrWithKey (\(row,col) _ basemap -> Data_Seq.update (col) (Data_Seq.update (row) "." (Data_Seq.index basemap (col))) basemap) monst_map (potions dung_map)
                              let w_map = Map.foldrWithKey (\(row,col) _ basemap -> Data_Seq.update (col) (Data_Seq.update (row) "w" (Data_Seq.index basemap (col))) basemap) p_map (weapons dung_map)                              
                              --putStrLn (temp_dis (monst_map))
                              putStrLn(Data_Seq.foldrWithIndex (\_ a b -> (Data_Seq.foldrWithIndex (\_ a b -> a++b) "" a)++"\n"++b) "\n" w_map)
    
    temp_dis :: Data_Seq.Seq (Data_Seq.Seq String) -> String                          
    temp_dis mymap = if (Data_Seq.null (mymap)) then 
                        ""
                     else
                        ((foldr (++) "" (Data_Fold.toList (Data_Seq.index (mymap) 0))) ++ "\n" ++ (temp_dis (Data_Seq.drop 1 (mymap))))
                     
    handle_exit = do
                  putStrLn "GGWP!!\n"
                  return ()

    move_up dung_map = do
                       let new_x = (fst(entry_point(dung_map)))
                       let new_y = (snd(entry_point(dung_map))) - 1
                       if (Map.member (new_x, new_y) (potions(dung_map))) then
                          do
                          let player_mod = Character {stats=(stats(player(dung_map))), items = (digPotion(items(player(dung_map)))), status=(status(player(dung_map)))}
                          move_monsters DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (new_x, new_y), exit_point = (exit_point(dung_map)), player = player_mod, monsters=(monsters(dung_map)), randomnums = (randomnums(dung_map)), floor_number = (floor_number(dung_map)), potions=((Map.delete (new_x, new_y) (potions(dung_map)))), weapons=(weapons(dung_map))}
                       else if (Map.member (new_x, new_y) (weapons(dung_map))) then
                          do
                          let new_weapon = fromJust (Map.lookup (new_x, new_y) (weapons(dung_map)))
                          let new_item_set = (items(player(dung_map))) ++ [(WeaponTag (new_weapon))]
                          let player_mod = Character {stats=(stats(player(dung_map))), items = new_item_set, status=(status(player(dung_map)))}
                          move_monsters DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (new_x, new_y), exit_point = (exit_point(dung_map)), player = player_mod, monsters=(monsters(dung_map)), randomnums = (randomnums(dung_map)), floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(Map.delete (new_x, new_y) (weapons(dung_map)))} 
                       else
                          do
                          move_monsters DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (new_x, new_y), exit_point = (exit_point(dung_map)), player = (player(dung_map)), monsters=(monsters(dung_map)), randomnums = (randomnums(dung_map)), floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))} 
                          
    move_down dung_map = do
                         let new_x = (fst(entry_point(dung_map)))
                         let new_y = (snd(entry_point(dung_map))) + 1
                         if (Map.member (new_x, new_y) (potions(dung_map))) then
                            do
                            let player_mod = Character {stats=(stats(player(dung_map))), items = (digPotion(items(player(dung_map)))), status=(status(player(dung_map)))}
                            move_monsters DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (new_x, new_y), exit_point = (exit_point(dung_map)), player = player_mod, monsters=(monsters(dung_map)), randomnums = (randomnums(dung_map)), floor_number = (floor_number(dung_map)), potions=(Map.delete (new_x, new_y) (potions(dung_map))), weapons=(weapons(dung_map))}
                         else if (Map.member (new_x, new_y) (weapons(dung_map))) then
                            do
                            let new_weapon = fromJust (Map.lookup (new_x, new_y) (weapons(dung_map)))
                            let new_item_set = (items(player(dung_map))) ++ [(WeaponTag (new_weapon))]
                            let player_mod = Character {stats=(stats(player(dung_map))), items = new_item_set, status=(status(player(dung_map)))}
                            move_monsters DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (new_x, new_y), exit_point = (exit_point(dung_map)), player = player_mod, monsters=(monsters(dung_map)), randomnums = (randomnums(dung_map)), floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(Map.delete (new_x, new_y) (weapons(dung_map)))} 
                         else
                            do
                          move_monsters DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (new_x, new_y), exit_point = (exit_point(dung_map)), player = (player(dung_map)), monsters=(monsters(dung_map)), randomnums = (randomnums(dung_map)), floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))} 

    move_left dung_map = do
                         let new_x = (fst(entry_point(dung_map))) - 1
                         let new_y = (snd(entry_point(dung_map)))
                         if (Map.member (new_x, new_y) (potions(dung_map))) then
                            do
                            let player_mod = Character {stats=(stats(player(dung_map))), items = (digPotion(items(player(dung_map)))), status=(status(player(dung_map)))}
                            move_monsters DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (new_x, new_y), exit_point = (exit_point(dung_map)), player = player_mod, monsters=(monsters(dung_map)), randomnums = (randomnums(dung_map)), floor_number = (floor_number(dung_map)), potions=(Map.delete (new_x, new_y) (potions(dung_map))), weapons=(weapons(dung_map))}
                         else if (Map.member (new_x, new_y) (weapons(dung_map))) then
                            do
                            let new_weapon = fromJust (Map.lookup (new_x, new_y) (weapons(dung_map)))
                            let new_item_set = (items(player(dung_map))) ++ [(WeaponTag (new_weapon))]
                            let player_mod = Character {stats=(stats(player(dung_map))), items = new_item_set, status=(status(player(dung_map)))}
                            move_monsters DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (new_x, new_y), exit_point = (exit_point(dung_map)), player = player_mod, monsters=(monsters(dung_map)), randomnums = (randomnums(dung_map)), floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(Map.delete (new_x, new_y) (weapons(dung_map)))} 
                         else
                            do
                            move_monsters DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (new_x, new_y), exit_point = (exit_point(dung_map)), player = (player(dung_map)), monsters=(monsters(dung_map)), randomnums = (randomnums(dung_map)), floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))} 

    move_right dung_map = do
                          let new_x = (fst(entry_point(dung_map))) + 1
                          let new_y = (snd(entry_point(dung_map))) 
                          if (Map.member (new_x, new_y) (potions(dung_map))) then
                             do
                             let player_mod = Character {stats=(stats(player(dung_map))), items = (digPotion(items(player(dung_map)))), status=(status(player(dung_map)))}
                             move_monsters DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (new_x, new_y), exit_point = (exit_point(dung_map)), player = player_mod, monsters=(monsters(dung_map)), randomnums = (randomnums(dung_map)), floor_number = (floor_number(dung_map)), potions=(Map.delete (new_x, new_y) (potions(dung_map))), weapons=(weapons(dung_map))}
                          else if (Map.member (new_x, new_y) (weapons(dung_map))) then
                             do
                             let new_weapon = fromJust (Map.lookup (new_x, new_y) (weapons(dung_map)))
                             let new_item_set = (items(player(dung_map))) ++ [(WeaponTag (new_weapon))]
                             let player_mod = Character {stats=(stats(player(dung_map))), items = new_item_set, status=(status(player(dung_map)))}
                             move_monsters DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (new_x, new_y), exit_point = (exit_point(dung_map)), player = player_mod, monsters=(monsters(dung_map)), randomnums = (randomnums(dung_map)), floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(Map.delete (new_x, new_y) (weapons(dung_map)))} 
                          else
                             do
                             move_monsters DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (new_x, new_y), exit_point = (exit_point(dung_map)), player = (player(dung_map)), monsters=(monsters(dung_map)), randomnums = (randomnums(dung_map)), floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))} 

    validate_and_move_up dung_map = do
                                    let cur_floor = (dung_floor(dung_map))
                                    let y = -1 + (snd(entry_point(dung_map)))
                                    let x = (fst(entry_point(dung_map)))
                                    if (Data_Seq.index (Data_Seq.index cur_floor y) x) == Floor || isJust(Map.lookup (x,y) (monsters dung_map)) then
                                       game_loop dung_map
                                    else
                                       move_up dung_map

    validate_and_move_down dung_map = do
                                      let cur_floor = (dung_floor(dung_map))
                                      let y = 1 + (snd(entry_point(dung_map)))
                                      let x = (fst(entry_point(dung_map)))
                                      if (Data_Seq.index (Data_Seq.index cur_floor y) x) == Floor || isJust(Map.lookup (x,y) (monsters dung_map)) then
                                         game_loop dung_map
                                      else
                                         move_down dung_map

    validate_and_move_left dung_map = do
                                      let cur_floor = (dung_floor(dung_map))
                                      let y = (snd(entry_point(dung_map)))
                                      let x = -1 + (fst(entry_point(dung_map)))
                                      if (Data_Seq.index (Data_Seq.index cur_floor y) x) == Floor || isJust(Map.lookup (x,y) (monsters dung_map)) then
                                         game_loop dung_map
                                      else
                                         move_left dung_map

    validate_and_move_right dung_map = do
                                       let cur_floor = (dung_floor(dung_map))
                                       let y = (snd(entry_point(dung_map)))
                                       let x = 1 + (fst(entry_point(dung_map)))
                                       if (Data_Seq.index (Data_Seq.index cur_floor y) x) == Floor || isJust(Map.lookup (x,y) (monsters dung_map)) then
                                          game_loop dung_map
                                       else
                                          move_right dung_map

    move_monsters dung_map = move_monsters_impl dung_map (map (\(a,b) -> a) (Map.toList (monsters dung_map)))

    move_monsters_impl :: DungeonMap -> [(Int,Int)] -> IO ()
    move_monsters_impl dung_map [] = game_loop dung_map
    move_monsters_impl dung_map (coord_head:coord_tail) =
        do
            let playpos = entry_point dung_map
            let head_monster = trace "fromJust is used here...0\n" (fromJust (Map.lookup coord_head (monsters dung_map)))
            let monster_weapon = getWeapon (items head_monster)
            let absvalue = (\a -> if (a<0) then (-a) else a)
            let monst_dist = (absvalue((fst playpos) - (fst coord_head))) + (absvalue((snd playpos) - (snd coord_head)))
            let can_attack = fromMaybe False (monster_weapon >>= (\w -> Just (inRange monst_dist (minrange w) (maxrange w))))
            let curr_floor = dung_floor(dung_map)
            let (monst,playerpost,randSeq) = if can_attack then
                                                trace "Combat going to occur!!\n"
                                                --trace (show(player(dung_map)) ++ "\n" ++ show(head_monster) ++ "\n")
                                                (combat head_monster (player dung_map) monst_dist (map (`rem` 100) (randomnums(dung_map))))
                                        else
                                            (Just head_monster,Just (player dung_map),(randomnums dung_map))
            let newpos = trace "BLAH!!\n" (if (can_attack) then
                            trace "Combat occured!!\n"
                            trace (show(playerpost) ++ "\n" ++ show(monst) ++ "\n")                            
                            coord_head
                        else if ((fst coord_head) < (fst playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (snd coord_head)) (1+fst coord_head))==Room && isNothing(Map.lookup ((fst coord_head)+1,(snd coord_head)) (monsters dung_map))) then
                            ((fst coord_head) + 1,snd coord_head)
                        else if ((fst coord_head) > (fst playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (snd coord_head)) (-1+fst coord_head))==Room && isNothing(Map.lookup ((fst coord_head)-1,(snd coord_head)) (monsters dung_map))) then
                            ((fst coord_head) - 1,snd coord_head)
                        else if ((snd coord_head) < (snd playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (1+snd coord_head)) (fst coord_head))==Room && isNothing(Map.lookup ((fst coord_head),(snd coord_head)+1) (monsters dung_map))) then
                            (fst coord_head,(snd coord_head) + 1)
                        else if ((snd coord_head) > (snd playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (-1+snd coord_head)) (fst coord_head))==Room && isNothing(Map.lookup ((fst coord_head),(snd coord_head)-1) (monsters dung_map))) then
                            (fst coord_head,(snd coord_head) - 1)
                        else
                            trace "Did not move...\n"
                            coord_head)
            if isNothing(playerpost) then
                putStrLn "Sorry, you got KOed!\n"
            else if isNothing(monst) then
                do
                --putStrLn"The monster died!\n"
                trace "fromJust is used here...1\n" (move_monsters_impl DungeonMap{dung_floor=dung_floor(dung_map),room_data=(room_data(dung_map)), path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.delete coord_head (monsters dung_map)), randomnums=randSeq, floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))} coord_tail)
            else
                do
              --  putStrLn "Nobody died!\n"
                trace "fromJust is used here...2\n" (move_monsters_impl DungeonMap{dung_floor=dung_floor(dung_map),room_data=(room_data(dung_map)), path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.insert newpos (fromJust monst) (Map.delete coord_head (monsters dung_map))), randomnums=randSeq, floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))} coord_tail)

    select_coords :: [RoomData] -> [Int] -> (Int, Int)
    select_coords list_of_rooms rands = let
                                         select_room = (rands!!0 `rem` (length(list_of_rooms)))
                                         cur_room = (list_of_rooms)!!select_room
                                         x = rands!!1 `rem` ((down_right_x(cur_room)) - (up_left_x(cur_room)))
                                         y = rands!!2 `rem` ((down_right_y(cur_room)) - (up_left_y(cur_room)))
                                         select_x_coord = (up_left_x(cur_room)) + x
                                         select_y_coord = (up_left_y(cur_room)) + y
                                         in
                                         (select_x_coord, select_y_coord)

    make_monster :: DungeonMap -> DungeonMap
    make_monster dung_map = if (make_decision (take 10 (randomnums(dung_map))) 10) && (Map.null(monsters(dung_map))) then
                               let
                               mons_coords = select_coords (room_data(dung_map)) (take 3 (tail(randomnums(dung_map))))
                               new_mons = Character {stats = Stats{hp=20, strength=5, skill=5, speed=10, luck=5, defense=5}, items=[WeaponTag Weapon{charges=46, weight=5, might=5, hit=90, crit=0, minrange=1, maxrange=1}], status=Status{currhp=20, condition=Healthy}}
                               in
                               trace ("Monster at " ++ show(mons_coords) ++ "\n")
                               DungeonMap {dung_floor = (dung_floor(dung_map)), room_data=(room_data(dung_map)), path_data=(path_data(dung_map)), entry_point = (entry_point(dung_map)), exit_point=(exit_point(dung_map)), player=(player(dung_map)), monsters=(Map.insert (mons_coords) (new_mons) (monsters(dung_map))), randomnums = (drop 4 (randomnums(dung_map))), floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))}
                            else
                               DungeonMap {dung_floor = (dung_floor(dung_map)), room_data=(room_data(dung_map)), path_data=(path_data(dung_map)), entry_point = (entry_point(dung_map)), exit_point=(exit_point(dung_map)), player=(player(dung_map)), monsters=(monsters(dung_map)), randomnums = (tail(randomnums(dung_map))), floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))}

    get_monster :: String -> DungeonMap -> (Character, (Int, Int))
    get_monster direction dung_map = let 
                                    player_pos = (entry_point(dung_map))
                                    on_left = ((fst(player_pos)) - 1, (snd(player_pos)))
                                    on_right = ((fst(player_pos)) + 1, (snd(player_pos)))
                                    on_top = ((fst(player_pos)), (snd(player_pos))-1)
                                    on_bottom = ((fst(player_pos)), (snd(player_pos))+1)
                                    in
                                    if direction == "up" then
                                       trace "fromJust is used here...3\n"
                                       ((fromJust (Map.lookup (on_top) (monsters(dung_map)))), (on_top))
                                    else if direction == "down" then 
                                       trace "fromJust is used here...4\n"
                                       ((fromJust (Map.lookup (on_bottom) (monsters(dung_map)))), (on_bottom))
                                    else if direction == "left" then
                                       trace "fromJust is used here...5\n"
                                       ((fromJust (Map.lookup (on_left) (monsters(dung_map)))), (on_left))
                                    else 
                                       trace "fromJust is used here...6\n"
                                       ((fromJust (Map.lookup (on_right) (monsters(dung_map)))), (on_right)) 
                                     
    check_if_monster_nearby :: String -> DungeonMap -> Bool
    check_if_monster_nearby direction dung_map = let
                                                player_pos = (entry_point(dung_map))
                                                on_left = ((fst(player_pos)) - 1, (snd(player_pos)))
                                                on_right = ((fst(player_pos)) + 1, (snd(player_pos)))
                                                on_top = ((fst(player_pos)), (snd(player_pos))-1)
                                                on_bottom = ((fst(player_pos)), (snd(player_pos))+1)
                                                in
                                                if direction == "up" then
                                                   (Map.member (on_top) (monsters(dung_map)))
                                                else if direction == "down" then
                                                   (Map.member (on_bottom) (monsters(dung_map)))
                                                else if direction == "left" then 
                                                   (Map.member (on_left) (monsters(dung_map)))
                                                else 
                                                   (Map.member (on_right) (monsters(dung_map)))
                                     
    handle_wield_up dung_map
                      | check_if_monster_nearby "up" dung_map = do
                                                                       let (head_monst, coord_head) = get_monster "up" dung_map
                                                                       let playpos = (entry_point(dung_map))
                                                                       let monst_dist = (abs((fst playpos) - (fst coord_head))) + (abs((snd playpos) - (snd coord_head)))
                                                                       let (playerpost, monst, randSeq) = (combat (player dung_map) (head_monst) monst_dist (map (`rem` 100) (randomnums(dung_map))))
                                                                       let newpos = coord_head 
                                                                       if isNothing(monst) then
                                                                          do
            --                                                              putStrLn "The Monster died!!\n"
                                                                          trace "fromJust is used here...7\n" (move_monsters DungeonMap{dung_floor=dung_floor(dung_map),room_data=(room_data(dung_map)), path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.delete coord_head (monsters dung_map)), randomnums=randSeq, floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))})
                                                                       else 
                                                                          do
          --                                                                putStrLn "Nobody died..\n"
                                                                          trace "fromJust is used here...8\n" (move_monsters DungeonMap{dung_floor=dung_floor(dung_map),room_data=(room_data(dung_map)), path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.insert newpos (fromJust monst) (Map.delete coord_head (monsters dung_map))), randomnums=randSeq, floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))})
                      | otherwise = move_monsters dung_map

    handle_wield_down dung_map
                        | check_if_monster_nearby "down" dung_map = do
                                                                    let (head_monst, coord_head) = get_monster "down" dung_map
                                                                    let playpos = (entry_point(dung_map))
                                                                    let monst_dist = (abs((fst playpos) - (fst coord_head))) + (abs((snd playpos) - (snd coord_head)))
                                                                    let (playerpost, monst, randSeq) = (combat (player dung_map) (head_monst) monst_dist (map (`rem` 100) (randomnums(dung_map))))
                                                                    let newpos = coord_head 
                                                                    if isNothing(monst) then
                                                                       do
        --                                                               putStrLn "The Monster died!!\n"
                                                                       trace "fromJust is used here...9\n" (move_monsters DungeonMap{dung_floor=dung_floor(dung_map),room_data=(room_data(dung_map)), path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.delete coord_head (monsters dung_map)), randomnums=randSeq, floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))})
                                                                    else
                                                                       do
      --                                                                 putStrLn "Nobody died..\n"
                                                                       trace "fromJust is used here...10\n" (move_monsters DungeonMap{dung_floor=dung_floor(dung_map),room_data=(room_data(dung_map)), path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.insert newpos (fromJust monst) (Map.delete coord_head (monsters dung_map))), randomnums=randSeq, floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))})                                                                        
                        | otherwise = move_monsters dung_map

    handle_wield_left dung_map
                        | check_if_monster_nearby "left" dung_map = do
                                                                    let (head_monst, coord_head) = get_monster "left" dung_map
                                                                    let playpos = (entry_point(dung_map))
                                                                    let monst_dist = (abs((fst playpos) - (fst coord_head))) + (abs((snd playpos) - (snd coord_head)))
                                                                    let (playerpost, monst, randSeq) = (combat (player dung_map) (head_monst) monst_dist (map (`rem` 100) (randomnums(dung_map))))
                                                                    let newpos = coord_head 
                                                                    if isNothing(monst) then
                                                                       do
    --                                                                   putStrLn "The Monster died!!\n"
                                                                       trace "fromJust is used here...11\n" (move_monsters DungeonMap{dung_floor=dung_floor(dung_map),room_data=(room_data(dung_map)), path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.delete coord_head (monsters dung_map)), randomnums=randSeq, floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))})
                                                                    else
                                                                       do
  --                                                                     putStrLn "Nobody died..\n"
                                                                       trace "fromJust is used here...12\n" (move_monsters DungeonMap{dung_floor=dung_floor(dung_map),room_data=(room_data(dung_map)), path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.insert newpos (fromJust monst) (Map.delete coord_head (monsters dung_map))), randomnums=randSeq, floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))})
                        | otherwise = move_monsters dung_map

    handle_wield_right dung_map
                         | check_if_monster_nearby "right" dung_map = do
                                                                      let (head_monst, coord_head) = get_monster "right" dung_map
                                                                      let playpos = (entry_point(dung_map))
                                                                      let monst_dist = (abs((fst playpos) - (fst coord_head))) + (abs((snd playpos) - (snd coord_head)))
                                                                      let (playerpost, monst, randSeq) = (combat (player dung_map) (head_monst) monst_dist (map (`rem` 100) (randomnums(dung_map))))
                                                                      let newpos = coord_head 
                                                                      if isNothing(monst) then
                                                                         do
--                                                                         putStrLn "The Monster died!!\n"
                                                                         trace "fromJust is used here...13\n" (move_monsters DungeonMap{dung_floor=dung_floor(dung_map),room_data=(room_data(dung_map)), path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.delete coord_head (monsters dung_map)), randomnums=randSeq, floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))})
                                                                      else
                                                                         do
--                                                                         putStrLn "Nobody died..\n"
                                                                         trace "fromJust is used here...14\n" (move_monsters DungeonMap{dung_floor=dung_floor(dung_map),room_data=(room_data(dung_map)), path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.insert newpos (fromJust monst) (Map.delete coord_head (monsters dung_map))), randomnums=randSeq, floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))})
                         | otherwise = move_monsters dung_map
                         
    handle_quaff dung_map = let
                            cur_remain = (remain(fromJust(getPotion(items(player(dung_map))))))
                            in
                            if cur_remain > 0 then
                               let 
                               new_health = min ((currhp(status(player(dung_map))))+10) (hp(stats(player(dung_map))))
                               player_mod = Character {stats=(stats(player(dung_map))), items=(usePotion(items(player(dung_map)))), status=Status {currhp = new_health, condition=(condition(status(player(dung_map))))}}
                               in
                               game_loop DungeonMap {dung_floor = (dung_floor(dung_map)), room_data=(room_data(dung_map)), path_data=(path_data(dung_map)), entry_point = (entry_point(dung_map)), exit_point=(exit_point(dung_map)), player=player_mod, monsters=(monsters(dung_map)), randomnums = (drop 4 (randomnums(dung_map))), floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))}
                            else
                               game_loop DungeonMap {dung_floor = (dung_floor(dung_map)), room_data=(room_data(dung_map)), path_data=(path_data(dung_map)), entry_point = (entry_point(dung_map)), exit_point=(exit_point(dung_map)), player=(player(dung_map)), monsters=(monsters(dung_map)), randomnums = (drop 4 (randomnums(dung_map))), floor_number = (floor_number(dung_map)), potions=(potions(dung_map)), weapons=(weapons(dung_map))}
