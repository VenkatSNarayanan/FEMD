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

    data DungeonMap = DungeonMap { dung_floor :: Data_Seq.Seq (Data_Seq.Seq Tile), room_data :: [RoomData], path_data :: [PathData], entry_point :: (Int, Int), exit_point :: (Int, Int), player :: Character, monsters :: Map.Map (Int,Int) Character, randomnums :: [Int]}
    data Inputs = Quit | MLeft | MDown | MUp | MRight | Quaff | WieldLeft | WieldDown | WieldUp | WieldRight | Wait | Whatever

    eventloop pchar_init = do
                           let rand_n = randomRs (0, maxBound :: Int) (mkStdGen 0)
                           let (n, lor) = place_rooms 10 10 10 rand_n
                           let entry = get_entry_point lor
                           let exit = get_exit_point lor
                           let exit_room = build_room (fst exit) (snd exit) (fst exit) (snd exit)
                           let all_p = make_all_paths lor
                           let floor_1 = floor_init 10 10 Floor
                           let floor_2 = floor_build lor floor_1 Room
                           let floor_3 = floor_build (exit_room:[]) floor_2 Exit
                           let floor_4 = path_build all_p floor_3
                           game_loop (DungeonMap (conv_floor_to_seq floor_4) lor all_p entry exit pchar_init Map.empty n)

    game_loop dung_map = do
                         let new_dung_map = make_monster dung_map
                         handle_display new_dung_map
                         input <- get_input
                         case input of
                              Quit -> handle_exit
                              MUp -> validate_and_move_up new_dung_map
                              MRight -> validate_and_move_right new_dung_map
                              MLeft -> validate_and_move_left new_dung_map
                              MDown -> validate_and_move_down new_dung_map
                              Quaff -> game_loop new_dung_map
                              WieldUp -> game_loop new_dung_map
                              WieldDown -> game_loop new_dung_map
                              WieldLeft -> game_loop new_dung_map
                              WieldRight -> game_loop new_dung_map
                              Wait -> move_monsters new_dung_map
                              Whatever -> game_loop new_dung_map

    get_input = do
                what_to_do <- getLine
                case what_to_do of
                     "x" -> return Quit
                     "h" -> return MLeft
                     "j" -> return MDown
                     "k" -> return MUp
                     "l" -> return MRight
                     "q" -> return Quaff
                     "w" -> return WieldUp
                     "a" -> return WieldLeft
                     "s" -> return WieldDown
                     "d" -> return WieldRight
                     "m" -> return Wait
                     _   -> return Whatever

    handle_display dung_map = do
                              let floor_map = floor_display (dung_floor(dung_map))
                              let base_map = Data_Seq.update (fst $ entry_point dung_map) (Data_Seq.update (snd $ entry_point dung_map) "@" (Data_Seq.index floor_map (fst $ entry_point dung_map))) floor_map
                              let monst_map = Map.foldrWithKey (\(row,col) _ basemap -> Data_Seq.update (row) (Data_Seq.update (col) "T" (Data_Seq.index basemap (row))) basemap) base_map (monsters dung_map)
                              putStrLn (Data_Seq.foldrWithIndex (\i a b -> (Data_Seq.foldrWithIndex (\i a b -> a ++ b) "" a)++"\n"++b) "\n" monst_map)

    handle_exit = do
                  putStrLn "GGWP!!\n"
                  return ()

    move_up dung_map = do
                      game_loop DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (-1+(fst(entry_point(dung_map))), (snd(entry_point(dung_map)))), exit_point = (exit_point(dung_map)), player = (player(dung_map)), monsters=Map.empty, randomnums = (randomnums(dung_map))}

    move_down dung_map = do
                         game_loop DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = (1+(fst(entry_point(dung_map))), (snd(entry_point(dung_map)))), exit_point = (exit_point(dung_map)), player = (player(dung_map)), monsters=Map.empty, randomnums = (randomnums(dung_map))}

    move_left dung_map = do
                         game_loop DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = ((fst(entry_point(dung_map))), (-1+snd(entry_point(dung_map)))), exit_point = (exit_point(dung_map)), player = (player(dung_map)), monsters=Map.empty, randomnums = (randomnums(dung_map))}

    move_right dung_map = do
                          game_loop DungeonMap {dung_floor = (dung_floor(dung_map)), room_data = (room_data(dung_map)), path_data = (path_data(dung_map)), entry_point = ((fst(entry_point(dung_map))), (1+snd(entry_point(dung_map)))), exit_point = (exit_point(dung_map)), player = (player(dung_map)), monsters=Map.empty, randomnums = (randomnums(dung_map))}

    validate_and_move_up dung_map = do
                                    let cur_floor = (dung_floor(dung_map))
                                    let y = (snd(entry_point(dung_map)))
                                    let x = -1 + (fst(entry_point(dung_map)))
                                    if (Data_Seq.index (Data_Seq.index cur_floor x) y) == Floor || isJust(Map.lookup (x,y) (monsters dung_map)) then
                                       game_loop dung_map
                                    else
                                       move_up dung_map

    validate_and_move_down dung_map = do
                                      let cur_floor = (dung_floor(dung_map))
                                      let y = (snd(entry_point(dung_map)))
                                      let x = 1 + (fst(entry_point(dung_map)))
                                      if (Data_Seq.index (Data_Seq.index cur_floor x) y) == Floor || isJust(Map.lookup (x,y) (monsters dung_map)) then
                                         game_loop dung_map
                                      else
                                         move_down dung_map

    validate_and_move_left dung_map = do
                                      let cur_floor = (dung_floor(dung_map))
                                      let y = -1 + (snd(entry_point(dung_map)))
                                      let x = (fst(entry_point(dung_map)))
                                      if (Data_Seq.index (Data_Seq.index cur_floor x) y) == Floor || isJust(Map.lookup (x,y) (monsters dung_map)) then
                                         game_loop dung_map
                                      else
                                         move_left dung_map

    validate_and_move_right dung_map = do
                                       let cur_floor = (dung_floor(dung_map))
                                       let y = 1 + (snd(entry_point(dung_map)))
                                       let x = (fst(entry_point(dung_map)))
                                       if (Data_Seq.index (Data_Seq.index cur_floor x) y) == Floor || isJust(Map.lookup (x,y) (monsters dung_map)) then
                                          game_loop dung_map
                                       else
                                          move_right dung_map

    move_monsters dung_map = move_monsters_impl dung_map (map (\(a,b) -> a) (Map.toList (monsters dung_map)))

    move_monsters_impl :: DungeonMap -> [(Int,Int)] -> IO ()
    move_monsters_impl dung_map [] = game_loop dung_map
    move_monsters_impl dung_map (coord_head:coord_tail) =
        do
            let playpos = entry_point dung_map
            let head_monster = fromJust (Map.lookup coord_head (monsters dung_map))
            let monster_weapon = getWeapon (items head_monster)
            let absvalue = (\a -> if (a<0) then (-a) else a)
            let monst_dist = (absvalue((fst playpos) - (fst coord_head))) + (absvalue((snd playpos) - (snd coord_head)))
            let can_attack = fromMaybe False (monster_weapon >>= (\w -> Just (inRange monst_dist (minrange w) (maxrange w))))
            let curr_floor = dung_floor(dung_map)
            let (monst,playerpost,randSeq) = if can_attack then
                                                combat head_monster (player dung_map) monst_dist (randomnums dung_map)
                                        else
                                            (Just head_monster,Just (player dung_map),(randomnums dung_map))
            let newpos = if (can_attack) then
                            coord_head
                        else if ((fst coord_head) < (fst playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (1+fst coord_head)) (snd coord_head))==Room && isNothing(Map.lookup ((fst coord_head)+1,(snd coord_head)) (monsters dung_map))) then
                            ((fst coord_head) + 1,snd coord_head)
                        else if ((fst coord_head) > (fst playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (-1+fst coord_head)) (snd coord_head))==Room && isNothing(Map.lookup ((fst coord_head)-1,(snd coord_head)) (monsters dung_map))) then
                            ((fst coord_head) - 1,snd coord_head)
                        else if ((snd coord_head) < (snd playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (fst coord_head)) (1+snd coord_head))==Room && isNothing(Map.lookup ((fst coord_head),(snd coord_head)+1) (monsters dung_map))) then
                            (fst coord_head,(snd coord_head) + 1)
                        else if ((snd coord_head) > (snd playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (fst coord_head)) (-1+snd coord_head))==Room && isNothing(Map.lookup ((fst coord_head),(snd coord_head)-1) (monsters dung_map))) then
                            (fst coord_head,(snd coord_head) - 1)
                        else
                            coord_head
            if isNothing(playerpost) then
                putStrLn "Sorry, you got KOed!\n"
            else if isNothing(monst) then
                do
                putStrLn"The monster died!\n"
                move_monsters_impl DungeonMap{dung_floor=dung_floor(dung_map),room_data=(room_data(dung_map)), path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.delete coord_head (monsters dung_map)), randomnums=randSeq} coord_tail
            else
                do
                putStrLn "Combat occurred!"
                move_monsters_impl DungeonMap{dung_floor=dung_floor(dung_map),room_data=(room_data(dung_map)), path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.insert newpos (fromJust monst) (Map.delete coord_head (monsters dung_map))), randomnums=randSeq} coord_tail

    select_coords :: [RoomData] -> [Int] -> (Int, Int)
    select_coords list_of_rooms rands = let
                                         select_room = (rands!!0 `rem` (length(list_of_rooms)))
                                         cur_room = (list_of_rooms)!!select_room
                                         select_x_coord = (up_left_x(cur_room)) + (rands!!1 `rem` ((down_right_x(cur_room)) - (up_left_x(cur_room)) + 1))
                                         select_y_coord = (up_left_y(cur_room)) + (rands!!2 `rem` ((down_right_y(cur_room)) - (up_left_y(cur_room)) + 1))
                                         in
                                         (select_x_coord, select_y_coord)

    make_monster :: DungeonMap -> DungeonMap
    make_monster dung_map = if (make_decision (take 10 (randomnums(dung_map))) 10) then
                               let
                               mons_coords = select_coords (room_data(dung_map)) (take 3 (tail(randomnums(dung_map))))
                               new_mons = Character {stats = Stats{hp=20, strength=10, skill=5, speed=10, luck=5, defense=5}, items=[WeaponTag Weapon{charges=46, weight=5, might=5, hit=90, crit=0, minrange=1, maxrange=1}], status=Status{currhp=20, condition=Healthy}}
                               in
                               DungeonMap {dung_floor = (dung_floor(dung_map)), room_data=(room_data(dung_map)), path_data=(path_data(dung_map)), entry_point = (entry_point(dung_map)), exit_point=(exit_point(dung_map)), player=(player(dung_map)), monsters=(Map.insert (mons_coords) (new_mons) (monsters(dung_map))), randomnums = (drop 4 (randomnums(dung_map)))}
                            else
                               DungeonMap {dung_floor = (dung_floor(dung_map)), room_data=(room_data(dung_map)), path_data=(path_data(dung_map)), entry_point = (entry_point(dung_map)), exit_point=(exit_point(dung_map)), player=(player(dung_map)), monsters=(monsters(dung_map)), randomnums = (tail(randomnums(dung_map)))}

    make_decision :: [Int] -> Int -> Bool
    make_decision randoms percentage = let
                                       rand_100 = map (`rem` 100) randoms
                                       new_randoms = filter (> (100 - percentage)) rand_100
                                       in
                                       if (length(new_randoms)) > 0 then
                                          True
                                       else
                                          False
