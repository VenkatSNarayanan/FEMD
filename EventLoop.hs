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

    data DungeonMap = DungeonMap { dung_floor :: Data_Seq.Seq (Data_Seq.Seq Tile), path_data :: [PathData], entry_point :: (Int, Int), exit_point :: (Int, Int), player :: Character, monsters :: Map.Map (Int,Int) Character, randomnums :: [Int]}
    --data Inputs = Whatever | Quit | Display deriving(Eq)
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
                           game_loop (DungeonMap (conv_floor_to_seq floor_4) all_p entry exit pchar_init Map.empty n)

    game_loop dung_map = do
                         handle_display dung_map
                         input <- get_input
                         case input of
                              Quit -> handle_exit
                              MUp -> validate_and_move_up dung_map
                              MRight -> validate_and_move_right dung_map
                              MLeft -> validate_and_move_left dung_map
                              MDown -> validate_and_move_down dung_map
                              Quaff -> game_loop dung_map
                              WieldUp -> game_loop dung_map
                              WieldDown -> game_loop dung_map
                              WieldLeft -> game_loop dung_map
                              WieldRight -> game_loop dung_map
                              Wait -> move_monsters dung_map
                              Whatever -> game_loop dung_map

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
                              putStrLn (Data_Seq.foldrWithIndex (\i a b -> (Data_Seq.foldrWithIndex (\i a b -> a ++ b) "" a)++"\n"++b) "\n" base_map)

    handle_exit = do
                  putStrLn "GGWP!!\n"
                  return ()

    move_up dung_map = do
                      game_loop DungeonMap {dung_floor = (dung_floor(dung_map)), path_data = (path_data(dung_map)), entry_point = (-1 + (fst(entry_point(dung_map))), (snd(entry_point(dung_map)))), exit_point = (exit_point(dung_map)), player = (player(dung_map)), monsters=Map.empty, randomnums = (randomnums(dung_map))}

    move_down dung_map = do
                         game_loop DungeonMap {dung_floor = (dung_floor(dung_map)), path_data = (path_data(dung_map)), entry_point = (1 + (fst(entry_point(dung_map))), (snd(entry_point(dung_map)))), exit_point = (exit_point(dung_map)), player = (player(dung_map)), monsters=Map.empty, randomnums = (randomnums(dung_map))}

    move_left dung_map = do
                         game_loop DungeonMap {dung_floor = (dung_floor(dung_map)), path_data = (path_data(dung_map)), entry_point = ((fst(entry_point(dung_map))), -1 + (snd(entry_point(dung_map)))), exit_point = (exit_point(dung_map)), player = (player(dung_map)), monsters=Map.empty, randomnums = (randomnums(dung_map))}

    move_right dung_map = do
                          game_loop DungeonMap {dung_floor = (dung_floor(dung_map)), path_data = (path_data(dung_map)), entry_point = ((fst(entry_point(dung_map))), 1 + (snd(entry_point(dung_map)))), exit_point = (exit_point(dung_map)), player = (player(dung_map)), monsters=Map.empty, randomnums = (randomnums(dung_map))}

    validate_and_move_up dung_map = do
                                    let cur_floor = (dung_floor(dung_map))
                                    let y = (snd(entry_point(dung_map)))
                                    let x = -1 + (fst(entry_point(dung_map)))
                                    if (Data_Seq.index (Data_Seq.index cur_floor x) y) == Floor then
                                       game_loop dung_map
                                    else
                                       move_up dung_map

    validate_and_move_down dung_map = do
                                      let cur_floor = (dung_floor(dung_map))
                                      let y = (snd(entry_point(dung_map)))
                                      let x = 1 + (fst(entry_point(dung_map)))
                                      if (Data_Seq.index (Data_Seq.index cur_floor x) y) == Floor then
                                         game_loop dung_map
                                      else
                                         move_down dung_map

    validate_and_move_left dung_map = do
                                      let cur_floor = (dung_floor(dung_map))
                                      let y = -1 + (snd(entry_point(dung_map)))
                                      let x = (fst(entry_point(dung_map)))
                                      if (Data_Seq.index (Data_Seq.index cur_floor x) y) == Floor then
                                         game_loop dung_map
                                      else
                                         move_left dung_map

    validate_and_move_right dung_map = do
                                       let cur_floor = (dung_floor(dung_map))
                                       let y = 1 + (snd(entry_point(dung_map)))
                                       let x = (fst(entry_point(dung_map)))
                                       if (Data_Seq.index (Data_Seq.index cur_floor x) y) == Floor then
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
                        else if ((fst coord_head) < (fst playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (1+fst coord_head)) (snd coord_head))==Floor && isNothing(Map.lookup ((fst coord_head)+1,(snd coord_head)) (monsters dung_map))) then
                            ((fst coord_head) + 1,snd coord_head)
                        else if ((fst coord_head) > (fst playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (-1+fst coord_head)) (snd coord_head))==Floor && isNothing(Map.lookup ((fst coord_head)-1,(snd coord_head)) (monsters dung_map))) then
                            ((fst coord_head) - 1,snd coord_head)
                        else if ((snd coord_head) < (snd playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (fst coord_head)) (1+snd coord_head))==Floor && isNothing(Map.lookup ((fst coord_head),(snd coord_head)+1) (monsters dung_map))) then
                            (fst coord_head,(snd coord_head) + 1)
                        else if ((snd coord_head) > (snd playpos)) && ((Data_Seq.index (Data_Seq.index curr_floor (fst coord_head)) (-1+snd coord_head))==Floor && isNothing(Map.lookup ((fst coord_head),(snd coord_head)-1) (monsters dung_map))) then
                            (fst coord_head,(snd coord_head) - 1)
                        else
                            coord_head
            if isNothing(playerpost) then
                putStrLn "Sorry, you got KOed!\n"
            else if isNothing(monst) then
                --putStrLn "The monster died!\n"
                move_monsters_impl DungeonMap{dung_floor=dung_floor(dung_map),path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.delete coord_head (monsters dung_map)), randomnums=randSeq} coord_tail
            else
                --putStrLn "Combat occurred!"
                move_monsters_impl DungeonMap{dung_floor=dung_floor(dung_map),path_data=path_data(dung_map),entry_point=entry_point(dung_map),exit_point=exit_point(dung_map),player=fromJust(playerpost),monsters=(Map.insert newpos (fromJust monst) (Map.delete coord_head (monsters dung_map))), randomnums=randSeq} coord_tail
