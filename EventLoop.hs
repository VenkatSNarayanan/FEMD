module EventLoop where
    import Dungeon
    import System.Random
    import System.IO
    import Control.Monad

    data DungeonMap = DungeonMap { dung_floor :: [[Tile]], path_data :: [PathData] }
    data Inputs = Whatever | Quit | Display deriving(Eq)

    eventloop = do
                let rand_n = randomRs (0, maxBound :: Int) (mkStdGen 0)
                let (n, lor) = place_rooms 10 10 10 rand_n
                let all_p = make_all_paths lor
                let floor_1 = floor_init 10 10 Floor
                let floor_2 = floor_build lor floor_1 Room
                let floor_3 = path_build all_p floor_2
                game_loop (DungeonMap floor_3 all_p)
       
    game_loop dung_map = do
                         input <- get_input
                         case input of 
                              Quit    -> handle_exit
                              Display -> handle_display dung_map
                              Whatever-> handle_input dung_map input
                                           
    -- to be modified as per your requirements
    get_input = do
                what_to_do <- getChar 
                case what_to_do of
                     '0' -> return Quit
                     '1' -> return Display
                     _   -> return Whatever                     
                
    handle_input dung_map input = do
                                  putStrLn "DO SOMETHING!!\n"
                                  game_loop dung_map

    handle_display dung_map = do
                              floor_display (dung_floor(dung_map))
                              game_loop dung_map
                              
    handle_exit = do
                  putStrLn "GGWP!!\n"
                  return ()
