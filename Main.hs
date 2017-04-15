module Main where
    import EventLoop
    import Entity
    import Dungeon
    import System.IO
    import Control.Monad
    import System.Process

    play_handle = do
           new_eventloop 0 Character {_stats = Stats{_hp=40, _strength=10, _skill=5, _speed=10, _luck=5, _defense=5}, _items = [WeaponTag Weapon{_charges=46, _weight=5, _might=5, _hit=90, _crit=0, _minrange=1, _maxrange=1}, PotionTag Potion{_remain=3}], _status=Status{_currhp=40}} 

    data Menu_Input = Play | Instructions | Exit_Game

    main = do
           system "clear"
           putStrLn "Welcome to FEMD : RogueLike in Haskell\n1. Play\n2. Instructions\n3. Exit\nEnter the option to continue on the corresponding page\n"
           menu_input <- get_menu_input
           case menu_input of
                Play -> play_handle
                Instructions -> instruction_handle
                Exit_Game -> exit_handle

    instruction_handle = do
                         system "clear"
                         putStrLn "The Instructions for playing this game are as follows: \nH --> Move Left \nJ --> Move Down \nK --> Move Up \nL --> Move Right \nW --> Wield Up / Attack Up \nA --> Wield Left / Attack Left \nS --> Wield Down / Attack Down \nD --> Wield Right / Attack Right \nQ --> Quaff / Drink Potion \n\nMonsters are represented as \"T(Therian)\"\nYou(Player) are represented as \"@\"\nPotions are represented as \"P\"\nWeapons are represented as \"w\"\nFloor that you can move on are \".\"\nYou cannot move onto Blank Space\n\n\nGood Luck, Have Fun!!!\n\n(Press any character to exit this page)\n"
                         exit_input <- getChar
                         case exit_input of
                            _  -> main

    exit_handle = do
                  putStrLn "Bye!!!!\n"
                  return ()

    get_menu_input = do
                     something_input <- getChar
                     case something_input of
                          '1' -> return Play
                          '2' -> return Instructions
                          '3' -> return Exit_Game
