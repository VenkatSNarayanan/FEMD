module Entity where
    import Control.Monad
    import Data.Maybe
    import Control.Exception
    import Control.Arrow
    import Debug.Trace

    data Weapon = Weapon {charges :: Int, weight :: Int, might::Int, hit:: Int, crit:: Int, minrange :: Int, maxrange :: Int} deriving(Show)
    data PotionEffect = Heal | Restore | Poison deriving (Show)
    data Condition = Healthy | Poisoned deriving (Show)
    data Potion = Potion {effect :: PotionEffect, remain :: Int} deriving (Show)
    data Item = WeaponTag Weapon | PotionTag Potion deriving(Show)
    --data Item = (Right Weapon) | (Left Potion) deriving (Show)
    data Stats = Stats { hp :: Int, strength :: Int,skill :: Int, speed :: Int, luck:: Int, defense :: Int} deriving (Show)
    data Status = Status { currhp :: Int, condition :: Condition} deriving (Show)
    data Character = Character {stats:: Stats, items :: [Item], status :: Status} deriving (Show)

    getWeapon :: [Item] -> Maybe Weapon
    getWeapon [] = Nothing
    getWeapon ((PotionTag _):xs) = getWeapon xs
    getWeapon ((WeaponTag weapon) :xs) = Just (weapon)
    
    getPotion :: [Item] -> Maybe Potion
    getPotion [] = Nothing
    getPotion ((WeaponTag _):xs) = getPotion xs
    getPotion ((PotionTag potion) :xs) = Just (potion)

    useWeapon :: [Item] -> [Item]
    useWeapon [] = []
    useWeapon ((PotionTag x):xs) = (PotionTag x):useWeapon xs
    useWeapon ((WeaponTag weapon):xs) =
        if (charges weapon)==1
        then xs
        else (WeaponTag weapon{charges=((charges weapon)-1)}):xs
        
    usePotion :: [Item] -> [Item]
    usePotion [] = []
    usePotion ((WeaponTag x):xs) = (WeaponTag x):usePotion xs
    usePotion ((PotionTag potion):xs) = (PotionTag potion{remain=((remain potion)-1)}):xs
    
    digPotion :: [Item] -> [Item]
    digPotion [] = []
    digPotion ((WeaponTag x):xs) = (WeaponTag x):digPotion xs
    digPotion ((PotionTag potion):xs) = (PotionTag potion{remain=((remain potion)+1)}):xs

    attackspeed :: Character -> Int
    attackspeed charac = (speed (stats charac)) - (fromMaybe 0 (liftM weight (getWeapon (items charac))))

    hitrate :: Character -> Int
    hitrate charac = (2 * (skill (stats charac))) + ((luck (stats charac)) `div` 2) + (fromMaybe 0 (liftM hit (getWeapon (items charac))))

    evaderate :: Character -> Int
    evaderate charac = (2 * (speed (stats charac))) + (luck (stats charac))

    inRange :: Int -> Int -> Int -> Bool
    inRange a l u = (a >= l) && (a <= u)

    combat :: Character -> Character -> Int -> [Int] -> (Maybe Character,Maybe Character,[Int])
    combat (Character {stats=stats1,items=items1,status=status1}) (Character {stats=stats2,items=items2,status=status2}) range randomstream =
        let weapon1 = trace "Just entered combat!!!\n" (getWeapon items1)
            weapon2 = getWeapon items2
            p1 = trace (show(Character {stats=stats1,items=items1,status=status1}) ++ "\n") Character {stats=stats1,items=items1,status=status1}
            p2 = trace (show(Character {stats=stats2,items=items2,status=status2}) ++ "\n") (Character {stats=stats2,items=items2,status=status2})
            as1 = attackspeed p1
            as2 = attackspeed p2
            hitrate1 = hitrate p1
            evaderate1 = evaderate p1
            hitrate2 = hitrate p2
            evaderate2 = evaderate p2
            ishit1 = fromMaybe False (weapon1 >> (Just (hitrate1-evaderate2)) >>=
                (\hitrate ->
                    if (hitrate < head (randomstream))
                    then Just False
                    else Just True))
            strstate1 = (trace ("Attacker 1 Hit? " ++ (show(ishit1)) ++ "\n"))
                        (tail randomstream)
            dmg1 = max 0 (
                if ishit1
                then ((strength stats1)-(defense stats2)+(fromMaybe 0 (liftM might weapon1)))
                else 0)
            survive1 =
                if dmg1 >= (currhp status2)
                then False
                else True
            ishit2 =
                if (survive1 && (fromMaybe False ((getWeapon (items p2))>>=(\weapon -> Just (inRange range (minrange weapon) (maxrange weapon))))))
                then (fromMaybe False (weapon2 >> (Just (hitrate2-evaderate1)) >>= (\hitrate ->
                    if (hitrate < head (strstate1))
                    then Just False
                    else Just True)))
                else False
            strstate2 =
                (trace ("Attacker 2 Hit? " ++ (show(ishit2)) ++ "\n"))
                (if (survive1)
                then tail(strstate1)
                else strstate1)
            dmg2 = max 0 (
                if ishit2
                then ((strength stats2)-(defense stats1)+(fromMaybe 0 (liftM might weapon1)))
                else 0)
            survive2 =
                if dmg2 >= (currhp status2)
                then False
                else True
            weaponuse1 =
                if ishit1
                then useWeapon items1
                else items1
            weaponuse2 =
                if ishit2
                then useWeapon items2
                else items2
            currhp1 = (currhp status1)-dmg2
            currhp2 = (currhp status2)-dmg1
            cond1 = condition status1
            cond2 = condition status2
            newstatus1 = Status {currhp=currhp1,condition=cond1}
            newstatus2 = Status {currhp=currhp2,condition=cond2}
            in
                (
                if (currhp1 > 0)
                then Just (Character {stats=stats1,items=weaponuse1,status=newstatus1})
                else (Nothing),
                if (currhp2 > 0)
                then Just ((Character {stats=stats2,items=weaponuse2,status=newstatus2}))
                else (Nothing),
                trace "going to exit combat!!\n"strstate2
                )
