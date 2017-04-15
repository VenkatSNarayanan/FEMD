{-# LANGUAGE TemplateHaskell#-}
module Entity where
    import Control.Monad
    import Data.Maybe
    import Control.Exception
    import Control.Arrow
    import Control.Lens

    data Weapon = Weapon {_charges :: Int, _weight :: Int, _might::Int, _hit:: Int, _crit:: Int, _minrange :: Int, _maxrange :: Int} deriving(Show)
    makeLenses ''Weapon
    data PotionEffect = Heal | Restore | Poison deriving (Show)
    data Condition = Healthy | Poisoned deriving (Show)
    data Potion = Potion {_effect :: PotionEffect, _remain :: Int} deriving (Show)
    makeLenses ''Potion
    data Item = WeaponTag Weapon | PotionTag Potion deriving(Show)
    data Stats = Stats {_hp :: Int, _strength :: Int,_skill :: Int, _speed :: Int, _luck:: Int, _defense :: Int} deriving (Show)
    makeLenses ''Stats
    data Status = Status { _currhp :: Int, _condition :: Condition} deriving (Show)
    makeLenses ''Status
    data Character = Character {_stats:: Stats, _items :: [Item], _status :: Status} deriving (Show)
    makeLenses ''Character

    getWeapon :: [Item] -> Maybe Weapon
    getWeapon items = foldr (\item rest -> case item of (WeaponTag w) -> Just (w)
                                                        (PotionTag _) -> rest) Nothing items
    getPotion :: [Item] -> Maybe Potion
    getPotion items = foldr (\item rest -> case item of (PotionTag p) -> Just (p)
                                                        (WeaponTag _) -> rest) Nothing items

    useWeapon :: [Item] -> [Item]
    useWeapon [] = []
    useWeapon ((PotionTag x):xs) = (PotionTag x):useWeapon xs
    useWeapon ((WeaponTag weapon):xs) =
        if (_charges weapon)==1
        then xs
        else (WeaponTag ((weapon) & charges -~1)):xs

    usePotion :: [Item] -> [Item]
    usePotion [] = []
    usePotion ((WeaponTag x):xs) = (WeaponTag x):usePotion xs
    usePotion ((PotionTag potion):xs) = (PotionTag ((potion) & remain -~1)):xs

    digPotion :: [Item] -> [Item]
    digPotion [] = []
    digPotion ((WeaponTag x):xs) = (WeaponTag x):digPotion xs
    digPotion ((PotionTag potion):xs) = (PotionTag ((potion) & remain +~1)):xs
    attackspeed :: Character -> Int
    attackspeed charac = (charac ^. (stats . speed)) - (fromMaybe 0 (liftM (view weight) (getWeapon (charac ^. items))))

    hitrate :: Character -> Int
    hitrate charac = (2 * (charac ^. (stats . skill))) + ((charac ^. (stats . luck)) `div` 2) + (fromMaybe 0 (liftM (view hit) (getWeapon (charac ^. items))))

    evaderate :: Character -> Int
    evaderate charac = (2 * (charac ^. (stats . speed))) + (charac ^. (stats . luck))

    inRange :: Int -> Int -> Int -> Bool
    inRange a l u = (a >= l) && (a <= u)

    combat :: Character -> Character -> Int -> [Int] -> (Maybe Character,Maybe Character,[Int])
    combat p1 p2 range randomstream =
        let weapon1 = getWeapon items1
            weapon2 = getWeapon items2
            stats1 = p1 ^. stats
            items1 = p1 ^. items
            status1 = p1 ^. status
            stats2 = p2 ^. stats
            items2 = p2 ^. items
            status2 = p2 ^. status
            as1 = attackspeed p1
            as2 = attackspeed p2
            hitrate1 = hitrate p1
            evaderate1 = evaderate p1
            hitrate2 = hitrate p2
            evaderate2 = evaderate p2
            ishit1 = fromMaybe False (liftM (\hitrate ->
                if (hitrate < head (randomstream))
                then False
                else True) (weapon1 >> (Just (hitrate1-evaderate2))))
            strstate1 = tail randomstream
            dmg1 = max 0 (
                if ishit1
                then ((stats1 ^. strength)-(stats2 ^. defense)+(fromMaybe 0 (liftM (view might) weapon1)))
                else 0)
            survive1 =
                if dmg1 >= (status2 ^. currhp)
                then False
                else True
            ishit2 =
                if (survive1 && (fromMaybe False ((liftM (\weapon -> inRange range (weapon ^. minrange) (weapon ^. maxrange))) (getWeapon (p2 ^. items)))))
                then (fromMaybe False ((liftM (\hitrate ->
                    if (hitrate < head (strstate1))
                    then False
                    else True)) (weapon2>>(Just (hitrate2-evaderate1)))))
                else False
            strstate2 =
                if (survive1)
                then tail(strstate1)
                else strstate1
            dmg2 = max 0 (
                if ishit2
                then ((stats2 ^. strength)-(stats1 ^. defense)+(fromMaybe 0 (liftM (view might) weapon1)))
                else 0)
            survive2 =
                if dmg2 >= (status2 ^. currhp)
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
            currhp1 = (status1 ^. currhp)-dmg2
            currhp2 = (status2 ^. currhp)-dmg1
            newstatus1 = status1 & currhp .~ currhp1
            newstatus2 = status2 & currhp .~ currhp2
            in
                (
                if (currhp1 > 0)
                then Just (p1 & items .~ weaponuse1 & status .~ newstatus1)
                else (Nothing),
                if (currhp2 > 0)
                then Just (p2 & items .~ weaponuse2 & status .~ newstatus2)
                else (Nothing),
                strstate2
                )
