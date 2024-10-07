module Gal.Game.Types where

import Foreign.C (CInt)

data Player = Player { playerX :: CInt
                     , playerY :: CInt
                     , playerWidth :: CInt
                     , playerHeight :: CInt
                     }
  deriving (Eq, Ord, Show)

data Coin = Coin { coinX :: CInt
                 , coinY :: CInt
                 , coinWidth :: CInt
                 , coinHeight :: CInt
                 }
  deriving (Eq, Ord, Show)

data Game = Game { player :: Player
                 , coins :: [Coin]
                 }
  deriving (Eq, Ord, Show)

data GameEvent = PlayerMoved (CInt, CInt)
               | CoinStolen Coin
               | Restart
  deriving (Eq, Ord, Show)
