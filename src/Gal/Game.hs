module Gal.Game where

import Foreign.C (CInt)
import Data.Function ((&))

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

processEvent :: GameEvent -> Game -> (Game, [GameEvent])
processEvent (PlayerMoved (x, y)) game = (movePlayer x y game, coinsStolen game)
processEvent (CoinStolen c) game       = (stealCoin c game, [])
-- On restart, keep the player at the mouse position.
processEvent Restart game              = (initialGameState
                                          & movePlayer (playerX $ player game)
                                                       (playerY $ player game)
                                         , [])

processEvents :: [GameEvent] -> Game -> (Game, [GameEvent])
processEvents evs game =
  foldr (\ev (g, es) ->
           let
             (g', es') = processEvent ev g
           in
             (g', es <> es')
        ) (game, []) evs

movePlayer :: CInt -> CInt -> Game -> Game
movePlayer x y game = game { player = (player game) { playerX = x
                                                    , playerY = y
                                                   }
                           }

stealCoin :: Coin -> Game -> Game
stealCoin c game = game { coins = filter (/= c) (coins game) }

coinsStolen :: Game -> [GameEvent]
coinsStolen game =
  CoinStolen <$> filter (isTouching $ player game) (coins game)

isTouching :: Player -> Coin -> Bool
isTouching p c =
  playerX p + playerWidth p > coinX c
  && playerY p + playerHeight p > coinY c
  && coinX c + coinWidth c > playerX p
  && coinY c + coinHeight c > playerY p

initialGameState :: Game
initialGameState =
  let
    p = Player 0 0 32 32
    cs = [ Coin 100 200 32 32, Coin 450 200 32 32 ]
  in
    Game { player = p
         , coins = cs
         }
