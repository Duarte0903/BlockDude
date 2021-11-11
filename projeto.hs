module PROJETO where
import Text.Show (Show)

type Coordenadas = (Int,Int)
data Peca = Bloco | Porta | Caixa | Vazio deriving Show
type Mapa = [[Peca]] 

data Direcao = Este | Oeste
data Jogador = Jogador Coordenadas Direcao Bool

data Jogo = Jogo Mapa Jogador

-- determinha o n de elemetos iguais (t1 p2)
elemiguais :: Peca -> [Peca] -> Int
elemiguais n [] = 0
elemiguais n (x:xs) = 
     if n == x then 1 + elemiguais n xs 
     else elemiguais n xs

-- verifica se e caixa (t1 p3)
ecaixa :: (Peca,Coordenadas) -> Bool
ecaixa (x,_) | X == Caixa = True
             | otherwise = False
     
-- verifica se e bloco (t1 p3)
ebloco :: (Peca,Coordenadas) -> Bool
ebloco (x,_) | x == Bloco = True
             | otherwise = False
   

-- verifica se a caixa flutua (t1 p3)
caixaflutua :: [(Peca,Coordenadas)] -> Bool
caixaflutua [] = False

{-validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False 
validaPotencialMapa (x:xs) | snd x == snd (head xs) = False
                           |
                           | -}
                           
                          
