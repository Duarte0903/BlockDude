{- |
Module      : Tarefa2_2021li1g029
Description : Construção/Desconstrução do mapa
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g029 where

import LI12122

import Data.List

import Tarefa1_2021li1g029


{- | Determina o x maior

@
xmax :: [(Peca,Coordenadas )] -> Int
xmax [] = 0
xmax (x:xs) = fst (last (ordena (x:xs))) 
@

== Exemplos de utlização

>>> xmax [(Bloco,(1,2)),(Bloco,(2,2))]
2

-}
xmax :: [(Peca,Coordenadas)] -> Int
xmax [] = 0
xmax (x:xs) = fst (last (ordena (x:xs)))


{- | Determina o y maior

@
ymax :: [(Peca,Coordenadas )] -> Int 
ymax [] = 0
ymax (x:xs) = snd (last (ordena (x:xs)))
@

== Exemplos de utilização

>>> xmax [(Bloco,(1,2)),(Bloco,(2,2))]
2

-}
ymax :: [(Peca,Coordenadas)] -> Int 
ymax [] = 0
ymax (x:xs) = snd (last (ordena (x:xs)))


{- | Transforma a lista de peças e coorenadas num mapa

@
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa pecas = aux1 (sort pecas) 0 0 
@

== Exemplos de utilização

>>> constroiMapa [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),(Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0)),(Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]
[[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

-}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa pecas = aux1 (sort pecas) 0 0 


{- | verifica se o número de linhas é máximo, ou seja, se é igual a (ymax l)

@
aux1 :: [(Peca, Coordenadas)] -> Int -> Int -> Mapa
aux1 [] _ _ = [] 
aux1 l x y | y > ymax l = []
           | otherwise = aux2 l x y : aux1 l x (y+1)
@

== Exemplos de utilização 

>>> aux1 [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),(Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0)),(Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]
[(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),(Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0)),(Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]

-}
aux1 :: [(Peca, Coordenadas)] -> Int -> Int -> Mapa
aux1 [] _ _ = [] 
aux1 l x y | y > ymax l = []
           | otherwise = aux2 l x y : aux1 l x (y+1)


{- | verifica se o número de colunas é maximo, ou seja, se é igual a (xmax l)

@
aux2 :: [(Peca, Coordenadas)] -> Int -> Int -> [Peca]
aux2 l x y | x > xmax l = []
           | otherwise = aux3 l x y : aux2 l (x+1) y 
           where aux3 :: [(Peca, Coordenadas)] -> Int -> Int -> Peca
                 aux3 [] _ _ = Vazio 
                 aux3 ((p,(a,b)):t) x y | x == a && y == b = p 
                                        | otherwise = aux3 t x y
@

== Exemplos de utilização

>>> [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),(Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0)),(Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]
[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]

-}
aux2 :: [(Peca, Coordenadas)] -> Int -> Int -> [Peca]
aux2 l x y | x > xmax l = []
           | otherwise = aux3 l x y : aux2 l (x+1) y 
           where aux3 :: [(Peca, Coordenadas)] -> Int -> Int -> Peca
                 aux3 [] _ _ = Vazio 
                 aux3 ((p,(a,b)):t) x y | x == a && y == b = p 
                                        | otherwise = aux3 t x y



{- | Transforma um mapa numa lista de peças e as suas respetivas coordenadas

@
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = auxdes mapa 0 0 
@

== Exemplos de utilização

>>> desconstroiMapa [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
[(Bloco, (6,0)), (Bloco, (6,1)), (Porta, (0,2)), (Caixa, (4,2)),(Bloco, (6,2)), (Bloco, (0,3)), (Bloco, (1,3)), (Bloco, (2,3)),(Bloco, (3,3)), (Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,3))]

-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = auxdes mapa 0 0 


{- | Recebe um mapa e devolve a lista das peças e das suas coorenadas. O principal
objetivo é atribuir coordenadas.

@
auxdes :: Mapa -> Int -> Int -> [(Peca, Coordenadas)]
auxdes [] _ _ = []
auxdes (h:t) x y = aux h x y ++ auxdes t (x+1) y 
             where aux :: [Peca] -> Int -> Int -> [(Peca,Coordenadas)]
                   aux [] _ _ = []
                   aux (h:t) x y | h == Vazio = aux t x (y+1)
                                 | otherwise = (h,(x,y)) : aux t x (y+1)
@

== Exemplos de utilização 

>>> [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
[[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

-}
auxdes :: Mapa -> Int -> Int -> [(Peca, Coordenadas)]
auxdes [] _ _ = []
auxdes (h:t) x y = aux h x y ++ auxdes t (x+1) y 
             where aux :: [Peca] -> Int -> Int -> [(Peca,Coordenadas)]
                   aux [] _ _ = []
                   aux (h:t) x y | h == Vazio = aux t x (y+1)
                                 | otherwise = (h,(x,y)) : aux t x (y+1)


