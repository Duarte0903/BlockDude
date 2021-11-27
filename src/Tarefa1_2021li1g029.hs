      {- |
Module      : Tarefa1_2021li1g029
Description : Validação de um potencial mapa
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g029 where

import LI12122

import Data.List


validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False 
validaPotencialMapa pecas = posicaoigual pecas == False && nportas pecas == 1 && caixaflutua pecas == False && espacovazio pecas >= 1 && ponto5daTarefa1 pecas 

-- determina se existem mais que uma pecas na mesma posicao (t1 p1)

{- | devolve a lista de coordenadas das pecas

@
coord :: [(Peca,Coordenadas)] -> [Coordenadas]
coord [] = []
coord [(x,y)] = [y]
coord (x:xs) = snd x : coord xs
@

== Exemplos de utilização

>>> coord [(Bloco,(1,2)),(Porta,(3,4))]
[(1,2),(3,4)]

-}
coord :: [(Peca,Coordenadas)] -> [Coordenadas]  
coord [] = []
coord [(x,y)] = [y]
coord (x:xs) = snd x : coord xs

{- | verifica se existem mais que uma pecas na mesma posicao

@
posicaoigual :: [(Peca,Coordenadas)] -> Bool
posicaoigual [] = False
posicaoigual (x:xs) | (snd x) `elem` (coord xs) = True
                    | otherwise = posicaoigual xs
@

== Exemplos de utilização

>>> posicaoigual [(Bloco,(1,2)),(Porta,(3,4))]
False
>>> posicaoigual [(Bloco,(1,2)),(Porta,(1,2))]
True

-}

posicaoigual :: [(Peca,Coordenadas)] -> Bool
posicaoigual [] = False
posicaoigual (x:xs) | (snd x) `elem` (coord xs) = True
                    | otherwise = posicaoigual xs

{- | determinha o n de portas(t1 p2)

@
nportas :: [(Peca,Coordenadas)] -> Int
nportas [] = 0
nportas (x:xs) =
     if fst x == Porta then 1 + nportas xs
     else nportas xs
@

== Exemplos de utilização

>>> nportas [(Porta,(1,2))]
1
>>> nportas [(Bloco,(1,2))]
0

-}
nportas :: [(Peca,Coordenadas)] -> Int
nportas [] = 0
nportas (x:xs) =
     if fst x == Porta then 1 + nportas xs
     else nportas xs

{- | verifica se e caixa (t1 p3)

@
ecaixa :: (Peca,Coordenadas) -> Bool
ecaixa (x,_) | x == Caixa = True
             | otherwise = False
@

== Exemplos de utilização

>>> ecaixa (Caixa,(1,2))
True
>>> ecaixa (Bloco,(1,2))
False

-}
ecaixa :: (Peca,Coordenadas) -> Bool
ecaixa (x,_) | x == Caixa = True
             | otherwise = False

{- | verifica se e bloco (t1 p3)

@
ebloco :: (Peca,Coordenadas) -> Bool
ebloco (x,_) | x == Bloco = True
             | otherwise = False
@

== Exemplos de utilização 

>>> ebloco (Bloco,(1,2))
True
>>> ebloco (Porta,(1,2))
False

-}
ebloco :: (Peca,Coordenadas) -> Bool
ebloco (x,_) | x == Bloco = True
             | otherwise = False

{- | verifica se a caixa flutua (t1 p3)

@
caixaflutua :: [(Peca,Coordenadas)] -> Bool
caixaflutua ((p,(x,y)):xs) =
      if ecaixa (p,(x,y-1)) || ebloco (p,(x,y-1)) then caixaflutua xs
      else False
@

== Exemplos de utilização 

>>> caixaflutua [(Caixa,(5,2))]
True
>>> caixaflutua [(Caixa,(0,1)),(Bloco,(0,0))]
False

-}
caixaflutua :: [(Peca,Coordenadas)] -> Bool
caixaflutua ((p,(x,y)):xs) =
      if ecaixa (p,(x,y-1)) || ebloco (p,(x,y-1)) then caixaflutua xs
      else False

-- verifica que existe no min um espaco vazio (t1 p4)

{- | cria uma lista de coordenadas ordenada 

@
ordena :: [(Peca,Coordenadas )] -> [Coordenadas]
ordena [] = []
ordena (x:xs) = sort (coord (x:xs))
@

== Exemplos de utilização

>>> ordena [(Bloco,(0,0)),(Porta,(1,1))]
[(0,0),(1,1)]

-}
ordena :: [(Peca,Coordenadas )] -> [Coordenadas]
ordena [] = []
ordena (x:xs) = sort (coord (x:xs))

{- | determina o comprimento da matriz

@
xmaior :: [(Peca,Coordenadas )] -> Int
xmaior [] = 0
xmaior (x:xs) = fst (last (ordena (x:xs))) + 1
@

>>> xmaior [(Bloco,(0,0)),(Bloco,(1,0))]
2

-}
xmaior :: [(Peca,Coordenadas )] -> Int
xmaior [] = 0
xmaior (x:xs) = fst (last (ordena (x:xs))) + 1

{- | calcula a largua da matriz

@
ymaior :: [(Peca,Coordenadas )] -> Int 
ymaior [] = 0
ymaior (x:xs) = snd (last (ordena (x:xs))) + 1
@

== Exemplos de utilização

>>> ymaior [(Bloco,(0,0)),(Bloco,(0,1))]
2

-}
ymaior :: [(Peca,Coordenadas )] -> Int 
ymaior [] = 0
ymaior (x:xs) = snd (last (ordena (x:xs))) + 1

{- | calcula a area do mapa

@
areamapa :: [(Peca,Coordenadas)] -> Int
areamapa [] = 0 
areamapa (x:xs) = xmaior (x:xs) * ymaior (x:xs)
@

== Exemplos de utilização

>>> [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(0,1)),(Bloco,(1,1))]
4

-}
areamapa :: [(Peca,Coordenadas)] -> Int
areamapa [] = 0 
areamapa (x:xs) = xmaior (x:xs) * ymaior (x:xs)

{- | conta o numero de caixas

@
contacaixa :: [(Peca,Coordenadas)] -> Int 
contacaixa [] = 0
contacaixa (x:xs) = 
       if fst x == Caixa then 1 + contacaixa xs
       else contacaixa xs
@

== Exemplos de utilização

>>> contacaixa [(Caixa,(0,0))]
1
>>> contacaixa [(Bloco,(0,0))]
0

-}
contacaixa :: [(Peca,Coordenadas)] -> Int 
contacaixa [] = 0
contacaixa (x:xs) = 
       if fst x == Caixa then 1 + contacaixa xs
       else contacaixa xs

{- | conta o numero de blocos

@
contabloco :: [(Peca,Coordenadas)] -> Int
contabloco [] = 0
contabloco (x:xs) = 
       if fst x == Bloco then 1 + contabloco xs
       else contabloco xs
@

== Exemplos de utilização

>>> contabloco [(Bloco,(0,0))]
1
>>> contabloco [(Porta,(0,0))]
0

-}
contabloco :: [(Peca,Coordenadas)] -> Int
contabloco [] = 0
contabloco (x:xs) = 
       if fst x == Bloco then 1 + contabloco xs
       else contabloco xs

{- | conta o numero de portas

@
contaporta :: [(Peca,Coordenadas)] -> Int 
contaporta [] = 0
contaporta (x:xs) =
       if fst x == Porta then 1 + contaporta xs
       else contaporta xs
@

== Exemplos de utlização

>>> contaporta [(Porta,(0,0))]
1
>>> contaporta [(Bloco,(0,0))]
0


-}
contaporta :: [(Peca,Coordenadas)] -> Int 
contaporta [] = 0
contaporta (x:xs) =
       if fst x == Porta then 1 + contaporta xs
       else contaporta xs

{- | conta os espcaços vazios

@
espacovazio :: [(Peca,Coordenadas)] -> Int
espacovazio [] = 0
espacovazio (x:xs) = areamapa (x:xs) - (contaporta (x:xs) + contabloco (x:xs)+ contacaixa (x:xs))
@

>>> espacovazio [(Bloco,(1,0)),(Bloco,(0,1)),(Bloco,(1,1))]
1

-}
espacovazio :: [(Peca,Coordenadas)] -> Int
espacovazio [] = 0
espacovazio (x:xs) = areamapa (x:xs) - (contaporta (x:xs) + contabloco (x:xs)+ contacaixa (x:xs))

-- verifica que a base do mapa e composta por blocos (t1 p5)

ponto5daTarefa1 :: [(Peca, Coordenadas)] -> Bool
ponto5daTarefa1 lo = all (\(x,l) -> Bloco == (fst $ last l)) l
    where l = ordenaColunas lo

ordenaColunas :: [(Peca, Coordenadas)] -> [(Int, [(Peca, Int)])] -- Peças por ordem, mas colunas não
ordenaColunas [] = []
ordenaColunas l'@((p,(x,y)):t) = (x,l) : ordenaColunas t
    where
        l = sortOn snd $ aux x l'
        -- aux :: Int -> [(Peca, Coordenada)] -> [(Peca,Int)] -- Int é a linha
        aux _ [] = []
        aux i ((p,(x,y)):t)
            | x == i = (p,y) : aux i t
            | otherwise = aux i t










