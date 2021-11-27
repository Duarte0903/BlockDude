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

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa pecas = undefined 

matrizvazia :: [(Peca, Coordenadas)] -> Mapa
matrizvazia l = replicate (ymaior l) (replicate (xmaior l) Vazio)
{-
matrizporlinhas :: [(Peca, Coordenadas)] -> [[(Peca, Coordenadas)]]
matrizporlinhas [] = [[]]
matrizporlinhas x = [x]
matrizporlinhas (x:xs) = 
     if snd (snd x) == snd (snd (head xs)) then (x : (head xs)) : tail xs
     else [x] : xs -}

 
inserepeca :: [(Peca, Coordenadas)] -> Mapa -> Mapa 
inserepeca [] (y:ys) = (y:ys)
inserepeca (x:xs) [] = undefined  

                                  

{-
inserePeca :: [(Peca , Coordenadas )] -> [(Peca , Coordenadas )] -> [(Peca , Coordenadas )]
inserePeca [] [] = []
inserePeca [] y = y
inserePeca x [] = x
inserePeca (x:xs) (y:ys) |snd x == snd y = x : inserePeca xs ys 
                         |otherwise = y: inserePeca xs ys 
                         
--insereVazio :: Peca -> [(Peca , Coordenadas )] -> [(Peca , Coordenadas )] 
--insereVazio p [] = []
--insereVazio p   |snd x == snd xs = x : insereVazio p x xs
                            -- |otherwise = x: insereVazio p x xs
                            where p = Vazio 
                                


-- |p/= Bloco && p/= Caixa && p/= Porta = (Vazio,(x,y)) : metevazios t
 -- |otherwise =  (p,(x,y)) : metevazios t

-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = undefined 


auxmapa :: Mapa -> Int -> Int -> (Peca, Coordenadas)
auxmapa (x:xs) 0 0 = (head x, (0,0))
auxmapa (x:xs) a b = (head x, (a,b)) 
