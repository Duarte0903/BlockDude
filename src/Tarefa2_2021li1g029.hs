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

matrizvazia :: Int -> Int -> [(Peca,Coordenadas)]
matrizvazia 0 0 = [(Vazio,(0,0))]
matrizvazia 0 x = sortOn snd ([(Vazio,(x,0))] ++ matrizvazia 0 (x-1))
matrizvazia y 0 = sortOn snd ([(Vazio,(0,y))] ++ matrizvazia (y-1) 0)
matrizvazia y x | x > 0 && y > 0 = sortOn snd ((matrizvazia y x) ++ (matrizvazia (y-1) (x-1))) 
                                  


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


desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = undefined

 

