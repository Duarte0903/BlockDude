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

mapaporlinhas :: [(Peca,Coordenadas)] -> [[(Peca,Coordenadas)]]
mapaporlinhas [] = []
mapaporlinhas (x:xs) = 
     if snd (snd x) == snd (snd (head xs)) then [[x,head xs]] ++ mapaporlinhas (tail xs)
     else [[x],[head xs]] ++ mapaporlinhas (tail xs)
    where (x:xs) = sortOn snd (x:xs)


introduzvazio :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
introduzvazio [] = []
introduzvazio ((p,(x,y)):xs) = undefined 
                    
                     



desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = undefined



