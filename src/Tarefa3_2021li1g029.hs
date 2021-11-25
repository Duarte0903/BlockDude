{- |
Module      : Tarefa3_2021li1g029
Description : Representação textual do jogo
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g029 where

import LI12122

import Tarefa1_2021li1g029

instance Show Jogo where
  show = undefined

show :: Jogo -> String 
show (Jogo mapa (Jogador coordenadas direcao temccaixa)) = unlines $ map concat $ map (map showpeca ) mapa



showjogador :: Jogador -> String
showjogador (Jogador coordenadas direcao temcaixa) = 
  case direcao of Este -> ">"
                  Oeste -> "<" 


 
showpeca :: Peca -> String 
showpeca p = case p of 
  Vazio -> " "
  Bloco -> "X"
  Caixa -> "C"
  Porta -> "P"             