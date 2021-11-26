{- |
Module      : Tarefa3_2021li1g029
Description : Representação textual do jogo
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g029 where

import LI12122

{-
show :: Jogo -> String 
show (Jogo mapa (Jogador coordenadas direcao temccaixa)) = unlines $ map concat $ map (map showpeca ) mapa



showjogador :: Jogador -> String
showjogador (Jogador coordenadas direcao temcaixa) = 
  case direcao of Este -> ">"
                  Oeste -> "<" 


 
showpeca :: Peca -> String 
showpeca p = case p of 
  Vazio -> " "case direcao of Este -> ">"
                  Oeste -> "<" 


 
showpeca :: Peca -> String 
showpeca p = case p of 
  Vazio -> " "
  Bloco -> "X"
  Caixa -> "C"
  Porta -> "P" 
  Bloco -> "X"
  Caixa -> "C"
  Porta -> "P"            -}
  
show' :: Jogo -> String 
show' jogo = aux (0,0) jogo where  

aux :: Coordenadas -> Jogo -> String   -- cordenadas -> (mapa jogador)
aux _ (Jogo [] _) = ""       -- mapa vazio
aux _ (Jogo [[]] _) = ""
aux (x,y) (Jogo ([]:t) jogador) = '\n' : aux (0,y+1) (Jogo t jogador)   -- linha vazia
aux (x,y) (Jogo ((h:hs):t) (Jogador (xj,yj) dir temcaixa)) 
  | (x,y) == (xj,yj-1) && temcaixa = 'c' : aux (x+1,y) (Jogo (hs:t) (Jogador (xj,yj) dir temcaixa))
  | (x,y) == (xj,yj) = if dir == Oeste 
                       then  '<' : aux (x+1,y) (Jogo (hs:t) (Jogador (xj,yj) dir temcaixa))
                       else '>' : aux (x+1,y) (Jogo (hs:t) (Jogador (xj,yj) dir temcaixa))
  | h == Bloco = 'X' : mapa
  | h == Porta = 'P' : mapa
  | h == Caixa = 'C' : mapa
  | h == Vazio = ' ' : mapa 
  where mapa = aux (x+1,y) (Jogo (hs:t) (Jogador (xj,yj) dir temcaixa))
