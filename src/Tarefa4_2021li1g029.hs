    {- |
Module      : Tarefa4_2021li1g029
Description : Movimentação do personagem
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g029 where

import LI12122
import GHCi.Message (THMessage(AddCorePlugin))
import Data.Text.Unsafe (Iter(Iter))


moveJogador :: Jogo -> Movimento -> Jogo         -- jogo = (mapa (coordenadas direcao bool))
moveJogador (Jogo (mapa) (Jogador (x,y) dir b)) m | m == AndarEsquerda = (Jogo (mapa) (Jogador (x,y) Oeste b))                                  -- vira para oeste
                                                  | m == AndarEsquerda && dir == Oeste = (Jogo (mapa) (Jogador (x-1,y) Oeste b))                -- anda para oeste
                                                  | m == AndarDireita = (Jogo (mapa) (Jogador (x,y) Este b))                                    -- vira para este
                                                  | m == AndarEsquerda && dir == Oeste = (Jogo (mapa) (Jogador (x+1,y) Este b))                 -- anda para este
                                                  | m == Trepar && dir == Oeste = (Jogo (mapa) (Jogador (x-1,y+1) Oeste b))                     -- trepa virado para oeste
                                                  | m == Trepar && dir == Este = (Jogo (mapa) (Jogador (x+1,y+1) Este b))                       -- trepa virado para este
                                                  | m == InterageCaixa && dir == Este && b == False = (Jogo (mapa) (Jogador (x,y) Este True))   -- pega na caixa virado para este 
                                                  | m == InterageCaixa && dir == Este && b == True = (Jogo (mapa) (Jogador (x,y) Este False))   -- larga a caixa virado para este
                                                  | m == InterageCaixa && dir == Oeste && b == False = (Jogo (mapa) (Jogador (x,y) Oeste True)) -- pega na caixa virado para oeste 
                                                  | m == InterageCaixa && dir == Oeste && b == True = (Jogo (mapa) (Jogador (x,y) Oeste False)) -- larga a caixa virado para oeste 
                                                            

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo (mapa) (Jogador (x,y) dir b)) [] = (Jogo (mapa) (Jogador (x,y) Este False))
correrMovimentos (Jogo (mapa) (Jogador (x,y) dir b)) l = aux1 (Jogo (mapa) (Jogador (x,y) dir b)) l

aux1 :: Jogo -> [Movimento] -> Jogo
aux1 (Jogo (mapa) (Jogador (x,y) dir b)) l = aux3 (aux2 (Jogo (mapa) (Jogador (x,y) dir b)) l)

aux2 :: Jogo -> [Movimento] -> [Jogo]
aux2 (Jogo (mapa) (Jogador (x,y) dir b)) [] = [(Jogo (mapa) (Jogador (x,y) dir b))]
aux2 (Jogo (mapa) (Jogador (x,y) dir b)) (h:t) = moveJogador (Jogo (mapa) (Jogador (x,y) dir b)) h : aux2 (Jogo (mapa) (Jogador (x,y) dir b)) t

aux3 :: [Jogo] -> Jogo 
aux3 [(Jogo (mapa) (Jogador (x,y) dir b))] = (Jogo (mapa) (Jogador (x,y) dir b))
aux3 (h:t) = aux3 t