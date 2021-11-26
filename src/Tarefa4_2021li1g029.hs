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
moveJogador (Jogo (mapa) (Jogador (x,y) direcao bool)) m | m == AndarEsquerda = (Jogo (mapa) (Jogador (x,y) Oeste bool))
                                                         | m == AndarDireita  = (Jogo (mapa) (Jogador (x,y) Este bool))
                                                         | m == Trepar && direcao == Oeste = (Jogo (mapa) (Jogador (x-1,y+1) Oeste bool))
                                                         | m == Trepar && direcao == Este = (Jogo (mapa) (Jogador (x+1,y+1) Este bool))
                                                         | m == InterageCaixa = (Jogo (mapa) (Jogador (x,y) direcao True))

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo (mapa) (Jogador (x,y) direcao bool)) (m:ms) | m == AndarDireita  && (head ms) == AndarDireita  = correrMovimentos (Jogo (mapa) (Jogador (x+1,y) Este bool)) (tail ms)
                                                                   | m == AndarEsquerda && (head ms) == AndarEsquerda  = correrMovimentos (Jogo (mapa) (Jogador (x-1,y) Oeste bool)) (tail ms)
                                                                   | m == Trepar && direcao == Este = correrMovimentos (Jogo (mapa) (Jogador (x+1,y+1) Este bool)) ms
                                                                   | m == Trepar && direcao == Este = correrMovimentos (Jogo (mapa) (Jogador (x+1,y+1) Este bool)) ms
                                                                   | m == InterageCaixa = correrMovimentos (Jogo (mapa) (Jogador (x,y) direcao True)) ms
                                                                   | m == InterageCaixa && (head ms == InterageCaixa ) = correrMovimentos (Jogo (mapa) (Jogador (x,y) direcao False)) (tail ms)
                                                                   


