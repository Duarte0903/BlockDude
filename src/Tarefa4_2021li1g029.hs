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

import Tarefa1_2021li1g029
import Graphics.Rendering.OpenGL (PixelInternalFormat(CompressedRedRGTC1))

moveJogador :: Jogo -> Movimento -> Jogo         -- jogo = (mapa (coordenadas direcao bool))
moveJogador (Jogo (mapa) (Jogador (x,y) direcao bool)) m | m == AndarEsquerda = (Jogo (mapa) (Jogador (x,y) Oeste bool))
                                                         | m == AndarDireita  = (Jogo (mapa) (Jogador (x,y) Este bool))
                                                         | m == Trepar && direcao == Oeste = (Jogo (mapa) (Jogador (x-1,y+1) Oeste bool))
                                                         | m == Trepar && direcao == Este = (Jogo (mapa) (Jogador (x+1,y+1) Este bool))
                                                         | m == InterageCaixa = (Jogo (mapa) (Jogador (x,y) direcao True))

blocoempilhado :: [(Peca,Coordenadas)] -> Bool 
blocoempilhado [] = False 
blocoempilhado ((p,(x,y)):xs) = 
     if ebloco (p,(x,y+1)) then True 
     else False 

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo (mapa) (Jogador (x,y) direcao False)) (m:ms) -- jogador sem caixa
 | m == AndarDireita  && (head ms) == AndarDireita  = correrMovimentos (Jogo (mapa) (Jogador (x+1,y) Este False)) (tail ms)
 | m == AndarEsquerda && (head ms) == AndarEsquerda  = correrMovimentos (Jogo (mapa) (Jogador (x-1,y) Oeste False)) (tail ms)
 
correrMovimentos (Jogo (mapa) (Jogador (x,y) direcao False)) (m:ms) -- jogador trepa sem caixa
 | m == Trepar && direcao == Este = correrMovimentos (Jogo (mapa) (Jogador (x+1,y+1) Este False)) ms
 | m == Trepar && direcao == Oeste = correrMovimentos (Jogo (mapa) (Jogador (x-1,y-1) Oeste False)) ms
 
correrMovimentos (Jogo (mapa) (Jogador (x,y) direcao False)) (m:ms) -- jogador interage com a caixa
 | m == InterageCaixa = correrMovimentos (Jogo (mapa) (Jogador (x,y) direcao True)) ms  -- pega na caixa
 | m == InterageCaixa && (head ms == InterageCaixa ) = correrMovimentos (Jogo ( mapa) (Jogador (x,y) direcao False)) (tail ms) -- pega e larga a caixa
correrMovimentos (Jogo (mapa) (Jogador (x,y) direcao True)) (InterageCaixa:ms) = (Jogo (mapa) (Jogador (x,y) direcao False)) -- deixa cair a caixa
correrMovimentos (Jogo (mapa) (Jogador (x,y) direcao True)) (m:ms)    -- anda com a caixa em cima 
 | m == AndarDireita && (head ms) == AndarDireita  = correrMovimentos (Jogo (mapa) (Jogador (x+1,y) Este True)) (tail ms)   -- anda com caixa para direita                                                              
 | m == AndarEsquerda && (head ms) == AndarEsquerda  = correrMovimentos (Jogo (mapa) (Jogador (x-1,y) Oeste True)) (tail ms)  -- anda com caixa para esquerda 

correrMovimentos (Jogo (mapa) (Jogador (x,y) direcao True)) (m:ms)
 | m == Trepar && direcao == Este = correrMovimentos (Jogo (mapa) (Jogador (x+1,y+1) Este True)) ms -- trepa com caixa
 | m == Trepar && direcao == Oeste = correrMovimentos (Jogo (mapa) (Jogador (x-1,y-1) Oeste True)) ms