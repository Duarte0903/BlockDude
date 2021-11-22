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
constroiMapa pecas | fst (head pecas) == Porta = Porta ++ constroiMapa (tail pecas)
                   | fst (head pecas) == Bloco = Bloco ++ constroiMapa (tail pecas)
                   | fst (head pecas) == Caixa = Caixa ++ constroiMapa (tail pecas)
                   where pecas = sort (pecas)

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = undefined
