{- |
Module      : Tarefa5_2021li1g029
Description : Aplicação Gráfica
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy(loadJuicy)
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display

texto :: Picture
texto = translate 200 200 (Text "Jogar") 

window :: Display 
window = InWindow
  "Janela de Exemplo" -- Nome da Janela
  (1366,768)           -- Dimensão da Janela
  (683,384)             -- Posição no ecrã 

background :: Color 
background = white

main :: IO ()
main = do display window background texto