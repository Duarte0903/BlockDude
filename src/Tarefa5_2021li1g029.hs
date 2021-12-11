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

data Jogo = Jogo (Int,Int) [(Int,Int)]

data Menu = Controlador Opcoes | Modojogo Jogo | VenceuJogo 
 
data Opcoes = Jogar | Sair 

type World = (Menu,Jogo)


window :: Display 
window = InWindow
  "Block Dude"         -- Nome da Janela
  (1366,768)           -- Dimensão da Janela
  (683,384)            -- Posição no ecrã 


fr :: Int              -- Frame Rate
fr = 25


draw :: World -> Picture 
draw (VenceuJogo, jogo) = Translate (-200) 0 (color red (Text "Vitória"))
draw (Controlador Jogar, jogo) = Pictures [Color blue $ drawOption "Jogar", Translate (20) (-90) $ drawOption "Sair", Translate (-310) (100) $ color orange   $ Text "Block Dude"]
draw (Controlador Sair, jogo) = Pictures [drawOption "Jogar", Color blue $ Translate (20) (-90) $ drawOption "Sair", Translate (-310) (100) $ color orange $ Text "Block Dude"]
draw (Modojogo (Jogo (x,y) l), jogo) = undefined 


drawOption :: String -> Picture
drawOption option = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text option


engine :: (Int,Int) -> [(Int,Int)] -> Jogo
engine p l = Jogo p (filter (p/=) l)


event :: Event -> World -> World
event _ w = w
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, jogo) = (Modojogo jogo, jogo)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar, jogo) = (Controlador Sair, jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar, jogo) = (Controlador Sair, jogo)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair, jogo) = (Controlador Jogar, jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair, jogo) = (Controlador Jogar, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, jogo) = undefined


time :: Float -> World -> World
time _ w = w 


estado :: World
estado = (Controlador Jogar, Jogo (200, 100) [(50, 50), (-250, -100), (-100, -50)])


main :: IO ()
main = do 
  play window white fr estado draw event time 
