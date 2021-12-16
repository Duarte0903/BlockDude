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

data Menu = Controlador Opcoes | Modojogo Jogo | VenceuJogo | Modocred Cred
 
data Opcoes = Jogar | Creditos | Sair 

data Cred = VMenu

data Imagens = Imagens { 
  background :: Picture
  }

type World = (Menu,Jogo,Imagens)

type Creditos = String 


window :: Display 
window = InWindow
  "Block Dude"         -- Nome da Janela
  (1366,768)           -- Dimensão da Janela
  (683,384)            -- Posição no ecrã 

fr :: Int              -- Frame Rate
fr = 25


draw :: World -> Picture 
draw (VenceuJogo, jogo, imgs) = Translate (-200) 0 (color red (Text "VENCEU!"))
draw (Controlador Jogar, jogo, imgs) = Pictures [ background imgs, Color blue $ drawOption "Jogar", Translate (20) (-90) $ drawOption "Sair", Translate (-35) (-165) $ drawOption "Creditos", Translate (-310) (100) $ color orange   $ Text "Block Dude"]
draw (Controlador Creditos, jogo, imgs) = Pictures [drawOption "Jogar", Translate (20) (-90) $ drawOption "Sair", Translate (-35) (-165) $ Color blue $ drawOption "Creditos", Translate (-310) (100) $ color orange   $ Text "Block Dude"]
draw (Controlador Sair, jogo, imgs) = Pictures [drawOption "Jogar", Color blue $ Translate (20) (-90) $ drawOption "Sair",Translate (-35) (-165) $ drawOption "Creditos", Translate (-310) (100) $ color orange $ Text "Block Dude"]
draw (Modojogo (Jogo (x,y) l), jogo, imgs) = undefined
draw (Modocred VMenu, jogo, imgs) = Pictures drawCredits

drawCredits :: [Picture]   -- Desenha a página dos Créditos 
drawCredits = [Translate (-155) 0 $ Scale (0.5) (0.5) $ Color blue $ Text "Duarte Leitao", Translate (-155) (-70) $ Scale (0.5) (0.5) $ Color blue $ Color blue $ Text "Joao Pereira", Translate (-25) (-300) $ Scale (0.3) (0.3) $ Color red $ Color orange $ Text "Menu"]


drawOption :: String -> Picture
drawOption option = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text option


engine :: (Int,Int) -> [(Int,Int)] -> Jogo
engine p l = Jogo p (filter (p/=) l)


event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, jogo, imgs) = (Modojogo jogo, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar, jogo, imgs) = (Controlador Creditos, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar, jogo, imgs) = (Controlador Sair, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair, jogo, imgs) = (Controlador Jogar, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair, jogo, imgs) = (Controlador Creditos, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, jogo, imgs) = undefined
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Creditos, jogo, imgs) = (Controlador Jogar, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Creditos, jogo, imgs) = (Controlador Sair, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Creditos, jogo, imgs) = (Modocred VMenu, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (Modocred VMenu, jogo, imgs) = (Controlador Jogar, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo, jogo, imgs) = (Controlador Jogar, jogo, imgs)
event _ (Modojogo (Jogo (x, y) []), jogo, imgs) = (VenceuJogo, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Modojogo (Jogo (x, y) l), jogo, imgs) = (Modojogo $ engine (x + 50, y + 50) l, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _) (Modojogo (Jogo (x, y) l), jogo, imgs) = (Modojogo $ engine (x -50, y - 50) l, jogo, imgs)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Modojogo (Jogo (x, y) l), jogo, imgs) = (Modojogo $ engine (x - 50, y) l, jogo, imgs)
event (EventKey (SpecialKey KeyRight) Down _ _) (Modojogo (Jogo (x, y) l), jogo, imgs) = (Modojogo $ engine (x + 50, y) l, jogo, imgs)
event _ w = w



time :: Float -> World -> World
time _ w = w 


loadImages :: IO Imagens
loadImages = do
   backim <- loadBMP "background.bmp"
   return (Imagens backim)


main :: IO ()
main = do
  imagens <- loadImages 
  let estado = (Controlador Jogar, Jogo (200, 100) [(50, 50), (-250, -100), (-100, -50)], imagens)
  play window white fr estado draw event time 
