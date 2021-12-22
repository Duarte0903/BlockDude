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


data Jogo = Jogo (Int,Int) [(Int,Int)]

data Menu = Controlador Opcoes | Modojogo Jogo | VenceuJogo | Modocred Cred
 
data Opcoes = Jogar | Creditos | Sair 

data Cred = VMenu

data Imagens = Imagens { 
  background :: Picture,
  block :: Picture,
  dude :: Picture,
  jogar_preto :: Picture,
  jogar_azul :: Picture,
  sair_preto :: Picture,
  sair_zaul :: Picture, 
  creditos_azul :: Picture,
  creditos_preto :: Picture,
  nomes :: Picture,
  menu_laranja :: Picture  
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
draw (Controlador Jogar, jogo, imgs) = Pictures [drawBackground $ background imgs, drawJogar $ jogar_azul imgs, drawSair $ sair_preto imgs, drawCreditos $ creditos_preto imgs, drawBlock $ block imgs, drawDude $ dude imgs]
draw (Controlador Creditos, jogo, imgs) = Pictures [drawBackground $ background imgs, drawJogar $ jogar_preto imgs,  drawSair $ sair_preto imgs, drawCreditos $ creditos_azul imgs, drawBlock $ block imgs, drawDude $ dude imgs]
draw (Controlador Sair, jogo, imgs) = Pictures [drawBackground $ background imgs, drawJogar $ jogar_preto imgs,  drawSair $ sair_zaul imgs,drawCreditos $ creditos_preto imgs, drawBlock $ block imgs, drawDude $ dude imgs]
draw (Modojogo (Jogo (x,y) l), jogo, imgs) = undefined
draw (Modocred VMenu, jogo, imgs) = Pictures [drawBackground $ background imgs, drawNomes $ nomes imgs, drawMenuLaranja $ menu_laranja imgs]


drawBackground :: Picture -> Picture 
drawBackground pic = pic

drawBlock :: Picture -> Picture
drawBlock pic = Translate (-140) 140 $ Scale 5 5  pic

drawMenu :: Picture -> Picture 
drawMenu pic = Translate (-25) (-300) $ Scale 2 2  pic

drawDude :: Picture -> Picture
drawDude pic = Translate 240 140 $ Scale 5 5  pic

drawCreditos :: Picture -> Picture
drawCreditos pic = Translate 55 (-180) $ Scale 2 2 pic

drawJogar :: Picture -> Picture
drawJogar pic = Translate 30 0 $ Scale 2 2 pic 

drawSair :: Picture -> Picture
drawSair pic = Translate 25 (-80) $ Scale 2 2 pic 

drawNomes :: Picture -> Picture 
drawNomes pic = Translate (-80) 15 $ Scale 2 2  pic

drawMenuLaranja :: Picture -> Picture 
drawMenuLaranja pic = Translate 50 (-195) $ Scale 0.5 0.5 pic 


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
   backim <- loadBMP "imgs/background.bmp"       -- Criado por: Zeyu Ren 任泽宇 | Publicado em: opengameart.org
   blockim <- loadBMP "imgs/block.bmp"
   dudeim <- loadBMP "imgs/dude.bmp"
   jogar_pretoim <- loadBMP "imgs/jogar_preto.bmp"
   jogar_azulim <- loadBMP "imgs/jogar_azul.bmp"
   sair_pretoim <- loadBMP "imgs/sair_preto.bmp"
   sair_azulim <- loadBMP "imgs/sair_azul.bmp"
   creditos_azulim <- loadBMP "imgs/creditos_azul.bmp"
   creditos_pretoim <- loadBMP "imgs/creditos_preto.bmp"
   nomesim <- loadBMP "imgs/nomes.bmp"
   menu_laranjaim <- loadBMP "imgs/menu_laranja.bmp"
   return (Imagens backim blockim dudeim jogar_pretoim jogar_azulim sair_pretoim sair_azulim creditos_azulim creditos_pretoim nomesim menu_laranjaim)


main :: IO ()
main = do
  imagens <- loadImages 
  let estado = (Controlador Jogar, Jogo (200, 100) [(50, 50), (-250, -100), (-100, -50)], imagens)
  play window white fr estado draw event time 
 
 