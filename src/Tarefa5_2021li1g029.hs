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

data Menu = Controlador Opcoes | Modojogo Jogo | VenceuJogo | Modocred Cred | ModoMap Mapas
 
data Opcoes = Jogar | Creditos | Sair 

data Cred = VMenu

data Mapas = Mapa1 | Mapa2 | Mapa3 | Voltar

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
  menu_preto :: Picture, 
  menu_laranja :: Picture,
  nivel1_preto :: Picture, 
  nivel1_azul :: Picture,
  nivel2_preto :: Picture,
  nivel2_azul :: Picture,
  nivel3_preto :: Picture,
  nivel3_azul :: Picture 
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
draw (Modocred VMenu, jogo, imgs) = Pictures [drawBackground $ background imgs, drawNomes $ nomes imgs, drawMenu $ menu_laranja imgs]
draw (ModoMap Mapa1, jogo, imgs) = Pictures [drawBackground $ background imgs, drawNivel1 $ nivel1_azul imgs, drawNivel2 $ nivel2_preto imgs, drawNivel3 $ nivel3_preto imgs, drawMenu $ menu_preto imgs]
draw (ModoMap Mapa2, jogo, imgs) = Pictures [drawBackground $ background imgs, drawNivel1 $ nivel1_preto imgs, drawNivel2 $ nivel2_azul imgs, drawNivel3 $ nivel3_preto imgs, drawMenu $ menu_preto imgs]
draw (ModoMap Mapa3, jogo, imgs) = Pictures [drawBackground $ background imgs, drawNivel1 $ nivel1_preto imgs, drawNivel2 $ nivel2_preto imgs, drawNivel3 $ nivel3_azul imgs, drawMenu $ menu_preto imgs]
draw (ModoMap Voltar, jogo, imgs) = Pictures [drawBackground $ background imgs, drawNivel1 $ nivel1_preto imgs, drawNivel2 $ nivel2_preto imgs, drawNivel3 $ nivel3_preto imgs, drawMenu $ menu_laranja imgs]


drawBackground :: Picture -> Picture 
drawBackground pic = pic

drawBlock :: Picture -> Picture
drawBlock pic = Translate (-140) 140 $ Scale 5 5  pic

drawDude :: Picture -> Picture
drawDude pic = Translate 240 140 $ Scale 5 5  pic

drawCreditos :: Picture -> Picture
drawCreditos pic = Translate 60 (-180) $ Scale 2 2 pic

drawJogar :: Picture -> Picture
drawJogar pic = Translate 30 0 $ Scale 2 2 pic 

drawSair :: Picture -> Picture
drawSair pic = Translate 25 (-80) $ Scale 2 2 pic 

drawNomes :: Picture -> Picture 
drawNomes pic = Translate (-80) 15 $ Scale 2 2  pic

drawMenu :: Picture -> Picture
drawMenu pic = Translate 100 (-290) $ Scale 0.8 0.8 pic 

drawNivel1 :: Picture -> Picture 
drawNivel1 pic = Translate 40 0 pic

drawNivel2 :: Picture -> Picture 
drawNivel2 pic = Translate 40 (-80) pic

drawNivel3 :: Picture -> Picture 
drawNivel3 pic = Translate 40 (-160) pic


engine :: (Int,Int) -> [(Int,Int)] -> Jogo
engine p l = Jogo p (filter (p/=) l)


event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, jogo, imgs) = (ModoMap Mapa1, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar, jogo, imgs) = (Controlador Creditos, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar, jogo, imgs) = (Controlador Sair, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair, jogo, imgs) = (Controlador Jogar, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair, jogo, imgs) = (Controlador Creditos, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, jogo, imgs) = undefined
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Creditos, jogo, imgs) = (Controlador Jogar, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Creditos, jogo, imgs) = (Controlador Sair, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Creditos, jogo, imgs) = (Modocred VMenu, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (Modocred VMenu, jogo, imgs) = (Controlador Jogar, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _ ) (ModoMap Mapa1, jogo, imgs) = (ModoMap Mapa2, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _ ) (ModoMap Mapa2, jogo, imgs) = (ModoMap Mapa3, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _ ) (ModoMap Mapa3, jogo, imgs) = (ModoMap Voltar, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _ ) (ModoMap Voltar, jogo, imgs) = (ModoMap Mapa1, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _ ) (ModoMap Mapa3, jogo, imgs) = (ModoMap Mapa2, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _ ) (ModoMap Mapa2, jogo, imgs) = (ModoMap Mapa1, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _ ) (ModoMap Mapa1, jogo, imgs) = (ModoMap Voltar, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _ ) (ModoMap Voltar, jogo, imgs) = (ModoMap Mapa3, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (ModoMap Voltar, jogo, imgs) = (Controlador Jogar, jogo, imgs)
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
   menu_pretoim <- loadBMP "imgs/menu_preto.bmp"
   menu_laranjaim <- loadBMP "imgs/menu_laranja.bmp"
   nivel1_pretoim <- loadBMP "imgs/nivel1_preto.bmp"
   nivel1_azulim <- loadBMP "imgs/nivel1_azul.bmp"
   nivel2_pretoim <- loadBMP "imgs/nivel2_preto.bmp"
   nivel2_azulim <- loadBMP "imgs/nivel2_azul.bmp"
   nivel3_pretoim <- loadBMP "imgs/nivel3_preto.bmp"
   nivel3_azulim <- loadBMP "imgs/nivel3_azul.bmp"
   return (Imagens backim blockim dudeim jogar_pretoim jogar_azulim sair_pretoim sair_azulim creditos_azulim creditos_pretoim nomesim menu_pretoim menu_laranjaim nivel1_pretoim nivel1_azulim nivel2_pretoim nivel2_azulim nivel3_pretoim nivel3_azulim)


main :: IO ()
main = do
  imagens <- loadImages 
  let estado = (Controlador Jogar, Jogo (200, 100) [(50, 50), (-250, -100), (-100, -50)], imagens)
  play window white fr estado draw event time 
 
 