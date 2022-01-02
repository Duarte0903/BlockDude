  {- |
Module      : Tarefa5_2021li1g029
Description : Aplicação Gráfica
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import LI12122
import Mapas 
import Tarefa4_2021li1g029
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game





data Menu = Controlador Opcoes | Modojogo Jogo | VenceuJogo Vitoria | Modocred Cred | ModoMap Mapas | MenuPause Pause | ControlsMenu Controls
 
data Opcoes = Jogar | Creditos | Sair | ComoJogar

data Cred = VMenu

data Mapas = Mapa1 | Mapa2 | Mapa3 | Voltar

data Pause = Voltar2 | Reset 

data Controls = Controls 

data Vitoria = IrMenu | VerMapas

data Imagens = Imagens {
  venceu :: Picture,
  controls :: Picture,
  como_jogar_azul :: Picture,
  como_jogar_laranja :: Picture,
  mapas_preto :: Picture,
  mapas_laranja :: Picture,
  jogador_este :: Picture,
  jogador_oeste :: Picture,
  jogador_com_caixa_este :: Picture,
  jogador_com_caixa_oeste :: Picture,   
  vazio :: Picture,  
  bloco :: Picture,
  caixa :: Picture,
  porta :: Picture,
  jogador :: Picture,
  jogador_com_caixa :: Picture,    
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


loadImages :: IO Imagens
loadImages = do
   venceuim <- loadBMP "imgs/venceu.bmp"
   controlsim <- loadBMP "imgs/controls.bmp"
   como_jogar_azulim <- loadBMP "imgs/como_jogar_azul.bmp"
   como_jogar_laranjaim <- loadBMP "imgs/como_jogar_laranja.bmp" -- !!! É preto !!!  
   mapas_pretoim <- loadBMP "imgs/mapas_preto.bmp"
   mapas_laranjaim <- loadBMP "imgs/mapas_laranja.bmp"
   jogador_esteim <- loadBMP "pecas/jogador_este.bmp"
   jogador_oesteim <- loadBMP "pecas/jogador_oeste.bmp"
   jogador_com_caixa_esteim <- loadBMP "pecas/jogador_com_caixa_este.bmp"
   jogador_com_caixa_oesteim <- loadBMP "pecas/jogador_com_caixa_oeste.bmp"
   vazioim <- loadBMP "pecas/vazio.bmp"
   blocoim <- loadBMP "pecas/bloco.bmp"
   caixaim <- loadBMP "pecas/caixa.bmp"
   portaim <- loadBMP "pecas/porta.bmp"
   jogadorim <- loadBMP "pecas/jogador.bmp"
   jogador_com_caixaim <- loadBMP "pecas/jogador_com_caixa.bmp"
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
   return (Imagens venceuim controlsim como_jogar_azulim como_jogar_laranjaim mapas_pretoim mapas_laranjaim jogador_esteim jogador_oesteim jogador_com_caixa_esteim jogador_com_caixa_oesteim vazioim blocoim caixaim portaim jogadorim jogador_com_caixaim backim blockim dudeim jogar_pretoim jogar_azulim sair_pretoim sair_azulim creditos_azulim creditos_pretoim nomesim menu_pretoim menu_laranjaim nivel1_pretoim nivel1_azulim nivel2_pretoim nivel2_azulim nivel3_pretoim nivel3_azulim)


window :: Display 
window = FullScreen           


fr :: Int              
fr = 25


draw :: World -> Picture 
draw (VenceuJogo VerMapas, jogo, imagens) = Pictures [drawBackground $ background imagens, drawVenceu $ venceu imagens, drawMenuPause $ menu_preto imagens, drawMapas $ mapas_laranja imagens]
draw (VenceuJogo IrMenu, jogo, imagens) = Pictures [drawBackground $ background imagens, drawVenceu $ venceu imagens, drawMenuPause $ menu_laranja imagens, drawMapas $ mapas_preto imagens]
draw (Controlador Jogar, jogo, imgs) = Pictures [drawBackground $ background imgs, drawJogar $ jogar_azul imgs, drawSair $ sair_preto imgs, drawCreditos $ creditos_preto imgs, drawBlock $ block imgs, drawDude $ dude imgs, drawComoJogar $ como_jogar_laranja imgs]
draw (Controlador Creditos, jogo, imgs) = Pictures [drawBackground $ background imgs, drawJogar $ jogar_preto imgs,  drawSair $ sair_preto imgs, drawCreditos $ creditos_azul imgs, drawBlock $ block imgs, drawDude $ dude imgs, drawComoJogar $ como_jogar_laranja imgs]
draw (Controlador Sair, jogo, imgs) = Pictures [drawBackground $ background imgs, drawJogar $ jogar_preto imgs,  drawSair $ sair_zaul imgs,drawCreditos $ creditos_preto imgs, drawBlock $ block imgs, drawDude $ dude imgs, drawComoJogar $ como_jogar_laranja imgs]
draw (Controlador ComoJogar, jogo, imgs) = Pictures [drawBackground $ background imgs, drawJogar $ jogar_preto imgs,  drawSair $ sair_preto imgs,drawCreditos $ creditos_preto imgs, drawBlock $ block imgs, drawDude $ dude imgs, drawComoJogar $ como_jogar_azul imgs]
draw (Modocred VMenu, jogo, imgs) = Pictures [drawBackground $ background imgs, drawNomes $ nomes imgs, drawMenu $ menu_laranja imgs]
draw (ControlsMenu Controls, jogo, imgs) = Pictures [drawBackground $ background imgs, drawControls $ controls imgs, drawMenu $ menu_laranja imgs]
draw (ModoMap Mapa1, jogo, imgs) = Pictures [drawBackground $ background imgs, drawNivel1 $ nivel1_azul imgs, drawNivel2 $ nivel2_preto imgs, drawNivel3 $ nivel3_preto imgs, drawMenu $ menu_preto imgs]
draw (ModoMap Mapa2, jogo, imgs) = Pictures [drawBackground $ background imgs, drawNivel1 $ nivel1_preto imgs, drawNivel2 $ nivel2_azul imgs, drawNivel3 $ nivel3_preto imgs, drawMenu $ menu_preto imgs]
draw (ModoMap Mapa3, jogo, imgs) = Pictures [drawBackground $ background imgs, drawNivel1 $ nivel1_preto imgs, drawNivel2 $ nivel2_preto imgs, drawNivel3 $ nivel3_azul imgs, drawMenu $ menu_preto imgs]
draw (ModoMap Voltar, jogo, imgs) = Pictures [drawBackground $ background imgs, drawNivel1 $ nivel1_preto imgs, drawNivel2 $ nivel2_preto imgs, drawNivel3 $ nivel3_preto imgs, drawMenu $ menu_laranja imgs]
draw (MenuPause Voltar2, jogo, imgs) = Pictures [drawBackground $ background imgs, drawMenuPause $ menu_laranja imgs, drawMapas $ mapas_preto imgs]
draw (MenuPause Reset, jogo, imagens) = Pictures [drawBackground $ background imagens, drawMenuPause $ menu_preto imagens, drawMapas $ mapas_laranja imagens]
draw (Modojogo (Jogo m (Jogador (x,y) d c)), jogo, imgs) | m == mapa1 = Pictures [drawBackground $ background imgs, Translate (-390) 90 $ Pictures [Pictures ((desenhaMapa m (0,0) imgs) ++ (desenhaJogadorMapa m (0,0) (Jogador (x,y) d c) imgs))]]
                                                         | m == mapa2 = Pictures [drawBackground $ background imgs, Translate (-170) 90 $ Pictures [Pictures ((desenhaMapa m (0,0) imgs) ++ (desenhaJogadorMapa m (0,0) (Jogador (x,y) d c) imgs))]]
                                                         | m == mapa3 = Pictures [drawBackground $ background imgs, Translate (-160) 0 $ Pictures [Pictures ((desenhaMapa m (0,0) imgs) ++ (desenhaJogadorMapa m (0,0) (Jogador (x,y) d c) imgs))]] 

drawBackground :: Picture -> Picture 
drawBackground pic = pic

drawVenceu :: Picture -> Picture 
drawVenceu pic =Translate (-30) 90 pic

drawComoJogar :: Picture -> Picture 
drawComoJogar pic = Translate 10 (-450) pic

drawControls :: Picture -> Picture 
drawControls pic = Translate 0 (-5) $ Scale 1.2 1.2 pic

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

drawMenuPause ::  Picture -> Picture
drawMenuPause pic = Translate 100 (-80) $ Scale 1 1 pic

drawMapas :: Picture -> Picture
drawMapas pic = Translate 103 (-90) $ Scale 1.2 1.2 pic

drawNivel1 :: Picture -> Picture 
drawNivel1 pic = Translate 40 0 pic

drawNivel2 :: Picture -> Picture 
drawNivel2 pic = Translate 40 (-80) pic

drawNivel3 :: Picture -> Picture 
drawNivel3 pic = Translate 40 (-160) pic 


desenhaMapa :: Mapa -> (Int,Int) -> Imagens -> [Picture]
desenhaMapa [] _ _ = []
desenhaMapa (l:ls) (x,y) imagens = desenhaLinha l (x,y) imagens ++ desenhaMapa ls (0,y+1) imagens

desenhaLinha :: [Peca] -> (Int,Int) -> Imagens -> [Picture]
desenhaLinha [] _ _ = []
desenhaLinha (p:ps) (x,y) imagens | p == Vazio = [Translate (i-270) (j+268) $ Scale (6) (6) $ vazio imagens] ++ desenhaLinha ps (x+1,y) imagens
                                  | p == Bloco = [Translate (i-270) (j+268) $ Scale (6) (6) $ bloco imagens] ++ desenhaLinha ps (x+1,y) imagens
                                  | p == Porta = [Translate (i-270) (j+268) $ Scale (6) (6) $ porta imagens] ++ desenhaLinha ps (x+1,y) imagens
                                  | p == Caixa = [Translate (i-270) (j+268) $ Scale (6) (6) $ caixa imagens] ++ desenhaLinha ps (x+1,y) imagens

                                   where i = fromIntegral (x*60)
                                         j = fromIntegral (-y*60)


desenhaJogadorLinha :: [Peca] -> (Int,Int) -> Jogador -> Imagens -> [Picture]
desenhaJogadorLinha [] _ _ _= []
desenhaJogadorLinha (l:ls) (x,y) (Jogador (a,b) c d) imagens | x == a && y == b && c == Este && d == False = [Translate (i-270) (j+268) $ Scale (1.1) (1.1) $ jogador_este imagens]
                                                             | x == a && y == b && c == Oeste && d == False = [Translate (i-270) (j+268) $ Scale (1.1) (1.1) $ jogador_oeste imagens]
                                                             | x == a && y == b && c == Este && d == True = [Translate (i-270) (j+268) $ Scale (1.1) (1.1) $ jogador_com_caixa_este imagens]
                                                             | x == a && y == b && c == Oeste && d == True = [Translate (i-270) (j+268) $ Scale (1.1) (1.1) $ jogador_com_caixa_oeste imagens]
                                                             | x == a && y == b && c == Este = [Translate (i-270) (j+268) $ Scale (1.1) (1.1) $ jogador imagens]
                                                             | x == a && y == b && c == Oeste = [Translate (i-270) (j+268) $ Scale (1.1) (1.1) $ jogador imagens]
                                                             | otherwise = desenhaJogadorLinha ls (x+1,y) (Jogador (a,b) c d) imagens
     where
         i = fromIntegral (x*60)
         j = fromIntegral (-y*60)


desenhaJogadorMapa :: Mapa -> (Int,Int) -> Jogador -> Imagens -> [Picture]
desenhaJogadorMapa [] _ _ _ = []
desenhaJogadorMapa (l:ls) (x,y) (Jogador (a,b) c d) imagens = desenhaJogadorLinha l (x,y) (Jogador (a,b) c d) imagens ++ desenhaJogadorMapa ls (x,y+1) (Jogador (a,b) c d) imagens


engine :: Jogo -> Movimento -> Jogo
engine = moveJogador


event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, jogo, imgs) = (ModoMap Mapa1, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar, jogo, imgs) = (Controlador ComoJogar, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar, jogo, imgs) = (Controlador Sair, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair, jogo, imgs) = (Controlador Jogar, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair, jogo, imgs) = (Controlador Creditos, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, jogo, imgs) = undefined
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Creditos, jogo, imgs) = (Controlador ComoJogar, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Creditos, jogo, imgs) = (Controlador Sair, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador ComoJogar, jogo, imgs) = (Controlador Creditos, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador ComoJogar, jogo, imgs) = (Controlador Jogar, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador ComoJogar, jogo, imgs) = (ControlsMenu Controls, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _) (ControlsMenu Controls, jogo, imgs) = (Controlador Jogar, jogo, imgs)
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
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (ModoMap Mapa1, jogo, imgs) = (Modojogo (Jogo mapa1 (Jogador (10,6) Oeste False)), jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (ModoMap Mapa2, jogo, imgs) = (Modojogo (Jogo mapa2  (Jogador (1,8) Este False)), jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (ModoMap Mapa3, jogo, imgs) = (Modojogo (Jogo mapa3  (Jogador (1,6) Este False)), jogo, imgs)
event (EventKey (Char 'p') Down _ _) (Modojogo (Jogo mapa (Jogador (a,b) c d)),jogo, imagens) = (MenuPause Voltar2, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (MenuPause Voltar2, jogo, imagens) = (Controlador Jogar, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _ ) (MenuPause Voltar2, jogo, imagens) = (MenuPause Reset, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _ ) (MenuPause Reset, jogo, imagens) = (MenuPause Voltar2, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _ ) (MenuPause Reset, jogo, imagens) = (MenuPause Voltar2, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _ ) (MenuPause Voltar2, jogo, imagens) = (MenuPause Reset, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (MenuPause Reset, jogo, imagens) = (ModoMap Mapa1, jogo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Modojogo (Jogo mapa (Jogador (a,b) c d)),jogo, imagens) = (Modojogo (engine (Jogo mapa (Jogador (a,b) c d)) AndarEsquerda), jogo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Modojogo (Jogo mapa (Jogador (a,b) c d)),jogo, imagens) = (Modojogo (engine (Jogo mapa (Jogador (a,b) c d)) AndarDireita), jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Modojogo (Jogo mapa (Jogador (a,b) c d)),jogo, imagens) = (Modojogo (engine (Jogo mapa (Jogador (a,b) c d)) Trepar), jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Modojogo (Jogo mapa (Jogador (a,b) c d)),jogo, imagens) = (Modojogo (engine (Jogo mapa (Jogador (a,b) c d)) InterageCaixa), jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (Modojogo (Jogo mapa3 (Jogador (14,4) c d)),jogo, imagens) = (VenceuJogo IrMenu, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo IrMenu, jogo, imagens) = (VenceuJogo VerMapas, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo VerMapas, jogo, imagens) = (VenceuJogo IrMenu, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo IrMenu, jogo, imagens) = (VenceuJogo VerMapas, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo VerMapas, jogo, imagens) = (VenceuJogo IrMenu, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (VenceuJogo IrMenu, jogo, imagens) = (Controlador Jogar, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (VenceuJogo VerMapas, jogo, imagens) = (ModoMap Mapa1, jogo, imagens)

event _ w = w


time :: Float -> World -> World
time _ w = w 


main :: IO ()
main = do
  imagens <- loadImages 
  let estado = (Controlador Jogar, Jogo mapa1 (Jogador (0,0) Oeste False), imagens)
  play window white fr estado draw event time 
 
 