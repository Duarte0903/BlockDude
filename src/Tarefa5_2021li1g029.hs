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

data Pause = Continuar | Voltar2 | Maps 

data Controls = Controls 

data Vitoria = IrMenu | VerMapas

data Imagens = Imagens {
  continuar_azul :: Picture,
  continuar_laranja :: Picture, 
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


-- | Carrega todas as imagens usadas no jogo 
loadImages :: IO Imagens
loadImages = do
   continuar_azulim <- loadBMP "imgs/continuar_azul.bmp"
   continuar_laranjaim <- loadBMP "imgs/continuar_laranja.bmp"
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
   return (Imagens continuar_azulim continuar_laranjaim venceuim controlsim como_jogar_azulim como_jogar_laranjaim mapas_pretoim mapas_laranjaim jogador_esteim jogador_oesteim jogador_com_caixa_esteim jogador_com_caixa_oesteim vazioim blocoim caixaim portaim jogadorim jogador_com_caixaim backim blockim dudeim jogar_pretoim jogar_azulim sair_pretoim sair_azulim creditos_azulim creditos_pretoim nomesim menu_pretoim menu_laranjaim nivel1_pretoim nivel1_azulim nivel2_pretoim nivel2_azulim nivel3_pretoim nivel3_azulim)


-- | A janela do jogo vai ocupar todo o ecrã
window :: Display 
window = FullScreen           

-- | Frame rate (constante)
fr :: Int              
fr = 25


-- | Transforma um world numa Picture / determina o que aparece no ecrã de acordo com o estado
draw :: World -> Picture 
draw (VenceuJogo VerMapas, jogo, imagens) = Pictures [background imagens, drawVenceu $ venceu imagens, drawMenuPause $ menu_preto imagens, drawMapas $ mapas_laranja imagens]
draw (VenceuJogo IrMenu, jogo, imagens) = Pictures [background imagens, drawVenceu $ venceu imagens, drawMenuPause $ menu_laranja imagens, drawMapas $ mapas_preto imagens]
draw (Controlador Jogar, jogo, imgs) = Pictures [background imgs, drawJogar $ jogar_azul imgs, drawSair $ sair_preto imgs, drawCreditos $ creditos_preto imgs, drawBlock $ block imgs, drawDude $ dude imgs, drawComoJogar $ como_jogar_laranja imgs]
draw (Controlador Creditos, jogo, imgs) = Pictures [background imgs, drawJogar $ jogar_preto imgs,  drawSair $ sair_preto imgs, drawCreditos $ creditos_azul imgs, drawBlock $ block imgs, drawDude $ dude imgs, drawComoJogar $ como_jogar_laranja imgs]
draw (Controlador Sair, jogo, imgs) = Pictures [background imgs, drawJogar $ jogar_preto imgs,  drawSair $ sair_zaul imgs,drawCreditos $ creditos_preto imgs, drawBlock $ block imgs, drawDude $ dude imgs, drawComoJogar $ como_jogar_laranja imgs]
draw (Controlador ComoJogar, jogo, imgs) = Pictures [background imgs, drawJogar $ jogar_preto imgs,  drawSair $ sair_preto imgs,drawCreditos $ creditos_preto imgs, drawBlock $ block imgs, drawDude $ dude imgs, drawComoJogar $ como_jogar_azul imgs]
draw (Modocred VMenu, jogo, imgs) = Pictures [background imgs, drawNomes $ nomes imgs, drawMenu $ menu_laranja imgs]
draw (ControlsMenu Controls, jogo, imgs) = Pictures [background imgs, drawControls $ controls imgs, drawMenu $ menu_laranja imgs]
draw (ModoMap Mapa1, jogo, imgs) = Pictures [background imgs, drawNivel1 $ nivel1_azul imgs, drawNivel2 $ nivel2_preto imgs, drawNivel3 $ nivel3_preto imgs, drawMenu $ menu_preto imgs]
draw (ModoMap Mapa2, jogo, imgs) = Pictures [background imgs, drawNivel1 $ nivel1_preto imgs, drawNivel2 $ nivel2_azul imgs, drawNivel3 $ nivel3_preto imgs, drawMenu $ menu_preto imgs]
draw (ModoMap Mapa3, jogo, imgs) = Pictures [background imgs, drawNivel1 $ nivel1_preto imgs, drawNivel2 $ nivel2_preto imgs, drawNivel3 $ nivel3_azul imgs, drawMenu $ menu_preto imgs]
draw (ModoMap Voltar, jogo, imgs) = Pictures [background imgs, drawNivel1 $ nivel1_preto imgs, drawNivel2 $ nivel2_preto imgs, drawNivel3 $ nivel3_preto imgs, drawMenu $ menu_laranja imgs]
draw (MenuPause Voltar2, jogo, imgs) = Pictures [background imgs, drawMenuPause $ menu_laranja imgs, drawMapas $ mapas_preto imgs]
draw (MenuPause Maps, jogo, imagens) = Pictures [background imagens, drawMenuPause $ menu_preto imagens, drawMapas $ mapas_laranja imagens]
draw (Modojogo (Jogo m (Jogador (x,y) d c)), jogo, imgs) | m == mapa1 = Pictures [background imgs, Translate (-390) 90 $ Pictures [Pictures ((drawMapa m (0,0) imgs) ++ (drawJogadorMapa m (0,0) (Jogador (x,y) d c) imgs))]]
                                                         | m == mapa2 = Pictures [background imgs, Translate (-170) 90 $ Pictures [Pictures ((drawMapa m (0,0) imgs) ++ (drawJogadorMapa m (0,0) (Jogador (x,y) d c) imgs))]]
                                                         | m == mapa3 = Pictures [background imgs, Translate (-160) 0 $ Pictures [Pictures ((drawMapa m (0,0) imgs) ++ (drawJogadorMapa m (0,0) (Jogador (x,y) d c) imgs))]] 


-- As funções que se seguem têm como objetivo aplicar Translate ou Scale a uma imagem dos ficheiros do jogo

-- | desenha "venceu" no menu de vitória
drawVenceu :: Picture -> Picture 
drawVenceu pic =Translate (-30) 90 pic

-- | desenha "como jogar" no menu principal
drawComoJogar :: Picture -> Picture 
drawComoJogar pic = Translate 10 (-450) pic

-- | desenha as informações sobre o funcionamento do jogo no menu "como jogar"
drawControls :: Picture -> Picture 
drawControls pic = Translate 0 (-5) $ Scale 1.2 1.2 pic

-- | desenha "block" (titulo) no menu principal
drawBlock :: Picture -> Picture
drawBlock pic = Translate (-140) 140 $ Scale 5 5  pic

-- | desenha "dude" (titulo) no menu principal
drawDude :: Picture -> Picture
drawDude pic = Translate 240 140 $ Scale 5 5  pic

-- | desenha "creditos" (titulo) no menu principal
drawCreditos :: Picture -> Picture
drawCreditos pic = Translate 60 (-180) $ Scale 2 2 pic

-- | desenha a opção "jogar" no menu principal
drawJogar :: Picture -> Picture
drawJogar pic = Translate 30 0 $ Scale 2 2 pic 

-- | desenha a opção "sair" no menu principal
drawSair :: Picture -> Picture
drawSair pic = Translate 25 (-80) $ Scale 2 2 pic 

-- | desenha os nossos nomes no menu dos creditos
drawNomes :: Picture -> Picture 
drawNomes pic = Translate (-80) 15 $ Scale 2 2  pic

-- | desenha a opção "menu" que se situa na parte inferior de vários menus 
drawMenu :: Picture -> Picture
drawMenu pic = Translate 100 (-290) $ Scale 0.8 0.8 pic

-- | desenha a opção "menu" que se situa nos menus de pausa e vitoria
drawMenuPause ::  Picture -> Picture
drawMenuPause pic = Translate 103 (-80) $ Scale 1 1 pic

-- | desenha a opção "mapas" que se situa nos menus de pausa e vitoria
drawMapas :: Picture -> Picture
drawMapas pic = Translate 103 (-90) $ Scale 1.2 1.2 pic

-- | desenha "nivel 1" no menu dos mapas
drawNivel1 :: Picture -> Picture 
drawNivel1 pic = Translate 40 0 pic

-- | desenha "nivel 2" no menu dos mapas
drawNivel2 :: Picture -> Picture 
drawNivel2 pic = Translate 40 (-80) pic

-- | desenha "nivel 3" no menu dos mapas
drawNivel3 :: Picture -> Picture 
drawNivel3 pic = Translate 40 (-160) pic 

-- | Desenha um dos 3 mapas com o auxilio da função drawLinha
drawMapa :: Mapa -> (Int,Int) -> Imagens -> [Picture]
drawMapa [] _ _ = []
drawMapa (l:ls) (x,y) imagens = drawLinha l (x,y) imagens ++ drawMapa ls (0,y+1) imagens

-- | Recebe uma linha do mapa, um par de coordenadas e as imagens do jogo
-- Dependendo da peca, será atribuida uma imagem e uma translação e um Scale 
-- O processo e repetido para as outras pecas, devolvendo uma lista de pictures
drawLinha :: [Peca] -> (Int,Int) -> Imagens -> [Picture]
drawLinha [] _ _ = []
drawLinha (p:ps) (x,y) imagens | p == Vazio = [Translate (i-270) (j+268) $ Scale (6) (6) $ vazio imagens] ++ drawLinha ps (x+1,y) imagens
                               | p == Bloco = [Translate (i-270) (j+268) $ Scale (6) (6) $ bloco imagens] ++ drawLinha ps (x+1,y) imagens
                               | p == Porta = [Translate (i-270) (j+268) $ Scale (6) (6) $ porta imagens] ++ drawLinha ps (x+1,y) imagens
                               | p == Caixa = [Translate (i-270) (j+268) $ Scale (6) (6) $ caixa imagens] ++ drawLinha ps (x+1,y) imagens

                                   where i = fromIntegral (x*60)
                                         j = fromIntegral (-y*60)

-- | Recebe uma lista de pecas/linha do mapa, um par de coordenadas e as imagens do jogo
-- Se as coordenadas do jogador forem iguais ao par de coordenadas introduzido, será desenhado o jogador de acordo com o seu estado
drawJogadorLinha :: [Peca] -> (Int,Int) -> Jogador -> Imagens -> [Picture]
drawJogadorLinha [] _ _ _= []
drawJogadorLinha (l:ls) (x,y) (Jogador (a,b) c d) imagens | x == a && y == b && c == Este && d == False = [Translate (i-270) (j+268) $ Scale (1.1) (1.1) $ jogador_este imagens]
                                                          | x == a && y == b && c == Oeste && d == False = [Translate (i-270) (j+268) $ Scale (1.1) (1.1) $ jogador_oeste imagens]
                                                          | x == a && y == b && c == Este && d == True = [Translate (i-270) (j+268) $ Scale (1.1) (1.1) $ jogador_com_caixa_este imagens]
                                                          | x == a && y == b && c == Oeste && d == True = [Translate (i-270) (j+268) $ Scale (1.1) (1.1) $ jogador_com_caixa_oeste imagens]
                                                          | x == a && y == b && c == Este = [Translate (i-270) (j+268) $ Scale (1.1) (1.1) $ jogador imagens]
                                                          | x == a && y == b && c == Oeste = [Translate (i-270) (j+268) $ Scale (1.1) (1.1) $ jogador imagens]
                                                          | otherwise = drawJogadorLinha ls (x+1,y) (Jogador (a,b) c d) imagens
                                   
                                    where i = fromIntegral (x*60)
                                          j = fromIntegral (-y*60)


-- | Recorre à função drawJogadorLinha para colocar o jogador no mapa
-- Irá analizar as linhas e colocar o jogador no lugar correto depois de um movimento 
drawJogadorMapa :: Mapa -> (Int,Int) -> Jogador -> Imagens -> [Picture]
drawJogadorMapa [] _ _ _ = []
drawJogadorMapa (l:ls) (x,y) (Jogador (a,b) c d) imagens = drawJogadorLinha l (x,y) (Jogador (a,b) c d) imagens ++ drawJogadorMapa ls (x,y+1) (Jogador (a,b) c d) imagens


-- | Equivalente à função moveJogador da tarefa 4. como o nome indica faz o jogador trocar de posição
engine :: Jogo -> Movimento -> Jogo
engine = moveJogador


-- | Recebe um evento do teclado e altera o estado do jogo
event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, jogo, imgs) = (ModoMap Mapa1, jogo, imgs)               -- selecao de mapas 
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar, jogo, imgs) = (Controlador ComoJogar, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar, jogo, imgs) = (Controlador Sair, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair, jogo, imgs) = (Controlador Jogar, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair, jogo, imgs) = (Controlador Creditos, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, jogo, imgs) = undefined                                  -- sair do jogo
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Creditos, jogo, imgs) = (Controlador ComoJogar, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Creditos, jogo, imgs) = (Controlador Sair, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador ComoJogar, jogo, imgs) = (Controlador Creditos, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador ComoJogar, jogo, imgs) = (Controlador Jogar, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador ComoJogar, jogo, imgs) = (ControlsMenu Controls, jogo, imgs)   -- como jogar 
event (EventKey (SpecialKey KeyEnter) Down _ _) (ControlsMenu Controls, jogo, imgs) = (Controlador Jogar, jogo, imgs)       -- como jogar -> menu principal
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Creditos, jogo, imgs) = (Modocred VMenu, jogo, imgs)           -- ver creditos
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (Modocred VMenu, jogo, imgs) = (Controlador Jogar, jogo, imgs)             -- creditos -> menu principal 
event (EventKey (SpecialKey KeyDown) Down _ _ ) (ModoMap Mapa1, jogo, imgs) = (ModoMap Mapa2, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _ ) (ModoMap Mapa2, jogo, imgs) = (ModoMap Mapa3, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _ ) (ModoMap Mapa3, jogo, imgs) = (ModoMap Voltar, jogo, imgs)
event (EventKey (SpecialKey KeyDown) Down _ _ ) (ModoMap Voltar, jogo, imgs) = (ModoMap Mapa1, jogo, imgs)     
event (EventKey (SpecialKey KeyUp) Down _ _ ) (ModoMap Mapa3, jogo, imgs) = (ModoMap Mapa2, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _ ) (ModoMap Mapa2, jogo, imgs) = (ModoMap Mapa1, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _ ) (ModoMap Mapa1, jogo, imgs) = (ModoMap Voltar, jogo, imgs)
event (EventKey (SpecialKey KeyUp) Down _ _ ) (ModoMap Voltar, jogo, imgs) = (ModoMap Mapa3, jogo, imgs)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (ModoMap Voltar, jogo, imgs) = (Controlador Jogar, jogo, imgs)                                   -- mapas -> menu principal
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (ModoMap Mapa1, jogo, imgs) = (Modojogo (Jogo mapa1 (Jogador (10,6) Oeste False)), jogo, imgs)   -- Posição inicial mapa1
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (ModoMap Mapa2, jogo, imgs) = (Modojogo (Jogo mapa2  (Jogador (1,6) Este False)), jogo, imgs)    -- Posição inicial mapa2
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (ModoMap Mapa3, jogo, imgs) = (Modojogo (Jogo mapa3  (Jogador (1,6) Este False)), jogo, imgs)    -- Posição inical mapa3
event (EventKey (Char 'p') Down _ _) (Modojogo (Jogo mapa (Jogador (a,b) c d)),jogo, imagens) = (MenuPause Voltar2, jogo, imagens)                -- menu de pausa
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (MenuPause Voltar2, jogo, imagens) = (Controlador Jogar, jogo, imagens)                          -- menu pausa -> menu principal
event (EventKey (SpecialKey KeyDown) Down _ _ ) (MenuPause Voltar2, jogo, imagens) = (MenuPause Maps, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _ ) (MenuPause Maps, jogo, imagens) = (MenuPause Voltar2, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _ ) (MenuPause Maps, jogo, imagens) = (MenuPause Voltar2, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _ ) (MenuPause Voltar2, jogo, imagens) = (MenuPause Maps, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (MenuPause Maps, jogo, imagens) = (ModoMap Mapa1, jogo, imagens)                                -- menu pausa -> selecao de mapas
event (EventKey (SpecialKey KeyLeft) Down _ _) (Modojogo (Jogo mapa (Jogador (a,b) c d)),jogo, imagens) = (Modojogo (engine (Jogo mapa (Jogador (a,b) c d)) AndarEsquerda), jogo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Modojogo (Jogo mapa (Jogador (a,b) c d)),jogo, imagens) = (Modojogo (engine (Jogo mapa (Jogador (a,b) c d)) AndarDireita), jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Modojogo (Jogo mapa (Jogador (a,b) c d)),jogo, imagens) = (Modojogo (engine (Jogo mapa (Jogador (a,b) c d)) Trepar), jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Modojogo (Jogo mapa (Jogador (a,b) c d)),jogo, imagens) = (Modojogo (engine (Jogo mapa (Jogador (a,b) c d)) InterageCaixa), jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (Modojogo (Jogo mapa1 (Jogador (1,8) c d)),jogo, imagens) = (VenceuJogo IrMenu, jogo, imagens)  -- Vitoria mapa 1
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (Modojogo (Jogo mapa2 (Jogador (4,14) c d)),jogo, imagens) = (VenceuJogo IrMenu, jogo, imagens) -- Vitoria mapa 2
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (Modojogo (Jogo mapa3 (Jogador (14,4) c d)),jogo, imagens) = (VenceuJogo IrMenu, jogo, imagens) -- Vitoria mapa 3
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo IrMenu, jogo, imagens) = (VenceuJogo VerMapas, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo VerMapas, jogo, imagens) = (VenceuJogo IrMenu, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo IrMenu, jogo, imagens) = (VenceuJogo VerMapas, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo VerMapas, jogo, imagens) = (VenceuJogo IrMenu, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (VenceuJogo IrMenu, jogo, imagens) = (Controlador Jogar, jogo, imagens)   -- menu vitoria -> menu principal
event (EventKey (SpecialKey KeyEnter) Down _ _ ) (VenceuJogo VerMapas, jogo, imagens) = (ModoMap Mapa1, jogo, imagens)     --  menu vitoria -> selecao mapas
event _ w = w


time :: Float -> World -> World
time _ w = w 


main :: IO ()
main = do
  imagens <- loadImages 
  let estado = (Controlador Jogar, Jogo mapa1 (Jogador (0,0) Oeste False), imagens)
  play window white fr estado draw event time 
 
 