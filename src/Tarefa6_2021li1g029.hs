{- |
Module      : Tarefa6_2021li1g029
Description : Resolução de um puzzle
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1g029 where

import LI12122

resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo i jogo = undefined


-- | Todas as peças com excessão da porta e dos vazio recebem uma coordenada 
coordenadas :: Coordenadas -> Mapa -> [Coordenadas]
coordenadas (x,y) [] = []
coordenadas (x,y) (a:b) = coordenadasAux (x,y) a ++ coordenadas (0,y+1) b
                        where coordenadasAux :: Coordenadas -> [Peca] -> [Coordenadas]
                              coordenadasAux (x,y) [] = []
                              coordenadasAux  (x,y) (a:b) = 
                                   if a == Vazio || a == Porta then coordenadasAux (x+1,y) b
                                   else (x,y) : coordenadasAux (x+1,y) b


-- | Devolve as coordenadas da porta 
coordPorta :: Mapa -> Coordenadas 
coordPorta l = getCoord (procuraCoords (Porta) l)
                where getCoord :: [Coordenadas] -> Coordenadas
                      getCoord [(x,y)] = (x,y)
                      getCoord (x:xs) = getCoord [x]


-- | Procura as coordenadas de uma dada peça num mapa             
procuraCoords :: Peca -> Mapa -> [Coordenadas]
procuraCoords p m = procuraCoordsAux (0,0) p m

procuraCoordsAux :: Coordenadas -> Peca -> Mapa -> [Coordenadas]
procuraCoordsAux (x,y) p [] = []
procuraCoordsAux (x,y) p ((c:b):h) = (coordsPeca (x,y) p (c:b)) ++ procuraCoordsAux (x,y+1) p h
                           where coordsPeca :: Coordenadas -> Peca -> [Peca] -> [Coordenadas]
                                 coordsPeca (x,y) p [] = []
                                 coordsPeca (x,y) p (h:t) =
                                      if p == h then (x,y) : coordsPeca (x+1,y) p t
                                      else coordsPeca (x+1,y) p t



-- | Verifica se existe uma porta à direita (na horizontal)
portaDireita :: Jogador -> Coordenadas -> Bool
portaDireita (Jogador (x,y) _ _) (a,b) = 
     if  x == a - 1 && y == b then True
     else False

-- | Verifica se existe uma porta à esquerda (na horizintal)
portaEsquerda :: Jogador -> Coordenadas -> Bool
portaEsquerda (Jogador (x,y) _ _) (a,b) =
     if x == a - 1 && y == b then True
     else False

-- | Verifica se existe uma porta à direita (na diagonal, quando o jogador trepa)
portaDireitaDiag :: Jogador -> Coordenadas -> Bool
portaDireitaDiag (Jogador (x,y) _ _ ) (a,b) =
     if x == a - 1 && y == b + 1 then True
     else False

-- | Verifica se existe uma porta à esquerda (na diagonal, quando o jogador trepa)
portaEsquerdaDiag :: Jogador -> Coordenadas -> Bool
portaEsquerdaDiag (Jogador (x,y) _ _ ) (a,b) =
     if x == a + 1 && y == b + 1 then True
     else False



direcao :: Jogador -> Coordenadas -> String
direcao (Jogador (x,y) _ _ ) (a,b) | x < a = "SegueDireita"
                                  | x > a = "SegueEsquerda"
                                  | otherwise = "ChegouPorta"


-- | Esta função dá um movimento conforme a posição do jogador em relação à porta   
damovimento :: Jogador -> Coordenadas -> Movimento
damovimento j@( Jogador (x,y)f g) (a,b) |direcao j (a,b) == "SegueDireita" =AndarDireita
                                        |direcao j (a,b) == "SegueEsquerda" = AndarEsquerda
                                        |otherwise = Trepar



