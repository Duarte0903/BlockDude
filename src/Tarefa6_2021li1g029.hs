{- |
Module      : Tarefa6_2021li1g029
Description : Resolução de um puzzle
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1g029 where

import LI12122
import Tarefa4_2021li1g029
import Tarefa3_2021li1g029
import Tarefa2_2021li1g029
import Tarefa1_2021li1g029

resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo i jogo = undefined

-- | Recebe como argumento uma lista com um único movimento e retorna esse movimento
devolvemovimento :: [Movimento] -> Movimento
devolvemovimento [x] = x

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


-- | A função 'direcao' retorna uma String a partir da qual sabemos a direção a seguir, se o Jogador se apresentar á direita da porta então terá de se dirigir para a esquerda
direcao :: Jogador -> Coordenadas -> String
direcao (Jogador (x,y) _ _ ) (a,b) | x < a = "SegueDireita"
                                  | x > a = "SegueEsquerda"
                                  | otherwise = "ChegouPorta"


-- | Esta função dá um movimento conforme a posição do jogador em relação à porta   
damovimento :: Jogador -> Coordenadas -> Movimento
damovimento j@( Jogador (x,y)f g) (a,b) |direcao j (a,b) == "SegueDireita" =AndarDireita
                                        |direcao j (a,b) == "SegueEsquerda" = AndarEsquerda
                                        |otherwise = Trepar

aux1 :: Coordenadas -> Peca -> [Peca ] -> [Coordenadas]      
aux1 (x,y) a [] = []
aux1 (x,y) a (b:bs) | a == b = (x,y) :aux1 (x+1,y) a bs    
                    |otherwise = aux1 (x+1,y) a bs                        

contaCoordenadas :: Peca  -> Mapa -> [Coordenadas ]
contaCoordenadas p m = aux2 (0,0) p m
                        where aux2 :: Coordenadas -> Peca -> Mapa -> [Coordenadas]
                              aux2 (x,y) a [] = [] 
                              aux2 (x,y) a ((c:b):h)= (aux1 (x,y) a (c:b)) ++ aux2 (x,y+1) a h


-- | Devolve a coordenada da porta 
coordenadaPorta :: Mapa -> Coordenadas
coordenadaPorta l = aux (contaCoordenadas (Porta) l)
                 where aux :: [Coordenadas ] ->Coordenadas  
                       aux [(x,y)] =(x,y)
                       aux (x:xs)= aux [x]


-- | Se o jogador se encontra à direita da porta, retoma a lista de movimentos para que este va para a coluna da porta 
moveDireita :: Jogo -> [Movimento]
moveDireita j@(Jogo l(Jogador  (x,y) f g)) |g == False && (checkDireita (x,y) (coordenadasTotalMapa (0,0) l)) ==True && (checkDireita (x,y-1) (coordenadasTotalMapa (0,0)l)) == False = [Trepar]
                                           |g == True && (checkDireita  (x,y) (coordenadasTotalMapa (0,0) l)) ==True && (checkDireita (x,y-1) (coordenadasTotalMapa (0,0)l)) == False && (checkDireita (x,y-2) (coordenadasTotalMapa (0,0)l)) == False =[Trepar ]
                                           |g == True && (checkDireita  (x,y) (coordenadasTotalMapa (0,0) l)) ==False && (checkDireita (x,y-1) (coordenadasTotalMapa (0,0)l))== False = [AndarDireita ]
                                           |g == False && (checkDireita (x,y) (coordenadasTotalMapa (0,0) l)) == False = [AndarDireita ]
                                           |otherwise =  []


-- | Se o jogador se encontra à esquerda da porta, retoma a lista de movimentos para que este va para a coluna da porta 
moveEsquerda :: Jogo -> [Movimento]
moveEsquerda j@(Jogo l(Jogador  (x,y) f g)) |g == False && (checkEsquerda (x,y) (coordenadasTotalMapa (0,0) l)) ==True && (checkEsquerda (x,y-1) (coordenadasTotalMapa (0,0)l)) == False = [Trepar]
                                            |g == True && (checkEsquerda  (x,y) (coordenadasTotalMapa (0,0) l)) ==True && (checkEsquerda (x,y-1) (coordenadasTotalMapa (0,0)l)) == False && (checkEsquerda (x,y-2) (coordenadasTotalMapa (0,0)l)) == False =[Trepar ]
                                            |g == True && (checkEsquerda  (x,y) (coordenadasTotalMapa (0,0) l)) ==False && (checkEsquerda (x,y-1) (coordenadasTotalMapa (0,0)l))== False = [AndarEsquerda ]
                                            |g == False && (checkEsquerda (x,y) (coordenadasTotalMapa (0,0) l)) == False = [AndarEsquerda  ]
                                            |otherwise = []                                          

bot1 :: Jogo -> [Movimento]
bot1 j@(Jogo l (Jogador (x,y) f g)) |direcao (Jogador (x,y) f g) (coordenadaPorta l) == "ChegouPorta" =[]
                                    |direcao (Jogador (x,y) f g) (coordenadaPorta l) == "SegueEsquerda" && moveEsquerda j == [] = []
                                    |direcao (Jogador (x,y) f g) (coordenadaPorta l) == "VaiDireita" && moveDireita j == [] = []
                                    |direcao (Jogador (x,y) f g) (coordenadaPorta l) == "VaiDireita" && portaDireitaDiag (Jogador (x,y) f g) (coordenadaPorta l) ==True = [Trepar ]
                                    |direcao (Jogador (x,y) f g) (coordenadaPorta l) == "VaiEsquerda" && portaEsquerdaDiag (Jogador (x,y) f g )(coordenadaPorta l) ==True  =[Trepar ]
                                    |direcao (Jogador (x,y) f g) (coordenadaPorta l) == "VaiDireita" && portaDireitaDiag (Jogador (x,y) f g) (coordenadaPorta l) == True =[AndarDireita ]
                                    |direcao (Jogador (x,y) f g) (coordenadaPorta l) == "VaiEsquerda" && portaEsquerdaDiag (Jogador (x,y) f g) (coordenadaPorta l) ==True = [AndarEsquerda ]
                                    |direcao (Jogador (x,y) f g) (coordenadaPorta l) == "VaiDireita" = moveDireita j ++ bot1 ()



moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo l (Jogador (a,b) d f)) m | f == False && m == AndarDireita || f == False && m == AndarEsquerda = Jogo l (moveEsquerdaDireita (Jogador (a,b)d f) l m)
                                           | f == False && m == Trepar = Jogo l (trepaEsquerdaDireita (Jogador (a,b)d f) l m) 
                                           | f == False && m == InterageCaixa = alteraMapa1 (Jogo l (Jogador (a,b) d f)) m  
                                           | f == True && m == InterageCaixa = alteraMapa1 (Jogo l (Jogador (a,b) d f)) m 
                                           | f == True && m == AndarEsquerda || f == True && m == AndarDireita = movercomCaixa (Jogo l (Jogador (a,b) d f)) m
                                           | f == True && m == Trepar = treparcomCaixa (Jogo l (Jogador (a,b) d f)) m 
                                           | otherwise = (Jogo l (Jogador (a,b) d f)) 




