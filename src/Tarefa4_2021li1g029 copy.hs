    {- |
Module      : Tarefa4_2021li1g029
Description : Movimentação do personagem
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g029 where

import LI12122

{- | Mostra como os diferentes movimentos afetam o jogador

@
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo mapa (Jogador (x,y) dir b)) m | m == AndarEsquerda = (Jogo mapa (Jogador (x,y) Oeste b))                                  
                                                | m == AndarEsquerda && dir == Oeste = (Jogo mapa (Jogador (x-1,y) Oeste b))                
                                                | m == AndarDireita = (Jogo mapa (Jogador (x,y) Este b))                                    
                                                | m == AndarEsquerda && dir == Oeste = (Jogo mapa (Jogador (x+1,y) Este b))                 
                                                | m == Trepar && dir == Oeste = (Jogo mapa (Jogador (x-1,y+1) Oeste b))                     
                                                | m == Trepar && dir == Este = (Jogo mapa (Jogador (x+1,y+1) Este b))                       
                                                | m == InterageCaixa && dir == Este && b == False = (Jogo mapa (Jogador (x,y) Este True))   
                                                | m == InterageCaixa && dir == Este && b == True = (Jogo mapa (Jogador (x,y) Este False))   
                                                | m == InterageCaixa && dir == Oeste && b == False = (Jogo mapa (Jogador (x,y) Oeste True))  
                                                | m == InterageCaixa && dir == Oeste && b == True = (Jogo mapa (Jogador (x,y) Oeste False))
@

== Exemplo de utilização

>>> moveJogador (Jogo mapa (Jogador (x,y) dir b)) AndarEsquerda  
(Jogo mapa (Jogador (x,y) Oeste b))

-}
moveJogador :: Jogo -> Movimento -> Jogo         -- jogo = (Jogo mapa (Jogador coordenadas direcao bool))
moveJogador (Jogo mapa (Jogador (x,y) dir b)) m | m == AndarEsquerda && dir == Este = (Jogo mapa (Jogador (x,y) Oeste b))                                  -- vira para oeste
                                                | m == AndarEsquerda && dir == Oeste = (Jogo mapa (Jogador (x-1,y) Oeste b))                -- anda para oeste
                                                | m == AndarDireita && dir == Oeste = (Jogo mapa (Jogador (x,y) Este b))                                    -- vira para este
                                                | m == AndarDireita && dir == Este = (Jogo mapa (Jogador (x+1,y) Este b))                 -- anda para este
                                                | m == Trepar && dir == Oeste = (Jogo mapa (Jogador (x-1,y-1) Oeste b))                     -- trepa virado para oeste
                                                | m == Trepar && dir == Este = (Jogo mapa (Jogador (x+1,y-1) Este b))                       -- trepa virado para este
                                                | m == InterageCaixa && dir == Este && b == False = (Jogo mapa (Jogador (x,y) Este True))   -- pega na caixa virado para este 
                                                | m == InterageCaixa && dir == Este && b == True = (Jogo mapa (Jogador (x,y) Este False))   -- larga a caixa virado para este
                                                | m == InterageCaixa && dir == Oeste && b == False = (Jogo mapa (Jogador (x,y) Oeste True)) -- pega na caixa virado para oeste 
                                                | m == InterageCaixa && dir == Oeste && b == True = (Jogo mapa (Jogador (x,y) Oeste False)) -- larga a caixa virado para oeste 




correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo l (Jogador (a,b) d f)) [] = Jogo l (Jogador (a,b) d f)
correrMovimentos (Jogo l (Jogador (a,b) d f)) (m:ms) = correrMovimentos (moveJogador (Jogo l (Jogador (a,b) d f)) m) ms 





-- | Testa se existe um obstáculo do lado direito
checkDireita :: Coordenadas -> [Coordenadas] -> Bool
checkDireita (x,y) [] = False
checkDireita (x,y) ((a,b):t) | y == b && x == a-1 = True
                             | otherwise = checkDireita (x,y) t


-- | Testa se existe um obstáculo do lado esquerdo
checkEsquerda :: Coordenadas -> [Coordenadas] -> Bool
checkEsquerda (x,y) [] = False
checkEsquerda (x,y) ((a,b):t) | y == b && x == a+1 = True
                              | otherwise = checkEsquerda (x,y) t


coordenadasCaixa :: Coordenadas -> [Peca] -> [Coordenadas]
coordenadasCaixa (x,y) [] = []
coordenadasCaixa (x,y) (z:zs) | z == Caixa = (x,y) : coordenadasCaixa (x+1,y) zs
                                  | otherwise = coordenadasCaixa (x+1,y) zs


coordenadasCaixaMapa :: Coordenadas -> Mapa -> [Coordenadas]
coordenadasCaixaMapa (x,y) [] = []
coordenadasCaixaMapa (x,y) (z:zs) = coordenadasCaixa (x,y) z ++ coordenadasCaixaMapa (0,y+1) zs



