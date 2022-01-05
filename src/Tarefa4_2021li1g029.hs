    {- |
Module      : Tarefa4_2021li1g029
Description : Movimentação do personagem
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g029 where

import LI12122

moveJogador :: Jogo -> Movimento -> Jogo         -- jogo = (Jogo mapa (Jogador coordenadas direcao bool))
moveJogador (Jogo mapa (Jogador (x,y) dir b)) m | m == AndarEsquerda && dir == Este = (Jogo mapa (Jogador (x,y) Oeste b))                                  -- vira para oeste
                                                | m == AndarEsquerda && dir == Oeste = (Jogo mapa (moveparaEsquerda (Jogador (x,y) Oeste b) mapa AndarEsquerda))                -- anda para oeste
                                                | m == AndarDireita && dir == Oeste = (Jogo mapa (Jogador (x,y) Este b))                                    -- vira para este
                                                | m == AndarDireita && dir == Este = (Jogo mapa (moveparaDireita (Jogador (x,y) Este b) mapa AndarDireita))                -- anda para este
                                                | m == Trepar && dir == Oeste = (Jogo mapa (Jogador (x-1,y-1) Oeste b))                     -- trepa virado para oeste
                                                | m == Trepar && dir == Este = (Jogo mapa (treparDireita (Jogador (x,y) Este b) mapa Trepar))                       -- trepa virado para este
                                                | m == InterageCaixa && dir == Este && b == False = (Jogo mapa (Jogador (x,y) Este True))   -- pega na caixa virado para este 
                                                | m == InterageCaixa && dir == Este && b == True = (Jogo mapa (Jogador (x,y) Este False))   -- larga a caixa virado para este
                                                | m == InterageCaixa && dir == Oeste && b == False = (Jogo mapa (Jogador (x,y) Oeste True)) -- pega na caixa virado para oeste 
                                                | m == InterageCaixa && dir == Oeste && b == True = (Jogo mapa (Jogador (x,y) Oeste False)) -- larga a caixa virado para oeste 


correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo l (Jogador (a,b) d f)) [] = Jogo l (Jogador (a,b) d f)
correrMovimentos (Jogo l (Jogador (a,b) d f)) (m:ms) = correrMovimentos (moveJogador (Jogo l (Jogador (a,b) d f)) m) ms 


coordenadasTotalMapa :: Coordenadas -> Mapa -> [Coordenadas]
coordenadasTotalMapa (x,y) [] = []
coordenadasTotalMapa (x,y) (a:b) = aux (x,y) a ++ coordenadasTotalMapa (0,y+1) b
                               
                               where aux :: Coordenadas -> [Peca] -> [Coordenadas]
                                     aux (x,y) [] = []
                                     aux (x,y) (a:b) | a == Vazio || a == Porta = aux (x+1,y) b 
                                                     | otherwise = (x,y) : aux (x+1,y) b 


fimDoMapaDireita :: [Coordenadas] -> Coordenadas 
fimDoMapaDireita [] = (0,0)
fimDoMapaDireita [(x,y)] = (x,y)
fimDoMapaDireita ((x,y):(a,b):z) | x >= a = fimDoMapaDireita ((x,y):z)
                                 | otherwise = fimDoMapaDireita ((a,b):z)
                                 
fim :: Coordenadas -> Int 
fim (x,y) = x

movimento :: Jogador -> Movimento -> Jogador 
movimento (Jogador (x,y) d c) m | m == AndarEsquerda = (Jogador (x-1,y) Oeste c)
                                | m == AndarDireita = (Jogador (x+1,y) Este c)

moveparaDireita :: Jogador -> Mapa -> Movimento -> Jogador
moveparaDireita j@(Jogador (a,b) d f) l@((x:xs):y) m | a == fim (fimDoMapaDireita (coordenadasTotalMapa (0,0) l)) = (Jogador (a,b) Este f)
                                                     | otherwise = aux (checkDireita (a,b) (coordenadasTotalMapa (0,0) l)) j m

                                                     where aux :: Bool -> Jogador -> Movimento -> Jogador
                                                           aux t (Jogador (a,b) d f) m | t == True = (Jogador (a,b) Este f)
                                                                                       | otherwise = movimento (Jogador (a,b) d f) m

moveparaEsquerda :: Jogador -> Mapa -> Movimento -> Jogador
moveparaEsquerda j@(Jogador (a,b) d f) l@((x:xs):y) m | a == 0 = (Jogador (a,b) Oeste f)
                                                      | otherwise = aux (checkEsquerda (a,b) (coordenadasTotalMapa (0,0) l)) j m

                                                     where aux :: Bool -> Jogador -> Movimento -> Jogador
                                                           aux t (Jogador (a,b) d f) m | t == True = (Jogador (a,b) Oeste f)
                                                                                       | otherwise = movimento (Jogador (a,b) d f) m


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


coordenadasCaixaMapa :: Coordenadas -> Mapa -> [Coordenadas]
coordenadasCaixaMapa (x,y) [] = []
coordenadasCaixaMapa (x,y) (z:zs) = coordenadasCaixa (x,y) z ++ coordenadasCaixaMapa (0,y+1) zs
                                  where coordenadasCaixa (x,y) [] = []
                                        coordenadasCaixa (x,y) (z:zs) | z == Caixa = (x,y) : coordenadasCaixa (x+1,y) zs
                                          
-- | verifica se existe algo para trepar à direita
muro :: Jogador -> Coordenadas
muro (Jogador (a,b) d f) = (a+1,b)

-- | verifica se é possivel trepar
podeTrepar :: Coordenadas -> [Coordenadas] -> Bool 
podeTrepar (x,y) [] = False
podeTrepar (x,y) ((a,b):z) | x == a && y == b+1 = True 
                           | otherwise = podeTrepar (x,y) z

treparDireita :: Jogador -> Mapa -> Movimento -> Jogador
treparDireita j@(Jogador (a,b) d f) l m = aux (podeTrepar (muro j) (coordenadasTotalMapa (0,0) l)) j (checkDireita (a,b) (coordenadasTotalMapa (0,0) l))  m
                                        where aux :: Bool -> Jogador -> Bool -> Movimento -> Jogador
                                              aux f (Jogador (a,b) m t) r s | f == True = (Jogador (a,b) m t)
                                                                            | f == False && r == False = (Jogador (a,b) m t)
                                                                            | otherwise = (Jogador (a+1,b-1) m t)
