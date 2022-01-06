    {- |
Module      : Tarefa4_2021li1g029
Description : Movimentação do personagem
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g029 where

import LI12122

-- | Altera um jogo quando lhe é aplicado um movimento 
moveJogador :: Jogo -> Movimento -> Jogo 
moveJogador (Jogo mapa (Jogador (x,y) dir b)) m | m == AndarEsquerda && dir == Este = (Jogo mapa (Jogador (x,y) Oeste b))                                                       -- vira para oeste
                                                | m == AndarEsquerda && dir == Oeste = (Jogo mapa(naoFlutua (moveparaEsquerda (Jogador (x,y) Oeste b) mapa AndarEsquerda) mapa))                -- anda para oeste
                                                | m == AndarDireita && dir == Oeste = (Jogo mapa (Jogador (x,y) Este b))                                                        -- vira para este
                                                | m == AndarDireita && dir == Este = (Jogo mapa (naoFlutua (moveparaDireita (Jogador (x,y) Este b) mapa AndarDireita) mapa))                -- anda para este
                                                | m == Trepar && dir == Oeste = (Jogo mapa (treparEsquerda (Jogador (x,y) Oeste b) mapa Trepar))                     -- trepa virado para oeste
                                                | m == Trepar && dir == Este = (Jogo mapa (treparDireita (Jogador (x,y) Este b) mapa Trepar))                       -- trepa virado para este
                                                | m == InterageCaixa && dir == Este && b == False = (Jogo mapa (Jogador (x,y) Este True))   -- pega na caixa virado para este
                                                | m == InterageCaixa && dir == Oeste && b == False = (Jogo mapa (Jogador (x,y) Oeste True)) -- pega na caixa virado para oeste 
                                                | m == InterageCaixa && dir == Este && b == True = (Jogo mapa (Jogador (x,y) Este False))  
                                                | m == InterageCaixa && dir == Oeste && b == True = (Jogo mapa (Jogador (x,y) Oeste False)) 


correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo l (Jogador (a,b) d f)) [] = Jogo l (Jogador (a,b) d f)
correrMovimentos (Jogo l (Jogador (a,b) d f)) (m:ms) = correrMovimentos (moveJogador (Jogo l (Jogador (a,b) d f)) m) ms 


-- | Verifica se o jogador está em cima de algo / não flutua
existeBase :: Coordenadas -> [Coordenadas] -> Bool
existeBase (x,y) [] = False
existeBase (x,y) ((a,b):c) | x == a && y == b-1 = True 
                           | otherwise = existeBase (x,y) c


-- | Retorna as coordenadas das pecas com a mesma abcissa
coordsColuna :: Coordenadas -> [Coordenadas] -> [Coordenadas] 
coordsColuna (x,y) [] = []
coordsColuna (x,y) ((a,b):c) | x == a = (a,b) : coordsColuna (x,y) c
                             | otherwise = coordsColuna (x,y) c 


-- | Devolve as coordenadas das pecas por baixo do jogador 
coordsInf :: Coordenadas -> [Coordenadas] -> [Coordenadas] 
coordsInf (x,y) [] = []
coordsInf (x,y)  ((a,b):c) | y >= b = coordsInf (x,y) c
                           | otherwise = (a,b) : coordsInf (x,y) c


-- | Devolve a coordenada com menor ordenada da coluna
chao :: [Coordenadas] -> Coordenadas
chao [(x,y)] = (x,y)
chao ((x,y):(a,b):c) | y < b = chao ((x,y):c)  
                     | otherwise = chao ((a,b):c)


naoFlutua :: Jogador -> Mapa -> Jogador
naoFlutua j@(Jogador (a,b) d f) l@((x:xs):y) = aux (chao (coordsInf (a,b) (coordsColuna (a,b) (coordenadasTotalMapa (0,0) l)))) (existeBase (a,b) (coordenadasTotalMapa (0,0) l)) j 
                                            where aux :: Coordenadas -> Bool -> Jogador -> Jogador
                                                  aux (x,y) e (Jogador (a,b) d c) | e == True = (Jogador (a,b) d c)
                                                                                  | otherwise = (Jogador (a,y-1) d c)


-- | Atribui coordenadas às caixas e blocos. As portas e vazios são ignorados 
coordenadasTotalMapa :: Coordenadas -> Mapa -> [Coordenadas]
coordenadasTotalMapa (x,y) [] = []
coordenadasTotalMapa (x,y) (a:b) = aux (x,y) a ++ coordenadasTotalMapa (0,y+1) b
                               
                               where aux :: Coordenadas -> [Peca] -> [Coordenadas]
                                     aux (x,y) [] = []
                                     aux (x,y) (a:b) | a == Vazio || a == Porta = aux (x+1,y) b 
                                                     | otherwise = (x,y) : aux (x+1,y) b 


-- | Marca o fim do mapa à direita. À esquerda será x = 0
fimDoMapaDireita :: [Coordenadas] -> Coordenadas 
fimDoMapaDireita [] = (0,0)
fimDoMapaDireita [(x,y)] = (x,y)
fimDoMapaDireita ((x,y):(a,b):z) | x >= a = fimDoMapaDireita ((x,y):z)
                                 | otherwise = fimDoMapaDireita ((a,b):z)


-- | Devolve a abcissa mais à direita do par determinado com fimDoMapaDireita
fim :: Coordenadas -> Int 
fim (x,y) = x


-- | Move o jogador uma unidade para esquerda ou direita. É auxiliar 
movimento :: Jogador -> Movimento -> Jogador 
movimento (Jogador (x,y) d c) m | m == AndarEsquerda = (Jogador (x-1,y) Oeste c)
                                | m == AndarDireita = (Jogador (x+1,y) Este c)


-- | Move o jogador para a direita quando não existem obstáculos
moveparaDireita :: Jogador -> Mapa -> Movimento -> Jogador
moveparaDireita j@(Jogador (a,b) d f) l@((x:xs):y) m | a == fim (fimDoMapaDireita (coordenadasTotalMapa (0,0) l)) = (Jogador (a,b) Este f)
                                                     | otherwise = aux (checkDireita (a,b) (coordenadasTotalMapa (0,0) l)) j m

                                                     where aux :: Bool -> Jogador -> Movimento -> Jogador
                                                           aux t (Jogador (a,b) d f) m | t == True = (Jogador (a,b) Este f)
                                                                                       | otherwise = movimento (Jogador (a,b) d f) m


-- | Move o jogador para a esquerda quando não existem obstaculos
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

                                          
-- | verifica se existe algo para trepar à direita
muroDireita :: Jogador -> Coordenadas
muroDireita (Jogador (a,b) d f) = (a+1,b)


-- verifica se existe algo para trepar à esquerda 
muroEsquerda :: Jogador -> Coordenadas
muroEsquerda (Jogador (a,b) d f) = (a-1,b)


-- | verifica se é possivel trepar
podeTrepar :: Coordenadas -> [Coordenadas] -> Bool 
podeTrepar (x,y) [] = False
podeTrepar (x,y) ((a,b):z) | x == a && y == b+1 = True 
                           | otherwise = podeTrepar (x,y) z


-- | faz o jogador trepar para a direita se existir algo para trepar com altura de apenas um bloco
treparDireita :: Jogador -> Mapa -> Movimento -> Jogador
treparDireita j@(Jogador (a,b) d f) l m = aux (podeTrepar (muroDireita j) (coordenadasTotalMapa (0,0) l)) j (checkDireita (a,b) (coordenadasTotalMapa (0,0) l))  m
                                        where aux :: Bool -> Jogador -> Bool -> Movimento -> Jogador
                                              aux f (Jogador (a,b) m t) r s | f == True = (Jogador (a,b) m t)
                                                                            | f == False && r == False = (Jogador (a,b) m t)
                                                                            | otherwise = (Jogador (a+1,b-1) m t)


-- | faz o jogador trepar para a esquerda se existir algo para trepar com altura de apenas um bloco
treparEsquerda :: Jogador -> Mapa -> Movimento -> Jogador
treparEsquerda j@(Jogador (a,b) d f) l m = aux (podeTrepar (muroEsquerda j) (coordenadasTotalMapa (0,0) l)) j (checkEsquerda (a,b) (coordenadasTotalMapa (0,0) l))  m
                                         where aux :: Bool -> Jogador -> Bool -> Movimento -> Jogador
                                               aux f (Jogador (a,b) m t) r s | f == True = (Jogador (a,b) m t)
                                                                             | f == False && r == False = (Jogador (a,b) m t)
                                                                             | otherwise = (Jogador (a-1,b-1) m t)


-- | Devolve uma lista com as coordenadas das caixas
coordenadasCaixaMapa :: Coordenadas -> Mapa -> [Coordenadas]
coordenadasCaixaMapa (x,y) [] = []
coordenadasCaixaMapa (x,y) (z:zs) = coordenadasCaixa (x,y) z ++ coordenadasCaixaMapa (0,y+1) zs
                                  where coordenadasCaixa (x,y) [] = []
                                        coordenadasCaixa (x,y) (h:t) | h == Caixa = (x,y) : coordenadasCaixa (x+1,y) t 


-- | Verifica se existe uma caixa à esquerda 
checkCaixaEsquerda :: Coordenadas -> [Coordenadas] -> Bool
checkCaixaEsquerda (x,y) [] = False
checkCaixaEsquerda (x,y) ((a,b):c) | x == a-1 && y == b = True 
                                   | otherwise = checkCaixaEsquerda (x,y) c


-- | Verifica se existe uma caixa à direita 
checkCaixaDireita :: Coordenadas -> [Coordenadas] -> Bool
checkCaixaDireita (x,y) [] = False
checkCaixaDireita (x,y) ((a,b):c) | x == a+1 && y == b = True 
                                  | otherwise = checkCaixaEsquerda (x,y) c


-- | O jogador interage com a caixa à sua direita se possivel 
intergeCaixaDireita :: Jogador -> Mapa -> Movimento -> Jogador
intergeCaixaDireita j@(Jogador (a,b) d f) l m = aux (checkCaixaDireita (a,b) (coordenadasCaixaMapa (0,0) l)) j m (podeTrepar (a,b) (coordenadasTotalMapa (0,0) l))
                                          
                                          where aux :: Bool -> Jogador -> Movimento -> Bool -> Jogador
                                                aux r j@(Jogador (a,b) d h) m l | h == False && m == InterageCaixa && r == True && l == False = (Jogador (a,b) d True)   
                                                                                | otherwise = j


-- | O jogador interage com a caixa à sua esquerda se possível 
intergeCaixaEsquerda :: Jogador -> Mapa -> Movimento -> Jogador
intergeCaixaEsquerda j@(Jogador (a,b) d f) l m = aux (checkCaixaEsquerda (a,b) (coordenadasCaixaMapa (0,0) l)) j m (podeTrepar (a,b) (coordenadasTotalMapa (0,0) l))
                                          
                                          where aux :: Bool -> Jogador -> Movimento -> Bool -> Jogador
                                                aux r j@(Jogador (a,b) d h) m l | h == False && m == InterageCaixa && r == True && l == False = (Jogador (a,b) d True)   
                                                                                | otherwise = j


-- | Faz o jogador largar a caixa. O bool corresponde ao resultado de checkDireita e checkEsquerda (se existem obstaculos)
largaCaixa :: Bool -> Jogador -> Movimento -> Jogador
largaCaixa b j@(Jogador (x,y) d c) m | c == True && b == False = (Jogador (x,y) d False)
                                     | otherwise = j 


-- | O jogador larga a caixa orientado para a direita, se não existir obstaculos 
largaCaixaDireita :: Jogador -> Mapa -> Movimento -> Jogador
largaCaixaDireita j@(Jogador (a,b) d f) l m = largaCaixa (checkDireita (a,b) (coordenadasTotalMapa (0,0) l)) j m 


-- | O jogador larga a caixa orientado para a esquerda, se não existir obstaculos
largaEsquerda :: Jogador -> Mapa -> Movimento -> Jogador
largaEsquerda j@(Jogador (a,b) d f) l m = largaCaixa (checkEsquerda (a,b) (coordenadasTotalMapa (0,0) l)) j m 


