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
moveJogador (Jogo mapa (Jogador (x,y) dir b)) m | m == AndarEsquerda = (Jogo mapa (Jogador (x,y) Oeste b))                                  -- vira para oeste
                                                | m == AndarEsquerda && dir == Oeste = (Jogo mapa (Jogador (x-1,y) Oeste b))                -- anda para oeste
                                                | m == AndarDireita = (Jogo mapa (Jogador (x,y) Este b))                                    -- vira para este
                                                | m == AndarEsquerda && dir == Oeste = (Jogo mapa (Jogador (x+1,y) Este b))                 -- anda para este
                                                | m == Trepar && dir == Oeste = (Jogo mapa (Jogador (x-1,y+1) Oeste b))                     -- trepa virado para oeste
                                                | m == Trepar && dir == Este = (Jogo mapa (Jogador (x+1,y+1) Este b))                       -- trepa virado para este
                                                | m == InterageCaixa && dir == Este && b == False = (Jogo mapa (Jogador (x,y) Este True))   -- pega na caixa virado para este
                                                | m == InterageCaixa && dir == Este && b == True = (Jogo mapa (Jogador (x,y) Este False))   -- larga a caixa virado para este
                                                | m == InterageCaixa && dir == Oeste && b == False = (Jogo mapa (Jogador (x,y) Oeste True)) -- pega na caixa virado para oeste
                                                | m == InterageCaixa && dir == Oeste && b == True = (Jogo mapa (Jogador (x,y) Oeste False)) -- larga a caixa virado para oeste

{- | Devolve o jogo final determinado em aux1 depois de correr
todos os movimentos

@
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo mapa (Jogador (x,y) dir b)) [] = Jogo mapa (Jogador (x,y) Este False)
correrMovimentos (Jogo mapa (Jogador (x,y) dir b)) l = aux1 (Jogo mapa (Jogador (x,y) dir b)) l
@

-}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo mapa (Jogador (x,y) dir b)) [] = Jogo mapa (Jogador (x,y) Este False)
correrMovimentos (Jogo mapa (Jogador (x,y) dir b)) l = aux1 (Jogo mapa (Jogador (x,y) dir b)) l

{- | Recebe um jogo e uma lista de movimentos.
Devolve um jogo onde foram corridos todos os movimentos

@
aux1 :: Jogo -> [Movimento] -> Jogo
aux1 (Jogo mapa (Jogador (x,y) dir b)) l = aux3 (aux2 (Jogo mapa (Jogador (x,y) dir b)) l)
@

-}
aux1 :: Jogo -> [Movimento] -> Jogo
aux1 (Jogo mapa (Jogador (x,y) dir b)) l = aux3 (aux2 (Jogo mapa (Jogador (x,y) dir b)) l)

{- | Recebe um jogo e uma lista de movimentos. Devolve uma lista de jogos, onde cada um corresponde
a correr um movimento no jogo original

@
aux2 :: Jogo -> [Movimento] -> [Jogo]
aux2 (Jogo mapa (Jogador (x,y) dir b)) [] = [Jogo mapa (Jogador (x,y) dir b)]
aux2 (Jogo mapa (Jogador (x,y) dir b)) (h:t) = moveJogador (Jogo mapa (Jogador (x,y) dir b)) h : aux2 (Jogo mapa (Jogador (x,y) dir b)) t
@

== Exemplos de utilização

>>> aux2 (Jogo mapa (Jogador (x,y) dir b)) []
[Jogo mapa (Jogador (x,y) dir b)]

>>> aux2 (Jogo mapa (Jogador (x,y) dir b)) (h:t)
moveJogador (Jogo mapa (Jogador (x,y) dir b)) h : aux2 (Jogo mapa (Jogador (x,y) dir b)) t

-}
aux2 :: Jogo -> [Movimento] -> [Jogo]
aux2 (Jogo mapa (Jogador (x,y) dir b)) [] = [Jogo mapa (Jogador (x,y) dir b)]
aux2 (Jogo mapa (Jogador (x,y) dir b)) (h:t) = moveJogador (Jogo mapa (Jogador (x,y) dir b)) h : aux2 (Jogo mapa (Jogador (x,y) dir b)) t

{- | Transforma uma lista de jogos num jogo

@
aux3 :: [Jogo] -> Jogo
aux3 [Jogo mapa (Jogador (x,y) dir b)] = Jogo mapa (Jogador (x,y) dir b)
aux3 (h:t) = aux3 t
@

== Exemplos de utilização

>>> aux3 [Jogo mapa (Jogador (x,y) dir b)]
Jogo mapa (Jogador (x,y) dir b)

>>> aux3 (h:t)
aux3 t

-}
aux3 :: [Jogo] -> Jogo
aux3 [Jogo mapa (Jogador (x,y) dir b)] = Jogo mapa (Jogador (x,y) dir b)
aux3 (h:t) = aux3 t