{- |
Module      : Tarefa1_2021li1g029
Description : Validação de um potencial mapa
Copyright   : Duarte Leitão <a100550@alunos.uminho.pt>;
            : João Pereira <a100900@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g029 where

import LI12122

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa pecas = undefined

-- determinha o n de elemetos iguais (t1 p2)
elemiguais :: Peca -> [Peca] -> Int
elemiguais n [] = 0
elemiguais n (x:xs) = 
     if n == x then 1 + elemiguais n xs 
     else elemiguais n xs

-- verifica se e caixa (t1 p3)
ecaixa :: (Peca,Coordenadas) -> Bool
ecaixa (x,_) | x == Caixa = True
             | otherwise = False
     
-- verifica se e bloco (t1 p3)
ebloco :: (Peca,Coordenadas) -> Bool
ebloco (x,_) | x == Bloco = True
             | otherwise = False
   

-- verifica se a caixa flutua (t1 p3)
caixaflutua :: [(Peca,Coordenadas)] -> Bool
caixaflutua [] = False

{-}
validaPotencialMapa' :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa' [] = False 
validaPotencialMapa'  (x:xs) | snd x == snd (head xs) = False
                            | (elemiguais n (x:xs)) > 1 = False
                                                -}
                                                    

