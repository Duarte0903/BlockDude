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
validaPotencialMapa [] = False 
validaPotencialMapa pecas = posicaoigual pecas == False && nportas pecas <= 1 && caixaflutua pecas == True && espacovazio pecas >= 1 && chao pecas == True 

-- determina se existem mais que uma pecas na mesma posicao (t1 p1)
posicaoigual :: [(Peca,Coordenadas)] -> Bool 
posicaoigual [] = False 
posicaoigual (x:xs) = 
       if snd x == snd (head xs) then False 
       else posicaoigual xs
 
-- determinha o n de portas(t1 p2)
nportas :: [(Peca,Coordenadas)] -> Int
nportas [] = 0
nportas (x:xs) = 
     if fst x == Porta then 1 + nportas xs 
     else nportas xs

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
caixaflutua ((p,(x,y)):xs) = 
      if ecaixa (p,(x,y-1)) || ebloco (p,(x,y-1)) then caixaflutua xs
      else False 

-- verifica que existe no min um espaco vazio (t1 p4)
espacovazio :: [(Peca,Coordenadas)] -> Int 
espacovazio [] = 0
espacovazio (x:xs) = 
      if fst x == Vazio then 1 + espacovazio xs 
      else espacovazio xs

-- verifica que a base do mapa e composta por blocos (t1 p5)
chao :: [(Peca,Coordenadas)] -> Bool 
chao [] = False
chao ((p,(a,b)):xs) = 
      if p == Bloco && b == 0 && a >= 0 then chao xs
      else False 
 

--validaPotencialMapa' :: [(Peca, Coordenadas)] -> Bool
--validaPotencialMapa' [] = False 
--validaPotencialMapa' (x:xs) | snd x /= snd (head xs) && (elemiguais x (x:xs)) == 1 && caixaflutua (x:xs) &&  espacovazio (x:xs) >= 1 && chao (x:xs)  
  --                          | otherwise = False 