module Mapas where 

import LI12122
import Tarefa2_2021li1g029

mapa1 :: Mapa
mapa1 = constroiMapa [(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Bloco,(0,5)),(Bloco,(0,6)),(Bloco,(0,7)),(Bloco,(0,8)),(Bloco,(0,9)),(Bloco,(1,1)),(Porta,(1,8)),(Bloco,(1,9)),(Bloco,(1,10)),(Bloco,(1,11)),(Bloco,(1,12)),(Bloco,(2,1)),(Bloco,(2,12)),(Bloco,(3,1)),(Bloco,(3,9)),(Bloco,(3,10)),(Bloco,(3,11)),(Bloco,(3,12)),(Bloco,(4,1)),(Bloco,(4,9)),(Bloco,(5,0)),(Bloco,(5,8)),(Bloco,(5,9)),(Bloco,(6,0)),(Bloco,(6,5)),(Bloco,(6,6)),(Bloco,(6,7)),(Bloco,(6,8)),(Bloco,(7,0)),(Caixa,(7,7)),(Bloco,(7,8)),(Bloco,(8,1)),(Caixa,(8,7)),(Bloco,(8,8)),(Bloco,(9,1)),(Caixa,(9,7)),(Bloco,(9,8)),(Bloco,(10,1)),(Caixa,(10,7)),(Bloco,(10,8)),(Bloco,(11,1)),(Bloco,(11,8)),(Bloco,(11,9)),(Bloco,(12,0)),(Bloco,(12,9)),(Bloco,(12,10)),(Bloco,(12,11)),(Bloco,(12,12)),(Bloco,(12,13)),(Bloco,(13,0)),(Bloco,(13,13)),(Bloco,(14,0)),(Bloco,(14,9)),(Bloco,(14,10)),(Bloco,(14,11)),(Bloco,(14,12)),(Bloco,(14,13)),(Bloco,(15,0)),(Bloco,(15,10)),(Bloco,(15,11)),(Bloco,(15,12)),(Bloco,(16,0)),(Bloco,(16,12)),(Bloco,(17,0)),(Bloco,(17,12)),(Bloco,(18,0)),(Caixa,(18,11)),(Bloco,(18,12)),(Bloco,(19,0)),(Caixa,(19,10)),(Caixa,(19,11)),(Bloco,(19,12)),(Bloco,(20,0)),(Caixa,(20,10)),(Caixa,(20,11)),(Bloco,(20,12)),(Bloco,(21,0)),(Caixa,(21,9)),(Caixa,(21,10)),(Caixa,(21,11)),(Bloco,(21,12)),(Bloco,(22,1)),(Bloco,(22,2)),(Bloco,(22,3)),(Bloco,(22,4)),(Bloco,(22,5)),(Bloco,(22,6)),(Bloco,(22,7)),(Bloco,(22,8)),(Bloco,(22,9)),(Bloco,(22,10)),(Bloco,(22,11)),(Bloco,(22,12))]

mapateste :: Mapa 
mapateste = constroiMapa [(Bloco, (1,1)),(Porta,(1,0)),(Caixa,(1,2))]