module Tarefa6 where 

import LI12122
import Tarefa4_2021li1g029 
import Data.List


-- | Recebe um numero maximo de movimentos (int) e um jogo e devolve a lista com o mínimo de movimentos que solucionam o puzzle
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo int jogoBase = let jogosGanhos = filter venceu (verificaJogos int jogoBase) in
    if null jogosGanhos    -- o jogador não acabou na porta 
    then Nothing 
    else Just (jogosParaMovimentos (head (sortOn length jogosGanhos)))  -- devolve a lista de movimentos com menor comprimento


-- | Para cada jogo recebido, calcula os jogos possiveis retirando sucessivamente uma unidade ao int
-- irá devovler uma lista de listas em que o jogo base é a cabeça e os restantes elemetos serão os novos jogos com movimentos aplicados
verificaJogos :: Int -> Jogo -> [[Jogo]]
verificaJogos int jogoBase | int <= 0 = [[jogoBase]]  -- se o int for zero devolve-se o jogo inicial 
                           | otherwise = [jogoBase] : map (jogoBase :) (concatMap (verificaJogos (int - 1)) (recebeNovosJogos jogoBase))


-- | Irá aplicar cada um dos movimentos possíveis para devolver uma lista de jogos. Se o movimento não for executável será ignorado 
recebeNovosJogos :: Jogo -> [Jogo] 
recebeNovosJogos = recebeNovosJogos' [AndarEsquerda, AndarDireita, Trepar, InterageCaixa] where 
    -- | auxiliar: aplica os movementos ao jogo base 
    recebeNovosJogos' :: [Movimento] -> Jogo -> [Jogo]
    recebeNovosJogos' [] _ = []
    recebeNovosJogos' (h:t) jogoBase = let novoJogo = moveJogador jogoBase h in
        if novoJogo /= jogoBase then novoJogo : recebeNovosJogos' t jogoBase   -- o movimento foi executado 
        else recebeNovosJogos' t jogoBase   -- o movimento não foi válido para o jogo base 


-- | verifica se as coordenadas do jogador no último jogo são iguais às coordenadas da porta 
venceu :: [Jogo] -> Bool
venceu jogosPercorridos = (mapa !! y) !! x == Porta   -- y = linha e x = coluna
    where Jogo mapa (Jogador (x,y) _ _) = last jogosPercorridos


-- | Transforma uma lista de jogos numa lista de movimentos que foram executados nesses jogos
jogosParaMovimentos :: [Jogo] -> [Movimento]
jogosParaMovimentos [] = []
jogosParaMovimentos [_] = [] 
jogosParaMovimentos (x:y:t) = parDeJogosParaMovimento x y : jogosParaMovimentos (y:t) where
     
     -- | auxiliar: Retoma o movimento de transformou o jogo base no jogo com o movimento já aplicado  
     parDeJogosParaMovimento :: Jogo -> Jogo -> Movimento
     parDeJogosParaMovimento (Jogo _ (Jogador (_,y1) _ caixa1)) (Jogo _ (Jogador (_,y2) dir2 caixa2)) | caixa1 /= caixa2 = InterageCaixa
                                                                                                      | y1 > y2 = Trepar
                                                                                                      | dir2 == Este = AndarDireita 
                                                                                                      | otherwise = AndarEsquerda
        


