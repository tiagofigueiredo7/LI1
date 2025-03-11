{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Tiago Silva Figueiredo <a106856@alunos.uminho.pt>
              Beatriz Lima Simões <a104432@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa1


valida :: Jogo -> Bool
valida jogo
    =  verificaChao mapajogo                            -- ponto 1
    && verificaRessalta player                          -- ponto 2  
    && verificaRessaltaLista ini
    && colisaoPosicaoInicialLista mapajogo player ini   -- ponto 3
    && verificaInimigos ini                             -- ponto 4
    && verificaVidaFantasmas ini                        -- ponto 5 
    && verificaEscadas mapajogo                         -- ponto 6
    && verificaTamanhoAlcapoes player                   -- ponto 7
    && verificaColecionaveisFinal col mapajogo          -- ponto 8
    && verificaPersonagem player mapajogo
    && verificaPersonagemFinal ini mapajogo
        where mapajogo = mapa jogo 
              ini = inimigos jogo 
              col = colecionaveis jogo 
              player = jogador jogo

-- | Função que valida um jogo conforme os 8 pontos

-- | Ponto 1
verificaChao :: Mapa  -> Bool
verificaChao (Mapa _ _ []) = False                 -- Matriz vazia, ou seja sem mapa definido
verificaChao (Mapa _ _ matriz)
    | [] `elem` matriz = False          -- Matriz com uma das linhas vazias, ou seja, mapa inválido
    | Escada `elem` last matriz = False 
    | Alcapao `elem` last matriz = False
    | Vazio `elem` last matriz = False
    | otherwise = True 

{-|
Função que verifica se a última linha da matriz apenas tem Plataformas

== Exemplos de utilização:

>>> verificaChao mapajogo2
False 

>>> verificaChao mapajogo
True
|-}


-- | Ponto 2
verificaRessaltaLista :: [Personagem] -> Bool 
verificaRessaltaLista [] = False 
verificaRessaltaLista lp = all verificaRessalta lp

{-| 
Função que verifica a propriedade Ressalta numa lista de Personagens

== Exemplos de utilização:

>>> verificaRessaltaLista [(Personagem (0,0) Fantasma (1,1) Oeste (2,2) False True 2 2 (True,2)),(Personagem (1,1) MacacoMalvado (2,2) Oeste (3,3) False True 2 2 (True,2))]
True

>>> verificaRessaltaLista [(Personagem (0,0) Fantasma (1,1) Oeste (2,2) False False 2 2 (True,2)),(Personagem (1,1) MacacoMalvado (2,2) Oeste (3,3) False True 2 2 (True,2))]
False
|-}


verificaRessalta :: Personagem -> Bool 
verificaRessalta (Personagem _ ent _ _ _ _ ress _ _ _) 
    | ent == Fantasma && ress = True
    | ent == MacacoMalvado && ress = True
    | ent == Jogador && not ress = True   -- Ressalta -> False no Jogador
    | otherwise = False 

{-|
Função que verifica a propriedade Ressalta num Personagem

== Exemplos de utilização:

>>> verificaRessalta (Personagem (1,1) MacacoMalvado (2,2) Oeste (3,3) False True 2 2 (True,2))
True

>>> verificaRessalta (Personagem (2,2) Jogador (3,3) Oeste (4,4) False False 2 2 (True,2))
True

>>> verificaRessalta (Personagem (2,2) Jogador (3,3) Oeste (4,4) False True 2 2 (True,2))
False
|-}


-- | Ponto 3
colisaoPosicaoInicialLista ::  Mapa -> Personagem -> [Personagem] -> Bool
colisaoPosicaoInicialLista (Mapa ((x,y),_)_ _) p lp 
    = all (\ini -> colisaoHitbox ((x,y), (x+l,y+a)) ini) li
        where (l,a) = tamanho p
              li = map fazHitbox lp


colisaoHitbox :: Hitbox -> Hitbox -> Bool
colisaoHitbox ((x1j,y1j),(x2j,y2j)) ((x1i,y1i),(x2i,y2i))
    | x1i >= x1j && x1i <= x2j && y1i <= y1j && y1i >= y2j = True
    | x2i >= x1j && x2i <= x2j && y2i <= y1j && y2i >= y2j= True
    | otherwise = False

fazHitbox :: Personagem -> Hitbox
fazHitbox p
    | direcao p == Este = ((x,y), (x+l,y-a))
    | direcao p == Oeste = ((x,y), (x-l,y-a))
    where (x,y) = posicao p 
          (l,a) = tamanho p

 
-- | Funções que verificam se a posição inicial do jogador não colide com a posição de um inimigo



-- | Ponto 4
verificaInimigos :: [Personagem] -> Bool
verificaInimigos linimigos =  length linimigos >= 2

{-| 
Função que verifica que os inimigos são pelo menos 2

== Exemplos de utilização:

>>> verificaInimigos [(Personagem (0,0) Fantasma (1,1) Oeste (2,2) False True 1 2 (True,2)),(Personagem (1,1) MacacoMalvado (2,2) Oeste (3,3) False True 2 2 (True,2))] 
True

>>> verificaInimigos [(Personagem (0,0) Fantasma (1,1) Oeste (2,2) False True 2 2 (True,2))]
False
|-}


-- | Ponto 5
verificaVidaFantasmas :: [Personagem] -> Bool
verificaVidaFantasmas [] = False
verificaVidaFantasmas linimigos 
    = all vidaFantasma (filter (\ (Personagem _ ent _ _ _ _ _ _ _ _ ) -> ent == Fantasma) linimigos)

vidaFantasma :: Personagem -> Bool
vidaFantasma (Personagem _ ent _ _ _ _ _ vida _ _ )
    = ent == Fantasma && vida == 1

{-| 
Funções que verificam se os Fantasmas têm apenas 1 vida

== Exemplos de utilização:

>>> verificaVidaFantasmas [(Personagem (0,0) Fantasma (1,1) Oeste (2,2) False True 1 2 (True,2)),(Personagem (1,1) Fantasma (2,2) Oeste (2,2) False True 1 2 (True,2)),(Personagem (1,1) MacacoMalvado (2,2) Oeste (3,3) False True 2 2 (True,2))]
True

>>> verificaVidaFantasmas [(Personagem (0,0) Fantasma (1,1) Oeste (2,2) False True 5 2 (True,2)),(Personagem (1,1) Fantasma (2,2) Oeste (2,2) False True 1 2 (True,2)),(Personagem (1,1) MacacoMalvado (2,2) Oeste (3,3) False True 2 2 (True,2))]
False

Nota:
Quando a lista é composta apenas por Macacos Malvados a função retorna TRUE
porque número de vidas dos Macacos não invalida o jogo. 
|-}


-- | Ponto 6
verificaEscadas :: Mapa -> Bool
verificaEscadas (Mapa _ _ matriz) =
    all (\posicaoBloco -> validaEscada posicaoBloco matriz) (posicoesEscadas matriz)

-- Retorna uma lista de posições de todas as escadas na matriz
posicoesEscadas :: [[Bloco]] -> [Posicao]
posicoesEscadas matriz =
    [(x, y) | (x, linha) <- zip [0..] matriz, (y, bloco) <- zip [0..] linha, bloco == Escada]

-- Verifica se uma escada atende às condições especificadas
validaEscada :: Posicao -> [[Bloco]] -> Bool
validaEscada (x, y) matriz =
    case matriz !! round x !! round y of
        Escada ->
            let acimaEhPlataforma = matriz !! (round x - 1) !! round y == Plataforma
                abaixoEhPlataforma = matriz !! (round x + 1) !! round y == Plataforma
            in acimaEhPlataforma || abaixoEhPlataforma
        _ -> True

{-|
Funções que verificam se as Escadas são válidas:
-> não começam/terminam em Alçapões
-> pelo menos uma das extremidades é Plataforma

== Exemplos de utilização:

>>> verificaEscadas (Mapa ((1,1),Oeste) (2,2) mapa3)
True 

>>> verificaEscadas (Mapa ((1,1),Oeste) (2,2) mapa2)
False
|-}


-- | Ponto 7
verificaTamanhoAlcapoes :: Personagem -> Bool
verificaTamanhoAlcapoes (Personagem _ _ _ _ (l,a) _ _ _ _ _) =  l < 1 

{-|
Função que verifica se o jogador é menos largo que os Alçapões

== Exemplos de utilização:

>>> verificaTamanhoAlcapoes (Personagem (2,2) Jogador (3,3) Oeste (0.5,4) False True 2 2 (True,2))
True 

>>> verificaTamanhoAlcapoes (Personagem (2,2) Jogador (3,3) Oeste (2,4) False True 2 2 (True,2))
False
|-}


-- | Ponto 8
-- | Colecionaveis
verificaColecionaveisFinal :: [(Colecionavel, Posicao)] -> Mapa -> Bool
verificaColecionaveisFinal colecionaveis mapa =
    all (\colecionavel -> verificaColecionaveis colecionavel mapa) colecionaveis


verificaColecionaveis :: (Colecionavel,Posicao) -> Mapa -> Bool
verificaColecionaveis (Moeda,(x,y)) mapa 
    =  all (==True) (map (verificaColecionaveisAux (Moeda,(x,y))) l)
    where l = matrizMapa mapa
verificaColecionaveis (Martelo,(x,y)) mapa 
    =  all (==True) (map (verificaColecionaveisAux (Moeda,(x,y))) l)
    where l = matrizMapa mapa


verificaColecionaveisAux :: (Colecionavel,Posicao) -> (Posicao,Bloco) -> Bool
verificaColecionaveisAux  (_,(z,w)) ((x,y),Plataforma) 
    | x == z && y == w = False                         -- Esta dentro da Plataforma
verificaColecionaveisAux  (_,(z,w)) ((x,y),Alcapao) 
    | x == z && y == w = False                         -- Esta dentro do Alcapao
verificaColecionaveisAux _ _ = True                    -- Qualquer outro caso esta fora 


-- | Personagens
verificaPersonagemAux :: Personagem -> (Posicao,Bloco) -> Bool
verificaPersonagemAux p ((x,y),Plataforma)
    | x == z && y == w = False            -- Esta dentro da Plataforma
    where (z,w) = posicao p
verificaPersonagemAux p ((x,y),Alcapao) 
    | x == z && y == w = False            -- Esta dentro do Alcapao
     where (z,w) = posicao p
verificaPersonagemAux _ _ = True             -- Qualquer outro caso esta fora



verificaPersonagem :: Personagem -> Mapa -> Bool
verificaPersonagem p mapa 
    =  all (==True) (map (verificaPersonagemAux p) l)
    where l = matrizMapa mapa


verificaPersonagemFinal :: [Personagem] -> Mapa -> Bool
verificaPersonagemFinal personagens mapa
    = all (\p -> verificaPersonagem p mapa) personagens

{-|
Funções que verificam que não há Colecionaveis/Personagens dentro de Plataformas ou Alçapões

== Exemplos de utilização:

>>> verificaColecionaveisFinal [(Moeda,(1,1)),(Moeda,(2,2))] (Mapa ((1,1),Oeste) (2,2) mapa1)
True

>>> verificaPersonagemFinal [(Personagem (0,0) Fantasma (1,1) Oeste (2,2) False True 5 2 (True,2)),(Personagem (1,1) Fantasma (2,2) Oeste (2,2) False True 1 2 (True,2)),(Personagem (1,1) MacacoMalvado (2,2) Oeste (3,3) False True 2 2 (True,2))] (Mapa ((1,1),Oeste) (2,2) mapa1)
True
|-}


-- | Exemplos de auxilio ao testes
mapa3 :: [[Bloco]]
mapa3= [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
       ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
       ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
       ,[Plataforma, Plataforma, Vazio, Vazio, Vazio,Vazio , Plataforma, Plataforma, Plataforma, Plataforma]
       ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
       ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
       ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

mapa2 :: [[Bloco]]
mapa2= [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
       ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
       ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
       ,[Plataforma, Plataforma, Vazio, Vazio, Vazio,Vazio , Plataforma, Plataforma, Alcapao, Plataforma]
       ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
       ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
       ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma]]

mapajogo :: Mapa 
mapajogo = Mapa ((1,5),Este) (1,2) mapa3

mapajogo2 :: Mapa 
mapajogo2 = Mapa ((1,5),Este) (1,2) mapa2

mapajogo3 :: Mapa 
mapajogo3 = Mapa ((1,5),Este) (1,2) mapa1

ini :: [Personagem]
ini = [Personagem (0,0) Fantasma (1,1) Oeste (1,1) False True 1 2 (True,2),Personagem (1,1) MacacoMalvado (2,2) Oeste (1,1) False True 2 2 (True,2)]
 
col :: [(Colecionavel,Posicao)]
col = [(Moeda,(1,1)),(Moeda,(2,2))]

player :: Personagem 
player = Personagem (2,2) Jogador (3,3) Oeste (0.5,1) False False 2 2 (True,2)

jogo1 ::Jogo
jogo1 = Jogo mapajogo ini col player 