{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Tiago Silva Figueiredo <a106856@alunos.uminho.pt>
              Beatriz Lima Simões <a104432@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324

-- | Exemplo de personagem
personagemEX :: Personagem
personagemEX = Personagem { posicao = (2, 3)
                          , tamanho = (1, 1)
                          , velocidade = (0, 0)       
                          , tipo = Jogador           
                          , direcao = Este
                          , emEscada = False
                          , ressalta = False
                          , vida = 100
                          , pontos = 0
                          , aplicaDano = (False, 0.0)
                          }
-- | Exemplo de mapa
mapa1 :: [[Bloco]]
mapa1= [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
       ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
       ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
       ,[Plataforma, Plataforma, Vazio, Vazio, Vazio,Vazio , Plataforma, Plataforma, Plataforma, Plataforma]
       ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
       ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
       ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]


{-|
Funções do enunciado:
colisoesParede :: Mapa -> Personagem -> Bool
colisoesPersonagens :: Personagem -> Personagem -> Bool
-}


-- | Função que retorna a hitbox que envolve um personagem
hitbox :: Personagem -> Hitbox
hitbox personagem = ((x, y), (x + largura, y + altura))
  where
    (x, y) = posicao personagem
    (largura, altura) = tamanho personagem

{-| Exemplo da função hitbox
>>> hitbox personagemEX
((2,3),(3,4))
-}



-- | Função que converte um mapa numa matriz
matrizMapa :: Mapa -> [(Posicao, Bloco)]
matrizMapa (Mapa _ _ blocos) = concatMap (\(y, linha) -> zipWith (\x bloco -> ((x, y), bloco)) [0..] linha) ( zip [0..] blocos)

{-| Exemplo da função matrizMapa
>>> matrizMapa mapa1
[((0,0),Plataforma),((1,0),Plataforma),((2,0),Plataforma),((3,0),Plataforma),((4,0),Plataforma),((5,0),Plataforma), ... ]
-}


-- | Função que verifica se existe interseção entre dois retângulos
colisaoRetangulos :: Hitbox -> Hitbox -> Bool
colisaoRetangulos (x,y) (xs,ys) = not (ys < x || xs > y)

{-| Exemplo colisaoRetangulos
>>> colisaoRetangulos ((2,3),(3,4)) ((3,4),(5,6))
True
-}


-- | Função que verifica se um personagem se encontra em colisão com algum dos limites do mapa ou bloco de plataforma
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede mapa personagem =
  any (\(_, bloco) -> colisaoRetangulos (hitbox personagem) (hitboxBloco bloco)) (matrizMapa mapa)
  where
    hitboxBloco :: Bloco -> Hitbox
    hitboxBloco bloco = case bloco of
      Escada -> ((x, y), (x + larguraBloco, y + alturaBloco))
      Plataforma -> ((x, y), (x + larguraBloco, y + alturaBloco))
      Alcapao -> ((x, y), (x + larguraBloco, y + alturaBloco))
      Vazio -> ((x, y), (x + larguraBloco, y + alturaBloco))
      where
        (x, y) = (0, 0)  
        larguraBloco = 1
        alturaBloco = 1

{-| Exemplo colisoesParede
>>> colisoesParede mapa1 personagemEx
False
-}


-- | Função que verifica se dois personagens se encontram em colisão
colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = colisaoRetangulos (hitbox p1) (hitbox p2)

{-| Exemplo colisoesPersonagens
>>> colisoesPersonagens personagemEx personagem2
True
-}
