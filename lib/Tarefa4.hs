{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Tiago Silva Figueiredo <a106856@alunos.uminho.pt>
              Beatriz Lima Simões <a104432@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe
import Data.List (find) 

import LI12324
{- Função do enunciado:
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
-}

-- exemplo personagemInicial
personagemInicial :: Personagem
personagemInicial =
  Personagem
    { velocidade = (0, 0)
    , tipo = Jogador
    , posicao = (0, 0)
    , direcao = Este
    , tamanho = (2, 2)
    , emEscada = False
    , ressalta = False
    , vida = 3
    , pontos = 0
    , aplicaDano = (False, 0)
    }


-- Exemplo jogoInicial
jogoInicial :: Jogo
jogoInicial =
  Jogo
    { mapa = Mapa ((0, 0), Este) (5, 5) [[Escada, Plataforma, Plataforma, Plataforma, Plataforma]
                                          ,[Vazio, Vazio, Vazio, Vazio, Vazio]
                                          ,[Vazio, Vazio, Vazio, Vazio, Vazio]
                                          ,[Vazio, Vazio, Vazio, Vazio, Vazio]
                                          ,[Vazio, Vazio, Vazio, Vazio, Vazio]]
    , inimigos = []
    , colecionaveis = [(Moeda, (2, 2)), (Martelo, (4, 3))]
    , jogador = personagemInicial
    }


-- | função para aplicar uma ação 
applyAction :: Acao -> Personagem -> Jogo -> Personagem
applyAction Subir p jogo =
    if direcao p == Norte
        then let novaPos = (fst (posicao p), snd (posicao p) - 1)
             in p { posicao = novaPos, velocidade = (fst (velocidade p), -1) }
        else p
applyAction Descer p jogo =
    if direcao p == Sul
        then let novaPos = (fst (posicao p), snd (posicao p) + 1)
             in p { posicao = novaPos, velocidade = (fst (velocidade p), 1) }
        else p
applyAction AndarDireita p jogo =
    let novaPos = (fst (posicao p) + 20, snd (posicao p))
    in p { velocidade = (1, snd (velocidade p)), posicao = novaPos, direcao = if direcao p == Oeste && ressalta (jogador jogo) then Oeste else Este }
applyAction AndarEsquerda p jogo =
    let novaPos = (fst (posicao p) - 20, snd (posicao p))
    in p { velocidade = (-1, snd (velocidade p)), posicao = novaPos, direcao = if direcao p == Este && ressalta (jogador jogo) then Este else Oeste }
applyAction Saltar p jogo =
    let plataformaSuperior = find (\pos -> pos == posicao p) (map fst (posicoesBloco (mapa jogo)))
        novaPos = (fst (posicao p), snd (posicao p) + 2)
    in if isJust plataformaSuperior then p { velocidade = (fst (velocidade p), 1), posicao = novaPos } else p
applyAction Parar p _ = p { velocidade = (0, snd (velocidade p)) }


-- | função que obtém as posições dos blocos no mapa
posicoesBloco :: Mapa -> [(Posicao, Bloco)]
posicoesBloco (Mapa _ _ blocos) = concatMap (\(pos, listaBlocos) -> map (\bloco -> (pos, bloco)) listaBlocos) (zip posicoes blocos)
  where
    posicoes = [(x, y) | x <- [0..fromIntegral (length blocos - 1)], y <- [0..fromIntegral (length (head blocos) - 1)]]


-- | função para atualizar as velocidades dos personagens
atualizaJogador :: [Maybe Acao] -> Personagem -> Jogo -> Personagem
atualizaJogador [] p _ = p
atualizaJogador (a:as) p jogo = atualizaJogador as (applyAction (fromMaybe Parar a) p jogo) jogo


-- | função que atualiza o estado do jogo com base nas ações do jogador
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza [] _ jogo = jogo
atualiza (a:as) m jogo =
  let jogo' = jogo { jogador = atualizaJogador as (jogador jogo) jogo }
  in if isJust m then jogo' { jogador = applyAction (fromJust m) (jogador jogo') jogo' } else jogo'


{- | 
Exemplo de uso:
>>> let jogoInicial = 
    Jogo {
        mapa = Mapa [],
        inimigos = [Personagem { velocidade = (1.0, 1.0), tipo = MacacoMalvado, posicao = (5, 6), direcao = Este, tamanho = (2,2), emEscada = False, ressalta = False, vida = 100, pontos = 0, aplicaDano = (False, 0) }],
        colecionaveis = [(Moeda, (7, 8))],
        jogador =
            Personagem
            { velocidade = (1.0, 1.0)
            , tipo = Jogador
            , posicao = (0, 0)  
            , direcao = Este
            , tamanho = (2,2)
            , emEscada = False
            , ressalta = False
            , vida = 3
            , pontos = 5
            , aplicaDano = (True, 10.0)
            }
        }

>>> let acoes = [Nothing, Just Saltar, Nothing, Just Parar, Just AndarDireita, Just AndarEsquerda]
>>> let acaoMomento = Just Saltar
>>> let jogoAtualizado = atualiza acoes acaoMomento jogoInicial
>>> print jogoAtualizado
-}
