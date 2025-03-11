module Main where

import Test.HUnit
import LI12324
import Tarefa1
import Tarefa2
import Tarefa4

-- Testes para a Tarefa 1
test_suite_01 = test [
    "Colisão entre dois retângulos" ~: colisaoRetangulos ((2, 3), (3, 4)) ((3, 4), (5, 6)) ~?= True,
    "Não haverá colisão com as paredes" ~: colisoesParede mapajogo3 personagemEX ~?= False,
    "Haverá colisão entre dois personagens" ~: colisoesPersonagens personagemEX personagemEX ~?= True
    ]

-- Testes para a Tarefa 2
test_suite_02 = test 
    ["Valida o mapa" ~: True ~=? verificaChao mapajogo,
     "Valida o Ressalta das Personagens" ~: True ~=? verificaRessaltaLista [(Personagem (0,0) Fantasma (1,1) Oeste (2,2) False True 2 2 (True,2)),(Personagem (1,1) MacacoMalvado (2,2) Oeste (3,3) False True 2 2 (True,2))],
     "Valida a quantidade dos inimigos" ~: True ~=? verificaInimigos [(Personagem (0,0) Fantasma (1,1) Oeste (2,2) False True 1 2 (True,2)),(Personagem (1,1) MacacoMalvado (2,2) Oeste (3,3) False True 2 2 (True,2))],
     "Valida a vida dos Fantasmas" ~: True ~=? verificaVidaFantasmas [(Personagem (0,0) Fantasma (1,1) Oeste (2,2) False True 1 2 (True,2)),(Personagem (1,1) Fantasma (2,2) Oeste (2,2) False True 1 2 (True,2)),(Personagem (1,1) MacacoMalvado (2,2) Oeste (3,3) False True 2 2 (True,2))],
     "Valida as escadas de um mapa" ~: True ~=? verificaEscadas (Mapa ((1,1),Oeste) (2,2) mapa3),
     "Valida o tamanho dos alçapões" ~: True ~=? verificaTamanhoAlcapoes (Personagem (2,2) Jogador (3,3) Oeste (0.5,4) False True 2 2 (True,2)),
     "Valida os colecionáveis" ~: True ~=? verificaColecionaveisFinal [(Moeda,(1,1)),(Moeda,(2,2))] (Mapa ((1,1),Oeste) (2,2) mapa1),
     "Valida os Personagens" ~: True ~=? verificaPersonagemFinal [(Personagem (0,0) Fantasma (1,1) Oeste (2,2) False True 5 2 (True,2)),(Personagem (1,1) Fantasma (2,2) Oeste (2,2) False True 1 2 (True,2)),(Personagem (1,1) MacacoMalvado (2,2) Oeste (3,3) False True 2 2 (True,2))] (Mapa ((1,1),Oeste) (2,2) mapa1),
     "Valida o jogo" ~: False ~=? valida jogo1
    ]


-- Testes para a Tarefa 4
test_suite_04 = test
  [ "applyAction Subir" ~: applyAction Subir personagemInicial jogoInicial ~?= personagemInicial { velocidade = (0, 1) }
  , "applyAction Descer" ~: applyAction Descer personagemInicial jogoInicial ~?= personagemInicial { velocidade = (0, -1) }
  , "applyAction AndarDireita" ~: applyAction AndarDireita personagemInicial jogoInicial ~?= personagemInicial { velocidade = (1, 0), direcao = Este }
  , "applyAction AndarEsquerda" ~: applyAction AndarEsquerda personagemInicial jogoInicial ~?= personagemInicial { velocidade = (-1, 0), direcao = Oeste }
  , "applyAction Saltar" ~: applyAction Saltar personagemInicial jogoInicial ~?= personagemInicial { velocidade = (0, 1) }
  , "applyAction Parar" ~: applyAction Parar personagemInicial jogoInicial ~?= personagemInicial { velocidade = (0, 0) }
  , "atualizaJogador sem ações" ~: atualizaJogador [] personagemInicial jogoInicial ~?= personagemInicial
  , "atualizaJogador com ações" ~: atualizaJogador [Just AndarDireita, Just AndarEsquerda, Nothing] personagemInicial jogoInicial ~?= personagemInicial { velocidade = (0, 0), direcao = Oeste }
  , "atualiza sem ações momentâneas" ~: atualiza [Just AndarDireita, Just AndarEsquerda] Nothing jogoInicial ~?= jogoInicial { jogador = personagemInicial { velocidade = (0, 0), direcao = Oeste } }
  , "atualiza com ação momentânea" ~: atualiza [Just AndarDireita, Just AndarEsquerda] (Just Saltar) jogoInicial ~?= jogoInicial { jogador = personagemInicial { velocidade = (0, 1) } }
  , "atualiza com ação momentânea e colisão de plataforma" ~: atualiza [Just AndarDireita, Just Saltar] (Just AndarEsquerda) jogoInicial ~?= jogoInicial { jogador = personagemInicial { velocidade = (0, 1) } }
  ]



main :: IO ()
main = runTestTTAndExit $ test [test_suite_01,test_suite_02,test_suite_04]
