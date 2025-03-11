module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitFailure)
import System.Random
import Data.Maybe
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3 
import Tarefa4


-- | Data-Types e funções relevantes
data Estado = Estado -- Estado (Jogo,Imagens,Modo de Jogo)
  { jogo :: Jogo,
    imagens :: Imagens,
    modo :: Modo,
    tempo :: Float,
    pausa :: Bool
  }

data Imagem
  = MARIO
  | FANTASMA
  | MACACO 
  | PLATAFORMA
  | ESCADA
  | ALCAPAO 
  | PRINCESA
  | MARTELO 
  | MOEDA
  | VAZIO
  deriving (Show, Eq)

type Imagens = [(Imagem, Picture)]

data MenuInicialOpcoes = Jogar 
                       | Sair deriving (Show, Eq)

data Modo = MenuInicial MenuInicialOpcoes 
          | EmJogo
          | Vitoria
          | GameOver
          deriving (Show, Eq)

getImagem :: Imagem -> Imagens -> Picture
getImagem k d = fromJust $ lookup k d

getMapa :: Mapa ->  [[Bloco]]
getMapa (Mapa _ _ m) = m

l :: Int
l = 25

l'::Float 
l' = 25

-- | Definição do mapa e jogo em geral
jogoFinal :: Jogo
jogoFinal = Jogo mapaFinal [macaco,fantasma1,fantasma2,fantasma3] [martelo,moeda1,moeda2] mario


matrizFinal :: [[Bloco]]
matrizFinal = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio]
           ,[Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio]
           ,[Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio]
           ,[Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio]
           ,[Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio]
           ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
       ]

mapaFinal :: Mapa
mapaFinal = Mapa ((1,1),Oeste) (-30,-190) matrizFinal

macaco :: Personagem
macaco =
  Personagem
    { velocidade = (0, 0)
    , tipo = MacacoMalvado
    , posicao = (-10,-261)
    , direcao = Oeste
    , tamanho = (2, 2)
    , emEscada = False
    , ressalta = True
    , vida = 3
    , pontos = 0
    , aplicaDano = (False, 0)
    }

mario :: Personagem
mario =
  Personagem
    { velocidade = (0, 0)
    , tipo = Jogador
    , posicao =(-140,-190)
    , direcao = Este
    , tamanho = (2, 2)
    , emEscada = False
    , ressalta = False
    , vida = 3
    , pontos = 0
    , aplicaDano = (False, 0)
    }

fantasma1 :: Personagem
fantasma1 =
  Personagem
    { velocidade = (0, 0)
    , tipo = Fantasma
    , posicao = (-100,-110)
    , direcao = Oeste
    , tamanho = (2, 2)
    , emEscada = False
    , ressalta = True
    , vida = 1
    , pontos = 0
    , aplicaDano = (False, 0)
    }

fantasma2 :: Personagem
fantasma2 =
  Personagem
    { velocidade = (0, 0)
    , tipo = Fantasma
    , posicao = (140,110)
    , direcao = Oeste
    , tamanho = (2, 2)
    , emEscada = False
    , ressalta = True
    , vida = 1
    , pontos = 0
    , aplicaDano = (False, 0)
    }


fantasma3 :: Personagem
fantasma3 =
  Personagem
    { velocidade = (0, 0)
    , tipo = Fantasma
    , posicao = (-10,40)
    , direcao = Oeste
    , tamanho = (2, 2)
    , emEscada = False
    , ressalta = True
    , vida = 1
    , pontos = 0
    , aplicaDano = (False, 0)
    }

martelo :: (Colecionavel,Posicao)
martelo = (Martelo, (-50,-40))

moeda1 :: (Colecionavel,Posicao)
moeda1 = (Moeda,(-140,121))

moeda2 :: (Colecionavel,Posicao)
moeda2 = (Moeda, (120,-110))

-- | Opções de Jogo
opcaoJogar :: Picture
opcaoJogar = Translate (-150) 100 $ Text "Jogar"

opcaoSair :: Picture
opcaoSair = Translate (-150) (-100) $ Text "Sair"

-- | Função desenha
desenha :: Estado -> IO Picture
desenha e@Estado {modo = MenuInicial Jogar} =
  return $ Pictures [Color red opcaoJogar, opcaoSair]
desenha e@Estado {modo = MenuInicial Sair} =
  return $ Pictures [opcaoJogar, Color red opcaoSair]
desenha e@Estado {modo = EmJogo, imagens = imgs , jogo = (Jogo mapa inimigos col jogador) , tempo = t} =
  return $ Pictures ([desenhoDosPontos]++[desenhoDaVida]++[desenhoDoTempo] ++ desenhoDoMapa ++ desenhoDasPersonagens ++ desenhoDosColecionaveis ++ desenhoDaPrincesa)
  where desenhoDoMapa = desenhaMapa matriz (-15,11) imgs
        desenhoDoTempo = Translate (-350) 350 $ Color black $ Scale 0.2 0.2 $ Text $ "Tempo: " ++ show (round t) 
        desenhoDaVida = Translate (-50) 350 $ Color black $ Scale 0.2 0.2 $ Text $ "Vida: " ++ show (vida jogador)  
        desenhoDosPontos = Translate 200 350 $ Color black $ Scale 0.2 0.2 $ Text $ "Pontos: " ++ show (pontos jogador)
        matriz = getMapa mapa
        desenhoDasPersonagens = desenhaPersonagens (inimigos++[jogador]) imgs
        desenhoDosColecionaveis = desenhaColecionaveis col imgs
        desenhoDaPrincesa = desenhaPrincesa mapa imgs
desenha e@Estado {modo = Vitoria, imagens = imgs} =
  return $ Pictures [Color green $ Translate (-360) 0 $ Scale 0.5 0.5 $ Text "Jogo ganho. Parabens!"]
desenha e@Estado { modo = GameOver, imagens = imgs } =
  return $ Pictures [Color red $ Translate (-200) 0 $ Scale 0.5 0.5 $ Text "Perdeste!", opcaoSair]

-- DESENHA O MAPA
desenhaMapa :: [[Bloco]] -> (Int, Int)-> Imagens -> [Picture]
desenhaMapa (h:t) (x,y) imgs = tl (Pictures (desenhaLinha h (x, y) imgs)) : desenhaMapa t (x, y-1) imgs
    where tl = Translate 0 (fromIntegral (y*l))
desenhaMapa _ _ _ = []

desenhaLinha :: [Bloco] -> (Int, Int) -> Imagens -> [Picture]
desenhaLinha [] _ _ = []
desenhaLinha (h:t) (x, y) imgs = tl (desenhaBloco h imgs) : desenhaLinha t (x+1, y) imgs
    where tl = Translate (fromIntegral (x*l)) 0

desenhaBloco :: Bloco -> Imagens -> Picture
desenhaBloco Plataforma imgs = getImagem PLATAFORMA imgs
desenhaBloco Escada imgs = getImagem ESCADA imgs
desenhaBloco Alcapao imgs = getImagem ALCAPAO imgs
desenhaBloco Vazio imgs = getImagem VAZIO imgs

-- DESENHA OS PERSONAGENS

desenhaPersonagens :: [Personagem] -> Imagens -> [Picture]
desenhaPersonagens personagens imgs = map (\personagem -> desenhaPersonagem personagem imgs) personagens

desenhaPersonagem :: Personagem -> Imagens -> Picture
desenhaPersonagem (Personagem {tipo = Jogador, posicao = (x,y)}) imgs 
  = Translate (realToFrac x) (-(realToFrac y)) $ getImagem MARIO imgs
desenhaPersonagem (Personagem {tipo = MacacoMalvado, posicao = (x,y)}) imgs 
  = Translate (realToFrac x) (-(realToFrac y)) $ getImagem MACACO imgs
desenhaPersonagem (Personagem {tipo = Fantasma, posicao = (x,y)}) imgs 
  = Translate (realToFrac x) (-(realToFrac y)) $ getImagem FANTASMA imgs

-- DESENHA OS COLECIONÀVEIS

desenhaColecionaveis :: [(Colecionavel, Posicao)] -> Imagens -> [Picture]
desenhaColecionaveis colecionaveis imgs = map (\colecionavel -> desenhaColecionavel colecionavel imgs) colecionaveis

desenhaColecionavel :: (Colecionavel, Posicao) -> Imagens -> Picture
desenhaColecionavel (Moeda, (x, y)) imgs 
  = Translate (realToFrac x) (-(realToFrac y)) $ getImagem MOEDA imgs
desenhaColecionavel (Martelo, (x, y)) imgs 
  = Translate (realToFrac x) (-(realToFrac y)) $ getImagem MARTELO imgs

-- DESENHA PRINCESA

desenhaPrincesa :: Mapa -> Imagens -> [Picture]
desenhaPrincesa (Mapa _ (x,y) _) imgs = [Translate (realToFrac x) (-(realToFrac y)) $ getImagem PRINCESA imgs] 

-- | Função Tempo

reageTempo :: Float -> Estado -> IO Estado
reageTempo t e | pausa e = return e 
               | otherwise = return $ e { jogo = movimenta 12 (realToFrac t) game, tempo = tempo e + t }
               where game = jogo e

-- | Função Reage
reageJogo :: Event -> Estado -> IO Estado
reageJogo (EventKey (SpecialKey KeyF5) Down _ _) e = 
    return $ e {pausa = not (pausa e) }  
reageJogo (EventKey (SpecialKey KeyF9) Down _ _) e = 
    return $ e {modo = Vitoria}
reageJogo (EventKey (SpecialKey KeyRight) Down _ _) e =
    return $ e { jogo = atualiza [Just AndarDireita] (Just AndarDireita) (jogo e) }
reageJogo (EventKey (SpecialKey KeyLeft) Down _ _) e =
    return $ e { jogo = atualiza [Just AndarEsquerda] (Just AndarEsquerda) (jogo e) }
reageJogo (EventKey (SpecialKey KeySpace) Down _ _) e =
    return $ e { jogo = atualiza [Just Saltar] (Just Saltar) (jogo e) }
reageJogo (EventKey (SpecialKey KeyRight) Up _ _) e =
    return $ e { jogo = atualiza [Just Parar] (Just Parar) (jogo e) }
reageJogo (EventKey (SpecialKey KeyLeft) Up _ _) e =
    return $ e { jogo = atualiza [Just Parar] (Just Parar) (jogo e) }
reageJogo evento estado = eventosNoMenu evento estado


eventosNoMenu :: Event -> Estado -> IO Estado
eventosNoMenu (EventKey (SpecialKey KeyEnter) Down _ _) (Estado {modo = MenuInicial Jogar, imagens = imgs , jogo = jogoFinal, tempo = t, pausa = p}) =
    return (Estado {modo = EmJogo, imagens = imgs, jogo = jogoFinal, tempo = 0.0 ,pausa = p})
eventosNoMenu (EventKey (SpecialKey KeyEnter) Down _ _) (Estado {modo = MenuInicial Sair}) = 
    exitFailure
eventosNoMenu (EventKey (SpecialKey KeyEsc) Down _ _) (Estado {modo = EmJogo, imagens = imgs , jogo = jogoFinal, tempo = t , pausa = p}) =
    return (Estado {modo = MenuInicial Sair, imagens = imgs , jogo = jogoFinal, tempo = t, pausa = p})
eventosNoMenu (EventKey (SpecialKey KeyDown) Down _ _) (Estado {modo = MenuInicial Jogar, imagens = imgs , jogo = jogoFinal, tempo = t, pausa =p}) =
    return (Estado {modo = MenuInicial Sair, imagens = imgs , jogo = jogoFinal, tempo = t, pausa = p})
eventosNoMenu (EventKey (SpecialKey KeyUp) Down _ _) (Estado {modo = MenuInicial Sair, imagens = imgs , jogo = jogoFinal, tempo = t, pausa= p}) =
    return (Estado {modo = MenuInicial Jogar, imagens = imgs , jogo = jogoFinal,tempo =  t, pausa = p})
eventosNoMenu (EventKey (SpecialKey KeyEsc) Down _ _) (Estado {modo = Vitoria, imagens = imgs , jogo = jogoFinal, tempo = t, pausa= p}) =
    return (Estado {modo = MenuInicial Sair, imagens = imgs , jogo = jogoFinal,tempo =  t, pausa = p})
eventosNoMenu (EventKey (SpecialKey KeyEnter) Down _ _) (Estado { modo = GameOver}) =
  exitFailure
eventosNoMenu _ e = return e

-- | Configuração da janela do jogo
janela :: Display
janela =
  InWindow
    "Donkey Kong" -- título da janela
    (800, 750) -- dimensão da janela
    (600,100) -- posição no ecrã

-- | Cor de Fundo do Jogo
corFundo :: Color
corFundo = greyN 1.0

-- | Frame Rate do jogo
frameRate :: Int
frameRate = 60

-- | Carrega as imagens do jogo
carregarImagens :: IO Imagens
carregarImagens = do
  mario <- loadBMP "src/mario.bmp"
  fantasma <- loadBMP "src/estrela.bmp"
  estrela <- loadBMP "src/princesa.bmp"
  plataforma <- loadBMP "src/aplataforma.bmp"
  alcapao <- loadBMP "src/aalcapao.bmp"
  martelo <- loadBMP "src/coleccionavel.bmp"
  moeda <- loadBMP "src/moeda.bmp"
  macaco <- loadBMP "src/amacaco.bmp"
  escada <- loadBMP "src/aescada.bmp"
  vazio <- loadBMP "src/vazio.bmp"
  let imgs = [(MARIO,scale 2 2  mario)
             ,(FANTASMA, scale 1.8 1.8 fantasma)
             ,(PRINCESA, scale 1.7 1.7 estrela)
             ,(PLATAFORMA,scale 0.58 0.58 plataforma)
             ,(ALCAPAO,scale 0.58 0.7 alcapao)
             ,(MARTELO,scale 2 2 martelo)
             ,(ESCADA, scale 0.53 0.6 escada)
             ,(MACACO,scale 0.8 0.8 macaco)
             ,(MOEDA,scale 2 2 moeda)
             ,(VAZIO,scale 0.9 1 vazio)]
  return imgs

-- | Função Play
main :: IO ()
main = do 
  imgs <- carregarImagens
  playIO janela 
         corFundo 
         frameRate 
         (Estado {jogo = jogoFinal, imagens = imgs, modo = MenuInicial Jogar,tempo = 0, pausa = False}) 
         desenha 
         reageJogo 
         reageTempo
