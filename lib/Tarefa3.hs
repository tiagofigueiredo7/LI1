{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Tiago Silva Figueiredo <a106856@alunos.uminho.pt>
              Beatriz Lima Simões <a104432@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where


import LI12324
import Tarefa1
import Tarefa2
import System.Random
import Data.List (group, sortBy)

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta  semente time jogo = jogo { --mapa =  novoMapa,
                                     colecionaveis = novosColecionaveis, 
                                     inimigos = novosInimigos1,
                                     jogador = novoPlayer}
    where --mapajogo = mapa jogo
          col = colecionaveis jogo
          player = jogador jogo
          ini = inimigos jogo
          --------------------------------------------
          novoMapa = alcapaoDesaparece mapajogo player
          novosColecionaveis = retiraCol col player
          novosInimigos1 = novosInimigos semente ini player
          novoPlayer = novoJogador player ini col mapajogo


          

-- | Ponto1 
hitboxDano :: Personagem -> Hitbox
hitboxDano p
    | direcao p == Este = ((x+l,y), (x+l+1,y-a))
    | direcao p == Oeste = ((x,y), (x-1,y-a))
    where (x,y) = posicao p 
          (l,a) = tamanho p

colisaoArmado :: Personagem -> Personagem -> Bool
colisaoArmado jogador inimigo 
    | col == True && temp > 0 = colisaoHitbox (hitboxDano jogador) (fazHitbox inimigo)
    | otherwise = False
    where (col,temp) = aplicaDano jogador

inimigoPerdeVida :: Personagem -> Personagem -> Int 
inimigoPerdeVida jogador inimigo
    | colisaoArmado jogador inimigo = x-1
    where x = vida inimigo

inimigoPerdeVidaLista :: Personagem -> [Personagem] -> [Int]
inimigoPerdeVidaLista jogador li 
    = map (\ini -> inimigoPerdeVida jogador ini) li

{-|
Funções que tiram uma vida aos inimigos quando colidem com a hitbox de dano
|-}
-- | Ponto 2 

vidaZero :: Personagem -> Bool
vidaZero inimigo = v == 0
    where v = vida inimigo

{-|
Função que nos diz se a vida de um inimigo é zero

Importante ressaltar que na parte gráfica, caso a vida seja zero o inimigo não deve ser representado
|-}

-- | Ponto 3 

posicoesVazio :: [[Bloco]] -> [Posicao]
posicoesVazio matriz =
    [(x, y) | (x, linha) <- zip [0..] matriz, (y, bloco) <- zip [0..] linha, bloco == Vazio]

colisaoVazio :: Posicao -> Personagem -> Bool
colisaoVazio pos jogador = colisaoHitbox (hitboxBloco pos) (fazHitbox jogador)

hitboxBloco :: Posicao -> Hitbox
hitboxBloco (x,y) = ((x,y),(x+1,y-1))


colisaoVazioMapa :: [Posicao] -> Personagem -> Bool
colisaoVazioMapa lp jogador
    = all (==True) (map (\ pos -> colisaoVazio pos jogador) lp)

personagemCai :: Mapa -> Personagem -> (Double,Double)
personagemCai (Mapa _ _ matriz) jogador 
    | colisaoVazioMapa pos jogador = (0,10)
    |otherwise = (x,y)
    where pos = posicoesVazio matriz
          (x,y) = velocidade jogador
{-|
Funções que alteram a velociadade do jogador para a gravidade quando pisa no Vazio
|-}

-- | Ponto 4 

jogadorPerdeVida :: Personagem -> [Personagem] -> Int
jogadorPerdeVida j [] = vida j
jogadorPerdeVida  jogador (i:ini) 
    | fst (aplicaDano jogador) == False && colisaoHitbox (fazHitbox jogador) (fazHitbox i) = x-1
    |otherwise = jogadorPerdeVida jogador ini
    where x = vida jogador

{-|
Função que tira uma vida ao jogador caso este colida com um inimigo
|-}

-- | Ponto 5

hitboxColecionavel :: Posicao -> Hitbox
hitboxColecionavel (x,y) = ((x,y),(x+1,y-1))

colisaoJogMoeda :: (Colecionavel,Posicao) -> Personagem -> Int
colisaoJogMoeda (Moeda,pos) jogador
    | colisaoHitbox (hitboxColecionavel pos) (fazHitbox jogador) =  x + 100
    | otherwise = x
    where x = pontos jogador
colisaoJogMoeda _ p = pontos p

colisaoJogMartelo :: (Colecionavel, Posicao) -> Personagem -> (Bool,Double)
colisaoJogMartelo (Martelo,pos) jogador
    | colisaoHitbox (hitboxColecionavel pos) (fazHitbox jogador) = (True,10)
    | otherwise = (v,n)
    where (v,n) = aplicaDano jogador

{-|
Função que aumenta os pontos do jogador quando colide com uma moeda
Função que muda o parâmetro aplicaDano do jogador quando colide com um martelo
|-}

colisaoJogCol' :: (Colecionavel,Posicao) -> Personagem -> Bool
colisaoJogCol' (Moeda,pos) jogador =
    colisaoHitbox (hitboxColecionavel pos) (fazHitbox jogador) 
colisaoJogCol' (Martelo,pos) jogador =
    colisaoHitbox (hitboxColecionavel pos) (fazHitbox jogador)


retiraCol :: [(Colecionavel,Posicao)] -> Personagem -> [(Colecionavel,Posicao)]
retiraCol [] _ = []
retiraCol col@((c,pos):t) jogador
    | colisaoJogCol' (c,pos) jogador = t
    | otherwise = col


{-|
Função que retira um colecionável da lista quando colide com o jogador
|-}

-- | Ponto 6

posicaoMapa :: [(Posicao, Bloco)] -> [[Bloco]]
posicaoMapa [] = []
posicaoMapa lista =
  let sortedList = sortBy (\((x1, y1), _) ((x2, y2), _) -> compare y1 y2 <> compare x1 x2) lista
      groupedByRow = group sortedList
      toBloco (_, bloco) = bloco
  in map (map toBloco) groupedByRow

jogadorTocaAlcapao :: Personagem -> (Posicao,Bloco) -> (Posicao,Bloco)
jogadorTocaAlcapao _ (pos,Plataforma) = (pos,Plataforma)
jogadorTocaAlcapao _ (pos,Escada) = (pos,Escada)
jogadorTocaAlcapao _ (pos, Vazio) = (pos,Vazio)
jogadorTocaAlcapao p (pos,Alcapao)
    | e == Jogador && colisaoHitbox (fazHitbox p) (hitboxBloco pos) = (pos,Vazio)
    | otherwise = (pos,Alcapao)
    where e = tipo p


jogadorTocaAlcapaoLista :: Personagem -> [(Posicao,Bloco)] -> [(Posicao,Bloco)]
jogadorTocaAlcapaoLista jogador matriz
    = map (\(pos) -> jogadorTocaAlcapao jogador pos) matriz

alcapaoDesaparece :: Mapa -> Personagem -> Mapa
alcapaoDesaparece m@(Mapa a b matriz) j = Mapa a b newmatriz
    where newmatriz = posicaoMapa (jogadorTocaAlcapaoLista j (matrizMapa m))

{-|
Funções que fazem os alçapões desaparecer caso o jogador pise neles
|-}

-- | Ponto 7

hitboxEstrela :: Mapa -> Hitbox
hitboxEstrela (Mapa _ (x,y) _) = ((x,y),(x+1,y-1))

{-|
Função que cria a hitbox da estrela
|-}

colisaoJogEst :: Mapa -> Personagem -> Bool
colisaoJogEst m p = 
    tipo p == Jogador && colisaoHitbox (hitboxEstrela m) (fazHitbox p)

{-|
Função que verifica se a estrela e o jogador estao em colisão

Caso esta função retorne True, o jogador vence
|-}


-- | Movimento aleatório dos Fantamas 

moveAleatorio :: Semente -> Personagem -> Posicao
moveAleatorio semente p@(Personagem {tipo = Fantasma,posicao = (x,y)})
    | maximum number `div` 2 == 0 = (x+0.2,y)
    | maximum number `div` 2 /= 0 = (x-0.2,y)
    where number = geraAleatorios semente 100
moveAleatorio _ (Personagem {tipo = _ ,posicao = (x,y)}) = (x,y)


{-|
Função que atualiza a velocidade dos fantasmas aleatóriamente
|-}

novoJogador :: Personagem -> [Personagem] -> [(Colecionavel,Posicao)] -> Mapa -> Personagem
novoJogador jogador ini ((c,pos):t) m = 
    jogador {vida = jogadorPerdeVida jogador ini, 
             velocidade = personagemCai m jogador,
             pontos = colisaoJogMoeda (c,pos) jogador,
             aplicaDano = colisaoJogMartelo (c,pos) jogador}
novoJogador j _ _ _  = j

novosInimigos :: Semente -> [Personagem] -> Personagem -> [Personagem]
novosInimigos _ [] _ = []
novosInimigos semente (i:is) jogador = 
    i { vida = inimigoPerdeVida i jogador, 
       posicao = moveAleatorio semente i} : novosInimigos semente is jogador











