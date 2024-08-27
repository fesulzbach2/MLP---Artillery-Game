import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random (randomRIO)
import System.Exit (exitSuccess)

-- Definindo o estado do jogo
data Player = Player
  { position :: (Float, Float)  -- Posição do jogador
  , lives :: Int                -- Vidas do jogador
  , isActive :: Bool            -- Se o jogador está ativo (é a vez de atirar)
  , angle :: Float              -- Ângulo do disparo
  , force :: Float              -- Força do disparo
  , moveLeft :: Bool            -- Indica se a tecla de mover para a esquerda está pressionada
  , moveRight :: Bool           -- Indica se a tecla de mover para a direita está pressionada
  }

data GameState = GameState
  { player1 :: Player           -- Estado do jogador 1
  , player2 :: Player           -- Estado do jogador 2
  , projectilePos :: (Float, Float) -- Posição atual do projétil
  , projectileActive :: Bool    -- Se o projétil está em movimento
  , timeElapsed :: Float        -- Tempo decorrido desde o lançamento
  , gameOver :: Bool            -- Indica se o jogo acabou
  , winner :: Maybe String      -- Indica o vencedor quando o jogo acabar
  }

-- Configurações da janela
windowWidth, windowHeight :: Int
windowWidth = 1200
windowHeight = 800

initialLives :: Int
initialLives = 3

-- Estado inicial do jogo
initialState :: IO GameState
initialState = do
  p1Pos <- randomRIO (-600, -200) -- Restrição para o lado esquerdo
  p2Pos <- randomRIO (200, 600)   -- Restrição para o lado direito
  return GameState
    { player1 = Player (p1Pos, -250) initialLives True 45 100 False False
    , player2 = Player (p2Pos, -250) initialLives False 45 100 False False
    , projectilePos = (p1Pos, -250)
    , projectileActive = False
    , timeElapsed = 0
    , gameOver = False
    , winner = Nothing
    }

-- Função principal
main :: IO ()
main = do
  state <- initialState
  playIO window background 60 state render handleInput update

-- Configuração da janela e fundo
window :: Display
window = InWindow "Combate de Alvos" (windowWidth, windowHeight) (100, 100)

background :: Color
background = white

-- Renderizando o jogo
render :: GameState -> IO Picture
render game
  | gameOver game =
      let winColor = case winner game of
            Just "Player 1" -> red
            Just "Player 2" -> blue
            _ -> black
      in return $ translate (-450) 0 $ scale 0.5 0.5 $ color winColor $ text ("Game Over! " ++ maybe "" id (winner game) ++ " Wins!")
  | otherwise = return $ pictures
      [ drawGround
      , drawPlayer (player1 game) red
      , drawPlayer (player2 game) blue
      , if projectileActive game then drawProjectile (projectilePos game) else Blank
      , drawLives (player1 game) (-500, 200)
      , drawLives (player2 game) (300, 200)
      , drawForce (player1 game) (-500, 150)
      , drawForce (player2 game) (300, 150)
      , drawTurnIndicator game (-100, 300)  -- Indicador de vez
      ]

-- Desenha o texto indicando a força de cada jogador
drawForce :: Player -> (Float, Float) -> Picture
drawForce player (x, y) =
  translate x y $ scale 0.3 0.3 $ color black $ text ("Force: " ++ show (force player))

-- Desenha o indicador de vez na parte superior da tela
drawTurnIndicator :: GameState -> (Float, Float) -> Picture
drawTurnIndicator game (x, y) =
  let activePlayerColor = if isActive (player1 game) then red else blue
      activePlayerText = if isActive (player1 game) then "Player 1" else "Player 2"
  in translate x y $ scale 0.5 0.5 $ color activePlayerColor $ text activePlayerText

-- Desenha o chão cinza
drawGround :: Picture
drawGround = translate 0 (-350) $ color (greyN 0.5) $ rectangleSolid (fromIntegral windowWidth) 200

-- Desenha o jogador com uma base fixa (trapézio) e um canhão rotacionável (retângulo)
drawPlayer :: Player -> Color -> Picture
drawPlayer player playerColor =
  let (x, y) = position player
      cannonAngle = if x < 0 then -angle player else angle player - 180
      baseVertices = [(-30, -15), (30, -15), (20, 15), (-20, 15)] -- Vértices para desenhar um trapézio
      base = color playerColor $ polygon baseVertices -- Base do trapézio
      cannon = rotate cannonAngle $ translate 30 0 $ color playerColor $ rectangleSolid 40 10 -- Cano do canhão que rotaciona
  in translate x y $ pictures [base, cannon]

drawProjectile :: (Float, Float) -> Picture
drawProjectile (x, y) = translate x y $ color black $ circleSolid 10

drawLives :: Player -> (Float, Float) -> Picture
drawLives player (x, y) =
  translate x y $ scale 0.3 0.3 $ color black $ text ("Lives: " ++ show (lives player))

-- Tratando a entrada do usuário
handleInput :: Event -> GameState -> IO GameState
-- Quando o espaço é pressionado, o projétil é disparado
handleInput (EventKey (SpecialKey KeySpace) Down _ _) game
  | not (projectileActive game) = do
      let shooter = currentPlayer game
      return game { projectileActive = True, timeElapsed = 0, projectilePos = position shooter }

-- Quando as setas de movimento são pressionadas
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) game
  | isActive (currentPlayer game) && not (projectileActive game) = return $ setMoveLeft True game
handleInput (EventKey (SpecialKey KeyRight) Down _ _) game
  | isActive (currentPlayer game) && not (projectileActive game) = return $ setMoveRight True game

-- Quando as setas de movimento são liberadas
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) game
  | isActive (currentPlayer game) = return $ setMoveLeft False game
handleInput (EventKey (SpecialKey KeyRight) Up _ _) game
  | isActive (currentPlayer game) = return $ setMoveRight False game

-- Ajustando o ângulo
handleInput (EventKey (SpecialKey KeyUp) Down _ _) game
  | isActive (currentPlayer game) && not (projectileActive game) = return $ updatePlayerAngle (currentPlayer game) game (-5)
handleInput (EventKey (SpecialKey KeyDown) Down _ _) game
  | isActive (currentPlayer game) && not (projectileActive game) = return $ updatePlayerAngle (currentPlayer game) game 5

-- Ajustando a força
handleInput (EventKey (Char 'q') Down _ _) game
  | isActive (currentPlayer game) && not (projectileActive game) = return $ updatePlayerForce (currentPlayer game) game (-10)
handleInput (EventKey (Char 'e') Down _ _) game
  | isActive (currentPlayer game) && not (projectileActive game) = return $ updatePlayerForce (currentPlayer game) game 10

-- Finaliza o jogo
handleInput (EventKey (Char 'p') Down _ _) game = 
  exitSuccess >> return game

handleInput _ game = return game

-- Funções auxiliares para definir o movimento
setMoveLeft :: Bool -> GameState -> GameState
setMoveLeft status game =
  if isActive (player1 game)
    then game { player1 = (player1 game) { moveLeft = status } }
    else game { player2 = (player2 game) { moveLeft = status } }

setMoveRight :: Bool -> GameState -> GameState
setMoveRight status game =
  if isActive (player1 game)
    then game { player1 = (player1 game) { moveRight = status } }
    else game { player2 = (player2 game) { moveRight = status } }

-- Movimenta o jogador com base nas teclas pressionadas
movePlayer :: Float -> GameState -> GameState
movePlayer seconds game =
  let moveSpeed = 100 * seconds -- Velocidade de movimento
      game' = if moveLeft (player1 game) && isActive (player1 game)
                then updatePlayerPosition (player1 game) game (-moveSpeed)
                else game
      game'' = if moveRight (player1 game) && isActive (player1 game)
                 then updatePlayerPosition (player1 game) game' moveSpeed
                 else game'
      game''' = if moveLeft (player2 game) && isActive (player2 game)
                  then updatePlayerPosition (player2 game) game'' (-moveSpeed)
                  else game''
  in if moveRight (player2 game) && isActive (player2 game)
       then updatePlayerPosition (player2 game) game''' moveSpeed
       else game'''

-- Atualizando o estado do jogo
update :: Float -> GameState -> IO GameState
update seconds game
  | gameOver game = return game -- Não atualiza o jogo se ele já acabou
  | projectileActive game = do
      let newPos = calculateProjectilePosition game
      if checkCollision newPos (position (targetPlayer game)) || outOfBounds newPos
        then do
          updatedGame <- if checkCollision newPos (position (targetPlayer game))
                            then hitTarget game
                            else return game
          if lives (targetPlayer updatedGame) <= 0
            then return updatedGame { gameOver = True, winner = Just (if isActive (player1 game) then "Player 1" else "Player 2") }
            else do
              let finalGameState = switchTurn updatedGame
              return finalGameState { projectileActive = False, timeElapsed = 0 }
        else return game { projectilePos = newPos, timeElapsed = timeElapsed game + seconds }
  | otherwise = return $ movePlayer seconds game

-- Calcula a nova posição do projétil
calculateProjectilePosition :: GameState -> (Float, Float)
calculateProjectilePosition game =
  let t = timeElapsed game * 8 -- Multiplicador para aumentar a velocidade do projétil
      player = currentPlayer game
      v = force player -- Usando a força do jogador atual
      g = 10.0 -- Gravidade ajustada para um movimento mais suave
      a = if position player == position (player2 game)
            then pi - angle player * pi / 180 -- Jogador 2 dispara para a esquerda
            else angle player * pi / 180 -- Jogador 1 dispara para a direita
      (x0, y0) = position player
      newX = x0 + v * t * cos a
      newY = y0 + v * t * sin a - 0.5 * g * t * t -- Gravidade ajustada
  in (newX, newY)


-- Verifica se o projétil saiu dos limites da tela
outOfBounds :: (Float, Float) -> Bool
outOfBounds (x, y) = x < -fromIntegral windowWidth / 2 || x > fromIntegral windowWidth / 2
                  || y < -fromIntegral windowHeight / 2 || y > fromIntegral windowHeight / 2

-- Verifica se houve colisão do projétil com o alvo
checkCollision :: (Float, Float) -> (Float, Float) -> Bool
checkCollision (projX, projY) (targetX, targetY) =
  let targetSize = 40
      projectileRadius = 10
      targetHitbox = (targetX - targetSize / 2, targetY - targetSize / 2, targetX + targetSize / 2, targetY + targetSize / 2)
  in rectCircleCollision (projX, projY) projectileRadius targetHitbox

-- Função para verificar colisão entre um círculo (projétil) e um retângulo (alvo)
rectCircleCollision :: (Float, Float) -> Float -> (Float, Float, Float, Float) -> Bool
rectCircleCollision (cx, cy) radius (left, bottom, right, top) =
  let closestX = clamp cx left right
      closestY = clamp cy bottom top
      distanceX = cx - closestX
      distanceY = cy - closestY
  in (distanceX * distanceX + distanceY * distanceY) < (radius * radius)

-- Função auxiliar para limitar valores
clamp :: Float -> Float -> Float -> Float
clamp x minVal maxVal = max minVal (min x maxVal)

-- Atualiza o estado do jogo quando um jogador é atingido
hitTarget :: GameState -> IO GameState
hitTarget game = do
  let target = targetPlayer game
  let updatedTarget = target { lives = lives target - 1 }
  if isActive (player1 game)
    then return game { player2 = updatedTarget, projectileActive = False }
    else return game { player1 = updatedTarget, projectileActive = False }

-- Alterna a vez de cada jogador
switchTurn :: GameState -> GameState
switchTurn game =
  if isActive (player1 game)
    then game { player1 = (player1 game) { isActive = False }, player2 = (player2 game) { isActive = True }, projectilePos = position (player2 game) }
    else game { player1 = (player1 game) { isActive = True }, player2 = (player2 game) { isActive = False }, projectilePos = position (player1 game) }

-- Retorna o jogador atual (que está atirando)
currentPlayer :: GameState -> Player
currentPlayer game =
  if isActive (player1 game)
    then player1 game
    else player2 game

-- Retorna o jogador alvo (que está sendo atacado)
targetPlayer :: GameState -> Player
targetPlayer game =
  if isActive (player1 game)
    then player2 game
    else player1 game

-- Atualiza o ângulo do jogador ativo
updatePlayerAngle :: Player -> GameState -> Float -> GameState
updatePlayerAngle player game delta =
  let isPlayer1 = position player == position (player1 game)
      -- Para o player à esquerda, o ângulo aumenta com a tecla para cima (ângulo positivo)
      -- Para o player à direita, o ângulo também deve aumentar com a tecla para cima, mas em sentido inverso
      newAngle = if isPlayer1
                   then clamp (angle player + delta) 0 90 -- Player 1 (lado esquerdo)
                   else clamp (angle player - delta) 0 90 -- Player 2 (lado direito)
  in if isPlayer1
       then game { player1 = player { angle = newAngle } }
       else game { player2 = player { angle = newAngle } }




-- Atualiza a força do jogador ativo
updatePlayerForce :: Player -> GameState -> Float -> GameState
updatePlayerForce player game delta =
  let newForce = clamp (force player + delta) 50 200 -- Reduzindo a força máxima
  in if isActive (player1 game)
       then game { player1 = player { force = newForce } }
       else game { player2 = player { force = newForce } }

-- Atualiza a posição horizontal do jogador ativo
updatePlayerPosition :: Player -> GameState -> Float -> GameState
updatePlayerPosition player game delta =
  let (x, y) = position player
      screenLimit = fromIntegral windowWidth / 2
      margin = 0.2 * screenLimit -- Reduzindo o limite em 10% para cada jogador
      newX = if isActive (player1 game)
               then clamp (x + delta) (-screenLimit + 30) (-margin) -- Limite para o Player 1
               else clamp (x + delta) margin (screenLimit - 30) -- Limite para o Player 2
  in if isActive (player1 game)
       then game { player1 = player { position = (newX, y) } }
       else game { player2 = player { position = (newX, y) } }
