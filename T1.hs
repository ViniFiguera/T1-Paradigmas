module Main where

import qualified Data.Map as M
import Data.List (intercalate)

-- ==========================================
-- 0. Definições de Tipos
-- ==========================================

data Cell 
    = Empty 
    | Val Int 
    | Block 
    | BlockSum Int
    deriving (Eq, Show)

type Pos = (Int, Int)
type Board = M.Map Pos Cell

-- ==========================================
-- 1. Utilitários e Inicialização
-- ==========================================

isWhiteOrEmpty :: Cell -> Bool
isWhiteOrEmpty Empty   = True
isWhiteOrEmpty (Val _) = True
isWhiteOrEmpty _       = False

getN :: [[Cell]] -> Int
getN grid = maximum (map (\row -> length (filter isWhiteOrEmpty row)) grid)

toBoard :: [[Cell]] -> Board
toBoard rows = M.fromList
    [ ((r, c), cell)
    | (r, row) <- zip [0..] rows
    , (c, cell) <- zip [0..] row ]

-- ==========================================
-- 2. Motor de Busca Backtracking (Sem Monads)
-- ==========================================

solveKakkuru :: [[Cell]] -> [Board]
solveKakkuru grid = solve (toBoard grid) (getN grid)

solve :: Board -> Int -> [Board]
solve board n = go emptyPositions board
  where
    emptyPositions = [p | (p, Empty) <- M.toList board]

    go :: [Pos] -> Board -> [Board]
    go [] b = [b] 
    go (p:ps) b = 
        let 
            gerarTentativa = \val -> M.insert p (Val val) b
            todasTentativas = map gerarTentativa [1..n]
            
            tentativasValidas = filter (\b' -> isValid b' n p) todasTentativas
            
            continuarBusca = \bValido -> go ps bValido
            
        in concat (map continuarBusca tentativasValidas)

-- ==========================================
-- 3. Regras e Validações
-- ==========================================

isValid :: Board -> Int -> Pos -> Bool
isValid b n (r, c) =
    let 
        valCell = M.lookup (r,c) b
    in case valCell of
        Just (Val val) -> isUniqueInRow b r val && isUniqueInCol b c val && partialSumsOk b n
        _ -> False

isUniqueInRow :: Board -> Int -> Int -> Bool
isUniqueInRow b r val =
    let naLinha = filter (\((r', _), cell) -> r' == r && cell == Val val) (M.toList b)
    in length naLinha == 1

isUniqueInCol :: Board -> Int -> Int -> Bool
isUniqueInCol b c val =
    let naColuna = filter (\((_, c'), cell) -> c' == c && cell == Val val) (M.toList b)
    in length naColuna == 1

partialSumsOk :: Board -> Int -> Bool
partialSumsOk b n =
    let blocos = filter isBlockSum (M.toList b)
    in verificaTodos blocos
  where
    isBlockSum ((_, BlockSum _)) = True
    isBlockSum _ = False
    
    verificaTodos [] = True
    verificaTodos (bloco:resto) = checkSum b n bloco && verificaTodos resto

checkSum :: Board -> Int -> (Pos, Cell) -> Bool
checkSum b n (p, BlockSum target) =
    let 
        neigs = getNeighbors p b
        filled = [v | Val v <- neigs]
        empties = length (filter (== Empty) neigs)
        currentSum = sum filled
    in if empties == 0
       then currentSum == target 
       else currentSum + (empties * 1) <= target && currentSum + (empties * n) >= target
checkSum _ _ _ = True

getNeighbors :: Pos -> Board -> [Cell]
getNeighbors (r, c) b =
    let posicoes = [(nr, nc) | nr <- [r-1 .. r+1], nc <- [c-1 .. c+1], (nr, nc) /= (r, c)]
        celulas = map (\pos -> M.lookup pos b) posicoes
        celulasValidas = [celula | Just celula <- celulas]
    in filter isWhiteOrEmpty celulasValidas

-- ==========================================
-- 4. Funções de Impressão e Main
-- ==========================================

formatCell :: Cell -> String
formatCell Empty        = "   "
formatCell (Val v)      = " " ++ show v ++ " "
formatCell Block        = "[B]"
formatCell (BlockSum s) = "[B" ++ show s ++ "]"

printBoard :: Int -> Int -> Board -> IO ()
printBoard rows cols b = mapM_ putStrLn 
    [ intercalate " | " [ formatCell (b M.! (r, c)) | c <- [0..cols-1] ]
    | r <- [0..rows-1] ]

main :: IO ()
main = do
    let example = [
            [Empty, BlockSum 4, Empty, BlockSum 4],
            [BlockSum 4, Empty, BlockSum 6, Empty],
            [Empty, BlockSum 6, BlockSum 7, Empty],
            [BlockSum 4, Empty, Empty, BlockSum 2]
          ]
    
    putStrLn "=== Resolvendo Kakkuru (Haskell Clássico) ===\n"
    let solutions = solveKakkuru example
    
    if null solutions
        then putStrLn "❌ Nenhuma solução encontrada."
        else do
            putStrLn "✅ Solução Encontrada:"
            printBoard 4 4 (head solutions)
