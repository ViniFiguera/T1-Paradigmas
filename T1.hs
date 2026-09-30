module Main where

import qualified Data.Map as M
import Data.List (intercalate)

-- ==========================================
-- 0. Definições de Tipos
-- ==========================================

data Celula 
    = Vazia          -- casa vazia para colocar um numero
    | Valor Int      -- casa preenchida com um numero
    | BlocoPreto     -- bloco preto sem dica
    | BlocoSoma Int  -- bloco preto com dica de soma
    deriving (Eq, Show) 

type Coordenada = (Int, Int) 
type Tabuleiro = M.Map Coordenada Celula 

-- ==========================================
-- 1. Utilitários e Inicialização
-- ==========================================

-- Verifica se uma célula é válida para receber verificação (não é bloco preto)
ehBrancaOuVazia :: Celula -> Bool
ehBrancaOuVazia Vazia     = True
ehBrancaOuVazia (Valor _) = True
ehBrancaOuVazia _         = False

-- Descobre o limite máximo (N) de números permitidos
descobrirNMaximo :: [[Celula]] -> Int
descobrirNMaximo grade = maximum (map (\linha -> length (filter ehBrancaOuVazia linha)) grade)

-- Converte a matriz de listas para um Dicionário (Map) usando coordenadas
criarTabuleiro :: [[Celula]] -> Tabuleiro
criarTabuleiro linhas = M.fromList
    [ ((lin, col), celula)
    | (lin, linha) <- zip [0..] linhas
    , (col, celula) <- zip [0..] linha ]

-- ==========================================
-- 2. Motor de Busca Backtracking 
-- ==========================================

-- Função principal que prepara o tabuleiro para a busca
resolverSummen :: [[Celula]] -> [Tabuleiro]
resolverSummen grade = tentarResolver (criarTabuleiro grade) (descobrirNMaximo grade)

-- Motor de tentativa e erro (Backtracking)
tentarResolver :: Tabuleiro -> Int -> [Tabuleiro]
tentarResolver tab nMaximo = preencherPosicoes posicoesVazias tab
  where
    posicoesVazias = [coord | (coord, Vazia) <- M.toList tab]

    preencherPosicoes :: [Coordenada] -> Tabuleiro -> [Tabuleiro]
    preencherPosicoes [] tabAtual = [tabAtual] 
    preencherPosicoes (coordAtual:proximasCoords) tabAtual = 
        let 
            gerarTentativa = \num -> M.insert coordAtual (Valor num) tabAtual
            todasTentativas = map gerarTentativa [1..nMaximo]
            
            tentativasValidas = filter (\tabTeste -> ehJogadaValida tabTeste nMaximo coordAtual) todasTentativas
            
            continuarBusca = \tabValido -> preencherPosicoes proximasCoords tabValido
            
        in concat (map continuarBusca tentativasValidas)

-- ==========================================
-- 3. Regras e Validações
-- ==========================================

-- Verifica se a jogada atual respeita as regras do jogo
ehJogadaValida :: Tabuleiro -> Int -> Coordenada -> Bool
ehJogadaValida tab nMaximo (lin, col) =
    let 
        celulaAtual = M.lookup (lin,col) tab
    in case celulaAtual of
        Just (Valor num) -> numeroUnicoNaLinha tab lin num && 
                            numeroUnicoNaColuna tab col num && 
                            inspecionarSomas tab nMaximo
        _ -> False
        
-- Verifica se o número não se repete na linha
numeroUnicoNaLinha :: Tabuleiro -> Int -> Int -> Bool
numeroUnicoNaLinha tab lin num =
    let naLinha = filter (\((l, _), cel) -> l == lin && cel == Valor num) (M.toList tab)
    in length naLinha == 1
    
-- Verifica se o número não se repete na coluna
numeroUnicoNaColuna :: Tabuleiro -> Int -> Int -> Bool
numeroUnicoNaColuna tab col num =
    let naColuna = filter (\((_, c), cel) -> c == col && cel == Valor num) (M.toList tab)
    in length naColuna == 1
    
-- Vai atrás dos "BlocoSoma" e utiliza a função 'calcularSomaVizinhos' para validar
inspecionarSomas :: Tabuleiro -> Int -> Bool
inspecionarSomas tab nMaximo =
    let blocosComDica = filter ehBlocoSoma (M.toList tab)
    in validarTodosOsBlocos blocosComDica
  where
    ehBlocoSoma ((_, BlocoSoma _)) = True
    ehBlocoSoma _ = False
    
    validarTodosOsBlocos [] = True
    validarTodosOsBlocos (blocoAtual:resto) = calcularSomaVizinhos tab nMaximo blocoAtual && validarTodosOsBlocos resto
    
-- Verifica se a soma matemática ao redor de um "BlocoSoma" está correta (Calculadora)
calcularSomaVizinhos :: Tabuleiro -> Int -> (Coordenada, Celula) -> Bool
calcularSomaVizinhos tab nMaximo (coord, BlocoSoma alvo) =
    let 
        vizinhos = pegarVizinhos coord tab
        preenchidos = [v | Valor v <- vizinhos]
        qtdVazios = length (filter (== Vazia) vizinhos)
        somaAtual = sum preenchidos
    in if qtdVazios == 0
       then somaAtual == alvo 
       else somaAtual + (qtdVazios * 1) <= alvo && somaAtual + (qtdVazios * nMaximo) >= alvo
calcularSomaVizinhos _ _ _ = True

-- Pega as 8 casas vizinhas de uma certa coordenada
pegarVizinhos :: Coordenada -> Tabuleiro -> [Celula]
pegarVizinhos (lin, col) tab =
    let coordsVizinhas = [(l, c) | l <- [lin-1 .. lin+1], c <- [col-1 .. col+1], (l, c) /= (lin, col)]
        celulas = map (\coord -> M.lookup coord tab) coordsVizinhas
        celulasValidas = [cel | Just cel <- celulas]
    in filter ehBrancaOuVazia celulasValidas

-- ==========================================
-- 4. Funções de Impressão e Main
-- ==========================================

-- Converte cada casa numa string para exibição
formatarCelula :: Celula -> String
formatarCelula Vazia         = "   "
formatarCelula (Valor v)     = " " ++ show v ++ " "
formatarCelula BlocoPreto    = "[B]"
formatarCelula (BlocoSoma s) = "[B" ++ show s ++ "]"

-- Imprime linha por linha 
imprimirTabuleiro :: Int -> Int -> Tabuleiro -> IO ()
imprimirTabuleiro linhas colunas tab = mapM_ putStrLn 
    [ intercalate " | " [ formatarCelula (tab M.! (lin, col)) | col <- [0..colunas-1] ]
    | lin <- [0..linhas-1] ]

main :: IO ()
main = do
    let exemploSummen = [
            [Vazia, BlocoSoma 11, BlocoSoma 9, Vazia, BlocoSoma 14, Vazia, Vazia, Vazia],
            [Vazia, Vazia, Vazia, BlocoSoma 17, Vazia, Vazia, BlocoSoma 18, BlocoSoma 12],
            [Vazia, BlocoSoma 17, BlocoSoma 18, Vazia, Vazia, BlocoSoma 14, Vazia, Vazia],
            [BlocoSoma 12, Vazia, BlocoSoma 16, Vazia, BlocoSoma 16, Vazia, Vazia, Vazia],
            [Vazia, Vazia, Vazia, BlocoSoma 15, Vazia, BlocoSoma 16, Vazia, BlocoSoma 9],
            [BlocoSoma 12, Vazia, Vazia, Vazia, BlocoSoma 10, Vazia, BlocoSoma 18, Vazia],
            [Vazia, BlocoSoma 20, Vazia, BlocoSoma 20, Vazia, BlocoSoma 16, Vazia, Vazia],
            [BlocoSoma 6, Vazia, Vazia, Vazia, Vazia, Vazia, BlocoSoma 11, BlocoSoma 9]
          ]
    
    putStrLn "=== Resolvendo Summen ===\n"
    let solucoes = resolverSummen exemploSummen
    
    if null solucoes
        then putStrLn "Nenhuma solução encontrada."
        else do
            putStrLn "Solução Encontrada:"
            imprimirTabuleiro 8 8 (head solucoes)
