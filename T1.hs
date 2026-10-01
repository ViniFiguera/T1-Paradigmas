module Main where

import qualified Data.Map as M
import Data.List (intercalate)

data Celula 
    = Vazia          -- casa vazia para colocar um numero
    | Valor Int      -- casa preenchida com um numero
    | BlocoSoma Int  -- bloco preto com dica de soma
    deriving (Eq, Show) -- para poder usar operacoes de igualdade e pode converter pra texto

type Coordenada = (Int, Int) --criamos um novo tipo (linha,coluna)
type Tabuleiro = M.Map Coordenada Celula --tabuleiro é um dicionario com Coordenada e Celula (tipo de casa)

-- essa funcao verifica se a casa eh jogavel ou nao(recebe numero ou nao)
ehBrancaOuVazia :: Celula -> Bool --criamos uma funcao q recebe um tipo celula e retorna um bool
ehBrancaOuVazia Vazia     = True -- se é uma casa vazia, retorna true
ehBrancaOuVazia (Valor _) = True -- se for uma casa de numero(valor) retorna True
ehBrancaOuVazia _         = False -- se for qualquer outra coisa retorna False

-- funcao que verifica o numero maximo permitido (depende do numero de casa jogaveis)
descobrirNMaximo :: [[Celula]] -> Int --recebe lista de listas das casas e retorna int
descobrirNMaximo grade = length (filter ehBrancaOuVazia (head grade)) --filter devolve uma lista menor
--head devolve o primeiro item da lista grade (pega uma linha apenas) e length pega a quantidade de casa branca/vazia

criarTabuleiro :: [[Celula]] -> Tabuleiro -- pega a matriz de celulas e retorna tabuleiro
criarTabuleiro grade = M.fromList coordenadasFinais --retorna grade, e transforma coordenadas finais num dicionario
  where
    -- numera cada linha do tabuleiro tipo [(0, BlocoSoma 10, Vazia), (1, Vazia, Vazia), ...]
    linhasNumeradas = zip [0..] grade
    
    -- numera cada coluna e junta para fazer a coordenada da casa
    numerarCelulasDaLinha (numeroDaLinha, linha) = 
        [ ((numeroDaLinha, numeroDaColuna), celula) | (numeroDaColuna, celula) <- zip [0..] linha ]
        
    -- junta tudo numa lista só por concatenacao
    coordenadasFinais = concat (map numerarCelulasDaLinha linhasNumeradas)

--
resolverSummen :: [[Celula]] -> [Tabuleiro]
resolverSummen grade = tentarResolver (criarTabuleiro grade) (descobrirNMaximo grade) --tentar resolver recebe o retorno de duas funcoes, e guarda em grade o [Tabuleiro]

-- Motor de tentativa e erro (Backtracking)
tentarResolver :: Tabuleiro -> Int -> [Tabuleiro] --confuso, mas ele apenas pega tabuleiro e um int, e retorna numa lista de tabuleiros
tentarResolver tab nMaximo = preencherPosicoes posicoesVazias tab -- a funcao preencher posicoes recebe como parametro posicoes vazias e tab, retornando tab e nMaximo
  where
    --procura casa vazia e coloca numa lista pra resolver depois
    posicoesVazias = [coord | (coord, Vazia) <- M.toList tab] 
    
    --recebe tabuleiro e lista de coordenadas vazias, DEVOLVE LISTA DE TABULEIROS! (considerados possibilidades)
    preencherPosicoes :: [Coordenada] -> Tabuleiro -> [Tabuleiro]
    
    --se lista de vazias acabou, deu certo!
    preencherPosicoes [] tabAtual = [tabAtual] --verifica 
    preencherPosicoes (coordAtual:proximasCoords) tabAtual = 
        let 
            --gera jogadas inserindo numero de 1 ate o maximo na casa atual
            gerarTentativa = \num -> M.insert coordAtual (Valor num) tabAtual
            
            todasTentativas = map gerarTentativa [1..nMaximo]
            
            --filtra as jogadas invalidas, deixa apenas as que deram certo ate agora
            tentativasValidas = filter (\tabTeste -> ehJogadaValida tabTeste nMaximo coordAtual) todasTentativas
            
            continuarBusca = \tabValido -> preencherPosicoes proximasCoords tabValido
            
        --junta as tentativas que conseguiram chegar no fim
        in concat (map continuarBusca tentativasValidas)


-- fim luisa
-- ......................................................................
-- inicio vinicius
--Regras e Validações

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
