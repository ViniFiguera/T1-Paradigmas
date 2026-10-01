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
        celulaAtual = M.lookup (lin,col) tab --busca conteudo da celula que acabamos de preencher
    in case celulaAtual of --se for um número checamos as 3 regras
        Just (Valor num) -> numeroUnicoNaLinha tab lin num && -- não repete na linha
                            numeroUnicoNaColuna tab col num && -- não repete na coluna
                            inspecionarSomas tab nMaximo -- não invalida as dicas de soma
        _ -> False -- controle de erros
        
-- Verifica se o número não se repete na linha
numeroUnicoNaLinha :: Tabuleiro -> Int -> Int -> Bool
numeroUnicoNaLinha tab lin num =
    -- Filtra as casas do tabuleiro pra pegar SÓ as que estao na mesma linha ('l == lin') e tem o mesmo valor ('cel == Valor num')
    let naLinha = filter (\((l, _), cel) -> l == lin && cel == Valor num) (M.toList tab)
    -- Como acabamos de inserir esse numero ele tem que aparecer EXATAMENTE 1 vez -> se for mais, ele repetiu
    in length naLinha == 1
    
-- Verifica se o número não se repete na coluna
numeroUnicoNaColuna :: Tabuleiro -> Int -> Int -> Bool -- mesma ideida do unico na linha
numeroUnicoNaColuna tab col num =
    let naColuna = filter (\((_, c), cel) -> c == col && cel == Valor num) (M.toList tab)
    in length naColuna == 1
    
-- Vai atrás dos "BlocoSoma" e utiliza a função 'calcularSomaVizinhos' para validar
inspecionarSomas :: Tabuleiro -> Int -> Bool
inspecionarSomas tab nMaximo =
    -- Filtra o tabuleiro inteiro pra separar numa lista só os blocos que tem dica de soma
    let blocosComDica = filter ehBlocoSoma (M.toList tab)
    in validarTodosOsBlocos blocosComDica -- Passa essa lista pra funcao abaixo conferir um por um
  where
    -- auxiliar que reconhece o que é bloco de soma
    ehBlocoSoma ((_, BlocoSoma _)) = True
    ehBlocoSoma _ = False
    
    validarTodosOsBlocos [] = True -- recursiva para varrer a lista de blocos
    -- pega o primeiro bloco (blocoAtual), confere ele, E (&&) chama a funcao pro restante da lista (resto)
    validarTodosOsBlocos (blocoAtual:resto) = calcularSomaVizinhos tab nMaximo blocoAtual && validarTodosOsBlocos resto
    
-- Verifica se a soma matemática ao redor de um "BlocoSoma" está correta (Calculadora)
calcularSomaVizinhos :: Tabuleiro -> Int -> (Coordenada, Celula) -> Bool
calcularSomaVizinhos tab nMaximo (coord, BlocoSoma alvo) =
    let 
        vizinhos = pegarVizinhos coord tab -- Pega as casas que estao em volta do bloco
        preenchidos = [v | Valor v <- vizinhos] -- Extrai apenas os numeros das casas vizinhas que ja jogamos
        qtdVazios = length (filter (== Vazia) vizinhos) -- Conta quantas casas em volta do bloco ainda estao em branco
        somaAtual = sum preenchidos -- Soma matematica dos valores ja jogados
    in if qtdVazios == 0 -- Se nao tem mais espaco pra jogar, a soma bate exatamente com o alvo?
       then somaAtual == alvo 
       -- Se AINDA TEM espaco pra jogar, a gente faz uma previsao pra ver se ainda está no intervalo possível:
       -- 1. A soma atual + (pior cenario jogando numero 1) NAO pode ultrapassar o alvo
       -- 2. A soma atual + (melhor cenario jogando nMaximo) TEM que conseguir alcancar o alvo
       else somaAtual + (qtdVazios * 1) <= alvo && somaAtual + (qtdVazios * nMaximo) >= alvo
calcularSomaVizinhos _ _ _ = True -- Tratamento de seguranca: se chamar a funcao pra algo que nao é BlocoSoma, só ignora

-- Pega as 8 casas vizinhas de uma certa coordenada
pegarVizinhos :: Coordenada -> Tabuleiro -> [Celula]
pegarVizinhos (lin, col) tab =
    let coordsVizinhas = [(l, c) | l <- [lin-1 .. lin+1], c <- [col-1 .. col+1], (l, c) /= (lin, col)]
        -- pega arredores (linha -1 ate +1, coluna -1 ate +1) e ignora o centro (/= lin, col)
        celulas = map (\coord -> M.lookup coord tab) coordsVizinhas
        -- Remove as que cairam fora do mapa (acima da linha 0, retorna Nothing)
        celulasValidas = [cel | Just cel <- celulas]
    -- Pega essas casas vizinhas que existem e retorna apenas as que importam para jogar/somar (Tira os blocos pretos vizinhos)
    in filter ehBrancaOuVazia celulasValidas

--Impressão e Main

-- Converte cada casa numa string para exibição
formatarCelula :: Celula -> String
formatarCelula Vazia         = "   "
formatarCelula (Valor v)     = " " ++ show v ++ " "
formatarCelula (BlocoSoma s) = "[B" ++ show s ++ "]"

-- Imprime linha por linha 
imprimirTabuleiro :: Int -> Int -> Tabuleiro -> IO ()
imprimirTabuleiro linhas colunas tab = mapM_ putStrLn  -- Cada string é uma linha do tabuleiro
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
          ] -- estado inicial
    
    putStrLn "=== Resolvendo Summen ===\n"
    let solucoes = resolverSummen exemploSummen
    
    if null solucoes
        then putStrLn "Nenhuma solução encontrada."
        else do
            putStrLn "Solução Encontrada:"
            imprimirTabuleiro 8 8 (head solucoes) -- é o primeiro tabuleiro q deu certo
