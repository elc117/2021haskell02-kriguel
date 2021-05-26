-- Prática 02 de Haskell
-- Nome: Leonardo Cargnin Krugel

-- 1) Separa pessoas que estao com febre em uma lista de temperaturas
febre :: Float -> Bool
febre temp = temp > 37.8

comFebre :: [Float] -> [Float]
comFebre lista = filter febre lista


-- 2) Funcao anterior so que usando lambda
comFebre' :: [Float] -> [Float]
comFebre' lista = filter (\temp -> temp>37.8) lista


-- 3) Adiciona tags HTML a uma lista de strings
itemize :: [String] -> [String]
itemize lista = map (\str -> "<li>" ++ str ++ "</li>") lista


-- 4) Retorna uma lista com raios de cirulo cujo a area e maior que o numero recebido
bigCircles :: Float -> [Float] -> [Float]
bigCircles area lista = filter (\r -> (pi*r^2) >  area) lista


-- 5) Recebe uma lista de tuplas com nomes e temperatura e retorna uma lista de quem tem febre
quarentena :: [(String,Float)] -> [(String,Float)]
quarentena pessoas = filter (\p -> febre (snd p)) pessoas


-- 6) Calcula a idade em um ano especifico com base nos anos de nascimento recebidos
idadesEm :: [Int] -> Int -> [Int]
idadesEm lista ano = map (\n -> ano-n) lista


-- 7) Altera um lista de nomes para colocar 'Super' na frente dos que iniciam com 'A'
doTheChange :: String -> String
doTheChange nome
    | (head nome) == 'A' = "Super " ++ nome
    | otherwise = nome


changeNames :: [String] -> [String]
changeNames nomes = map (\n -> doTheChange(n)) nomes


-- 8)  Retorna uma lista apenas strings com menos de 5 caracteres
onlyShorts :: [String] -> [String]
onlyShorts list = filter (\s -> length s < 5) list
