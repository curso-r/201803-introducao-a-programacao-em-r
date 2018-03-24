# ------------------------------------------------------------------------------
# Author: Curso-R
# Atividade: Soluções da aula de transformação de dados
# ------------------------------------------------------------------------------

## Exercício 1: Setup 
# 1.1. Abra o arquivo `manip-script.Rmd`, dentro da pasta `aula-manip/`
# 1.2. Encontre a linha onde está esse exercíicio.
# 1.3. Coloque o cursor abaixo do 1.5.
# 1.4. Digite `Ctrl+Alt+I`. Isso abrirá um *chunk*. 
# - É dentro do chunk que você roda códigos R.
# - NÀO ESCREVA CÓDIGOS R FORA DOS CHUNKS
# 1.5. Rode `library(tidyverse)`

library(tidyverse)
imdb <- read_rds("aula-manip/data/dados_imdb.rds")

## Exercício 2

# 2.1. Qual a diferença entre uma `matrix` e um `data.frame`?

cat(
"  Matrizes são homogêneas e data.frames são heterogêneos. Ou seja, todos os
  elementos de uma matriz devem ter a mesma class(), enquanto cada coluna de 
  um data.frame pode ter uma class() diferente das demais.
  
  Em análises de dados,
  utilizamos vários tipos de variáveis (categóricas, numéricas etc), portanto
  é mais razoável considerar data.frames, que lidam naturalmente com vários
  tipos de colunas.
  "
)

glimpse(imdb)

## Exercício 3:
  
# 3.1. Quais são os 5 melhores filmes segundo o `imdb_score`?
imdb %>% 
  arrange(desc(imdb_score)) %>% 
  head(5)

# 3.2. Quais são os 3 filmes em preto e branco mais recentes?
# - Dica: use `title_year` e `color` 

imdb %>% 
  arrange(color, desc(title_year)) %>% 
  head(3)

imdb %>% 
  filter(color %in% "Black and White") %>%
  arrange(desc(title_year)) %>% 
  head(3)


## Exercício 4:

# 4.1. Selecione todas as colunas de atores.

imdb %>% 
  select(starts_with("actor"))

# 4.2. Selecione todas as colunas que acabam com `_name`.

imdb %>% 
  select(ends_with("_name"))

# 4.3. Selecione todas as colunas com `title`.

imdb %>% 
  select(contains("title"))

# 4.4. Selecione todas as colunas menos `movie_facebook_likes`.

imdb %>% 
  select(-movie_facebook_likes)

# 4.5. Selecione todas as colunas entre `color` e `imdb_score`.

imdb %>% 
  select(color:imdb_score)

## Exercício 5:

# 5.1. Filtre todos os filmes dos Estados Unidos

imdb %>% 
  filter(country == "USA")

# 5.2. Filtre todos os filmes dos Estados Unidos ou do Reino unido
# - Dica: use `%in%`

imdb %>% 
  filter(country %in% c("USA", "UK"))

# 5.3. Filtre todos os filmes com `imdb_score` acima de 9 e ordene por 
# `imdb_score` de forma crescente

imdb %>% 
  filter(imdb_score > 9) %>% 
  arrange(imdb_score)

# 5.4. Filtre todos os filmes dos Estados Unidos com `budget` 
# abaixo de 20.000 e ordene por `gross` de forma decrescente. 
# Selecione as colunas `movie_title`, `title_year`, `gross` e `budget`. 
# Verifique no Wikipédia o que raios ocorreu com o filme que ficou no topo.

imdb %>% 
  filter(country == "USA", budget < 20000) %>% 
  arrange(desc(gross)) %>% 
  select(movie_title, title_year, gross, budget)

## Exercício 6:

# 6.1. Qual é o filme menos rentável?

imdb %>% 
  mutate(rentabilidade = gross / budget) %>% 
  arrange(rentabilidade)

# 6.2. Nova métrica
# a. Retire filmes com rentabilidade acima de 100.
# b. Coloque a rentabilidade no intervalo [0,1]. 
# c. Crie um novo índice obtido pela multiplicação de `imdb_score` e 
# a `rentabilidade` padronizada.
# d. Qual é o melhor filme segundo esse índice?

imdb %>% 
  mutate(rentabilidade = gross / budget) %>% 
  filter(rentabilidade <= 100) %>% 
  mutate(rentabilidade = rentabilidade / max(rentabilidade),
         indice = imdb_score * rentabilidade) %>% 
  arrange(desc(rentabilidade))

## Exercício 7:
  
# 7.1. Quais diretores tem maior soma, média e mediana de `gross`?

imdb %>% 
  # problema do BD: gross está como INT
  mutate(gross = as.numeric(gross)) %>% 
  group_by(director_name) %>% 
  summarise(n = n(),
            soma = sum(gross, na.rm = TRUE),
            media = mean(gross, na.rm = TRUE),
            mediana = median(gross, na.rm = TRUE)) %>% 
  # apenas diretores com 5 filmes ou mais. O critério é arbitrário
  filter(n >= 5) %>% 
  # você pode ordenar por outras colunas
  arrange(desc(mediana))

# 7.2. Qual a combinação de gêneros mais comum?
# Dica: use `count()`

imdb %>% 
  count(genres, sort = TRUE)

## Exercício 8:

# 8.1. Dentre os diretores, qual o primeiro nome mais comum?

imdb %>% 
  separate(director_name, c("nome", "sobrenome"), sep = " ",
           extra = "drop", fill = "right") %>% 
  count(nome, sort = TRUE)

## Exercício 9:
  
# 9.1. Qual gênero é mais lucrativo?
# a. Separe os 7 primeiros gêneros
# b. Empilhe todos os gêneros
# c. Retire os `NA`s
# d. Obtenha quantidade, nota média e razão `gross` / `budget` para cada gênero
# e. Retire gêneros com menos de 100 filmes
# f. Ordene os gêneros pela razão de forma decrescente

imdb %>% 
  # problema do BD: gross está como INT
  mutate(gross = as.numeric(gross), budget = as.numeric(budget)) %>% 
  filter(!is.na(gross), !is.na(budget)) %>% 
  # separando generos
  separate(genres, letters[1:7], sep = "\\|",
           extra = "drop", fill = "right") %>% 
  # empilhando generos
  gather(nm_genero, genero, a:g) %>% 
  # retirando vazios
  filter(!is.na(genero)) %>% 
  # sumarizando, filtrando e ordenando.
  group_by(genero) %>% 
  summarise(n = n(),
            nota_media = mean(imdb_score, na.rm = TRUE),
            razao = sum(gross) / sum(budget)) %>% 
  filter(n > 100) %>% 
  arrange(desc(razao))

### Exercício 10
# 
# Use `janitor::get_dupes()` para averiguar os casos em que há repetição de combinações de colunas.
# 

library(janitor)
imdb %>% 
 get_dupes(movie_title)

# 
# Sim, tem duplicatas. E muitas! Talvez seja necessário realizar algumas análises novamente.

# 10.1. Dos estudos feitos anteriormente, 
# quais são afetados por esse problema na base?

cat(
  "Exercícios 3, 7, 8, 9"
)

# 10.2. Como você arrumaria a base usando a função `distinct()`?

imdb_distinct <- imdb %>% 
  distinct(movie_title, .keep_all = TRUE)

# 10.3. Refaça as análises afetadas com a base arrumada.

cat(
  "Rode os mesmos códigos com imdb_distinct no lugar de imdb."
)

# ------------------------------------------------------------------------------