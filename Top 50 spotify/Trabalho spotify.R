library(readr)
library(gtsummary)
library(flextable)
library(dplyr)
library(stringr)

top_50_2023 <- read_csv("top_50_2023.csv")
top_50_2023$loudness <- NULL
top_50_2023$acousticness <- NULL
top_50_2023$instrumentalness <- NULL
top_50_2023$liveness <- NULL
top_50_2023$mode <- NULL
top_50_2023$time_signature <- NULL
top_50_2023$key <- as.factor(top_50_2023$key)
levels(top_50_2023$key)
levels(top_50_2023$key) <- c("Do", "Do#", "Re", "Re#", "Mi", "Fa", 
  "Fa#", "Sol", "Sol#", "La", "La#", "Si")
top_50_2023 <- as.data.frame(top_50_2023)


row.names(top_50_2023) <- top_50_2023$track_name
top_50_2023$track_name <- NULL

top_50_2023$duration_ms <- top_50_2023$duration_ms / 60000

colnames(top_50_2023)
colnames(top_50_2023) <- c(
  "artist_name", "is_explicit", "album_release_date",
  "genres", "danceability", "valence",
  "energy", "speechiness", "key",
  "tempo", "duration_MN", "popularity"
)

theme_gtsummary_journal("jama")

# Configurando o idioma e a formatação numérica para Português com theme_gtsummary_language
theme_gtsummary_language(
  language = "pt", # Define o idioma para Português
  decimal.mark = ",", # Define a vírgula como separador decimal
  big.mark = ".", # Define o ponto como separador de milhares
  iqr.sep = "-", # Define o hífen como separador para intervalos interquartis
  ci.sep = "-", # Define o hífen como separador para intervalos de confiança
  set_theme = TRUE
)
theme_gtsummary_journal("lancet")

colnames(top_50_2023)[1] <- "Nomes dos Artistas"
colnames(top_50_2023)
colnames(top_50_2023) <- c(
  "Nomes dos Artistas", "Explícito", "Data de Lançamento do Album", "Gênero",
  "Dançável", "Positividade", "Intensidade", "Quantidade de Palavras",
  "Tom", "Tempo(BPM)", "Minutagem", "Popularidade"
)

top_50_2023$Artistas <- top_50_2023$`Nomes dos Artistas`

top_50_2023 <- top_50_2023 %>% mutate(Artistas = case_when(
  `Nomes dos Artistas` %in% c("Bad Bunny", "Bizarrap", 
  "d4vd", "SZA", "Taylor Swift", "The Weeknd") ~
    Artistas, TRUE ~ "Outros" # o default, para os gêneros não citados acima, é "outros"
))

# ordenando colunas
colnames(top_50_2023)
top_50_2023 <- top_50_2023[, c(13, 1:12)]
# Ver a ordem das linhas
levels(top_50_2023$Artistas)
# Transformando em fator
top_50_2023$Artistas <- as.factor(top_50_2023$Artistas)
# Mudar a ordem.
top_50_2023$Artistas <- factor(top_50_2023$Artistas, levels = c("Bad Bunny", "Bizarrap", "d4vd", "SZA", "Taylor Swift", "The Weeknd", "Outros"))

#resumindo e organizando os generos
categorize_genres <- function(Gênero) {
  if (grepl("pop|dance pop|chill pop|bedroom pop|k-pop", Gênero, ignore.case = TRUE)) {
    return("Pop")
  } else if (grepl("hip hop|rap|trap|detroit hip hop", Gênero, ignore.case = TRUE)) {
    return("Hip Hop/Rap")
  } else if (grepl("reggaeton|latino|corrido|sierreno", Gênero, ignore.case = TRUE)) {
    return("Reggaeton/Latino")
  } else if (grepl("rock|indie", Gênero, ignore.case = TRUE)) {
    return("Rock/Indie")
  } else if (grepl("r&b|soul", Gênero, ignore.case = TRUE)) {
    return("R&B/Soul")
  } else {
    return("Outros")  # Caso haja algum gênero que não se encaixe nos cinco principais
  }
}

top_50_2023$genre_simplified <- sapply(top_50_2023$Gênero, categorize_genres)


top_50_2023$Gênero <- NULL
#top_50_2023$`Data de Lançamento do Album` <- NULL 
colnames(top_50_2023)[13] <- "Gêneros"
top_50_2023 <- top_50_2023 [,c(1:3, 13, 4:12)]


top_50_2023$Gêneros <-
  factor(
   top_50_2023$Gêneros,
    levels = c("Rock/Indie",
               "Reggaeton/Latino",
               "Hip Hop/Rap", "Pop", "Outros")
  )

# Cria a coluna "ano" a partir da coluna de data e remove a coluna original
top_50_2023 <- top_50_2023 %>%
  mutate(`Ano de Lançamento do Album` = as.factor(format(as.Date
  (`Data de Lançamento do Album`, format = "%Y-%m-%d"), "%Y"))) %>%
  select(-`Data de Lançamento do Album`) # Remove a coluna original


tbl_summary(top_50_2023[, -2]) %>%
  bold_labels() %>%
  italicize_levels() %>% 
   as_flex_table() %>% 
   save_as_docx(path = "Tabela3.docx")

View(top_50_2023)
