# Cargamos librerías
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
#install.packages("tidytext")
library(wordcloud)
library(tidytext)


# Cargamos los ficheros de tweets de cada persona

tw1 <- read.csv("tweets_billgates.csv",stringsAsFactors = FALSE)
tw2 <- read.csv("tweets_elonmusk.csv",stringsAsFactors = FALSE)
tw3 <- read.csv("tweets_mayoredlee.csv", stringsAsFactors = FALSE)


# Unificamos en un único dataset los tres dataframes

tweets <- bind_rows(tw1, tw2,tw3)

# Cambio nombre de columnas
colnames(tweets)[colnames(tweets) == "screen_name"] <- "autor"
colnames(tweets)[colnames(tweets) == "created_at"] <- "fecha"

# Establecer formato fecha en la columna fecha

tweets$fecha <- as.Date(tweets$fecha)

# Cambio los nombres de los autores
tweets$autor[tweets$autor == "elonmusk"] <- "Elonmusk"
tweets$autor[tweets$autor == "mayoredlee"] <- "Mayoredlee"

# Número de tweets de cada persona
tweets %>% group_by(autor)%>%summarise(número_tweets = n())


# Distribución de los tweets de los autores a lo largo del tiempo 

ggplot(tweets, aes(x = fecha, fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "8 month") +
  labs(x = "Fecha de Publicación", y = "Número de tweets") +
  facet_wrap(~autor , ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))



# Tokenización de palabras: consite en dividir el texto en tokens  Un
# Un token es una unidad de texto, estas pueden ser palabras individuales, oraciones, párrafos, etc
# Utilizaremos la función unnest_tokens()

# tbl = Selección del data frame.
# output = La columna en la cual se van a crear los tokens.
# input = La columna de la cual va a extraer el texto.
# token = Definicion de la unidad de texto, algunas de estas pueden ser “words” 
# (palabras individuales), “sentences” (oraciones), “paragraphs” (párrafos) 
# o “ngrams” (secuencia de palabras consecutivas).


tw_token <- unnest_tokens(tbl=tweets, 
                          output = "word", 
                          input = "text", 
                          token = "words")


# Utilizamos la función anti_join consite en eliminar
# las palabras comunes del inglés como "the", "this", "what"
# utilizando el parametro stop_words 

tw_token <- anti_join(x=tw_token,
                          y=stop_words,
                          by="word")

# Creamos un vector con las palabras que queremos eliminar
eliminar <- c("t.co", "https", "http", "â", "amp", "iâ", "hereâ", "3")


tw_token <- tw_token%>%filter(!(word %in% eliminar))

# Nube de palabras para cada autor 

tw_freq <- tw_token %>% count(autor, word, sort=TRUE)

# Autor BillGates
tw_freq %>%
    filter(autor=="BillGates")%>%
    with(wordcloud(word, n, min.freq = 1,  
                   max.words=200, random.order=FALSE, rot.per=0.45, colors=brewer.pal(8, "Dark2")))
# Autor Mayoredlee
tw_freq %>%
  filter(autor=="Mayoredlee")%>%
  with(wordcloud(word, n, min.freq = 1,  
                 max.words=200, random.order=FALSE, rot.per=0.45, ))
# Autor Elonmusk
tw_freq %>%
  filter(autor=="Elonmusk")%>%
  with(wordcloud(word, n, min.freq = 1,  
                 max.words=200, random.order=FALSE))

# World cloud de los tres autores diferenciado por color
wordcloud(words = tw_freq$word, freq = tw_freq$n, min.freq = 1,  
          max.words=200, random.order=FALSE, rot.per=0.45,  
          colors=brewer.pal(8, "Dark2"))

# Distribución de frecuencias de palabras mas usuales de cada autor

tw_token %>% group_by(autor, word) %>% count(word) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x=reorder(word, n), y = n, fill = autor))+
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor, scales="free", ncol=1, drop=TRUE)


# Realizar la correlación entre los tres autores

# Función spread se usada cuando tenemosuna observación dispersa en múltiples filas.

tw_spread <- tw_token %>% group_by(autor, word) %>% count(word) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)

tw_spread 

# Correlación entre Mayoredlee y Elonmusk

cor.test(~ Mayoredlee + Elonmusk, method = "pearson", data = tw_spread)

# Correlación entre Elonmusk y BillGates

cor.test(~tw_spread$BillGates + tw_spread$Elonmusk, method = "pearson")

# Correlación entre BillGates y Mayoredlee

cor.test(~tw_spread$BillGates + tw_spread$Mayoredlee, method = "pearson")

# Análisis de las palabras utilizando tidytext

# El paquete tidytext contiene 3 diccionarios distintos:

# AFINN: asigna a cada palabra un valor entre -5 y 5. 
# Siendo -5 el máximo de negatividad y +5 el máximo de positividad.

# bing: clasifica las palabras de forma binaria positivo/negativo.

# nrc: clasifica cada palabra en uno o más de los siguientes sentimientos: positive, negative,
# anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.

analysis_word <- tw_freq %>%
                            inner_join(get_sentiments("bing"), by = c("word"))


analysis_word%>%count(autor, sentiment)%>%spread(key = sentiment, value = n, fill = 0)

# Palabras discriminatorias de Elonmusk
analysis_word%>%filter(autor=="Elonmusk" & sentiment =="negative")%>%head(20)

# Palabras discriminatorias de BillGates
analysis_word%>%filter(autor=="BillGates" & sentiment =="negative")%>%head(20)

# Palabras discriminatorias de Mayoredlee
analysis_word%>%filter(autor=="Mayoredlee" & sentiment =="negative")%>%head(20)





