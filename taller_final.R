#De la linea 6 a la 198 lo referente al Quijote de la mancha
#De la linea 200 a la 394 lo referente a los Cuatro Jinetes del Apocalipsis
#De la linea 400 a la 594 lo referente a Harry potter 1
#De la linea 600 a la 771 lo referente a las graficas de las 15 palabras mas usadas de cada texto,
#y la correlación de los 3 libros
##Quijote de la mancha
## nrc
library(tidyverse)
library(tidytext)
library(syuzhet)
sentimientos <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())
source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")
texto_entrada <- read_lines("https://gist.githubusercontent.com/jsdario/6d6c69398cb0c73111e49f1218960f79/raw/8d4fc4548d437e2a7203a5aeeace5477f598827d/el_quijote.txt",
                            locale = default_locale())
texto_analizar1 <- tibble(texto = texto_entrada)
#tokenizar:
texto_analizar1 <- texto_analizar1 %>%
  unnest_tokens(palabra, texto) %>%
  mutate(pagina = (1:n()) %/% 400 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentimiento, pagina = pagina) %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(negativo = negativo*-1)
#representación gráfica
puntuacion <- texto_analizar1 %>%
  mutate(sentimiento = positivo+negativo) %>%
  select(pagina, sentimiento)

ggplot(data = puntuacion, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("El Quijote de la Mancha")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))

texto_trans1 <- get_dct_transform(puntuacion$sentimiento,
                                 low_pass_size = 10,
                                 scale_range = TRUE)

texto_trans1 <- tibble(pagina = seq_along(texto_trans1),
                      ft = texto_trans1)
ggplot(texto_trans1, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("El Quijote de la Mancha"))))

##afinn
# Filtrar el diccionario para obtener solo AFINN
afinn_es <- sentimientos %>% filter(lexicon == "AFINN")
# Convertir a tibble y tokenizar
texto_analizar2 <- tibble(texto = texto_entrada) %>%
  unnest_tokens(palabra, texto) %>%
  mutate(pagina = (1:n()) %/% 400 + 1) %>%
  inner_join(afinn_es, by = "palabra") %>%
  group_by(pagina) %>%
  summarise(sentimiento = sum(valor))

# Representación gráfica
ggplot(data = texto_analizar2, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("El Quijote de la Mancha")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))

texto_trans2 <- get_dct_transform(texto_analizar2$sentimiento,
                                 low_pass_size = 10,
                                 scale_range = TRUE)

texto_trans2 <- tibble(pagina = seq_along(texto_trans2),
                      ft = texto_trans2)

ggplot(texto_trans2, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("El Quijote de la Mancha"))))

##bing
# Filtrar el diccionario para obtener solo Bing
bing_es <- sentimientos %>% filter(lexicon == "bing")

# Convertir a tibble y tokenizar
texto_analizar3 <- tibble(texto = texto_entrada) %>%
  unnest_tokens(palabra, texto) %>%
  mutate(pagina = (1:n()) %/% 400 + 1) %>%
  inner_join(bing_es, by = "palabra") %>%
  count(sentimiento, pagina = pagina) %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(negativo = negativo * -1) %>%
  mutate(sentimiento = positivo + negativo)

# Representación gráfica
ggplot(data = texto_analizar3, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("El Quijote de la Mancha")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))

# Transformación con syuzhet
texto_trans3 <- get_dct_transform(texto_analizar3$sentimiento,
                                 low_pass_size = 10,
                                 scale_range = TRUE)

texto_trans3 <- tibble(pagina = seq_along(texto_trans3),
                      ft = texto_trans3)

ggplot(texto_trans3, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("El Quijote de la Mancha"))))


## Vista graficas unidas
p1_1 = ggplot(data = puntuacion, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("El Quijote de la Mancha (nrc)")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))


p2_1 = ggplot(data = texto_analizar2, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("El Quijote de la Mancha (afinn)")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))


p3_1 = ggplot(data = texto_analizar3, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("El Quijote de la Mancha (bing)")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))



# Mostrar las gráficas
library(gridExtra)
grid.arrange(p1_1, p2_1, p3_1, ncol = 1)


p1_2 = ggplot(texto_trans1, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("El Quijote de la Mancha (nrc)"))))

p2_2 = ggplot(texto_trans2, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("El Quijote de la Mancha (afinn)"))))

p3_2 = ggplot(texto_trans3, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("El Quijote de la Mancha (bing)"))))

grid.arrange(p1_2, p2_2, p3_2, ncol = 1)

##Cuatro jinetes del apocalipsis
## nrc
library(tidyverse)
library(tidytext)
library(syuzhet)
sentimientos <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())
source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")
texto_entrada <- read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/textos/cuatro_jinetes_apocalipsis.txt",
                            locale = default_locale())

texto_analizar1 <- tibble(texto = texto_entrada)
#tokenizar:
texto_analizar1 <- texto_analizar1 %>%
  unnest_tokens(palabra, texto) %>%
  mutate(pagina = (1:n()) %/% 400 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentimiento, pagina = pagina) %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(negativo = negativo*-1)

#representación gráfica
puntuacion <- texto_analizar1 %>%
  mutate(sentimiento = positivo+negativo) %>%
  select(pagina, sentimiento)

ggplot(data = puntuacion, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Cuatro jinetes del apocalipsis")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))

texto_trans1 <- get_dct_transform(puntuacion$sentimiento,
                                  low_pass_size = 10,
                                  scale_range = TRUE)

texto_trans1 <- tibble(pagina = seq_along(texto_trans1),
                       ft = texto_trans1)
ggplot(texto_trans1, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Cuatro jinetes del apocalipsis"))))

##afinn
# Filtrar el diccionario para obtener solo AFINN
afinn_es <- sentimientos %>% filter(lexicon == "AFINN")
# Convertir a tibble y tokenizar
texto_analizar2 <- tibble(texto = texto_entrada) %>%
  unnest_tokens(palabra, texto) %>%
  mutate(pagina = (1:n()) %/% 400 + 1) %>%
  inner_join(afinn_es, by = "palabra") %>%
  group_by(pagina) %>%
  summarise(sentimiento = sum(valor))

# Representación gráfica
ggplot(data = texto_analizar2, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Cuatro jinetes del apocalipsis")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))

texto_trans2 <- get_dct_transform(texto_analizar2$sentimiento,
                                  low_pass_size = 10,
                                  scale_range = TRUE)

texto_trans2 <- tibble(pagina = seq_along(texto_trans2),
                       ft = texto_trans2)

ggplot(texto_trans2, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Cuatro jinetes del apocalipsis"))))

##bing
# Filtrar el diccionario para obtener solo Bing
bing_es <- sentimientos %>% filter(lexicon == "bing")

# Convertir a tibble y tokenizar
texto_analizar3 <- tibble(texto = texto_entrada) %>%
  unnest_tokens(palabra, texto) %>%
  mutate(pagina = (1:n()) %/% 400 + 1) %>%
  inner_join(bing_es, by = "palabra") %>%
  count(sentimiento, pagina = pagina) %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(negativo = negativo * -1) %>%
  mutate(sentimiento = positivo + negativo)

# Representación gráfica
ggplot(data = texto_analizar3, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Cuatro jinetes del apocalipsis")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))

# Transformación con syuzhet
texto_trans3 <- get_dct_transform(texto_analizar3$sentimiento,
                                  low_pass_size = 10,
                                  scale_range = TRUE)

texto_trans3 <- tibble(pagina = seq_along(texto_trans3),
                       ft = texto_trans3)

ggplot(texto_trans3, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Cuatro jinetes del apocalipsis"))))


## Vista graficas unidas
p1_1 = ggplot(data = puntuacion, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Cuatro jinetes del apocalipsis (nrc)")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))


p2_1 = ggplot(data = texto_analizar2, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Cuatro jinetes del apocalipsis (afinn)")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))


p3_1 = ggplot(data = texto_analizar3, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Cuatro jinetes del apocalipsis (bing)")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))



# Mostrar las gráficas
library(gridExtra)
grid.arrange(p1_1, p2_1, p3_1, ncol = 1)


p1_2 = ggplot(texto_trans1, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Cuatro jinetes del apocalipsis (nrc)"))))

p2_2 = ggplot(texto_trans2, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Cuatro jinetes del apocalipsis (afinn)"))))

p3_2 = ggplot(texto_trans3, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Cuatro jinetes del apocalipsis (bing)"))))

grid.arrange(p1_2, p2_2, p3_2, ncol = 1)





## Harry potter 1
## nrc
library(tidyverse)
library(tidytext)
library(syuzhet)
sentimientos <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())
source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")
texto_entrada <- read_lines("https://raw.githubusercontent.com/busiris2014/7506Condor1C2014/14958ac2235e95907593c8209666e336fc2e32d7/datos2011/trunk/libros/J.K.%20Rowling%20-%20Harry%20Potter%201%20-%20La%20Piedra%20Filosofal.txt",
                            locale = default_locale())

texto_analizar1 <- tibble(texto = texto_entrada)
#tokenizar:
texto_analizar1 <- texto_analizar1 %>%
  unnest_tokens(palabra, texto) %>%
  mutate(pagina = (1:n()) %/% 400 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentimiento, pagina = pagina) %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(negativo = negativo*-1)

#representación gráfica
puntuacion <- texto_analizar1 %>%
  mutate(sentimiento = positivo+negativo) %>%
  select(pagina, sentimiento)

ggplot(data = puntuacion, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Harry potter 1")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))

texto_trans1 <- get_dct_transform(puntuacion$sentimiento,
                                  low_pass_size = 10,
                                  scale_range = TRUE)

texto_trans1 <- tibble(pagina = seq_along(texto_trans1),
                       ft = texto_trans1)
ggplot(texto_trans1, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Harry potter 1"))))

##afinn
# Filtrar el diccionario para obtener solo AFINN
afinn_es <- sentimientos %>% filter(lexicon == "AFINN")
# Convertir a tibble y tokenizar
texto_analizar2 <- tibble(texto = texto_entrada) %>%
  unnest_tokens(palabra, texto) %>%
  mutate(pagina = (1:n()) %/% 400 + 1) %>%
  inner_join(afinn_es, by = "palabra") %>%
  group_by(pagina) %>%
  summarise(sentimiento = sum(valor))

# Representación gráfica
ggplot(data = texto_analizar2, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Harry potter 1")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))

texto_trans2 <- get_dct_transform(texto_analizar2$sentimiento,
                                  low_pass_size = 10,
                                  scale_range = TRUE)

texto_trans2 <- tibble(pagina = seq_along(texto_trans2),
                       ft = texto_trans2)

ggplot(texto_trans2, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Harry potter 1"))))

##bing
# Filtrar el diccionario para obtener solo Bing
bing_es <- sentimientos %>% filter(lexicon == "bing")

# Convertir a tibble y tokenizar
texto_analizar3 <- tibble(texto = texto_entrada) %>%
  unnest_tokens(palabra, texto) %>%
  mutate(pagina = (1:n()) %/% 400 + 1) %>%
  inner_join(bing_es, by = "palabra") %>%
  count(sentimiento, pagina = pagina) %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(negativo = negativo * -1) %>%
  mutate(sentimiento = positivo + negativo)

# Representación gráfica
ggplot(data = texto_analizar3, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Harry potter 1")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))

# Transformación con syuzhet
texto_trans3 <- get_dct_transform(texto_analizar3$sentimiento,
                                  low_pass_size = 10,
                                  scale_range = TRUE)

texto_trans3 <- tibble(pagina = seq_along(texto_trans3),
                       ft = texto_trans3)

ggplot(texto_trans3, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Harry potter 1"))))


## Vista graficas unidas
p1_1 = ggplot(data = puntuacion, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Harry potter 1 (nrc)")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))


p2_1 = ggplot(data = texto_analizar2, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Harry potter 1 (afinn)")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))


p3_1 = ggplot(data = texto_analizar3, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Harry potter 1 (bing)")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))



# Mostrar las gráficas
library(gridExtra)
grid.arrange(p1_1, p2_1, p3_1, ncol = 1)


p1_2 = ggplot(texto_trans1, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Harry potter 1 (nrc)"))))

p2_2 = ggplot(texto_trans2, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Harry potter 1 (afinn)"))))

p3_2 = ggplot(texto_trans3, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Harry potter 1 (bing)"))))

grid.arrange(p1_2, p2_2, p3_2, ncol = 1)





library(tidyverse)
library(tidytext)

text_quijote = read_lines("https://gist.githubusercontent.com/jsdario/6d6c69398cb0c73111e49f1218960f79/raw/8d4fc4548d437e2a7203a5aeeace5477f598827d/el_quijote.txt",
                          locale = default_locale())

# Convertir a tibble
quijote_df = tibble(text = text_quijote)

# Tokenización y eliminación de stop words
tidy_quijote = quijote_df %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(language = "es")) %>% # eliminar stop words en español
  count(word, sort = TRUE)

# Seleccionar las 15 palabras más frecuentes
top_15_palabras <- tidy_quijote %>%
  slice_max(order_by = n, n = 15, with_ties = FALSE)

# Asegurarse de que las palabras se reordenen en función de su frecuencia
top_15_palabras <- top_15_palabras %>%
  mutate(word = reorder(word, n))

# Graficar las 15 palabras más frecuentes
ggplot(top_15_palabras, aes(x = word, y = n)) +
  geom_col(fill = "steelblue") +
  xlab(NULL) +
  ylab("Frecuencia") +
  ggtitle("15 palabras más frecuentes en El Quijote") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, hjust = 1))





text_jinetes = read_lines("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/textos/cuatro_jinetes_apocalipsis.txt",
                          locale = default_locale())

# Convertir a tibble
jinetes_df = tibble(text = text_jinetes)

# Tokenización y eliminación de stop words
tidy_jinetes = jinetes_df %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(language = "es")) %>% # eliminar stop words en español
  count(word, sort = TRUE)

# Seleccionar las 15 palabras más frecuentes
top_15_palabras <- tidy_jinetes %>%
  slice_max(order_by = n, n = 15, with_ties = FALSE)

# Asegurarse de que las palabras se reordenen en función de su frecuencia
top_15_palabras <- top_15_palabras %>%
  mutate(word = reorder(word, n))

# Graficar las 15 palabras más frecuentes
ggplot(top_15_palabras, aes(x = word, y = n)) +
  geom_col(fill = "steelblue") +
  xlab(NULL) +
  ylab("Frecuencia") +
  ggtitle("15 palabras más frecuentes en 4 Jinetes del Apocalipsis") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, hjust = 1))


text_hp = read_lines("https://raw.githubusercontent.com/busiris2014/7506Condor1C2014/14958ac2235e95907593c8209666e336fc2e32d7/datos2011/trunk/libros/J.K.%20Rowling%20-%20Harry%20Potter%201%20-%20La%20Piedra%20Filosofal.txt",
                     locale = default_locale())

# Convertir a tibble
hp_df = tibble(text = text_hp)

# Tokenización y eliminación de stop words
tidy_hp = hp_df %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(language = "es")) %>% # eliminar stop words en español
  count(word, sort = TRUE)

# Seleccionar las 15 palabras más frecuentes
top_15_palabras <- tidy_hp %>%
  slice_max(order_by = n, n = 15, with_ties = FALSE)

# Asegurarse de que las palabras se reordenen en función de su frecuencia
top_15_palabras <- top_15_palabras %>%
  mutate(word = reorder(word, n))

# Graficar las 15 palabras más frecuentes
ggplot(top_15_palabras, aes(x = word, y = n)) +
  geom_col(fill = "steelblue") +
  xlab(NULL) +
  ylab("Frecuencia") +
  ggtitle("15 palabras más frecuentes en Harry Potter") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, hjust = 1))
##Proporciones con respecto a los 4 Jinetes del Apocalipsis

library(scales)
library(tidyverse)

frequency2 <- bind_rows(
  mutate(tidy_quijote, author = "El Quijote"),
  mutate(tidy_hp, author = "Harry Potter"),
  mutate(tidy_jinetes, author = "Los 4 Jinetes del Apocalipsis")
) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `El Quijote`:`Harry Potter`)

# Gráfico de las proporciones
ggplot(frequency2, aes(x = proportion, y = `Los 4 Jinetes del Apocalipsis`,
                       color = abs(`Los 4 Jinetes del Apocalipsis` - proportion))) +
  geom_abline(color = "blue", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Los 4 Jinetes del Apocalipsis", x = NULL)




##Proporciones con respecto a El Quijote
frequency3 <- bind_rows(
  mutate(tidy_hp, author = "Harry Potter"),
  mutate(tidy_jinetes, author = "Los 4 Jinetes del Apocalipsis"),
  mutate(tidy_quijote, author = "El Quijote")
) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Harry Potter`:`Los 4 Jinetes del Apocalipsis`)

# Gráfico de las proporciones
ggplot(frequency3, aes(x = proportion, y = `El Quijote`,
                       color = abs(`El Quijote` - proportion))) +
  geom_abline(color = "blue", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "El Quijote", x = NULL)

# Pruebas de correlación
cor.test(data = frequency2[frequency2$author == "Harry Potter",],
         ~ proportion + `Los 4 Jinetes del Apocalipsis`)

cor.test(data = frequency2[frequency2$author == "El Quijote",],
         ~ proportion + `Los 4 Jinetes del Apocalipsis`)

cor.test(data = frequency3[frequency3$author == "Harry Potter",],
         ~ proportion + `El Quijote`)

