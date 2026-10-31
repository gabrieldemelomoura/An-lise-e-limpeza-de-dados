library(dplyr)
table <- read.csv("performace.csv")
View(table)



# gender 
table$gender <- tolower(
  trimws( # Tira espaços indevidos
    table$gender
  )
)

table$gender <- na_if( #Verifica onde tem espaços vazios
  table$gender, 
  ""
)

moda_gender <- names( #encontra a moda
  sort(
    table(
      table$gender
    ), 
    decreasing = TRUE
  )
)[1]

table$gender[
  is.na(table$gender)
] <- moda_gender


#race/ethnicity
table$race.ethnicity <- tolower(
  trimws(
    table$race.ethnicity
  )
)

table$race.ethnicity <- na_if(
  table$race.ethnicity, 
  ""
)

moda_race <- names(
  sort(
    table(
      table$race.ethnicity
    ), 
    decreasing = TRUE
  )
)[1]

table$race.ethnicity[
  is.na(table$race.ethnicity)
] <- moda_race


# parental level of education 
table$parental.level.of.education <- tolower(
  trimws(
    table$parental.level.of.education
  )
)

table$parental.level.of.education <- na_if(
  table$parental.level.of.education, 
  ""
)

moda_parental <- names(
  sort(
    table(
      table$parental.level.of.education
    ), 
    decreasing = TRUE
  )
)[1]

table$parental.level.of.education[
  is.na(table$parental.level.of.education)
] <- moda_parental


#lunch
table$lunch <- tolower(
  trimws(
    table$lunch
  )
)

table$lunch <- na_if(
  table$lunch, 
  ""
)

moda_lunch <- names(
  sort(
    table(
      table$lunch
    ), 
    decreasing = TRUE
  )
)[1]

table$lunch[
  is.na(table$lunch)
] <- moda_lunch


# test preparation course
table$test.preparation.course <- tolower(
  trimws(
    table$test.preparation.course
  )
)

table$test.preparation.course <- na_if(
  table$test.preparation.course, 
  ""
)

moda_prep <- names(
  sort(
    table(
      table$test.preparation.course
    ), 
    decreasing = TRUE
  )
)[1]

table$test.preparation.course[
  is.na(table$test.preparation.course)
] <- moda_prep

View(table)






table1 <- table %>% 
  select(gender,
         race.ethnicity,
         parental.level.of.education,
         math.score,
         reading.score,
         writing.score
        ) %>% head(10)
View(table1)




alunos_aprovados <- table %>% 
   filter(math.score >= 80,
        reading.score >= 80) %>%
  select(gender,
    race.ethnicity,
    math.score,
    reading.score,
    writing.score)
    
View(alunos_aprovados)






genero <- table %>% group_by(gender) %>%
summarize(
media_math = mean(math.score), #media em R
media_reading = mean(reading.score),
media_writing = mean(writing.score)
)

pie( #gráfico
  genero$media_math,
  labels = genero$gender,
  col = rainbow(length(genero$gender)),
  main = "Média de matemática"               
)
pie(
  genero$media_reading,
  labels = genero$gender,
  col = rainbow(length(genero$gender)),
  main = "Média de leitura"
)
pie(
  genero$media_writing,
  labels = genero$gender,
  col = rainbow(length(genero$gender)),
  main = "Média de escrita"
)
View(genero)







nivel_escolaridade <- table %>% distinct(parental.level.of.education)

View(nivel_escolaridade)



media.nivel_escolaridade <- table %>% group_by(parental.level.of.education) %>%
  summarise(
  media.math = round(mean(math.score),2), #round - quantidade de casas decimais
  media_reading = round(mean(reading.score),2),
  media_writing = round(mean(writing.score),2)
  )
View(media.nivel_escolaridade)






alunos.race.ethnicity <- table %>% distinct(sort(race.ethnicity))
View(alunos.race.ethnicity)





quant.p.categoria <- table %>% 
  group_by(race.ethnicity) %>%
  summarise(
    quantidade = n(),
    quantidade_porcentagem = round((n() / nrow(table) * 100), 2)
  )
rotulos <- paste(quant.p.categoria$race.ethnicity, paste0(quant.p.categoria$quantidade_porcentagem, "%")) #texto de categoria e sinal de %(paste0()"%")

pie(
  quant.p.categoria$quantidade_porcentagem,
  labels = rotulos,
  col = rainbow(length(quant.p.categoria$race.ethnicity)),
  main = "Gráfico de distribuição"
)

View(quant.p.categoria)




quant.p.categoria <- table %>% 
  group_by(race.ethnicity) %>%
  summarise(
    quantidade = n(),
    quantidade_porcentagem = round((n() / nrow(table) * 100), 2)
  )
rotulos <- paste(quant.p.categoria$race.ethnicity, paste0(quant.p.categoria$quantidade_porcentagem, "%")) #texto de categoria e sinal de %(paste0()"%")

pie(
  quant.p.categoria$quantidade_porcentagem,
  labels = rotulos,
  col = rainbow(length(quant.p.categoria$race.ethnicity)),
  main = "Gráfico de distribuição"
)

View(quant.p.categoria)