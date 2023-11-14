library(vegan)
library(tidyverse)
theme_set(theme_bw() + theme(legend.position = "bottom"))

env0 <- readxl::read_excel("data/Karabash_2023-10-30.xlsx", sheet = 2, skip = 1) %>% 
    filter(!is.na(Зона)) %>% 
    rename(id = `код пробной площади`)

df <- env0 %>% 
    select(id, `Sylvaemus uralensis`:`Mustela nivalis`)

env0 <- env0 %>% 
    select(
        -contains("откл"),
        -contains("ошибк"),
        -contains("балл"), 
        -contains("температур"),
        -Зона,
        -`высота над уровнем моря`, 
        -`объемная влажность горизонтов АО+А1, %`, 
        -`Увлажнение почв, баллы`
        ) %>% 
    select(id:`свинец в А1, среднее`)

# dis <- vegan::vegdist(df[,-1])

rda_result <- rda(df[,-1] ~ ., env0[,2:5])
ordiplot(rda_result, type = "n")
points(rda_result, display = "species", col = "blue")
text(rda_result, display = "species", col = "red", cex = 0.7)



ordiplot(rda_result, type = "n")  # Создаем пустой график

# Визуализация важности переменных
orditorp(rda_result, display = "species", col = "blue", label = TRUE, cex = 0.7)
















