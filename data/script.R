## Preparação do histórico ## 
library(rinmet)
library(tidyverse)
library(lubridate)
library(rio)

memory.limit(24576)

##########
datas <- 2023:year(Sys.Date())
for(i in seq_along(datas)) {
  try({tempo <- get_base_inmet(datas[i]) #Try passa pra o próximo em caso de erro.
  f <- file(sprintf("tempo_%s.csv", datas[i]), encoding = "UTF-8")
  write_csv(tempo, f)
  })
}

tempo <- import_list(dir(pattern = ".csv"), rbind = TRUE, encoding = "UTF-8")
tempo <- tempo[,c(1:3, 11, 10, 21, 22)]
colnames(tempo) <- c("data", "hora", "precipitacao_total", "temp_min", "temp_max", "uf", "estacao")

arquivos_csv <- dir(pattern = ".csv")
if (file.exists(arquivos_csv)) {
  file.remove(arquivos_csv)
}

tempo <- tempo %>% 
  filter(uf=="SE") %>% 
  select(-uf) %>% 
  mutate(data = ymd(data),
         temp_media = (temp_min+temp_max)/2) %>% 
  relocate(temp_media, .after = temp_min) %>% 
  group_by(data, estacao) %>% 
  summarise(across(precipitacao_total, sum), round(across(temp_min:temp_max, mean),2), .groups = "drop") %>% 
  arrange(estacao, data) %>% 
  mutate(estacao = str_to_title(estacao),
         estacao = case_when(estacao == 'Poco Verde' ~ 'Poço Verde',
                             estacao == 'Nossa Senhora Da Gloria' ~ 'Nossa Senhora da Glória',
                             TRUE ~ estacao))
saveRDS(tempo, 'data/tempo.rds')
