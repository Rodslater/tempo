## Preparação do histórico ## 
library(rinmet)
library(tidyverse)
library(lubridate)
library(rio)

##########
datas <- 2023:year(Sys.Date())
for(i in seq_along(datas)) {
  try({tempo <- get_base_inmet(datas[i]) #Try passa pra o próximo em caso de erro.
  f <- file(sprintf("tempo_%s.rds", datas[i]), encoding = "UTF-8")
  saveRDS(tempo, f)
  })
}

tempo <- import_list(dir(pattern = ".rds"), rbind = TRUE, encoding = "UTF-8")
tempo <- tempo[,c(1:3, 11, 10, 21, 22)]
colnames(tempo) <- c("data", "hora", "precipitacao_total", "temp_min", "temp_max", "uf", "estacao")


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
saveRDS(tempo, paste0("bases/tempo_", datas[1], "a", datas[length(datas)], ".rds"))

arquivos_rds <- dir(pattern = ".rds")
if (file.exists(arquivos_rds)) {
  file.remove(arquivos_rds)
}

##########




#Juntando bases

diretorio <- getwd()
novo_diretorio <- paste0(diretorio, "/bases/")
setwd(novo_diretorio)
#tempo <- import_list(dir(path = "/bases/", pattern = ".rds"), rbind = TRUE) #tá dando erro qd tenta acessar dentro da pasta. na raiz fica normal
tempo <- import_list(dir(pattern = ".rds"), rbind = TRUE)
tempo <- tempo %>% select(-`_file`)
setwd(diretorio)

saveRDS(tempo, paste0("tempo_", year(min(tempo$data)), "a", year(max(tempo$data)), ".rds"))
