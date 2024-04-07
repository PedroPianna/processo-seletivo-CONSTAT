library(ggplot2)
library(stringr)
library(dplyr)

df <- read.csv("eco101.csv", sep = ";", encoding="UTF-8")
summary(df)

# Histograma por km
df$km <- as.integer(df$km)
df_km_tratado <- df[df$km < 480, ]
ggplot(df_km_tratado, aes(km)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Número de Acidentes pelo KM da rodovia", y = "Acidentes") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("histograma_visao_geral.jpg", path = "./", width = 6, height = 4, device='tiff', dpi=700)

# Tabela com os km's mais perigosos
df_n_kms <- df_km_tratado %>% count(km)
df_n_kms <- df_n_kms[order(df_n_kms$n, decreasing = TRUE), ]

# Para cada km, ver a propocao de acidentes no sentido sul e norte
df_n_kms <- df_n_kms %>% mutate(prop_norte = 0, prop_sul = 0)
for (i in 1:nrow(df_n_kms)) {
  km <- df_n_kms$km[i]
  df_km <- df_km_tratado[df_km_tratado$km == km, ]
  n_norte <- nrow(df_km[df_km$sentido == "Norte", ])
  n_sul <- nrow(df_km[df_km$sentido == "Sul", ])
  df_n_kms$prop_norte[i] <- round(n_norte / (n_norte + n_sul),2)
  df_n_kms$prop_sul[i] <- round(n_sul / (n_norte + n_sul), 2)
}

# Remover valores NA
df_n_kms <- df_n_kms[complete.cases(df_n_kms), ]

# Montar tabela com o resumo dos acidentes
df_summary <- df_n_kms %>% summarise(Média = mean(n), Desvio_Padrão = sd(n), Primeiro_Percentil = quantile(n, 0.25), Mediana = median(n), , Terceiro_Percentil = quantile(n, 0.75), Maior_Valor = quantile(n, 1))
df_summary = df_summary %>%
  mutate(Header = c("Número de Acidentes"),
         .before = 1)
# Criar um plot com os 10 kilometros mais perigosos e com a proporcao do sentido que os acidentes ocorrem
df_n_kms_10 <- df_n_kms[1:10, ]
df_n_kms_10$km <- as.character(df_n_kms_10$km)
df_n_kms_10$km <- str_pad(df_n_kms_10$km, 3, pad = "0")
df_n_kms_10$km <- paste("km", df_n_kms_10$km)
df_n_kms_10$km <- factor(df_n_kms_10$km, levels = df_n_kms_10$km)
df_n_kms_10$prop_norte <- round(df_n_kms_10$prop_norte, 2)
df_n_kms_10$prop_sul <- round(df_n_kms_10$prop_sul, 2)
ggplot(df_n_kms_10, aes(km, n, fill = prop_sul)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red", name = "sentido Sul") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Número de ocorrências por km", y = "Ocorrências", )

ggsave("10kms_mais_perigosos.jpg", path = "./", width = 6, height = 4, device='tiff', dpi=700)

# Faremos o histograma por horario
df$hora <- str_extract(df$hora, "^[0-9]{2}")
df$hora <- as.integer(df$hora)

ggplot(df, aes(hora)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Número de Acidentes por Hora", y = "Acidentes") + 
  theme(plot.title = element_text(hjust = 0.5))
  
ggsave("histograma_horario.jpg", path = "./", width = 6, height = 4, device='tiff', dpi=700)

# Criar uma tabela com as horas mais perigosos
df_n_hora <- df %>% count(hora)
df_n_hora <- df_n_hora[order(df_n_hora$n, decreasing = TRUE), ]

# Para cada km, ver a propocao de acidentes letais e nao letais
df_n_hora <- df_n_hora %>% mutate(prop_letal = 0, prop_nao_letal = 0)
for (i in 1:nrow(df_n_hora)) {
  hora <- df_n_hora$hora[i]
  df_hora <- df[df$hora == hora, ]
  n_letal <- nrow(df_hora[df_hora$mortos > 0, ])
  n_nao_letal <- nrow(df_hora[df_hora$mortos == 0, ])
  df_n_hora$prop_letal[i] <- round(n_letal / (n_letal + n_nao_letal),2)
  df_n_hora$prop_nao_letal[i] <- round(n_nao_letal / (n_letal + n_nao_letal), 2)
}

df_n_hora_completo <- df %>% count(hora)
df_n_hora_completo <- df_n_hora_completo[order(df_n_hora_completo$n, decreasing = TRUE), ]
df_n_hora_completo <- df_n_hora_completo %>% mutate(prop_automovel = 0, prop_bicicleta = 0, prop_caminhao = 0, prop_moto = 0, prop_onibus = 0)
for (i in 1:nrow(df_n_hora_completo)) {
  hora <- df_n_hora_completo$hora[i]
  df_hora <- df[df$hora == hora, ]
  n_auto <- nrow(df_hora[df_hora$auto > 0, ])
  n_bicicleta <- nrow(df_hora[df_hora$bicicleta > 0, ])
  n_caminhao <- nrow(df_hora[df_hora$caminhao > 0, ])
  n_moto <- nrow(df_hora[df_hora$moto > 0, ])
  n_onibus <- nrow(df_hora[df_hora$onibus > 0, ])
  total <- n_auto + n_bicicleta + n_caminhao + n_moto + n_onibus
  df_n_hora_completo$prop_automovel[i] <- round(n_auto / total, 2)
  df_n_hora_completo$prop_bicicleta[i] <- round(n_bicicleta / total, 2)
  df_n_hora_completo$prop_caminhao[i] <- round(n_caminhao / total, 2)
  df_n_hora_completo$prop_moto[i] <- round(n_moto / total, 2)
  df_n_hora_completo$prop_onibus[i] <- round(n_onibus / total, 2)
}

# Remover valores NA
df_n_hora <- df_n_hora[complete.cases(df_n_hora), ]

# juntar as duas tabelas
df_n_hora_completo <- cbind(df_n_hora_completo, df_n_hora)
df_n_hora_completo <- subset(df_n_hora_completo, select=-c(n,hora))
df_n_hora_completo <- df_n_hora_completo[order(df_n_hora_completo$hora, decreasing = FALSE), ]
df_n_hora_completo <- df_n_hora_completo[, c(6,7,8,9,1,2,3,4,5)]
oldnames = c(colnames(df_n_hora_completo))
newnames = c("Hora do dia","Número de acidentes", "acidentes Letais", "acidentes não Letais", "automóveis","bicicletas", "caminhões", "Motocicletas", "Ônibus")
df_n_hora_completo <- df_n_hora_completo %>% rename_at(all_of(oldnames), ~ newnames)
write.csv(df_n_hora_completo, file="tabela_proporcoes_por_hora.csv", row.names = FALSE)

# plot com as 10 horas mais perigosas e com a proporcao de mortos

df_n_hora_10 <- df_n_hora
df_n_hora_10$hora <- as.character(df_n_hora_10$hora)
df_n_hora_10$hora <- str_pad(df_n_hora_10$hora, 2, pad = "0")
df_n_hora_10$hora <- paste(df_n_hora_10$hora, "horas")
df_n_hora_10$hora <- factor(df_n_hora_10$hora, levels = df_n_hora_10$hora)
df_n_hora_10$prop_letal <- round(df_n_hora_10$prop_letal, 2)
df_n_hora_10$prop_nao_letal <- round(df_n_hora_10$prop_nao_letal, 2)
ggplot(df_n_hora_10, aes(hora, n, fill = prop_letal)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red", name = "Letalidade") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Número de Acidentes por Hora", y = "acidentes", )

ggsave("acidentes_por_hora.jpg", path = "./", width = 6, height = 4, device='tiff', dpi=700)