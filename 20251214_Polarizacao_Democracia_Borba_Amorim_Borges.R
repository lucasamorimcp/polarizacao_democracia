
#Banco ReDem

library(haven)

INCT_ReDem <- read_sav("INCT_ReDem.sav")

#Recodificando variaveis de valores democraticos (retirando valores omissos)

library(dplyr)

INCT_ReDem <- INCT_ReDem %>%
  mutate(across(
    .cols = matches("^P(3[7-9]|4[0-9]|5[0-3])$"),
    .fns = ~ ifelse(.x %in% c(888, 999), NA, .x)
  ))

#Analise fatorial para identificar valores latentes

library(psych)

af_valores <- fa(
  INCT_ReDem[, paste0("P", 37:53)],
  nfactors = 2,
  rotate = "varimax"
)
print(af_valores)

kmo_result <- KMO(INCT_ReDem[, paste0("P", 37:53)])
print(kmo_result)
bartlett_result <- cortest.bartlett(INCT_ReDem[, paste0("P", 37:53)])
print(bartlett_result)

scores <- factor.scores(INCT_ReDem[, paste0("P", 37:53)], af_valores)
INCT_ReDem$af_valores_autoritario <- scores$scores[, 1]   
INCT_ReDem$af_valores_liberal <- scores$scores[, 2]

#Robustez PCA e TRI

#PCA

library(psych)

pca_golpe <- principal(
  INCT_ReDem[, paste0("P", 37:53)],
  nfactors = 2,
  rotate   = "varimax",
  scores   = TRUE
)

print(pca_golpe)

#TRI

library(mirt)

irt_golpe <- mirt(
  data  = INCT_ReDem[, paste0("P", 37:53)],
  model = 2,
  itemtype = "graded",
  rotation = "varimax"
)

summary(irt_golpe)

loadings <- coef(irt_golpe, simplify = TRUE)$items[, 1:2]
ss_loadings <- colSums(loadings^2)
ss_loadings

library(ggplot2)

p1 <- ggplot(INCT_ReDem, aes(x = af_valores_autoritario)) +
  geom_histogram(binwidth = 0.5, fill = "white", color = "black") +
  labs(
    x = "Tolerância a práticas governamentais de\ncaráter autoritário",
    y = "Frequência",
    title = ""
  ) +
  theme_bw()

p2 <- ggplot(INCT_ReDem, aes(x = af_valores_liberal)) +
  geom_histogram(binwidth = 0.5, fill = "white", color = "black") +
  labs(
    x = "Adesão normativa aos princípios da\ndemocracia liberal",
    y = "Frequência",
    title = ""
  ) +
  theme_bw()

library(gridExtra)
fig_hist <- grid.arrange(p2, p1, ncol = 2)

ggsave(
  filename = "histogramas_dimensoes_democracia.png",
  plot = fig_hist,
  width = 10,
  height = 4.5,
  units = "in",
  dpi = 300
)

#Matching

#Recodificando variavel de desigualdade

#Avaliando os extremos de renda como aqueles abaixo ou acima do desvio padrao da renda

attributes(INCT_ReDem$RENDA_1)
table(INCT_ReDem$RENDA_1)


INCT_ReDem$RENDA_1 <- ifelse(INCT_ReDem$RENDA_1 > 6,NA,INCT_ReDem$RENDA_1)
prop.table(table(INCT_ReDem$RENDA_1))

mean(INCT_ReDem$RENDA_1, na.rm = TRUE)
sd(INCT_ReDem$RENDA_1, na.rm = TRUE) #Alta renda como 1,2,3 e 4 e Baixa renda como 6

INCT_ReDem$rico <- ifelse(INCT_ReDem$RENDA_1 < 5,1,0)
INCT_ReDem$pobre <- ifelse(INCT_ReDem$RENDA_1 > 5,1,0)

media_renda <- mean(INCT_ReDem$RENDA_1, na.rm = TRUE)
sd_renda    <- sd(INCT_ReDem$RENDA_1, na.rm = TRUE)

#Gráfico
p_renda <- ggplot(INCT_ReDem, aes(x = RENDA_1)) +
  geom_histogram(
    binwidth = 1,
    fill = "white",
    color = "black",
    boundary = 0.5
  ) +
  geom_vline(
    xintercept = media_renda,
    linetype = "solid",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = c(media_renda - sd_renda, media_renda + sd_renda),
    linetype = "dotted",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = media_renda - 0.4,
    y = 650,
    label = "Média",
    vjust = 1.5,
    size = 3.5
  ) +
  annotate(
    "text",
    x = (media_renda + sd_renda) - 0.4,
    y = 650,
    label = "+1 DV",
    vjust = 1.5,
    size = 3.5
  ) +
  annotate(
    "text",
    x = (media_renda - sd_renda) - 0.4,
    y = 650,
    label = "−1 DV",
    vjust = 1.5,
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = 1:6,
    labels = c(
      "1\n(>20 SM)",
      "2\n(10–20 SM)",
      "3\n(5–10 SM)",
      "4\n(2–5 SM)",
      "5\n(1–2 SM)",
      "6\n(≤1 SM)"
    )
  ) +
  labs(
    x = "Faixa de renda pessoal (no mês passado)",
    y = "Frequência",
    title = ""
  ) +
  theme_bw()

p_renda

#Salvar imagem
ggsave(
  filename = "distribuicao_renda_RENDA_1.png",
  plot = p_renda,
  width = 8,
  height = 5,
  dpi = 300
)

INCT_ReDem_rico <- INCT_ReDem %>% 
  filter(!is.na(rico))

library(dplyr)
library(MatchIt)
library(ggplot2)
library(gridExtra)
library(grid)

#Rico

matching_rico <- matchit(rico ~ UF+PORTE+IDADE_EX+SEXO+RACA+ESCOLARIDADE+
                               ATIVIDADE_RAMO, 
                             data = INCT_ReDem_rico, 
                             method = "nearest", 
                             ratio = 1)
summary(matching_rico)

matching_data_rico <- match.data(matching_rico)

mod_rico_liberal <- lm(af_valores_liberal ~ rico, data = matching_data_rico)
summary(mod_rico_liberal)

matching_rico_autoritario <- matchit(rico ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE +
                                       ATIVIDADE_RAMO,
                                     data = INCT_ReDem_rico,
                                     method = "nearest",
                                     ratio = 1)

summary(matching_rico_autoritario)

matching_data_rico_autoritario <- match.data(matching_rico_autoritario)

mod_rico_autoritario <- lm(af_valores_autoritario ~ rico, data = matching_data_rico_autoritario)
summary(mod_rico_autoritario)

#Pobre

INCT_ReDem_pobre <- INCT_ReDem %>%
  filter(!is.na(pobre))

matching_pobre <- matchit(pobre ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE +
                            ATIVIDADE_RAMO,
                          data = INCT_ReDem_pobre,
                          method = "nearest",
                          ratio = 1)

summary(matching_pobre)

matching_data_pobre <- match.data(matching_pobre)

mod_pobre_liberal <- lm(af_valores_liberal ~ pobre, data = matching_data_pobre)
summary(mod_pobre_liberal)

matching_pobre_autoritario <- matchit(pobre ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE +
                                        ATIVIDADE_RAMO,
                                      data = INCT_ReDem_pobre,
                                      method = "nearest",
                                      ratio = 1)

summary(matching_pobre_autoritario)

matching_data_pobre_autoritario <- match.data(matching_pobre_autoritario)

mod_pobre_autoritario <- lm(af_valores_autoritario ~ pobre, data = matching_data_pobre_autoritario)
summary(mod_pobre_autoritario)

#Agora Polarizacao ideologica
#Proxy: Variavel aborto

attributes(INCT_ReDem$P130)
table(INCT_ReDem$P130)
INCT_ReDem$P130 <- ifelse(INCT_ReDem$P130 > 10,NA,INCT_ReDem$P130)
prop.table(table(INCT_ReDem$P130))

mean(INCT_ReDem$P130, na.rm = TRUE)
sd(INCT_ReDem$P130, na.rm = TRUE) #0 e 1 como conservador e 10 como liberal

INCT_ReDem$conservador <- ifelse(INCT_ReDem$P130 < 2,1,0)
INCT_ReDem$liberal <- ifelse(INCT_ReDem$P130 > 9,1,0)

media_p130 <- mean(INCT_ReDem$P130, na.rm = TRUE)
sd_p130    <- sd(INCT_ReDem$P130, na.rm = TRUE)

p <- ggplot(INCT_ReDem, aes(x = P130)) +
  geom_histogram(
    binwidth = 1,
    fill = "white",
    color = "black"
  ) +
  geom_vline(
    xintercept = media_p130,
    linetype = "solid",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = c(media_p130 - sd_p130, media_p130 + sd_p130),
    linetype = "dotted",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = media_p130 - 0.4,
    y = 400,
    label = "Média",
    vjust = 1.5,
    size = 3.5
  ) +
  annotate(
    "text",
    x = (media_p130 + sd_p130) - 0.4,
    y = 400,
    label = "+1 DV",
    vjust = 1.5,
    size = 3.5
  ) +
  annotate(
    "text",
    x = (media_p130 - sd_p130) - 0.4,
    y = 400,
    label = "−1 DV",
    vjust = 1.5,
    size = 3.5
  ) +
  labs(
    x = "A decisão sobre fazer ou não um aborto deve ser tomada\nexclusivamente pela mulher",
    y = "Frequência",
    title = ""
  ) +
  theme_bw()

p

ggsave(
  filename = "distribuicao_aborto_P130.png",
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)

#Conservador

INCT_ReDem_conservador <- INCT_ReDem %>%
  filter(!is.na(conservador))

matching_conservador_liberal <- matchit(conservador ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE +
                                          ATIVIDADE_RAMO,
                                        data = INCT_ReDem_conservador,
                                        method = "nearest",
                                        ratio = 1)

summary(matching_conservador_liberal)

matching_data_conservador_liberal <- match.data(matching_conservador_liberal)

mod_conservador_liberal <- lm(af_valores_liberal ~ conservador, 
                              data = matching_data_conservador_liberal)
summary(mod_conservador_liberal)

matching_conservador_autoritario <- matchit(conservador ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE +
                                              ATIVIDADE_RAMO,
                                            data = INCT_ReDem_conservador,
                                            method = "nearest",
                                            ratio = 1)

summary(matching_conservador_autoritario)

matching_data_conservador_autoritario <- match.data(matching_conservador_autoritario)

mod_conservador_autoritario <- lm(af_valores_autoritario ~ conservador, 
                                  data = matching_data_conservador_autoritario)
summary(mod_conservador_autoritario)

#Liberal

INCT_ReDem_liberal <- INCT_ReDem %>%
  filter(!is.na(liberal))

matching_liberal_liberal <- matchit(liberal ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE +
                                      ATIVIDADE_RAMO,
                                    data = INCT_ReDem_liberal,
                                    method = "nearest",
                                    ratio = 1)

summary(matching_liberal_liberal)

matching_data_liberal_liberal <- match.data(matching_liberal_liberal)

mod_liberal_liberal <- lm(af_valores_liberal ~ liberal, 
                          data = matching_data_liberal_liberal)
summary(mod_liberal_liberal)

matching_liberal_autoritario <- matchit(liberal ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE +
                                          ATIVIDADE_RAMO,
                                        data = INCT_ReDem_liberal,
                                        method = "nearest",
                                        ratio = 1)

summary(matching_liberal_autoritario)

matching_data_liberal_autoritario <- match.data(matching_liberal_autoritario)

mod_liberal_autoritario <- lm(af_valores_autoritario ~ liberal, 
                              data = matching_data_liberal_autoritario)
summary(mod_liberal_autoritario)

#Agora Polarizacao Afetiva
#Lulista

attributes(INCT_ReDem$P60)
table(INCT_ReDem$P60)
INCT_ReDem$P60 <- ifelse(INCT_ReDem$P60 > 10,NA,INCT_ReDem$P60)
prop.table(table(INCT_ReDem$P60))

mean(INCT_ReDem$P60, na.rm = TRUE)
sd(INCT_ReDem$P60, na.rm = TRUE) #0 e 1 nao gosta do Lula, 10 gosta

INCT_ReDem$lulista <- ifelse(INCT_ReDem$P60 > 9,1,0)
INCT_ReDem$antilulista <- ifelse(INCT_ReDem$P60 < 2,1,0)

media_p60 <- mean(INCT_ReDem$P60, na.rm = TRUE)
sd_p60    <- sd(INCT_ReDem$P60, na.rm = TRUE)

#Gráfico
p_lula <- ggplot(INCT_ReDem, aes(x = P60)) +
  geom_histogram(
    binwidth = 1,
    fill = "white",
    color = "black"
  ) +
  geom_vline(
    xintercept = media_p60,
    linetype = "solid",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = c(media_p60 - sd_p60, media_p60 + sd_p60),
    linetype = "dotted",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = media_p60 - 0.4,
    y = 450,
    label = "Média",
    vjust = 1.5,
    size = 3.5
  ) +
  annotate(
    "text",
    x = (media_p60 + sd_p60) - 0.4,
    y = 450,
    label = "+1 DV",
    vjust = 1.5,
    size = 3.5
  ) +
  annotate(
    "text",
    x = (media_p60 - sd_p60) - 0.4,
    y = 450,
    label = "−1 DV",
    vjust = 1.5,
    size = 3.5
  ) +
  labs(
    x = "Grau de identificação com Luiz Inácio Lula da Silva",
    y = "Frequência",
    title = ""
  ) +
  theme_bw()

p_lula

#Salvar imagem
ggsave(
  filename = "distribuicao_identificacao_lula_P60.png",
  plot = p_lula,
  width = 8,
  height = 5,
  dpi = 300
)

#Bolsonarista

attributes(INCT_ReDem$P61)
table(INCT_ReDem$P61)
INCT_ReDem$P61 <- ifelse(INCT_ReDem$P61 > 10,NA,INCT_ReDem$P61)
prop.table(table(INCT_ReDem$P61))

mean(INCT_ReDem$P61, na.rm = TRUE)
sd(INCT_ReDem$P61, na.rm = TRUE) #0 n?o gosta, 8,9 e 10 gosta do Bolsonaro

INCT_ReDem$bolsonarista <- ifelse(INCT_ReDem$P61 > 7,1,0)
INCT_ReDem$antibolsonarista <- ifelse(INCT_ReDem$P61 < 1,1,0)

media_p61 <- mean(INCT_ReDem$P61, na.rm = TRUE)
sd_p61    <- sd(INCT_ReDem$P61, na.rm = TRUE)

#Gráfico
p_bolsonaro <- ggplot(INCT_ReDem, aes(x = P61)) +
  geom_histogram(
    binwidth = 1,
    fill = "white",
    color = "black"
  ) +
  geom_vline(
    xintercept = media_p61,
    linetype = "solid",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = c(media_p61 - sd_p61, media_p61 + sd_p61),
    linetype = "dotted",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = media_p61 - 0.4,
    y = 650,
    label = "Média",
    vjust = 1.5,
    size = 3.5
  ) +
  annotate(
    "text",
    x = (media_p61 + sd_p61) - 0.4,
    y = 650,
    label = "+1 DV",
    vjust = 1.5,
    size = 3.5
  ) +
  annotate(
    "text",
    x = (media_p61 - sd_p61) - 0.4,
    y = 650,
    label = "−1 DV",
    vjust = 1.5,
    size = 3.5
  ) +
  labs(
    x = "Grau de identificação com Jair Messias Bolsonaro",
    y = "Frequência",
    title = ""
  ) +
  theme_bw()

p_bolsonaro

#Salvar imagem
ggsave(
  filename = "distribuicao_identificacao_bolsonaro_P61.png",
  plot = p_bolsonaro,
  width = 8,
  height = 5,
  dpi = 300
)

#Lulista_polarizado

INCT_ReDem$lulista_polarizado <- INCT_ReDem$lulista + INCT_ReDem$antibolsonarista
INCT_ReDem$lulista_polarizado <- ifelse(INCT_ReDem$lulista_polarizado == 2,1,0)

#Bolsonarista_polarizado

INCT_ReDem$bolsonarista_polarizado <- INCT_ReDem$bolsonarista + INCT_ReDem$antilulista
INCT_ReDem$bolsonarista_polarizado <- ifelse(INCT_ReDem$bolsonarista_polarizado == 2,1,0)

#Matching
#Lulista
INCT_ReDem_lulista_pol <- INCT_ReDem %>%
  filter(!is.na(lulista_polarizado))

matching_lulista_pol_liberal <- matchit(lulista_polarizado ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE +
                                          ATIVIDADE_RAMO,
                                        data = INCT_ReDem_lulista_pol,
                                        method = "nearest",
                                        ratio = 1)
summary(matching_lulista_pol_liberal)

matching_data_lulista_pol_liberal <- match.data(matching_lulista_pol_liberal)

mod_lulista_liberal <- lm(af_valores_liberal ~ lulista_polarizado,
                              data = matching_data_lulista_pol_liberal)
summary(mod_lulista_liberal)

matching_lulista_pol_autoritario <- matchit(lulista_polarizado ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE +
                                              ATIVIDADE_RAMO,
                                            data = INCT_ReDem_lulista_pol,
                                            method = "nearest",
                                            ratio = 1)
summary(matching_lulista_pol_autoritario)

matching_data_lulista_pol_autoritario <- match.data(matching_lulista_pol_autoritario)

mod_lulista_autoritario <- lm(af_valores_autoritario ~ lulista_polarizado,
                                  data = matching_data_lulista_pol_autoritario)
summary(mod_lulista_autoritario)

#Bolsonarista

INCT_ReDem_bolsonarista_pol <- INCT_ReDem %>%
  filter(!is.na(bolsonarista_polarizado))

matching_bolsonarista_pol_liberal <- matchit(bolsonarista_polarizado ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE +
                                               ATIVIDADE_RAMO,
                                             data = INCT_ReDem_bolsonarista_pol,
                                             method = "nearest",
                                             ratio = 1)
summary(matching_bolsonarista_pol_liberal)

matching_data_bolsonarista_pol_liberal <- match.data(matching_bolsonarista_pol_liberal)

mod_bolsonarista_liberal <- lm(af_valores_liberal ~ bolsonarista_polarizado,
                                   data = matching_data_bolsonarista_pol_liberal)
summary(mod_bolsonarista_liberal)

matching_bolsonarista_pol_autoritario <- matchit(bolsonarista_polarizado ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE +
                                                   ATIVIDADE_RAMO,
                                                 data = INCT_ReDem_bolsonarista_pol,
                                                 method = "nearest",
                                                 ratio = 1)
summary(matching_bolsonarista_pol_autoritario)

matching_data_bolsonarista_pol_autoritario <- match.data(matching_bolsonarista_pol_autoritario)

mod_bolsonarista_autoritario <- lm(af_valores_autoritario ~ bolsonarista_polarizado,
                                       data = matching_data_bolsonarista_pol_autoritario)
summary(mod_bolsonarista_autoritario)

#Cruzamentos

#A direita

INCT_ReDem$rico_conservador <- INCT_ReDem$rico + INCT_ReDem$conservador
INCT_ReDem$rico_conservador <- ifelse(INCT_ReDem$rico_conservador == 2,1,0)

INCT_ReDem$bolsonarista_conservador <- INCT_ReDem$bolsonarista_polarizado + INCT_ReDem$conservador
INCT_ReDem$bolsonarista_conservador <- ifelse(INCT_ReDem$bolsonarista_conservador == 2,1,0)

INCT_ReDem$rico_bolsonarista <- INCT_ReDem$rico + INCT_ReDem$bolsonarista_polarizado
INCT_ReDem$rico_bolsonarista <- ifelse(INCT_ReDem$rico_bolsonarista == 2,1,0)

INCT_ReDem$rico_conservador_bolsonarista <- INCT_ReDem$rico + INCT_ReDem$conservador + INCT_ReDem$bolsonarista_polarizado
INCT_ReDem$rico_conservador_bolsonarista <- ifelse(INCT_ReDem$rico_conservador_bolsonarista == 3,1,0)

#A esquerda

INCT_ReDem$pobre_liberal <- INCT_ReDem$pobre + INCT_ReDem$liberal
INCT_ReDem$pobre_liberal <- ifelse(INCT_ReDem$pobre_liberal == 2, 1, 0)

INCT_ReDem$lulista_liberal <- INCT_ReDem$lulista_polarizado + INCT_ReDem$liberal
INCT_ReDem$lulista_liberal <- ifelse(INCT_ReDem$lulista_liberal == 2, 1, 0)

INCT_ReDem$pobre_lulista <- INCT_ReDem$pobre + INCT_ReDem$lulista_polarizado
INCT_ReDem$pobre_lulista <- ifelse(INCT_ReDem$pobre_lulista == 2, 1, 0)

INCT_ReDem$pobre_liberal_lulista <- INCT_ReDem$pobre + INCT_ReDem$liberal + INCT_ReDem$lulista_polarizado
INCT_ReDem$pobre_liberal_lulista <- ifelse(INCT_ReDem$pobre_liberal_lulista == 3, 1, 0)


#rico_conservador
INCT_ReDem_rico_conservador <- INCT_ReDem %>%
  filter(!is.na(rico_conservador))

matching_rico_conservador <- matchit(
  rico_conservador ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE + ATIVIDADE_RAMO,
  data = INCT_ReDem_rico_conservador,
  method = "nearest", ratio = 1
)
summary(matching_rico_conservador)

matching_data_rico_conservador <- match.data(matching_rico_conservador)

mod_rico_conservador_liberal <- lm(af_valores_liberal ~ rico_conservador, data = matching_data_rico_conservador)
summary(mod_rico_conservador_liberal)

mod_rico_conservador_autoritario <- lm(af_valores_autoritario ~ rico_conservador, data = matching_data_rico_conservador)
summary(mod_rico_conservador_autoritario)


#bolsonarista_conservador
INCT_ReDem_bolsonarista_conservador <- INCT_ReDem %>%
  filter(!is.na(bolsonarista_conservador))

matching_bolsonarista_conservador <- matchit(
  bolsonarista_conservador ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE + ATIVIDADE_RAMO,
  data = INCT_ReDem_bolsonarista_conservador,
  method = "nearest", ratio = 1
)
summary(matching_bolsonarista_conservador)

matching_data_bolsonarista_conservador <- match.data(matching_bolsonarista_conservador)

mod_bolsonarista_conservador_liberal <- lm(af_valores_liberal ~ bolsonarista_conservador, data = matching_data_bolsonarista_conservador)
summary(mod_bolsonarista_conservador_liberal)

mod_bolsonarista_conservador_autoritario <- lm(af_valores_autoritario ~ bolsonarista_conservador, data = matching_data_bolsonarista_conservador)
summary(mod_bolsonarista_conservador_autoritario)


#rico_bolsonarista
INCT_ReDem_rico_bolsonarista <- INCT_ReDem %>%
  filter(!is.na(rico_bolsonarista))

matching_rico_bolsonarista <- matchit(
  rico_bolsonarista ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE + ATIVIDADE_RAMO,
  data = INCT_ReDem_rico_bolsonarista,
  method = "nearest", ratio = 1
)
summary(matching_rico_bolsonarista)

matching_data_rico_bolsonarista <- match.data(matching_rico_bolsonarista)

mod_rico_bolsonarista_liberal <- lm(af_valores_liberal ~ rico_bolsonarista, data = matching_data_rico_bolsonarista)
summary(mod_rico_bolsonarista_liberal)

mod_rico_bolsonarista_autoritario <- lm(af_valores_autoritario ~ rico_bolsonarista, data = matching_data_rico_bolsonarista)
summary(mod_rico_bolsonarista_autoritario)


#rico_conservador_bolsonarista
INCT_ReDem_rico_conservador_bolsonarista <- INCT_ReDem %>%
  filter(!is.na(rico_conservador_bolsonarista))

matching_rico_conservador_bolsonarista <- matchit(
  rico_conservador_bolsonarista ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE + ATIVIDADE_RAMO,
  data = INCT_ReDem_rico_conservador_bolsonarista,
  method = "nearest", ratio = 1
)
summary(matching_rico_conservador_bolsonarista)

matching_data_rico_conservador_bolsonarista <- match.data(matching_rico_conservador_bolsonarista)

mod_rico_conservador_bolsonarista_liberal <- lm(af_valores_liberal ~ rico_conservador_bolsonarista, data = matching_data_rico_conservador_bolsonarista)
summary(mod_rico_conservador_bolsonarista_liberal)

mod_rico_conservador_bolsonarista_autoritario <- lm(af_valores_autoritario ~ rico_conservador_bolsonarista, data = matching_data_rico_conservador_bolsonarista)
summary(mod_rico_conservador_bolsonarista_autoritario)


#pobre_liberal
INCT_ReDem_pobre_liberal <- INCT_ReDem %>%
  filter(!is.na(pobre_liberal))

matching_pobre_liberal <- matchit(
  pobre_liberal ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE + ATIVIDADE_RAMO,
  data = INCT_ReDem_pobre_liberal,
  method = "nearest", ratio = 1
)
summary(matching_pobre_liberal)

matching_data_pobre_liberal <- match.data(matching_pobre_liberal)

mod_pobre_liberal_liberal <- lm(af_valores_liberal ~ pobre_liberal, data = matching_data_pobre_liberal)
summary(mod_pobre_liberal_liberal)

mod_pobre_liberal_autoritario <- lm(af_valores_autoritario ~ pobre_liberal, data = matching_data_pobre_liberal)
summary(mod_pobre_liberal_autoritario)


#lulista_liberal
INCT_ReDem_lulista_liberal <- INCT_ReDem %>%
  filter(!is.na(lulista_liberal))

matching_lulista_liberal <- matchit(
  lulista_liberal ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE + ATIVIDADE_RAMO,
  data = INCT_ReDem_lulista_liberal,
  method = "nearest", ratio = 1
)
summary(matching_lulista_liberal)

matching_data_lulista_liberal <- match.data(matching_lulista_liberal)

mod_lulista_liberal_liberal <- lm(af_valores_liberal ~ lulista_liberal, data = matching_data_lulista_liberal)
summary(mod_lulista_liberal_liberal)

mod_lulista_liberal_autoritario <- lm(af_valores_autoritario ~ lulista_liberal, data = matching_data_lulista_liberal)
summary(mod_lulista_liberal_autoritario)


#pobre_lulista
INCT_ReDem_pobre_lulista <- INCT_ReDem %>%
  filter(!is.na(pobre_lulista))

matching_pobre_lulista <- matchit(
  pobre_lulista ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE + ATIVIDADE_RAMO,
  data = INCT_ReDem_pobre_lulista,
  method = "nearest", ratio = 1
)
summary(matching_pobre_lulista)

matching_data_pobre_lulista <- match.data(matching_pobre_lulista)

mod_pobre_lulista_liberal <- lm(af_valores_liberal ~ pobre_lulista, data = matching_data_pobre_lulista)
summary(mod_pobre_lulista_liberal)

mod_pobre_lulista_autoritario <- lm(af_valores_autoritario ~ pobre_lulista, data = matching_data_pobre_lulista)
summary(mod_pobre_lulista_autoritario)


#pobre_liberal_lulista
INCT_ReDem_pobre_liberal_lulista <- INCT_ReDem %>%
  filter(!is.na(pobre_liberal_lulista))

matching_pobre_liberal_lulista <- matchit(
  pobre_liberal_lulista ~ UF + PORTE + IDADE_EX + SEXO + RACA + ESCOLARIDADE + ATIVIDADE_RAMO,
  data = INCT_ReDem_pobre_liberal_lulista,
  method = "nearest", ratio = 1
)
summary(matching_pobre_liberal_lulista)

matching_data_pobre_liberal_lulista <- match.data(matching_pobre_liberal_lulista)

mod_pobre_liberal_lulista_liberal <- lm(af_valores_liberal ~ pobre_liberal_lulista, data = matching_data_pobre_liberal_lulista)
summary(mod_pobre_liberal_lulista_liberal)

mod_pobre_liberal_lulista_autoritario <- lm(af_valores_autoritario ~ pobre_liberal_lulista, data = matching_data_pobre_liberal_lulista)
summary(mod_pobre_liberal_lulista_autoritario)

library(sf)
library(ggplot2)
library(gridExtra)
library(grid)

sf::sf_use_s2(FALSE)

#Criando diagramas de Venn
r <- 1
centros <- st_sfc(
  st_point(c(0.0, 0.0)),   # A
  st_point(c(1.1, 0.0)),   # B
  st_point(c(0.55, 0.95))  # C
)

circulos <- st_buffer(centros, dist = r, nQuadSegs = 360)
circulos <- st_set_precision(circulos, 1e6)
circulos <- st_make_valid(circulos)

venn <- st_sf(set = c("A","B","C"), geom = circulos)
A <- venn$geom[venn$set=="A"]
B <- venn$geom[venn$set=="B"]
C <- venn$geom[venn$set=="C"]

clean_poly <- function(g) {
  g |> st_make_valid() |> st_buffer(0) |> st_collection_extract("POLYGON")
}

ABC <- st_intersection(A, B) |> st_intersection(C) |> clean_poly()
AB  <- st_intersection(A, B) |> st_difference(C) |> st_difference(ABC) |> clean_poly()
AC  <- st_intersection(A, C) |> st_difference(B) |> st_difference(ABC) |> clean_poly()
BC  <- st_intersection(B, C) |> st_difference(A) |> st_difference(ABC) |> clean_poly()

only_A <- A |> st_difference(st_union(B, C)) |> clean_poly()
only_B <- B |> st_difference(st_union(A, C)) |> clean_poly()
only_C <- C |> st_difference(st_union(A, B)) |> clean_poly()

geoms <- do.call(c, list(only_A, only_B, only_C, AB, AC, BC, ABC))
regioes <- st_sf(
  region = c("A", "B", "C", "AnB", "AnC", "BnC", "AnBnC"),
  geometry = geoms
)

bbox <- st_bbox(st_union(circulos))
xpad <- diff(range(bbox[c("xmin","xmax")])) * 0.06
ypad <- diff(range(bbox[c("ymin","ymax")])) * 0.06
xlim <- c(bbox["xmin"] - xpad, bbox["xmax"] + xpad)
ylim <- c(bbox["ymin"] - ypad, bbox["ymax"] + ypad)

make_venn_plot <- function(regioes, cores, labels_text, title, caption) {
  labels_pts <- st_point_on_surface(regioes)
  labels_pts$label <- labels_text
  
  ggplot() +
    geom_sf(data = regioes, aes(fill = region), color = "black", linewidth = 0.7) +
    geom_sf_text(data = labels_pts, aes(label = label), size = 4.2, fontface = "bold", lineheight = 0.95) +
    scale_fill_manual(values = cores, guide = "none") +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = title, caption = caption) +
    theme_void(base_size = 12) +
    theme(
      plot.title   = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.caption = element_text(hjust = 0.98, size = 10, color = "gray35"),
      plot.margin  = margin(8, 8, 8, 8),
      # borda suave no painel para separar em arranjos 2x2
      panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.6)
    )
}

#ESQUERDA COMPROMISSO LIBERAL
cores_esq_lib <- c(
  "A"="#FDF3F3","B"="#D9EFD9","C"="#D9EFD9",
  "AnB"="#CCEACC","AnC"="#CCEACC","BnC"="#BFDEBF",
  "AnBnC"="#89AD89"
)
labels_esq_lib <- c("Pobre", "Liberal***", "Lulista*", "P + L***", "P + Lu**",
                    "L + Lu***", "P + L + Lu***")
graf_esq_liberal <- make_venn_plot(
  regioes, cores_esq_lib, labels_esq_lib,
  title = "Esquerda", caption = "Adesão Normativa"
)

#DIREITA COMPROMISSO LIBERAL
cores_dir_lib <- c(
  "A"="#FDF3F3","B"="#FAE6E6","C"="#FAE6E6",
  "AnB"="#FAE6E6","AnC"="#F7D9D9","BnC"="#FDF3F3",
  "AnBnC"="#F4CCCC"
)
labels_dir_lib <- c("Rico", "Conservador*", "Bolsonarista", "R + C", "R + B",
                    "C + B", "R + C + B")
graf_dir_liberal <- make_venn_plot(
  regioes, cores_dir_lib, labels_dir_lib,
  title = "Direita", caption = "Adesão Normativa"
)

#ESQUERDA TOLERANCIA AUTORITARIA
cores_esq_aut <- c(
  "A"="#F4CCCC","B"="#FDF3F3","C"="#FDF3F3",
  "AnB"="#FAE6E6","AnC"="#F4CCCC","BnC"="#E6F4E6",
  "AnBnC"="#F3F9F3"
)
labels_esq_aut <- c("Pobre***", "Liberal", "Lulista", "P + L", "P + Lu**",
                    "L + Lu", "P + L + Lu")
graf_esq_autoritario <- make_venn_plot(
  regioes, cores_esq_aut, labels_esq_aut,
  title = "Esquerda", caption = "Tolerância a\nPráticas Autoritárias"
)

#DIREITA TOLERANCIA AUTORITARIA
cores_dir_aut <- c(
  "A"="#E6F4E6","B"="#F3F9F3","C"="#D9EFD9",
  "AnB"="#BFDEBF","AnC"="#D9EFD9","BnC"="#CCEACC",
  "AnBnC"="#CCEACC"
)
labels_dir_aut <- c("Rico", "Conservador", "Bolsonarista**", "R + C**", "R + B",
                    "C + B**", "R + C + B")
graf_dir_autoritario <- make_venn_plot(
  regioes, cores_dir_aut, labels_dir_aut,
  title = "Direita", caption = "Tolerância a\nPráticas Autoritárias"
)

#ARRANJO DIAGRAMAS
rodape <- textGrob("",
                   x = 1, hjust = 1, gp = gpar(col = "gray35", fontsize = 9))

painel_2x2 <- arrangeGrob(
  arrangeGrob(graf_esq_liberal, graf_dir_liberal, ncol = 2),
  arrangeGrob(graf_esq_autoritario, graf_dir_autoritario, ncol = 2),
  heights = c(1, 1)
)
painel_final <- arrangeGrob(painel_2x2, bottom = rodape)

#SALVANDO
ggsave("graf_esq_liberal.png",      graf_esq_liberal,      width = 5.5, height = 5.5, dpi = 320)
ggsave("graf_dir_liberal.png",      graf_dir_liberal,      width = 5.5, height = 5.5, dpi = 320)
ggsave("graf_esq_autoritario.png",  graf_esq_autoritario,  width = 5.5, height = 5.5, dpi = 320)
ggsave("graf_dir_autoritario.png",  graf_dir_autoritario,  width = 5.5, height = 5.5, dpi = 320)

#PAINEL 2X2
ggsave("diagramas_venn_2x2.png", painel_final, width = 11.5, height = 11.5, dpi = 320) 

cores_dir_apresentacao <- c(
  "A"="white","B"="white","C"="white",
  "AnB"="white","AnC"="white","BnC"="white",
  "AnBnC"="white"
)
labels_dir_apresentacao <- c("Econômica", "Ideológica", "Afetiva", "E + I", "E + A",
                    "I + A", "E + I + A")
graf_dir_apresentacao <- make_venn_plot(
  regioes, cores_dir_apresentacao, labels_dir_apresentacao,
  title = "Polarização", caption = ""
)

ggsave("graf_apresentacao.png",      graf_dir_apresentacao,      width = 5.5, height = 5.5, dpi = 320)

###Grafico FINAL ARTIGO

library(dplyr)
library(purrr)
library(broom)
library(tidyr)
library(forcats)
library(ggplot2)
library(tibble)

modelos <- tibble(
  bloco = c(
    rep("Esquerda", 14),
    rep("Direita", 14)
  ),
  dv = c(
    rep("af_valores_liberal", 7),
    rep("af_valores_autoritario", 7),
    rep("af_valores_liberal", 7),
    rep("af_valores_autoritario", 7)
  ),
  espec = c(
    c("Lulista", "Liberal", "Baixa Renda", "Liberal Baixa Renda",
      "Lulista Liberal", "Lulista Baixa Renda", "Lulista Liberal Baixa Renda"),
    c("Lulista", "Liberal", "Baixa Renda", "Liberal Baixa Renda",
      "Lulista Liberal", "Lulista Baixa Renda", "Lulista Liberal Baixa Renda"),
    c("Bolsonarista", "Conservador", "Alta Renda", "Conservador Alta Renda",
      "Bolsonarista Conservador", "Bolsonarista Alta Renda",
      "Bolsonarista Conservador Alta Renda"),
    c("Bolsonarista", "Conservador", "Alta Renda", "Conservador Alta Renda",
      "Bolsonarista Conservador", "Bolsonarista Alta Renda",
      "Bolsonarista Conservador Alta Renda")
  ),
  mod = list(
    mod_lulista_liberal,
    mod_liberal_liberal,
    mod_pobre_liberal,
    mod_pobre_liberal_liberal,
    mod_lulista_liberal_liberal,
    mod_pobre_lulista_liberal,
    mod_pobre_liberal_lulista_liberal,
    mod_lulista_autoritario,
    mod_liberal_autoritario,
    mod_pobre_autoritario,
    mod_pobre_liberal_autoritario,
    mod_lulista_liberal_autoritario,
    mod_pobre_lulista_autoritario,
    mod_pobre_liberal_lulista_autoritario,
    mod_bolsonarista_liberal,
    mod_conservador_liberal,
    mod_rico_liberal,
    mod_rico_conservador_liberal,
    mod_bolsonarista_conservador_liberal,
    mod_rico_bolsonarista_liberal,
    mod_rico_conservador_bolsonarista_liberal,
    mod_bolsonarista_autoritario,
    mod_conservador_autoritario,
    mod_rico_autoritario,
    mod_rico_conservador_autoritario,
    mod_bolsonarista_conservador_autoritario,
    mod_rico_bolsonarista_autoritario,
    mod_rico_conservador_bolsonarista_autoritario
  )
)

coefs <- modelos %>%
  mutate(tidy = map(mod, ~ tidy(.x, conf.int = TRUE))) %>%
  unnest(tidy) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    bloco = factor(bloco, levels = c("Esquerda", "Direita")),
    dv = factor(
      dv,
      levels = c("af_valores_liberal", "af_valores_autoritario"),
      labels = c("Adesão Normativa", "Tolerância a\nPráticas Autoritárias")
    ),
    espec = espec |> factor(levels = unique(espec)) |> fct_rev(),
    sig = ifelse(p.value < 0.05, "sig", "ns")
  )

ggplot(
  coefs,
  aes(
    x = estimate,
    y = espec,
    xmin = conf.low,
    xmax = conf.high,
    color = sig
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  facet_grid(dv ~ bloco, scales = "free_y") +
  scale_color_manual(values = c("sig" = "red", "ns" = "black")) +
  labs(
    x = "Escores da Regressão",
    y = "Modelo",
    color = NULL
  ) +
  theme_bw()+ theme(legend.position = "none")

ggsave(
  filename = "grafico_polarizacoes.png",
  width = 8,       
  height = 6,      
  dpi = 400,        
  units = "in"      
)
