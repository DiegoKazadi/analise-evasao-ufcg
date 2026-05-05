# ==========================================
# Análise de evasão - UFCG (WINDOWS)
# ==========================================

# Pacotes (instalar apenas se necessário)
if (!require(tidyverse)) install.packages("tidyverse")

library(tidyverse)

# -----------------------------
# CAMINHO CORRETO (WINDOWS)
# -----------------------------
pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/estatisticas-periodos-exatos-ufcg/dados"
pasta_saida <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/estatisticas-periodos-exatos-ufcg/outputs"

# -----------------------------
# Criar pasta outputs
# -----------------------------
if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}

# -----------------------------
# VERIFICAR SE OS ARQUIVOS EXISTEM
# -----------------------------
list.files(pasta_dados)

# -----------------------------
# LEITURA SEGURA DOS DADOS
# -----------------------------
ler_csv_seguro <- function(nome) {
  caminho <- file.path(pasta_dados, nome)
  if (!file.exists(caminho)) {
    stop(paste("Arquivo não encontrado:", caminho))
  }
  read.csv(caminho)
}

p1 <- ler_csv_seguro("evasao_p1.csv"); p1$periodo_num <- 1
p2 <- ler_csv_seguro("evasao_p2.csv"); p2$periodo_num <- 2
p3 <- ler_csv_seguro("evasao_p3.csv"); p3$periodo_num <- 3
p4 <- ler_csv_seguro("evasao_p4.csv"); p4$periodo_num <- 4

# -----------------------------
# UNIR BASES
# -----------------------------
dados <- bind_rows(p1, p2, p3, p4)

# -----------------------------
# BOXPLOT
# -----------------------------
g1 <- ggplot(dados, aes(x = factor(periodo_num), y = taxa, fill = factor(curriculo))) +
  geom_boxplot() +
  labs(title = "Distribuição da Evasão", x = "Período", y = "Taxa (%)") +
  theme_minimal()

ggsave(file.path(pasta_saida, "boxplot.png"), g1, width = 8, height = 5)

# -----------------------------
# MÉDIA POR PERÍODO
# -----------------------------
media <- dados %>%
  group_by(curriculo, periodo_num) %>%
  summarise(taxa = mean(taxa, na.rm = TRUE), .groups = "drop")

g2 <- ggplot(media, aes(x = periodo_num, y = taxa, color = factor(curriculo))) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Evolução da Evasão", x = "Período", y = "Taxa média (%)") +
  theme_minimal()

ggsave(file.path(pasta_saida, "linha.png"), g2, width = 8, height = 5)

# -----------------------------
# BARRAS
# -----------------------------
g3 <- ggplot(media, aes(x = factor(periodo_num), y = taxa, fill = factor(curriculo))) +
  geom_col(position = "dodge") +
  labs(title = "Comparação por Período", x = "Período", y = "Taxa (%)") +
  theme_minimal()

ggsave(file.path(pasta_saida, "barras.png"), g3, width = 8, height = 5)

# -----------------------------
# DIFERENÇA
# -----------------------------
media_1999 <- media %>% filter(curriculo == 1999)
media_2017 <- media %>% filter(curriculo == 2017)

diff <- left_join(media_1999, media_2017, by = "periodo_num", suffix = c("_1999", "_2017")) %>%
  mutate(diferenca = taxa_1999 - taxa_2017)

g4 <- ggplot(diff, aes(x = periodo_num, y = diferenca)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Diferença (1999 - 2017)", x = "Período", y = "Diferença (%)") +
  theme_minimal()

ggsave(file.path(pasta_saida, "diferenca.png"), g4, width = 8, height = 5)

print("Gráficos gerados com sucesso!")