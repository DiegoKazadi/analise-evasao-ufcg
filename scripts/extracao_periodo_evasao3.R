# -----------------------------
# LEITURA
# -----------------------------
p1 <- ler_csv_seguro("evasao_p1.csv") %>% mutate(periodo_num = 1)
p2 <- ler_csv_seguro("evasao_p2.csv") %>% mutate(periodo_num = 2)
p3 <- ler_csv_seguro("evasao_p3.csv") %>% mutate(periodo_num = 3)
p4 <- ler_csv_seguro("evasao_p4.csv") %>% mutate(periodo_num = 4)

dados <- bind_rows(p1, p2, p3, p4)

# Garantir tipos corretos
dados <- dados %>%
  mutate(
    curriculo = as.integer(curriculo),
    periodo = as.character(periodo),
    ativos = as.integer(ativos),
    evadidos = as.integer(evadidos),
    taxa = as.numeric(taxa),
    periodo_num = as.integer(periodo_num)
  )

# -----------------------------
# FUNÇÕES AUXILIARES
# -----------------------------
top_max <- function(df, n = 2) df %>% slice_max(order_by = taxa, n = n, with_ties = FALSE)
top_min <- function(df, n = 2) df %>% slice_min(order_by = taxa, n = n, with_ties = FALSE)

# -----------------------------
# DESTAQUES POR PERÍODO EXATO
# -----------------------------
resumo_periodo <- dados %>%
  group_by(periodo_num, curriculo) %>%
  summarise(
    media = mean(taxa, na.rm = TRUE),
    sd = sd(taxa, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Picos e mínimos por período e currículo
picos <- dados %>%
  group_by(periodo_num, curriculo) %>%
  group_modify(~ top_max(.x, n = 2)) %>%
  ungroup()

minimos <- dados %>%
  group_by(periodo_num, curriculo) %>%
  group_modify(~ top_min(.x, n = 2)) %>%
  ungroup()

# -----------------------------
# DIFERENÇA ENTRE CURRÍCULOS POR PERÍODO LETIVO
# (somente quando ambos existem no mesmo "periodo")
# -----------------------------
diff_detalhado <- dados %>%
  select(curriculo, periodo, periodo_num, taxa, evadidos, ativos) %>%
  pivot_wider(
    names_from = curriculo,
    values_from = c(taxa, evadidos, ativos),
    names_glue = "{.value}_{curriculo}"
  ) %>%
  drop_na(taxa_1999, taxa_2017) %>%
  mutate(diferenca = taxa_1999 - taxa_2017)

maior_diff <- diff_detalhado %>% slice_max(order_by = diferenca, n = 3)
menor_diff <- diff_detalhado %>% slice_min(order_by = diferenca, n = 3)

# -----------------------------
# OUTLIERS (ex.: taxas muito altas por baixo N)
# -----------------------------
outliers <- dados %>%
  filter(taxa >= quantile(taxa, 0.95, na.rm = TRUE)) %>%
  arrange(desc(taxa))

# -----------------------------
# PRINT ORGANIZADO
# -----------------------------
cat("\n=== RESUMO (MÉDIA/SD) POR PERÍODO E CURRÍCULO ===\n")
print(resumo_periodo)

cat("\n=== PICOS (TOP 2) POR PERÍODO E CURRÍCULO ===\n")
print(picos %>% arrange(periodo_num, curriculo, desc(taxa)) %>%
        select(periodo_num, curriculo, periodo, ativos, evadidos, taxa))

cat("\n=== MENORES (BOTTOM 2) POR PERÍODO E CURRÍCULO ===\n")
print(minimos %>% arrange(periodo_num, curriculo, taxa) %>%
        select(periodo_num, curriculo, periodo, ativos, evadidos, taxa))

cat("\n=== MAIORES DIFERENÇAS (1999 - 2017) ===\n")
print(maior_diff %>%
        select(periodo_num, periodo, taxa_1999, taxa_2017, diferenca,
               evadidos_1999, evadidos_2017, ativos_1999, ativos_2017))

cat("\n=== MENORES DIFERENÇAS (1999 - 2017) ===\n")
print(menor_diff %>%
        select(periodo_num, periodo, taxa_1999, taxa_2017, diferenca))

cat("\n=== POSSÍVEIS OUTLIERS (taxas altas) ===\n")
print(outliers %>%
        select(periodo_num, curriculo, periodo, ativos, evadidos, taxa))
