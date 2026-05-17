# =====================================================
# TABELAS CONSOLIDADAS PARA SLIDES - PERÍODOS EXATOS
# =====================================================

# -----------------------------
# 1. Consolidar dados dos 4 períodos
# -----------------------------

p1 <- ler_csv_seguro("evasao_p1.csv") %>% mutate(periodo_num = 1)
p2 <- ler_csv_seguro("evasao_p2.csv") %>% mutate(periodo_num = 2)
p3 <- ler_csv_seguro("evasao_p3.csv") %>% mutate(periodo_num = 3)
p4 <- ler_csv_seguro("evasao_p4.csv") %>% mutate(periodo_num = 4)

dados <- bind_rows(p1, p2, p3, p4)

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
# 2. TABELA A: Média por período e currículo
# -----------------------------

tabela_media_slide <- dados %>%
  group_by(periodo_num, curriculo) %>%
  summarise(
    media_taxa = round(mean(taxa, na.rm = TRUE), 2),
    desvio_padrao = round(sd(taxa, na.rm = TRUE), 2),
    total_coortes = n(),
    .groups = "drop"
  ) %>%
  arrange(periodo_num, curriculo)

print("=== TABELA A: MÉDIAS PARA SLIDE ===")
print(tabela_media_slide)

# -----------------------------
# 3. TABELA B: Pico mais relevante por período e currículo
# -----------------------------

tabela_picos_slide <- dados %>%
  group_by(periodo_num, curriculo) %>%
  slice_max(order_by = taxa, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    periodo_num,
    curriculo,
    periodo,
    ativos,
    evadidos,
    taxa
  ) %>%
  arrange(periodo_num, curriculo)

print("=== TABELA B: PICOS RELEVANTES PARA SLIDE ===")
print(tabela_picos_slide)

# -----------------------------
# 4. TABELA C: Diferença média entre currículos por período
# -----------------------------

media_1999 <- tabela_media_slide %>%
  filter(curriculo == 1999) %>%
  select(periodo_num, media_1999 = media_taxa)

media_2017 <- tabela_media_slide %>%
  filter(curriculo == 2017) %>%
  select(periodo_num, media_2017 = media_taxa)

tabela_diferenca_slide <- media_1999 %>%
  inner_join(media_2017, by = "periodo_num") %>%
  mutate(
    diferenca_pp = round(media_1999 - media_2017, 2)
  )

print("=== TABELA C: DIFERENÇA MÉDIA ENTRE CURRÍCULOS ===")
print(tabela_diferenca_slide)

# -----------------------------
# 5. Exportar tabelas para CSV
# -----------------------------

write.csv(
  tabela_media_slide,
  file.path(pasta_saida, "tabela_media_periodos_exatos_slide.csv"),
  row.names = FALSE
)

write.csv(
  tabela_picos_slide,
  file.path(pasta_saida, "tabela_picos_periodos_exatos_slide.csv"),
  row.names = FALSE
)

write.csv(
  tabela_diferenca_slide,
  file.path(pasta_saida, "tabela_diferenca_periodos_exatos_slide.csv"),
  row.names = FALSE
)

print("Tabelas consolidadas para slides geradas com sucesso!")