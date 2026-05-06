# -----------------------------
# IDENTIFICAR PERÍODOS MARCANTES (COM PERÍODO LETIVO)
# -----------------------------

# 🔹 Maior evasão por currículo (com período letivo)
pico_1999 <- dados %>%
  filter(curriculo == 1999) %>%
  slice_max(order_by = taxa, n = 1)

pico_2017 <- dados %>%
  filter(curriculo == 2017) %>%
  slice_max(order_by = taxa, n = 1)

# 🔹 Menor evasão por currículo
min_1999 <- dados %>%
  filter(curriculo == 1999) %>%
  slice_min(order_by = taxa, n = 1)

min_2017 <- dados %>%
  filter(curriculo == 2017) %>%
  slice_min(order_by = taxa, n = 1)

# 🔹 Maior diferença ENTRE CURRÍCULOS POR PERÍODO LETIVO

diff_detalhado <- dados %>%
  select(curriculo, periodo_num, periodo_letivo, taxa) %>%
  pivot_wider(names_from = curriculo, values_from = taxa) %>%
  mutate(diferenca = `1999` - `2017`)

maior_diff <- diff_detalhado %>%
  slice_max(order_by = diferenca, n = 1)

menor_diff <- diff_detalhado %>%
  slice_min(order_by = diferenca, n = 1)

# -----------------------------
# PRINT
# -----------------------------

print("=== PICO DE EVASÃO ===")
print(pico_1999)
print(pico_2017)

print("=== MENOR EVASÃO ===")
print(min_1999)
print(min_2017)

print("=== MAIOR DIFERENÇA ===")
print(maior_diff)

print("=== MENOR DIFERENÇA ===")
print(menor_diff)