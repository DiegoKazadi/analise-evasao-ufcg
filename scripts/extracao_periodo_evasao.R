# -----------------------------
# IDENTIFICAR PERÍODOS MARCANTES
# -----------------------------

# Média por período já calculada em 'media'

# Diferença entre currículos já calculada em 'diff'

# 🔹 Maior evasão por currículo
pico_1999 <- media %>%
  filter(curriculo == 1999) %>%
  slice_max(order_by = taxa, n = 1)

pico_2017 <- media %>%
  filter(curriculo == 2017) %>%
  slice_max(order_by = taxa, n = 1)

# 🔹 Menor evasão por currículo
min_1999 <- media %>%
  filter(curriculo == 1999) %>%
  slice_min(order_by = taxa, n = 1)

min_2017 <- media %>%
  filter(curriculo == 2017) %>%
  slice_min(order_by = taxa, n = 1)

# 🔹 Maior diferença entre currículos
maior_diff <- diff %>%
  slice_max(order_by = diferenca, n = 1)

# 🔹 Menor diferença
menor_diff <- diff %>%
  slice_min(order_by = diferenca, n = 1)

# -----------------------------
# PRINT DOS RESULTADOS
# -----------------------------

print("=== PICO DE EVASÃO ===")
print(pico_1999)
print(pico_2017)

print("=== MENOR EVASÃO ===")
print(min_1999)
print(min_2017)

print("=== MAIOR DIFERENÇA ENTRE CURRÍCULOS ===")
print(maior_diff)

print("=== MENOR DIFERENÇA ENTRE CURRÍCULOS ===")
print(menor_diff)