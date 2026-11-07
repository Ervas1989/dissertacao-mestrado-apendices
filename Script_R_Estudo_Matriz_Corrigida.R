# =============================================================================
#  ESTUDO COM A MATRIZ DE CRUZAMENTO CORRIGIDA — SCRIPT PASSO A PASSO
#  Dissertação: Impacto dos Sistemas de Certificação de Qualidade nos Pequenos
#  Produtores Agrícolas na Agricultura Sustentável em Portugal
#
#  Lógica do estudo (a mesma da versão original):
#    Respostas do inquérito -> Tabela de correspondência -> Matriz de cruzamento
#    -> Gráficos aluviais -> Boxplots -> Testes de hipóteses (H0.1 a H0.41)
#
#  Como usar:
#    1. Coloque numa pasta o ficheiro dados_brutos.csv e este script.
#    2. Abra o R (ou o RStudio) e corra o script um PASSO de cada vez:
#       selecione as linhas de um passo e carregue em Ctrl+Enter (RStudio)
#       ou cole-as na janela de scripts do R Commander e carregue em «Submeter».
#    3. Os valores que aparecem no ecrã devem coincidir com os do documento
#       «Guia Detalhado do Script R» (Guia_Detalhado_Script_R_Passo_a_Passo.pdf).
#  Só usa o R base: não é preciso instalar pacotes.
# =============================================================================


# ---- PASSO 0. Preparar o R ----------------------------------------------------
# Escolha a pasta onde estão os ficheiros (abre uma janela; escolha dados_brutos.csv).
ficheiro <- file.choose()
setwd(dirname(ficheiro))
options(width = 90)


# ---- PASSO 1. Ler as respostas do inquérito ---------------------------------
# dados_brutos.csv = folha «Respostas do Inquérito» do Apêndice F, com uma linha
# por inquirido e uma coluna por pergunta (p4_1 = pergunta 4.1, etc.).
respostas <- read.csv("dados_brutos.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
respostas[respostas == ""] <- NA
nrow(respostas)          # deve dar 67
names(respostas)         # nomes das colunas


# ---- PASSO 2. Tabela de correspondência (corrigida) --------------------------
# Cada objetivo e cada indicador passa a ter as SUAS perguntas, sem perguntas
# repetidas entre colunas que se correlacionam (exceto 4.3, comum a Obj4 e IndEco,
# que nunca são correlacionados entre si). As perguntas de caracterização
# (1.1 a 2.2) ficam como variáveis de grupo, e não dentro das medidas; a 2.3 entra no Obj3.
correspondencia <- data.frame(
  Medida    = c("Obj1 Desafios", "Obj2 Simplificação / Custos", "Obj3 Certificação em grupo",
                "Obj4 Acesso a mercados", "Obj5 Educação / Consciencialização",
                "Obj6 Políticas / Apoio", "IndEco", "IndSoc", "IndAmbEC"),
  Perguntas = c("7.1 (n.º de obstáculos)", "8 (simplificação; redução de custos)",
                "2.3 e 8 (certificação em grupo)", "4.3a, 4.3b, 4.3c",
                "11 (formação recebida e importância)", "7.2 (apoios PAC)",
                "4.1, 4.2, 4.3a, 4.3b, 4.3c", "6.1, 6.2", "5.1, 5.2, 5.3, 5.4, 5.5"))
correspondencia


# ---- PASSO 3. Recodificar as respostas (1 = menos favorável) -----------------
# A função «codigo» dá a cada resposta a sua posição na lista indicada.
# Respostas fora da lista (por exemplo «Não aplicável») ficam em falta (NA).
codigo <- function(x, lista) match(trimws(x), lista)
# A função «escala5» passa um código para a escala comum de 1 a 5.
escala5 <- function(x, maximo) 1 + 4 * (x - 1) / (maximo - 1)

r <- respostas            # cópia de trabalho
# Indicador económico (Secção 4)
r$rend  <- escala5(codigo(r$p4_1, c("Diminuiu", "Manteve-se", "Aumentou ligeiramente", "Aumentou significativamente")), 4)
r$preco <- escala5(codigo(r$p4_2, c("Não", "Depende do mercado", "Sim")), 3)
r$merc  <- escala5(codigo(r$p4_3a, c("Discordo", "Neutro", "Concordo")), 3)
r$rel   <- escala5(codigo(r$p4_3b, c("Discordo", "Neutro", "Concordo")), 3)
r$proc  <- escala5(codigo(r$p4_3c, c("Discordo", "Neutro", "Concordo")), 3)
# Indicador ambiental e ecológico (Secção 5): «Reduziu» é favorável nos pesticidas e fertilizantes
r$pest  <- escala5(codigo(r$p5_1, c("Aumentou", "Igual", "Reduziu")), 3)
r$fert  <- escala5(codigo(r$p5_2, c("Aumentou", "Igual", "Reduziu")), 3)
r$agua  <- escala5(codigo(r$p5_3, c("Reduziu", "Igual", "Aumentou")), 3)
r$solo  <- escala5(codigo(r$p5_4, c("Reduziu", "Igual", "Aumentou")), 3)
r$bio   <- escala5(codigo(r$p5_5, c("Reduziu", "Igual", "Aumentou")), 3)
# Indicador social (Secção 6): 6.2 = número de impactos positivos na comunidade (0, 1, 2 ou mais)
r$qv    <- escala5(codigo(r$p6_1, c("Não", "Parcialmente", "Sim")), 3)
impactos <- strsplit(ifelse(is.na(r$p6_2), "", r$p6_2), ",")
n_impactos <- sapply(impactos, function(v) sum(trimws(v) != "" & trimws(v) != "Sem impacto relevante"))
r$com   <- escala5(pmin(n_impactos, 2) + 1, 3)
# Objetivos
obstaculos <- strsplit(ifelse(is.na(r$p7_1), "", r$p7_1), ",")
r$n_obstaculos <- sapply(obstaculos, function(v) sum(trimws(v) != "" & trimws(v) != "Outro"))
estrategias <- tolower(ifelse(is.na(r$p8), "", r$p8))
r$escolhe_simplif <- grepl("simplifica", estrategias)
r$escolhe_custos  <- grepl("redu", estrategias) & grepl("custo", estrategias)
r$escolhe_grupo   <- grepl("certificação em grupo", estrategias)
r$formacao  <- ifelse(tolower(trimws(r$p11)) == "sim", "Sim", "Não")
r$apoios    <- escala5(codigo(r$p7_2, c("Não", "Parcialmente", "Sim")), 3)


# ---- PASSO 4. Construir a matriz de cruzamento ------------------------------
# Regra: cada medida é a média dos seus itens (escala 1 a 5); um indicador com
# vários itens só é calculado se pelo menos metade dos itens tiver resposta.
media_itens <- function(itens, minimo) {
  m <- rowMeans(itens, na.rm = TRUE)
  m[rowSums(!is.na(itens)) < minimo] <- NA
  m
}
matriz <- data.frame(
  Inquirido = paste("Inquirido", r$id),
  tipo   = r$tipo, regiao = r$regiao, area = r$area, trab = r$trab,
  # Obj1: menos obstáculos = valor mais alto (0 obstáculos = 5; 6 obstáculos = 1)
  Obj1 = 5 - 4 * pmin(r$n_obstaculos, 6) / 6,
  # Obj2: prioridade dada à simplificação e à redução de custos (nenhuma = 1; uma = 3; ambas = 5)
  Obj2 = 1 + 2 * (r$escolhe_simplif + r$escolhe_custos),
  # Obj3: em grupo = 5; individual, mas escolhe a certificação em grupo como estratégia = 3; nenhum = 1
  Obj3 = ifelse(r$forma == "Certificação em grupo", 5, ifelse(r$escolhe_grupo, 3, 1)),
  # Obj4: média de 4.3a, 4.3b e 4.3c
  Obj4 = media_itens(cbind(r$merc, r$rel, r$proc), 2),
  # Obj5: sem formação = 1; com formação = média de 5 (recebeu) e da importância atribuída (1 a 5)
  Obj5 = ifelse(r$formacao == "Sim", rowMeans(cbind(5, r$p11_imp), na.rm = TRUE), 1),
  # Obj6: apoios PAC suficientes (Não = 1; Parcialmente = 3; Sim = 5)
  Obj6 = r$apoios,
  IndEco   = media_itens(cbind(r$rend, r$preco, r$merc, r$rel, r$proc), 3),
  IndSoc   = media_itens(cbind(r$qv, r$com), 1),
  IndAmbEC = media_itens(cbind(r$pest, r$fert, r$agua, r$solo, r$bio), 3))
head(matriz)
write.csv(matriz, "matriz_cruzamento_corrigida.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Resumo de cada medida: média, mediana e número de inquiridos
medidas <- c("Obj1", "Obj2", "Obj3", "Obj4", "Obj5", "Obj6", "IndEco", "IndSoc", "IndAmbEC")
resumo <- data.frame(n = sapply(matriz[medidas], function(x) sum(!is.na(x))),
                     media = sapply(matriz[medidas], mean, na.rm = TRUE),
                     mediana = sapply(matriz[medidas], median, na.rm = TRUE))
round(resumo, 2)

# Nível de cada valor: Baixo (< 2,5), Médio (2,5 a 3,49) ou Alto (>= 3,5)
nivel <- function(x) cut(x, c(0, 2.5, 3.5, 6), right = FALSE, labels = c("Baixo", "Médio", "Alto"))
t(sapply(matriz[medidas], function(x) table(nivel(x))))


# ---- PASSO 5. Dados para os gráficos aluviais (RAWGraphs) -------------------
# Formato igual ao das folhas «Gráfico Aluvial» do Apêndice F:
# uma linha por inquirido e por medida (67 x 9 = 603 linhas).
longo <- data.frame(Inquirido = rep(matriz$Inquirido, 9),
                    tipo = rep(matriz$tipo, 9), regiao = rep(matriz$regiao, 9),
                    area = rep(matriz$area, 9), trab = rep(matriz$trab, 9),
                    Objetivo = rep(medidas, each = nrow(matriz)),
                    Valor = unlist(matriz[medidas]))
longo$Categoria <- nivel(longo$Valor)
write.csv(longo, "dados_graficos_aluviais.csv", row.names = FALSE, fileEncoding = "UTF-8")
# Exemplo: Tipo de produtor x Objetivo x Categoria (número de inquiridos)
ftable(table(longo$tipo, longo$Objetivo, longo$Categoria))


# ---- PASSO 6. Boxplots por variável estrutural ------------------------------
# Gera um ficheiro PNG por variável estrutural, com as 9 medidas.
for (fator in c("tipo", "regiao", "area", "trab")) {
  png(paste0("boxplots_", fator, ".png"), width = 1600, height = 1200, res = 130)
  par(mfrow = c(3, 3), mar = c(4, 11, 2, 1))
  for (m in medidas) boxplot(matriz[[m]] ~ matriz[[fator]], horizontal = TRUE, las = 1,
                             main = m, xlab = "", ylab = "", ylim = c(1, 5), col = "lightblue")
  dev.off()
}


# ---- PASSO 7. Bloco 1 — Qui-quadrado (H0.1 a H0.7) --------------------------
# Para cada par: tabela cruzada, teste assintótico e teste por simulação de
# Monte Carlo (10 000 réplicas; a semente fixa garante o mesmo resultado).
matriz$maodeobra <- ifelse(matriz$trab == "Apenas mão de obra familiar", "Familiar", "Contratados")
qui <- function(a, b) {
  tabela <- table(matriz[[a]], matriz[[b]])
  print(tabela)
  print(suppressWarnings(chisq.test(tabela)))
  set.seed(2026)
  chisq.test(tabela, simulate.p.value = TRUE, B = 10000)
}
H0.1 <- qui("regiao", "trab");   H0.1
H0.2 <- qui("tipo", "trab");     H0.2
H0.3 <- qui("regiao", "tipo");   H0.3
H0.4 <- qui("regiao", "area");   H0.4
H0.5 <- qui("tipo", "area");     H0.5
H0.6 <- qui("area", "trab");     H0.6
H0.7 <- qui("regiao", "maodeobra"); H0.7


# ---- PASSO 8. Bloco 2 — Kruskal-Wallis e Mann-Whitney (H0.8 a H0.24) --------
# Antes de cada teste: média e mediana por grupo (aggregate).
grupos <- function(medida, fator) {
  x <- split(matriz[[medida]], matriz[[fator]])
  data.frame(grupo = names(x), n = sapply(x, function(v) sum(!is.na(v))),
             media = round(sapply(x, mean, na.rm = TRUE), 2), mediana = round(sapply(x, median, na.rm = TRUE), 2),
             row.names = NULL)
}
grupos("Obj2", "regiao");     H0.8  <- kruskal.test(Obj2 ~ regiao, data = matriz);     H0.8
grupos("Obj4", "regiao");     H0.9  <- kruskal.test(Obj4 ~ regiao, data = matriz);     H0.9
grupos("Obj1", "regiao");     H0.10 <- kruskal.test(Obj1 ~ regiao, data = matriz);     H0.10
grupos("IndEco", "regiao");   H0.11 <- kruskal.test(IndEco ~ regiao, data = matriz);   H0.11
grupos("IndSoc", "regiao");   H0.12 <- kruskal.test(IndSoc ~ regiao, data = matriz);   H0.12
grupos("IndAmbEC", "regiao"); H0.13 <- kruskal.test(IndAmbEC ~ regiao, data = matriz); H0.13
grupos("Obj2", "tipo");       H0.14 <- kruskal.test(Obj2 ~ tipo, data = matriz);       H0.14
grupos("IndSoc", "tipo");     H0.15 <- kruskal.test(IndSoc ~ tipo, data = matriz);     H0.15
grupos("IndEco", "tipo");     H0.16 <- kruskal.test(IndEco ~ tipo, data = matriz);     H0.16
grupos("Obj1", "area");       H0.17 <- kruskal.test(Obj1 ~ area, data = matriz);       H0.17
grupos("Obj2", "area");       H0.18 <- kruskal.test(Obj2 ~ area, data = matriz);       H0.18
grupos("IndEco", "area");     H0.19 <- kruskal.test(IndEco ~ area, data = matriz);     H0.19
grupos("IndAmbEC", "area");   H0.20 <- kruskal.test(IndAmbEC ~ area, data = matriz);   H0.20
grupos("Obj1", "trab");       H0.21 <- kruskal.test(Obj1 ~ trab, data = matriz);       H0.21
grupos("Obj6", "trab");       H0.22 <- kruskal.test(Obj6 ~ trab, data = matriz);       H0.22
grupos("IndEco", "maodeobra"); H0.23 <- wilcox.test(IndEco ~ maodeobra, data = matriz, exact = FALSE); H0.23
grupos("IndSoc", "trab");     H0.24 <- kruskal.test(IndSoc ~ trab, data = matriz);     H0.24


# ---- PASSO 9. Bloco 3 — ART + ANOVA (H0.25 a H0.30) --------------------------
# O método ART (Wobbrock et al., 2011) alinha e ordena os dados para cada efeito
# e aplica depois uma ANOVA fatorial. Não precisa de alterar a função: corra-a
# uma vez e use-a nos seis modelos. Cada modelo usa só as combinações de grupos
# com inquiridos suficientes (indicadas em cada linha).
art_anova <- function(y, A, B, nomes = c("Fator 1", "Fator 2")) {
  ok <- !is.na(y); y <- y[ok]; A <- factor(A[ok]); B <- factor(B[ok])
  options(contrasts = c("contr.sum", "contr.poly"))
  print(table(A, B))
  mu <- mean(y); celula <- ave(y, A, B); ma <- ave(y, A); mb <- ave(y, B); residuo <- y - celula
  efeitos <- list(ma - mu, mb - mu, celula - ma - mb + mu)
  X <- model.matrix(~ A * B); termo <- attr(X, "assign"); resultado <- NULL
  for (k in 1:3) {
    ordem <- rank(residuo + efeitos[[k]])
    completo <- lm.fit(X, ordem); sq <- sum(completo$residuals^2); gl_res <- length(ordem) - completo$rank
    reduzido <- sum(lm.fit(X[, termo != k, drop = FALSE], ordem)$residuals^2); gl <- sum(termo == k)
    F <- ((reduzido - sq) / gl) / (sq / gl_res)
    resultado <- rbind(resultado, data.frame(efeito = c(nomes, "Interação")[k],
                                             gl = gl, gl_res = gl_res, F = round(F, 3),
                                             p = round(pf(F, gl, gl_res, lower.tail = FALSE), 4)))
  }
  resultado
}
sub <- function(f1, n1, f2, n2) matriz[[f1]] %in% n1 & matriz[[f2]] %in% n2
s <- sub("tipo", c("Empresa familiar", "Produtor individual"), "area", c("< 5 ha", "5-10 ha", "21-50 ha"))
H0.25 <- art_anova(matriz$IndEco[s], matriz$tipo[s], matriz$area[s], c("Tipo", "Área")); H0.25
s <- sub("tipo", c("Empresa familiar", "Produtor individual"), "regiao", c("Açores", "Norte"))
H0.26 <- art_anova(matriz$Obj2[s], matriz$tipo[s], matriz$regiao[s], c("Tipo", "Região")); H0.26
s <- sub("trab", c("1-3 trabalhadores", "Apenas mão de obra familiar"), "area", c("< 5 ha", "5-10 ha", "21-50 ha"))
H0.27 <- art_anova(matriz$IndAmbEC[s], matriz$trab[s], matriz$area[s], c("Trabalhadores", "Área")); H0.27
s <- sub("tipo", c("Cooperativa", "Produtor individual"), "regiao", c("Açores", "Centro", "Norte"))
H0.28 <- art_anova(matriz$IndAmbEC[s], matriz$tipo[s], matriz$regiao[s], c("Tipo", "Região")); H0.28
H0.28b <- wilcox.test(IndAmbEC ~ tipo, data = matriz[matriz$tipo %in% c("Cooperativa", "Produtor individual"), ], exact = FALSE); H0.28b
s <- sub("tipo", c("Empresa familiar", "Produtor individual"), "trab", c("1-3 trabalhadores", "Apenas mão de obra familiar"))
H0.29 <- art_anova(matriz$Obj2[s], matriz$tipo[s], matriz$trab[s], c("Tipo", "Trabalhadores")); H0.29
s <- sub("area", c("< 5 ha", "5-10 ha", "21-50 ha"), "tipo", c("Empresa familiar", "Produtor individual"))
H0.30 <- art_anova(matriz$Obj2[s], matriz$area[s], matriz$tipo[s], c("Área", "Tipo")); H0.30


# ---- PASSO 10. Bloco 4 — Correlações de Spearman (H0.31 a H0.41) ------------
sp <- function(a, b) cor.test(matriz[[a]], matriz[[b]], method = "spearman", exact = FALSE)
H0.31 <- sp("Obj1", "IndEco");     H0.31
H0.32 <- sp("Obj2", "IndEco");     H0.32
H0.33 <- sp("Obj3", "IndEco");     H0.33
H0.34 <- sp("Obj4", "IndAmbEC");   H0.34
H0.35 <- sp("Obj5", "IndSoc");     H0.35
H0.36 <- sp("Obj6", "IndEco");     H0.36
H0.37 <- sp("IndEco", "IndSoc");   H0.37
H0.38 <- sp("IndSoc", "IndAmbEC"); H0.38
H0.39 <- sp("IndEco", "IndAmbEC"); H0.39
H0.40 <- sp("Obj1", "Obj2");       H0.40
H0.41 <- sp("Obj4", "IndSoc");     H0.41

# Matriz de Spearman completa (9 x 9): só descreve a matriz de cruzamento.
# Os 11 pares testados são os de cima; Obj4 x IndEco partilham a pergunta 4.3 e nunca são testados.
medidas <- c("Obj1", "Obj2", "Obj3", "Obj4", "Obj5", "Obj6", "IndEco", "IndSoc", "IndAmbEC")
rho_m <- cor(matriz[medidas], method = "spearman", use = "pairwise.complete.obs")
p_m <- outer(medidas, medidas, Vectorize(function(a, b) if (a == b) NA else sp(a, b)$p.value))
dimnames(p_m) <- dimnames(rho_m)
round(rho_m, 2)
round(p_m, 3)
write.csv2(round(rho_m, 3), "matriz_spearman.csv", fileEncoding = "UTF-8")


# ---- PASSO 11. Quadro-síntese e correção para testes múltiplos ---------------
# p de cada hipótese (no Bloco 1, o p de Monte Carlo; no Bloco 3, o menor p dos 3 efeitos)
p_bloco1 <- sapply(list(H0.1, H0.2, H0.3, H0.4, H0.5, H0.6, H0.7), function(t) t$p.value)
p_bloco2 <- sapply(list(H0.8, H0.9, H0.10, H0.11, H0.12, H0.13, H0.14, H0.15, H0.16, H0.17,
                        H0.18, H0.19, H0.20, H0.21, H0.22, H0.23, H0.24), function(t) t$p.value)
p_bloco3 <- sapply(list(H0.25, H0.26, H0.27, H0.28, H0.29, H0.30), function(t) min(t$p))
p_bloco4 <- sapply(list(H0.31, H0.32, H0.33, H0.34, H0.35, H0.36, H0.37, H0.38, H0.39, H0.40, H0.41),
                   function(t) t$p.value)
rho      <- sapply(list(H0.31, H0.32, H0.33, H0.34, H0.35, H0.36, H0.37, H0.38, H0.39, H0.40, H0.41),
                   function(t) unname(t$estimate))
sintese <- data.frame(H0 = paste0("H0.", 1:41), bloco = rep(1:4, c(7, 17, 6, 11)),
                      p = round(c(p_bloco1, p_bloco2, p_bloco3, p_bloco4), 4))
# Correção de Benjamini-Hochberg dentro de cada bloco
sintese$p_BH <- round(ave(sintese$p, sintese$bloco, FUN = function(p) p.adjust(p, "BH")), 4)
sintese$decisao <- ifelse(sintese$p_BH < 0.05, "Rejeita H0 (robusto)",
                   ifelse(sintese$p < 0.05, "Rejeita H0 (exploratório)", "Não rejeita H0"))
sintese
round(rho, 3)                                   # coeficientes do Bloco 4
write.csv(sintese, "sintese_hipoteses.csv", row.names = FALSE, fileEncoding = "UTF-8")


# ---- PASSO 12. Comparações post-hoc (Dunn com correção de Bonferroni) --------
# Corra só para os testes de Kruskal-Wallis com p < 0,05.
dunn <- function(medida, fator) {
  ok <- !is.na(matriz[[medida]]); x <- matriz[[medida]][ok]; g <- factor(matriz[[fator]][ok])
  N <- length(x); rk <- rank(x); empates <- table(x); C <- 1 - sum(empates^3 - empates) / (N^3 - N)
  pares <- combn(levels(g), 2); out <- NULL
  for (j in 1:ncol(pares)) {
    a <- g == pares[1, j]; b <- g == pares[2, j]
    z <- (mean(rk[a]) - mean(rk[b])) / sqrt(C * N * (N + 1) / 12 * (1 / sum(a) + 1 / sum(b)))
    out <- rbind(out, data.frame(par = paste(pares[1, j], "vs", pares[2, j]), z = round(z, 3), p = 2 * pnorm(-abs(z))))
  }
  out$p_Bonferroni <- round(pmin(1, out$p * nrow(out)), 4); out$p <- round(out$p, 4)
  out[order(out$p), ]
}
sintese$H0[sintese$bloco == 2 & sintese$p < 0.05]     # testes de Kruskal-Wallis com p < 0,05
dunn("Obj2", "regiao")        # H0.8
dunn("IndEco", "tipo")        # H0.16
dunn("IndAmbEC", "area")      # H0.20
dunn("IndSoc", "trab")        # H0.24


# ---- PASSO 13. Análises complementares --------------------------------------
# (a) H5 — antiguidade da certificação e impacto social
matriz$antiguidade <- codigo(r$anos, c("< 3", "3-5", "6-10", ">10"))
grupos("IndSoc", "antiguidade")      # 1 = < 3 anos; 2 = 3-5; 3 = 6-10; 4 = > 10 anos
cor.test(matriz$antiguidade, matriz$IndSoc, method = "spearman", exact = FALSE)
table(matriz$antiguidade, r$p6_1)    # qualidade de vida (6.1) por antiguidade
cor.test(matriz$antiguidade, r$qv, method = "spearman", exact = FALSE)
# (c) H2 — número de obstáculos por forma de certificação (grupo vs individual)
tapply(r$n_obstaculos, r$forma, mean)
wilcox.test(r$n_obstaculos ~ r$forma, exact = FALSE)
# (b) Motivações da Secção 3 e indicadores
motivacoes <- c("m_rend", "m_merc", "m_prod", "m_amb", "m_cli", "m_apoio")
for (m in motivacoes) for (ind in c("IndEco", "IndSoc", "IndAmbEC")) {
  t <- cor.test(r[[m]], matriz[[ind]], method = "spearman", exact = FALSE)
  cat(sprintf("%-8s x %-9s rho = %6.3f   p = %.4f\n", m, ind, t$estimate, t$p.value))
}

# ---- PASSO 14. Robustez dos resultados --------------------------------------
# Não altera nenhum resultado anterior: acrescenta a margem de erro, o poder,
# os tamanhos do efeito, os intervalos de confiança das correlações e a
# repetição do Bloco 4 separadamente nos Açores e nas restantes regiões.

# (a) Margem de erro máxima de uma proporção (95% de confiança; p = 0,5)
n <- nrow(matriz)
round(100 * qnorm(0.975) * sqrt(0.25 / n), 1)          # em pontos percentuais

# (b) Poder: menor |rho| detetável com 80% de poder (alfa = 0,05; bilateral)
rho_min <- function(n) tanh((qnorm(0.975) + qnorm(0.80)) / sqrt(n - 3))
round(c(todos = rho_min(67), IndAmbEC = rho_min(64), Acores = rho_min(38), outras = rho_min(29)), 3)

# (c) Tamanho do efeito de cada hipótese
#     Bloco 1: V de Cramér corrigido; Bloco 2: épsilon-quadrado (H / (n - 1)) e, na H0.23, r = |z| / raiz(n);
#     Bloco 3: eta-quadrado parcial do efeito com menor p; Bloco 4: o próprio rho.
#     No Bloco 1 usa-se o V de Cramér com a correção de enviesamento de Bergsma (2013),
#     porque o V simples sobrestima o efeito em tabelas grandes com poucos inquiridos.
v_cramer <- function(a, b) {
  t <- table(matriz[[a]], matriz[[b]]); n <- sum(t); r <- nrow(t); k <- ncol(t)
  phi2 <- unname(suppressWarnings(chisq.test(t, correct = FALSE))$statistic) / n
  phi2c <- max(0, phi2 - (k - 1) * (r - 1) / (n - 1))
  rc <- r - (r - 1)^2 / (n - 1); kc <- k - (k - 1)^2 / (n - 1)
  sqrt(phi2c / min(kc - 1, rc - 1))
}
pares1 <- list(c("regiao", "trab"), c("tipo", "trab"), c("regiao", "tipo"), c("regiao", "area"),
               c("tipo", "area"), c("area", "trab"), c("regiao", "maodeobra"))
ef1 <- sapply(pares1, function(v) v_cramer(v[1], v[2]))
medida2 <- c("Obj2", "Obj4", "Obj1", "IndEco", "IndSoc", "IndAmbEC", "Obj2", "IndSoc", "IndEco",
             "Obj1", "Obj2", "IndEco", "IndAmbEC", "Obj1", "Obj6", "IndEco", "IndSoc")
testes2 <- list(H0.8, H0.9, H0.10, H0.11, H0.12, H0.13, H0.14, H0.15, H0.16, H0.17,
                H0.18, H0.19, H0.20, H0.21, H0.22, H0.23, H0.24)
ef2 <- mapply(function(t, m) {
  n_m <- sum(!is.na(matriz[[m]]))
  if (grepl("Kruskal", t$method)) unname(t$statistic) / (n_m - 1)
  else abs(qnorm(t$p.value / 2)) / sqrt(n_m)             # H0.23 (Mann-Whitney): r
}, testes2, medida2)
ef3 <- sapply(list(H0.25, H0.26, H0.27, H0.28, H0.29, H0.30), function(t) {
  k <- which.min(t$p); t$F[k] * t$gl[k] / (t$F[k] * t$gl[k] + t$gl_res[k])
})
efeito <- data.frame(H0 = paste0("H0.", 1:41),
                     medida = rep(c("V de Cramér corrigido", "épsilon²", "eta² parcial", "rho"), c(7, 17, 6, 11)),
                     valor = round(c(ef1, ef2, ef3, rho), 3))
efeito$medida[23] <- "r"                                 # H0.23 é um Mann-Whitney
efeito

# (d) Intervalo de confiança a 95% de cada rho do Bloco 4 (bootstrap, 10 000 reamostragens)
pares4 <- list(c("Obj1", "IndEco"), c("Obj2", "IndEco"), c("Obj3", "IndEco"), c("Obj4", "IndAmbEC"),
               c("Obj5", "IndSoc"), c("Obj6", "IndEco"), c("IndEco", "IndSoc"), c("IndSoc", "IndAmbEC"),
               c("IndEco", "IndAmbEC"), c("Obj1", "Obj2"), c("Obj4", "IndSoc"))
set.seed(2026)
ic <- t(sapply(pares4, function(v) {
  d <- na.omit(matriz[v])
  b <- replicate(10000, { i <- sample(nrow(d), replace = TRUE); cor(d[i, 1], d[i, 2], method = "spearman") })
  quantile(b, c(0.025, 0.975), na.rm = TRUE)
}))
# (e) Bloco 4 repetido nos Açores e nas restantes regiões
por_regiao <- function(dados) t(sapply(pares4, function(v) {
  t <- cor.test(dados[[v[1]]], dados[[v[2]]], method = "spearman", exact = FALSE)
  c(n = sum(complete.cases(dados[v])), rho = unname(t$estimate), p = t$p.value)
}))
acores <- por_regiao(matriz[matriz$regiao == "Açores", ])
outras <- por_regiao(matriz[matriz$regiao != "Açores", ])
robustez <- data.frame(H0 = paste0("H0.", 31:41), rho = round(rho, 3),
                       IC95_inf = round(ic[, 1], 3), IC95_sup = round(ic[, 2], 3),
                       n_Acores = acores[, "n"], rho_Acores = round(acores[, "rho"], 3), p_Acores = round(acores[, "p"], 4),
                       n_outras = outras[, "n"], rho_outras = round(outras[, "rho"], 3), p_outras = round(outras[, "p"], 4))
robustez
write.csv(efeito, "tamanhos_do_efeito.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(robustez, "robustez_bloco4.csv", row.names = FALSE, fileEncoding = "UTF-8")
# =============================== FIM =========================================
