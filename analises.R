# Carregar bibliotecas necessárias
library(plyr)
library(car)
library(dunn.test)
library(graphics)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)

# Carregar dados
pibid <- read.csv2("pibid.csv", dec = ".", header = TRUE, strip.white = TRUE)
pibidnf <- read.csv2("pibidnf.csv", dec = ".", header = TRUE, strip.white = TRUE)

# Visualizar nomes das colunas
names(pibid)
names(pibidnf)


# Estatística descritiva por avaliador
avaliadores <- c("A1", "A2", "A3")
for (avaliador in avaliadores) {
    cat("Avaliador:", avaliador, "\n")
    cat("Summary:\n")
    print(summary(pibid$Notas[pibid$Avaliador == avaliador]))
    cat("Desvio padrão:", sd(pibid$Notas[pibid$Avaliador == avaliador]), "\n")
    cat("Mediana:", median(pibid$Notas[pibid$Avaliador == avaliador]), "\n\n")
}

# Estatística descritiva por turma
turmas <- c("E1", "E2", "C")
for (turma in turmas) {
    cat("Turma:", turma, "\n")
    cat("Summary:\n")
    print(summary(pibidnf$Notas[pibidnf$Turma == turma]))
    cat("Desvio padrão:", sd(pibidnf$Notas[pibidnf$Turma == turma]), "\n")
    cat("Mediana:", median(pibidnf$Notas[pibidnf$Turma == turma]), "\n\n")
}

# Estatística descritiva por aula
aulas <- unique(pibid$Aula)
for (aula in aulas) {
    cat("Aula:", aula, "\n")
    cat("Summary:\n")
    print(summary(pibid$Notas[pibid$Aula == aula]))
    cat("Desvio padrão:", sd(pibid$Notas[pibid$Aula == aula]), "\n\n")
}

# Gráficos Boxplot
ggplot(pibidnf, aes(x = Turma, y = Notas)) +
    geom_boxplot() +
    xlab("Turma") +
    ylab("Notas") +
    ggtitle("Boxplot das Notas por Turma")

ggplot(pibid, aes(x = Avaliador, y = Notas)) +
    geom_boxplot() +
    xlab("Avaliador") +
    ylab("Notas") +
    ggtitle("Boxplot das Notas por Avaliador")

# Notas medianas por aula para cada turma
medianas <- data.frame(
    Aulas = rep(aulas, each = 3),
    Turmas = rep(turmas, times = length(aulas)),
    Mediana = c(3.350, 5.415, 6.370, 2.465, 6.480, 7.100, 6.700, 3.585, 6.270, 7.265, 8.170, 3.330)
)

ggplot(medianas, aes(x = Aulas, y = Mediana, group = Turmas, shape = Turmas, colour = Turmas)) +
    geom_line() +
    geom_point(size = 3) +
    ylim(0, 10) +
    scale_color_manual(values = c("#999999", "#333333", "#666666")) +
    xlab("Aulas") +
    ylab("Nota mediana") +
    ggtitle("Notas medianas ao longo das aulas por turma")

# Testes de hipótese
# Shapiro-Wilk para normalidade
for (turma in turmas) {
    cat("Shapiro-Wilk Teste de Normalidade para Turma:", turma, "\n")
    print(shapiro.test(pibidnf$Notas[pibidnf$Turma == turma]))
}

for (avaliador in avaliadores) {
    cat("Shapiro-Wilk Teste de Normalidade para Avaliador:", avaliador, "\n")
    print(shapiro.test(pibid$Notas[pibid$Avaliador == avaliador]))
}

# Teste de Kruskal-Wallis e Dunn
cat("Teste de Kruskal-Wallis para Turmas:\n")
print(kruskal.test(Notas ~ Turma, data = pibidnf))

cat("Dunn Teste para Turmas:\n")
print(dunn.test(pibidnf$Notas, pibidnf$Turma, kw = FALSE))

cat("Teste de Kruskal-Wallis para Avaliadores:\n")
print(kruskal.test(Notas ~ Avaliador, data = pibid))

cat("Dunn Teste para Avaliadores:\n")
print(dunn.test(pibid$Notas, pibid$Avaliador, kw = FALSE))

# Análise de questionário
read_csv_utf8 <- function(file_path) {
    read.csv2(file_path, dec = ".", header = TRUE, strip.white = TRUE, fileEncoding = "UTF-8")
}
questionario <- read_csv_utf8("questionarios.csv")

Q1 <- read.csv2(file.choose(), dec = ".", header = TRUE, strip.white = TRUE)
Q2 <- read.csv2(file.choose(), dec = ".", header = TRUE, strip.white = TRUE)
Q3 <- read.csv2(file.choose(), dec = ".", header = TRUE, strip.white = TRUE)
Q4 <- read.csv2(file.choose(), dec = ".", header = TRUE, strip.white = TRUE)

# Visualizar nomes das colunas
names(questionario)
names(Q1)

# Gráficos de barras para questionários
plot_questionario <- function(df, questao) {
    ggplot(df, aes(x = Ensino, y = FR., fill = Respostas)) +
        geom_bar(stat = "identity", colour = "black") +
        scale_fill_brewer(palette = "Greys") +
        xlab("Tipo de Ensino") +
        ylab("FR%") +
        labs(fill = questao) +
        labs(title = questao) +
        theme(plot.title = element_text(hjust = 0)) +
        theme(legend.position = "bottom")
}

QS1 <- plot_questionario(Q1, "Questão 1")
QS2 <- plot_questionario(Q2, "Questão 2")
QS3 <- plot_questionario(Q3, "Questão 3")
QS4 <- plot_questionario(Q4, "Questão 4")

grid.arrange(QS1, QS2, QS3, QS4, nrow = 2)

# Qui-Quadrado para questionários
chi_square_analysis <- function(matrix_data) {
    cat("Matriz de Contingência:\n")
    print(matrix_data)
    chisq <- chisq.test(matrix_data)
    cat("Resultado do Teste Qui-Quadrado:\n")
    print(chisq)
    cat("Resíduos:\n")
    print(chisq$residuals)
    barplot(prop.table(matrix_data) * 100, legend.text = rownames(matrix_data))
}

Q1_matrix <- matrix(c(0, 1, 2, 3, 18, 15, 10, 11, 7, 15, 1, 3), nrow = 6, byrow = 2)
colnames(Q1_matrix) <- c("Convencional", "Técnico")
rownames(Q1_matrix) <- c(
    "0- Não respondeu", "1- Não há na escola", "2- Nunca é usado",
    "3- Raramente é usado", "4- De vez em quando", "5- Sempre é usado"
)

chi_square_analysis(Q1_matrix)

Q2_matrix <- matrix(c(5, 4, 3, 42, 26, 6), nrow = 3, byrow = 2)
colnames(Q2_matrix) <- c("Convencional", "Técnico")
rownames(Q2_matrix) <- c("0- Não respondeu", "3 - Regular", "5 - Ótimo")

chi_square_analysis(Q2_matrix)

Q3_matrix <- matrix(c(32, 44, 6, 4), nrow = 2, byrow = 2)
colnames(Q3_matrix) <- c("Convencional", "Técnico")
rownames(Q3_matrix) <- c("Não", "Sim")

chi_square_analysis(Q3_matrix)

Q4_matrix <- matrix(c(4, 9, 1, 1, 8, 4, 14, 13, 8, 19, 3, 2), nrow = 6, byrow = 2)
colnames(Q4_matrix) <- c("Convencional", "Técnico")
rownames(Q4_matrix) <- c(
    "0- Não respondeu", "1- Não há na escola", "2- Nunca é usado",
    "3- Raramente é usado", "4- De vez em quando", "5- Sempre é usado"
)

chi_square_analysis(Q4_matrix)
