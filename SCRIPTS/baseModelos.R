#################
#BIBLIOTECAS
suppressMessages(library("ggplot2"))
suppressMessages(library("stringi"))
suppressMessages(library("stringr"))
suppressMessages(library("readxl"))
suppressMessages(library("xlsx"))
suppressMessages(library("xtable"))
suppressMessages(library("dplyr"))
suppressMessages(library("plyr"))
suppressMessages(library(magrittr))
suppressMessages(library(reshape2))
################

######Lendo Dados
#Cursos INEP 2015
cursos <- read.csv2("../INEP/2015/DADOS/DM_CURSO.CSV", sep = "|", fileEncoding = "LATIN1")

#Apenas IFES
cursos <- filter(cursos, CO_CATEGORIA_ADMINISTRATIVA == 1 &
                         CO_ORGANIZACAO_ACADEMICA == 1)

#Selecionando Variaveis
cursosAREAOCDE <- select(cursos, CO_OCDE_AREA_GERAL, CO_OCDE_AREA_ESPECIFICA, NO_IES,NO_MUNICIPIO_CURSO, SGL_UF_CURSO, 
                         NO_CURSO, DS_GRAU_ACADEMICO, DS_MODALIDADE_ENSINO, QT_MATRICULA_CURSO,
                         QT_CONCLUINTE_CURSO, QT_INGRESSO_CURSO)

#Totalizando por AreaOCDE
totAREAOCDE <- ddply(cursosAREAOCDE, c("CO_OCDE_AREA_GERAL"), summarize, 
                     Cursos = length(NO_CURSO),
                     Ingressantes = sum(QT_INGRESSO_CURSO, na.rm = TRUE),
                     Matriculados = sum(QT_MATRICULA_CURSO, na.rm = TRUE),
                     Concluintes = sum(QT_CONCLUINTE_CURSO, na.rm = TRUE))

dataMelt <- melt(totAREAOCDE, id.vars = "CO_OCDE_AREA_GERAL")


ggplot(dataMelt, aes(x = factor(CO_OCDE_AREA_GERAL), y = value, fill = variable)) + geom_bar(stat = "identity", position = "dodge")

ggplot(filter(dataMelt, variable == "Cursos"), aes(x = factor(CO_OCDE_AREA_GERAL), y = value, fill = variable)) + geom_bar(stat = "identity", position = "dodge")
