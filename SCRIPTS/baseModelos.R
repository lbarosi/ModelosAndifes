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
suppressMessages(library("ggthemes"))
suppressMessages(library("dgof"))

################

######Lendo Dados
#Cursos INEP 2015
cursos <- read.csv2("../INEP/2015/DADOS/DM_CURSO.CSV", sep = "|", fileEncoding = "LATIN1")

#Apenas IFES
cursos <- filter(cursos, CO_CATEGORIA_ADMINISTRATIVA == 1 &
                         CO_ORGANIZACAO_ACADEMICA == 1 & DS_MODALIDADE_ENSINO == "Presencial")

#Selecionando Variaveis
cursosAREAOCDE <- select(cursos, CO_OCDE_AREA_GERAL, CO_OCDE_AREA_ESPECIFICA, NO_REGIAO_CURSO, NO_IES,NO_MUNICIPIO_CURSO, SGL_UF_CURSO, 
                         NO_CURSO, DS_GRAU_ACADEMICO, DS_MODALIDADE_ENSINO, QT_MATRICULA_CURSO,
                         QT_CONCLUINTE_CURSO, QT_INGRESSO_CURSO)

#Totalizando por AreaOCDE
totAREAOCDE <- ddply(cursosAREAOCDE, c("CO_OCDE_AREA_GERAL"), summarize, 
                     Cursos = length(NO_CURSO),
                     Ingressantes = sum(QT_INGRESSO_CURSO, na.rm = TRUE),
                     Matriculados = sum(QT_MATRICULA_CURSO, na.rm = TRUE),
                     Concluintes = sum(QT_CONCLUINTE_CURSO, na.rm = TRUE))

brAREAOCDE <- melt(totAREAOCDE, id.vars = "CO_OCDE_AREA_GERAL")
brAREAOCDE$regiao <- "BR"
brAREAOCDE <- select(brAREAOCDE, regiao, CO_OCDE_AREA_GERAL, variable, value)
names(brAREAOCDE) <- c("Região", "OCDE", "tipo", "Valor")

#Totalizando por AreaOCDE por Regiao
totregiaoAREAOCDE <- ddply(cursosAREAOCDE, c("NO_REGIAO_CURSO", "CO_OCDE_AREA_GERAL"), summarize, 
                     Cursos = length(NO_CURSO),
                     Ingressantes = sum(QT_INGRESSO_CURSO, na.rm = TRUE),
                     Matriculados = sum(QT_MATRICULA_CURSO, na.rm = TRUE),
                     Concluintes = sum(QT_CONCLUINTE_CURSO, na.rm = TRUE))


regioesAREAOCDE <- melt(totregiaoAREAOCDE, id.vars = c("NO_REGIAO_CURSO" ,"CO_OCDE_AREA_GERAL"))
names(regioesAREAOCDE) <- c("Região", "OCDE", "tipo", "Valor")
#####################

AREAOCDEcompleto <- rbind(brAREAOCDE, regioesAREAOCDE)

AREAOCDEcompleto$OCDE <- factor(AREAOCDEcompleto$OCDE)

levels(AREAOCDEcompleto$OCDE) <- c("Educação", "Humanidades", "Ciências Sociais", "Ciências",
                                   "Engenharia", "Agricultura", "Saúde", "Serviços")


cursosOCDE <- ggplot(filter(AREAOCDEcompleto, tipo == "Cursos"), aes(x = OCDE, y = Valor)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Região, scales = "free") +
  ggtitle("Número de Cursos por area OCDE") +
  labs(x = "", y = "") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1))+
  theme(legend.position = "none") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))




matriculadosOCDE <- ggplot(filter(AREAOCDEcompleto, tipo == "Matriculados"), aes(x = OCDE, y = Valor, fill = "orchid")) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Região, scales = "free") +
  ggtitle("Número de Matriculados por area OCDE") +
  labs(x = "", y = "") +
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1))+
  theme(legend.position = "none") +
  theme(legend.text = element_text(size = 6))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
#---------------------------------------------------
#Kolmogorov-Smirnov Discrete Test

BRAcursos <- filter(AREAOCDEcompleto, tipo == "Cursos", Região == "BR") %>% select(Valor) %>% data.matrix()
COcursos <- filter(AREAOCDEcompleto, tipo == "Cursos", Região == "Centro-Oeste") %>% select(Valor) %>% data.matrix()
NEcursos <- filter(AREAOCDEcompleto, tipo == "Cursos", Região == "Nordeste") %>% select(Valor) %>% data.matrix()
SEcursos <- filter(AREAOCDEcompleto, tipo == "Cursos", Região == "Sudeste") %>% select(Valor) %>% data.matrix()
Scursos <- filter(AREAOCDEcompleto, tipo == "Cursos", Região == "Sul") %>% select(Valor) %>% data.matrix()
Ncursos <- filter(AREAOCDEcompleto, tipo == "Cursos", Região == "Norte") %>% select(Valor) %>% data.matrix()


p1 <- ks.test(BRAcursos, COcursos, simulate.p.value = TRUE, B = 10000 )[[2]]
p2 <- ks.test(BRAcursos, NEcursos, simulate.p.value = TRUE, B = 10000 )[[2]]
p3 <- ks.test(BRAcursos, SEcursos, simulate.p.value = TRUE, B = 10000 )[[2]]
p4 <- ks.test(BRAcursos, Scursos, simulate.p.value = TRUE, B = 10000 )[[2]]
p5 <- ks.test(BRAcursos, Ncursos, simulate.p.value = TRUE, B = 10000 )[[2]]

p <- c(p1,p2,p3,p4,p5)

#------------------------------------------------