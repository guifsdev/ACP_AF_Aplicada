#############################################################################
#                       UNIVERSIDADE FEDERAL FLUMINENSE
#                               Orientação de TCC
# Prof. Ariel Levy
# Aluno: Guilherme Souza
#
#       Regressão Linear Múltipla sobre os dados do CPC do Enade 2015                        
#############################################################################
library(tidyverse)
library(car)
library(corrplot)
library(ggplot2)
library(scales)
library(DMwR)

setwd("../dados")
cpc <- readxl::read_excel("bd_cpc_2015.xlsx")

#CPC faixa: notas dos cursos####

barplot_cpc_faixa <- function(data) {
    t <- as.data.frame(table(data$`CPC Faixa`))
    print(t)
    print(ggplot(t,
                 aes(x = Var1,
                     y = Freq/sum(Freq))) + 
              geom_bar(stat = "identity") +
              xlab("") + ylab("Percentual de cursos") +
              scale_y_continuous(labels = scales::percent) + 
              theme(axis.text.x = element_text(angle = 45, hjust = 1)))
    
    t <-t %>%
        mutate(Percentual = percent(Freq/sum(Freq)))
    
    write.csv(t, "../tabelas/tbl-cpc-faixa.csv", quote = FALSE)
}

cpc.faixa <- barplot_cpc_faixa(cpc)


#Sumário estatístico das variáveis preditoras####
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

stats <- cpc %>% 
    dplyr::select(`Concluintes Inscritos`:`CPC Faixa`) %>% 
    dplyr::mutate(`CPC Faixa` = as.numeric(`CPC Faixa`))
#knn

get_summary <- function(df, na.handle="") {
    if(na.handle == "knn") {
        df <- knnImputation(as.data.frame(df))
    }
    else if(na.handle == "omit") {
        df <- na.omit(df)
    }
    tmp <- do.call(data.frame, 
                   list(mean = apply(df,2, mean),
                        sd = apply(df, 2, sd),
                        median = apply(df, 2, median),
                        mode = apply(df, 2, getmode),
                        min = apply(df, 2, min),
                        max = apply(df, 2, max),
                        n = apply(df,2, length)))
    
    tmp <- round(tmp, 4)
    if(na.handle == "knn") 
        write.csv(tmp, "../tabelas/tbl-sum-stat-knn.csv", quote = FALSE)
    else if(na.handle == "omit") 
        write.csv(tmp, "../tabelas/tbl-sum-stat-omit.csv", quote = FALSE)
    else
        write.csv(tmp, "../tabelas/tbl-sum-stat.csv", quote = FALSE)
    #write.csv(tmp, "../tabelas/tbl-sum-stat.csv", quote = FALSE)
    tmp
}

df <- get_summary(stats, na.handle = "omit")




#Gráfico do percentual de cursos por área de enquadramento####
qtd.cursos <- cpc %>% 
    group_by(`Área de Enquadramento`) %>% 
    summarise(N.Cursos = n())

print(ggplot(qtd.cursos,
             aes(x = reorder(`Área de Enquadramento`, -N.Cursos),
                 y = N.Cursos/sum(N.Cursos))) + 
          geom_bar(stat = "identity") +
          xlab("") + ylab("Percentual de cursos") +
          scale_y_continuous(labels = scales::percent) + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1)))

#Tabela de cursos por regiões####
cursos.all <- cpc %>% 
    select(`Área de Enquadramento`, `Sigla da UF`)

cursos.sudeste <- cursos.all %>% 
    filter(`Sigla da UF` %in% c('RJ', 'SP', 'ES', 'MG')) %>% 
    mutate(Região = 'Sudeste')

cursos.sul <- cursos.all %>% 
    filter(`Sigla da UF` %in% c('PR', 'SC', 'RS')) %>% 
    mutate(Região = 'Sul')
    
cursos.centro.oeste <- cursos.all %>% 
    filter(`Sigla da UF` %in% c('MT', 'DF', 'GO', 'MS')) %>% 
    mutate(Região = 'Centro Oeste')
    
cursos.nordeste <- cursos.all %>% 
    filter(`Sigla da UF` %in% c('MA', 'CE', 'RN', 'PB', 'PE', 'AL', 'SE', 'BA', 'PI')) %>% 
    mutate(Região = 'Nordeste')
    
cursos.norte <- cursos.all %>% 
    filter(`Sigla da UF` %in% c('AC', 'RO', 'AM', 'PA', 'TO', 'AP', 'RR')) %>% 
    mutate(Região = 'Norte')
    
joined <- rbind(cursos.sudeste, cursos.sul, cursos.centro.oeste, cursos.nordeste, cursos.norte)
rm(cursos.sudeste, cursos.sul, cursos.centro.oeste, cursos.nordeste, cursos.norte)

get.tbl_cursos_pr <- function(df.in) {
    cursos.por.regiao <- df.in %>% 
        group_by(`Área de Enquadramento`, Região) %>% 
        summarise(N.Cursos = n())
    
    nomes.cursos <- levels(as.factor(cursos.por.regiao$`Área de Enquadramento`))
    nomes.areas <- levels(as.factor(cursos.por.regiao$Região))

    df.out <- data.frame()
    for(i in 1:length(nomes.cursos)) {
        v <- c()
        for(j in 1:length(nomes.areas)) {
            tmp <- cursos.por.regiao %>%
                filter(`Área de Enquadramento` == nomes.cursos[i],
                       Região == nomes.areas[j])
                v <- c(v, tmp$N.Cursos)
        }
        df.out <- rbind(df.out, v)
    }
    colnames(df.out) <- nomes.areas
    row.names(df.out) <- nomes.cursos

    df.out$Total <- rowSums(df.out[1:5])
    
    df.out <- df.out[with(df.out, order(-Total)),]
    
    
    df.out
}

cursos.por.regiao <- get.tbl_cursos_pr(joined)

write.csv(cursos.por.regiao, "../tabelas/cursos-por-regiao.csv", quote = FALSE)

#Gráfico do percentual de cursos por Organização Academica####
org.acad <- cpc %>% 
    select(`Organização Acadêmica`)

org.acad <- as.data.frame(table(org.acad))


print(ggplot(org.acad,
             aes(x = reorder(org.acad, -Freq),
                 y = Freq/sum(Freq))) + 
          geom_bar(stat = "identity") +
          xlab("") + ylab("Percentual de cursos") +
          scale_y_continuous(labels = scales::percent) + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1)))
#IES por Organização Academica####

ies <- cpc %>% 
    select(`Sigla da IES`, `Código da IES`, `Organização Acadêmica`) %>% 
    group_by(`Código da IES`, `Sigla da IES`, `Organização Acadêmica`) %>% 
    summarise(N.Cursos = n())

ies <- as.data.frame(table(ies$`Organização Acadêmica`))

colnames(ies) <- c("Organização Acadêmica", "Total")
rownames(ies) <- c()

ies <- ies[with(ies, order(-Total)),]
write.csv(ies, "../tabelas/ies-org-acad.csv", quote = FALSE)

#Grafico do percentual de cursos por Categoria Administrativa####

cat.adm <- cpc %>% 
    select(`Categoria Administrativa`)

cat.adm <- as.data.frame(table(cat.adm))

print(ggplot(cat.adm,
             aes(x = reorder(cat.adm, -Freq),
                 y = Freq/sum(Freq))) + 
          geom_bar(stat = "identity") +
          xlab("") + ylab("Percentual de cursos") +
          scale_y_continuous(labels = scales::percent) + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1)))

#IES por Categoria Administrativa####
ies <- cpc %>% 
    select(`Sigla da IES`, `Código da IES`, `Categoria Administrativa`) %>% 
    group_by(`Código da IES`, `Sigla da IES`, `Categoria Administrativa`) %>% 
    summarise(N.Cursos = n())

ies <- as.data.frame(table(ies$`Categoria Administrativa`))

colnames(ies) <- c("Categoria Administrativa", "Total")
rownames(ies) <- c()

ies <- ies[with(ies, order(-Total)),]

write.csv(ies, "../tabelas/ies-cat-adm.csv", quote = FALSE)





#Outros####
adm_nac <- dados %>% 
    select(`Área de Enquadramento`, `Categoria Administrativa`,
           `Sigla da UF`, `Sigla da IES`,
           `Nota Contínua do Enade`, `Nota Padronizada - IDD`, `Nr. de Docentes`,
           `Nota Padronizada - Mestres`, `Nota Padronizada - Doutores`,
           `Nota Padronizada - Regime de Trabalho`, `Nota Padronizada - Organização Didático-Pedagógica`,
           `Nota Padronizada - Infraestrutura e Instalações Físicas`, 
           `Nota Padronizada - Oportunidades de Ampliação da Formação`,
           `CPC Contínuo`) %>%
    rename(NCE   = `Nota Contínua do Enade`, 
           NPIDD = `Nota Padronizada - IDD`,
           ND    = `Nr. de Docentes`,
           NPM   = `Nota Padronizada - Mestres`,
           NPD   = `Nota Padronizada - Doutores`,
           NPRT  = `Nota Padronizada - Regime de Trabalho`,
           NPODP = `Nota Padronizada - Organização Didático-Pedagógica`,
           NPIIF = `Nota Padronizada - Infraestrutura e Instalações Físicas`,
           NPOAF = `Nota Padronizada - Oportunidades de Ampliação da Formação`,
           CPCC  = `CPC Contínuo`) %>% 
    filter(`Área de Enquadramento` == "ADMINISTRAÇÃO") %>% 
    arrange(`Sigla da UF`)

cor_tbl <- adm_nac %>% select(`NCE`:`CPCC`)

#tabela de correlação
cor_tbl <- cor(cor_tbl, use = "complete.obs")

write.csv(round(cor_tbl, 4), "../tabelas/cor_tbl.csv", quote = FALSE)
corrplot(cor_tbl, method = "circle")

#scatterplotMatrix(cor_tbl, spread=FALSE, lty.smooth=2, cex = .25, main="Scatter Plot Matrix")









