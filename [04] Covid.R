rm(list = ls())

# INÍCIO
#==========================================================================================================
# Pacotes -----------------------------------------------------------------

library(countrycode)
library(seasonal)
library(readxl)
library(parsedate)
library(tidyverse)
library(mFilter)
library(lubridate)
library(ggpubr)
library(ggrepel)
library(modelr)
library(tsibble)
library(tsibbledata)
library(feasts)
library(fabletools)
library(fable)
library(forecast)
library(sidrar)
library(rbcb)
library(xts)
library(ggpubr)


# detach("package:vars", unload = TRUE)
# detach("package:MASS", unload = TRUE)

# Cores RGB ---------------------------------------------------------------

pal_v  <- paletavermelha <- c("#660000", "#990000", "#CC0000", "#FF0000", "#FF3333", "#FF6666", "#FF9999", "#FFCCCC")
pal_a  <- paletaazul     <- c("#003366", "#004C99", "#0066CC", "#0080FF", "#3399FF", "#66B2FF", "#99CCFF", "#CCE5FF")
pal_vd <- paletaverde    <- c("#336600", "#4C9900", "#66CC00", "#80FF00", "#99FF33", "#82FF66", "#CCFF99", "#E5FFCC")
pal_c  <- paletacinza    <- c("#000000", "#202020", "#404040", "#606060", "#808080", "#A0A0A0", "#C0C0C0", "#E0E0E0", "#FFFFFF")
fundo  <- theme(plot.background  = element_rect(fill = "#f4f5f6", color = NA), 
                panel.background = element_rect(fill = "#f4f5f6", color = NA))


# Temas GGPLOT -------------------------------------------------------------------

temasggplot <- function(fonte         = "Arial", 
                        cor           = paletacinza[1],
                        titlesize     = 11, 
                        subtitlesize  = 10, 
                        captionsize   = 8,
                        axistextsize  = 9, 
                        axistitlesize = 9) {
  
  g1 <<- theme(panel.grid.major     = element_blank(), 
               panel.grid.minor     = element_blank(), 
               panel.background     = element_rect(fill = "#FFFFFF", color = cor, size = 0.5), 
               plot.title           = element_text(family = fonte, color = cor, size = titlesize, face = "bold", margin = margin(t = 5, b = 2, unit = "pt")),
               plot.subtitle        = element_text(family = fonte, color = cor, size = subtitlesize),
               plot.caption         = element_text(family = fonte, color = cor, size = captionsize, margin = margin(b = 5, unit = "pt")),
               plot.margin          = unit(c(0,12,0,0), unit = "pt"),
               axis.line.x.bottom   = element_blank(), 
               axis.line.x.top      = element_blank(), 
               axis.line.y.left     = element_blank(),
               axis.line.y.right    = element_blank(), 
               axis.text.y          = element_text(family = fonte, color = cor, size = axistextsize,  margin = margin(l = 5, unit = "pt")),
               axis.text.x          = element_text(family = fonte, color = cor, size = axistextsize,  margin = margin(b = 5, unit = "pt")),
               axis.title.y         = element_text(family = fonte, color = cor, size = axistitlesize, hjust = 0.9, margin = margin(l = 3, unit = "pt")),
               axis.title.x         = element_text(family = fonte, color = cor, size = axistitlesize, hjust = 0.1, margin = margin(b = 3, unit = "pt")),
               legend.background    = element_blank(), 
               legend.key           = element_rect(fill = "#FFFFFF", size= 0.5), 
               legend.position      = c(0,1), 
               legend.justification = c(0,1)); g_1 <<- g1
               
               g2 <<- g1 + theme(panel.background     = element_blank(), 
                                 axis.line.y.left     = element_line(color = cor, size = 0.3), 
                                 axis.line.x.bottom   = element_line(color = cor, size = 0.3)); g_2 <<- g2
               
               g3 <<- g2 + theme(panel.grid.major.x = element_line(color = paletacinza[5], linetype = 3, size = 0.2), 
                                 panel.grid.major.y = element_line(color = paletacinza[5], linetype = 3, size = 0.2),
                                 panel.grid.minor.x = element_line(color = paletacinza[5], linetype = 3, size = 0.2),
                                 panel.grid.minor.y = element_line(color = paletacinza[5], linetype = 3, size = 0.2)); g_4 <<- g3
               
               g4 <<- g1 + theme(panel.background    = element_blank(),
                                 axis.line.x.bottom  = element_blank(), 
                                 axis.line.y.left    = element_blank(), 
                                 axis.title.x        = element_blank(), 
                                 axis.title.y        = element_blank(), 
                                 axis.text.x.bottom  = element_blank(),
                                 axis.ticks.x.bottom = element_blank(),
                                 panel.grid          = element_blank(),
                                 legend.position     = "none")
               
}; temasggplot()

leg01 <- theme(legend.justification = c(0,1), legend.position = c(0,1))
leg00 <- theme(legend.justification = c(0,0), legend.position = c(0,0))
leg11 <- theme(legend.justification = c(1,1), legend.position = c(1,1))
leg10 <- theme(legend.justification = c(1,0), legend.position = c(1,0))


#==========================================================================================================


# FUNÇÕES
#==========================================================================================================
# Número-Índice -----------------------------------------------------------

Ind <- function(x, nivel, final = FALSE) {
  # x     >> taxa de variação da série 
  # nível >> Nivelamento: 1 ou 100?
  # final >> Nivalmento n início ou final da série?
  if (!is.vector(x) || length(nivel) > 1 || !is.numeric(nivel)  || !is.logical(final) || length(final) >1) {
    stop("checar entradas!")
  } else {
    if (final == FALSE) {
      indice    <- nivel;
      for (i in 2:length(x)) {
        indice <- c(indice, last(indice)*(1+x[i]))
      }
    } else {
      indice <- nivel; 
      for (i in (length(x)):2) {
        indice <- c(indice, (last(indice)/(1+x[i])))
      }
      indice <- rev(indice)  #inverter a ordem do vetor
    }  
    indice
  }
}


# Multiplicador de preços ----------------------------------------------------

Mp <- function(x, final = FALSE) {
  # x     >> taxa de variação do índice de preços 
  # final >> Normalização no iníco ou no final?
  if (!is.vector(x) || !is.logical(final) || length(final) >1) {
    stop("checar entradas!")
  } else {
    if (final == FALSE) {
      indice    <- 1;
      for (i in 2:length(x)) {
        indice <- c(indice, last(indice)/(1+x[i]))
      }
    } else {
      indice <- 1; 
      for (i in (length(x)):2) {
        indice <- c(indice, (last(indice)*(1+x[i])))
      }
      indice <- rev(indice)  #inverter a ordem do vetor
    }  
    indice
  }
}


# Produzir séries reais ---------------------------------------------------

Real <- function(x, y, final = FALSE) {
  # x     >> taxa de variação dos preços
  # y     >> série nominal
  # final >> trazer para preços do início ou final do período (padrão: início)
  
  if (!is.vector(x) || !is.vector(y) || !(near(length(x),length(y))) || !is.logical(final) || length(final) > 1) {
    stop("checar entradas!")
  } else {
    if (final == FALSE) {
      indice    <- 1;
      for (i in 2:length(x)) {
        indice <- c(indice, last(indice)/(1+x[i]))
      }
    } else {
      indice <- 1; 
      for (i in (length(x)):2) {
        indice <- c(indice, (last(indice)*(1+x[i])))
      }
      indice <- rev(indice)  #inverter a ordem do vetor
    }  
    indice*y
  }
}


# Diferença em logs -------------------------------------------------------

diflog <- function(x, Mult = FALSE, n = 1) { 
  x1 <- log(x) 
  x2 <- x1 - lag(x1, n)
  
  if (Mult == TRUE) {x2 <- x2*100} else {}
  
  x2
}



# Taxa de variação --------------------------------------------------------

Dif <- function(x, Mult = FALSE, n = 1) {
  
  x1 <- (x/lag(x,n)) - 1
  
  if (Mult == TRUE) {x1 <- x1*100} else {}
  
  x1
  
}


#==========================================================================================================


# INPUTS
#==========================================================================================================
# IPCA mensal -------------------------------------------------------------

sidra_1737 <- get_sidra(api = "/t/1737/n1/all/v/63/p/all/d/v63%202")
ipcamensal <- 
  sidra_1737 %>%
  as_tibble() %>%
  select(`Mês`, Valor) %>%
  transmute(t = as.yearmon(`Mês`) %>% yearmonth(), IPCA = Valor)

# IPCA trimestral ---------------------------------------------------------

trimestral <- function() {
  x <- 
    ipcamensal %>%
    mutate(aa = year(t), tt = quarter(t)) %>%
    filter(aa == last(aa)) %>%
    filter(tt == last(tt)) %>%
    summarise(n = n(), tt = tt, aa = aa)
  
  aa <- x %>% pull(aa)
  tt <- x %>% pull(tt)
  n  <- x %>% pull(n)
  
  if (n < 3) {
    ipcatrimestral <<- 
      ipcamensal %>%
      filter(!(year(t) == aa & quarter(t) == tt)) %>%
      mutate(aa = year(t), tt = quarter(t)) %>%
      filter(aa != 1979) %>%
      group_by(aa, tt) %>%
      mutate(row = row_number()) %>%
      mutate(Ind = Ind(IPCA/100,1,FALSE)) %>%
      filter(row == 3) %>%
      ungroup() %>%
      transmute(t    = paste(aa, tt, sep = ":") %>% as.yearqtr(format = "%Y:%q") %>% yearquarter(), 
                IPCA = (Ind - 1)*100)
  } else if (n == 3) {
    ipcatrimestral <<- 
      ipcamensal %>%
      mutate(aa = year(t), tt = quarter(t)) %>%
      filter(aa != 1979) %>%
      group_by(aa, tt) %>%
      mutate(row = row_number()) %>%
      mutate(Ind = Ind(IPCA/100,1,FALSE)) %>%
      filter(row == 3) %>%
      ungroup() %>%
      transmute(t    = paste(aa, tt, sep = ":") %>% as.yearqtr(format = "%Y:%q") %>% yearquarter(), 
                IPCA = (Ind - 1)*100)
  } else {
    stop("analizar transformação do IPCA trimestral!")
  }
}; trimestral() 


# IPCA anual --------------------------------------------------------------

anual <- function() {
  x <- 
    ipcamensal %>%
    mutate(aa = year(t), mm = month(t)) %>%
    filter(aa == last(aa)) %>%
    filter(mm == last(mm))
  
  aa <- x %>% pull(aa)
  mm <- x %>% pull(mm)
  
  if (mm < 12) {
    ipcaanual <<- 
      ipcamensal %>%
      filter(year(t) != aa) %>%
      filter(year(t) != 1979) %>%
      mutate(aa = year(t), mm = month(t)) %>%
      group_by(aa) %>%
      mutate(Ind = Ind(IPCA/100,1,FALSE)) %>%
      filter(Ind == last(Ind)) %>%
      ungroup() %>%
      transmute(t = aa, IPCA = (Ind - 1)*100)
    
  } else if (mm == 12) {
    ipcaanual <<- 
      ipcamensal %>%
      filter(year(t) != 1979) %>%
      mutate(aa = year(t), mm = month(t)) %>%
      group_by(aa) %>%
      mutate(Ind = Ind(IPCA/100,1,FALSE)) %>%
      filter(Ind == last(Ind)) %>%
      ungroup() %>%
      transmute(t = aa, IPCA = (Ind - 1)*100)
    
  } else {
    stop("analizar transformação do IPCA anual")
  }
  
}; anual()


# PIB nominal trimestral sem ajuste sazonal -------------------------------

sidra_1846 <- get_sidra(api = "/t/1846/n1/all/v/all/p/all/c11255/90707/d/v585%200")
# sidra_1846 %>% colnames() %>% data.frame()
# sidra_1846 %>% pull(`Unidade de Medida`) %>% unique()
pib.tn <- 
  sidra_1846 %>%
  select(Trimestre, Valor) %>%
  rename(t = Trimestre, y = Valor) %>%
  transmute(t = paste(str_extract(t, "\\d*$"), str_extract(t, "^\\d"), sep = ":") %>% as.yearqtr(format = "%Y:%q") %>% yearquarter(), y)


# PIB nominal anual -------------------------------------------------------

pib.an <- 
  pib.tn %>%
  mutate(aa = year(t)) %>%
  group_by(aa) %>%
  summarise(y = sum(y)) %>%
  transmute(t = aa, y)


# RTN sem ajuste: mensal nominal -------------------------------------------

col <- c("Descrição", format(seq(yearmonth("1997 1"), yearmonth("2021 4"), 1), "%b %Y"))
x   <- read_excel("serie_historica_abr21.xlsx", sheet = 2, range = "A6:KG73", na = "-", col_names = col)
rtn1.mn <- 
  x %>%
  pivot_longer(`Jan 1997`:`Abr 2021`, names_to = "t", values_to = "y") %>%
  mutate(t     = t %>% as.yearmon() %>% yearmonth(), 
         Cod   = str_extract(`Descrição`, "^\\d{1,2}\\.(\\d{1,2}(\\.\\d{1,2})?)?"), 
         Desc1 = sub("^\\d{1,2}\\.(\\d{1,2}(\\.\\d{1,2})?)?", "", `Descrição`), 
         Desc2 = trimws(sub("\\d\\/$", "", Desc1)), 
         Desc3 = sub("^-\\s{1,2}", "", Desc2)) %>%
  transmute(t,Cod,y, Descr = Desc3) %>%
  group_by(t) %>%
  mutate(Serie = row_number()) %>%
  relocate(t, y, Serie) %>%
  ungroup() 


rtn1.mn %>% select(Serie, Cod, Descr) %>% unique() %>% print(n=Inf)
# Novas transferências EE/MM: Séries c(40, 43, 45, 47, 48, 50)


# RTN sem ajuste: mensal real ------------------------------------

rtn1.mr <-
  rtn1.mn %>%
  left_join(ipcamensal, by = "t") %>%
  group_by(Cod) %>%
  mutate(Real = Real(IPCA/100, y, TRUE)) %>%
  ungroup() %>%
  transmute(t, Serie, Cod, y = Real, Descr) %>%
  as_tsibble(index = t, key = c(Serie, Cod, Descr))


# RTN sem ajuste: trimestral nominal --------------------------------------------------

rtn1.tn <- 
  rtn1.mn %>%
  ungroup() %>%
  mutate(aa = year(t), tt = quarter(t)) %>%
  group_by(Serie, Cod, Descr, aa, tt) %>%
  summarise(y = sum(y, na.rm = TRUE)) %>%
  mutate(t = paste(aa, tt, sep = ":") %>% as.yearqtr(format = "%Y:%q") %>% yearquarter()) %>%
  ungroup() %>%
  select(-aa,-tt) %>%
  relocate(t,y)

# RTN sem ajuste: trimestral real -----------------------------------------------------

rtn1.tr <- 
  rtn1.tn %>%
  left_join(ipcatrimestral, by = "t") %>%
  group_by(Serie, Cod, Descr) %>%
  mutate(Real = Real(IPCA/100, y, TRUE)) %>%
  transmute(t, y = Real) %>%
  relocate(t,y)


# RTN sem ajuste: anual nominal -------------------------------------------------------

rtn1.an <- 
  rtn1.mn %>%
  mutate(aa = year(t), mm = month(t)) %>%
  group_by(Serie, Cod, Descr, aa) %>%
  summarise(y = sum(y, na.rm = TRUE)) %>%
  transmute(t = aa, y) %>%
  ungroup() %>%
  relocate(t,y)




# RTN sem ajuste: anual real ----------------------------------------------------------

rtn1.ar <- 
  rtn1.an %>%
  filter(t != 2021) %>%
  left_join(ipcaanual, by = "t") %>%
  group_by(Serie, Cod, Descr) %>%
  mutate(Real = Real(IPCA/100, y, TRUE)) %>%
  transmute(t, y = Real) %>%
  relocate(t,y)





# Gastos da pandemia: mensal nominal ------------------------------------------------------

x <- read_excel("serie_historica_abr21.xlsx", sheet = "4.1", range = "A5:P28", na = "-", col_names = TRUE)
n <- length(colnames(x)) - 1; colnames(x) <- c("Descr", as.character(as.yearmon(seq(ymd(20200201), by = "month", length = n))))

pandemia.mn <-
  x %>%
  pivot_longer(c(`Fev 2020`:`Abr 2021`), names_to = "t", values_to = "y") %>%
  mutate(t = t %>% as.yearmon() %>% yearmonth()) %>%
  group_by(t) %>%
  mutate(Serie = seq_along(Descr)) %>%
  relocate(Serie, t, y) %>%
  ungroup(); rm(x, n)


# Gastos da pandemia: mensal real -----------------------------------------

pandemia.mr <-
  pandemia.mn %>%
  group_by(Serie) %>%
  left_join(ipcamensal, by = "t") %>%
  mutate(y = Real(IPCA/100, y, TRUE)) %>%
  select(-IPCA)


#==========================================================================================================


# COVID-19
#==========================================================================================================
# Gastos da pandemia: gráfico ------------------------------------------

x <- x1 <- 
  pandemia.mr %>%
  rename(Abr2021 = y) %>%
  group_by(Serie, Descr) %>%
  summarise(Abr2021 = sum(Abr2021)) %>%
  mutate(PIB2020 = pull(filter(pib.an, t == 2020)))


IPCAacum <- 
  ipcamensal %>%
  as_tsibble(index = t) %>%
  filter_index("2020-6" ~ .) %>%
  mutate(Ind = Ind(IPCA/100, 1, FALSE)) %>%
  filter(t == last(t)) %>%
  pull(Ind) 

x <- 
  x %>% 
  mutate(Jun2020 = Abr2021/IPCAacum, 
         Part    = (Jun2020/PIB2020)*100) %>% 
  filter(!str_detect(Descr, "\\d+") | Serie == 22) %>%
  mutate(Grupo = case_when(Serie %in% c(3,10,19,21) ~ "Ministério \nda saúde", 
                           Serie == 5  ~ "Transferências \nEst./Mun",
                           Serie %in% c(7,8)  ~ "Auxílio \nemergencial \n& BF",
                           Serie %in% c(9,14) ~ "Prog. Man. Emp \n& Folha salarial", 
                           Serie == 12 ~ "Aquisição \nde vacinas", 
                           TRUE ~ "Outras")) %>%
  select(-PIB2020) %>%
  pivot_longer(c(Abr2021:Part), names_to = "k", values_to = "y") %>%
  group_by(Grupo, k) %>%
  summarise(y = sum(y)) %>%
  pivot_wider(names_from = k, values_from = y) %>%
  mutate(x       = factor(Grupo), 
         Abr2021 = Abr2021/1000, 
         label   = paste0("R$", round(Abr2021, digits = 0)," (", format(round(Part, digits = 1), decimal.mark = ","), "%", ")"), 
         y       = Abr2021) %>%
  ungroup() %>%
  select(x, y, label)

limy <- range(x$y)

x %>% 
  ggplot(aes(fct_reorder(x, y), y, fill = y)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = label), 
            size = 3, 
            color = pal_c[1], 
            fontface = "bold",
            nudge_y = 21) + 
  coord_flip() + 
  scale_y_continuous(NULL, expand = c(0,0), limits = c(0, limy[2] + 42)) + 
  scale_fill_gradient(low = pal_a[3], high = pal_a[1]) + 
  labs(title    = "DESPESAS DO GOVERNO CENTRAL: COMBATE AO COVID-19 (2020-21)", 
       subtitle = "R$ em bilhões de abril 2021 (% do PIB 2020)", 
       caption  = "Fonte: STN/IBGE") + 
  g4

x1 %>%
  mutate(Jun2020 = Abr2021/IPCAacum, 
         Part    = (Jun2020/PIB2020)*100) %>%
  filter(Serie == 1) %>% 
  mutate(y = Jun2020/1000, Part = (Jun2020/PIB2020)*100)

 
# Gastos com a pandemia: tabela completa ----------------------------------

pandemia.mr %>%
  rename(Abr2021 = y) %>%
  group_by(Serie, Descr) %>%
  summarise(Abr2021 = sum(Abr2021)) %>%
  mutate(PIB2020 = pull(filter(pib.an, t == 2020))) %>%
  mutate(Jun2020 = Abr2021/IPCAacum, 
         Part    = (Jun2020/PIB2020)*100) %>%
  select(-PIB2020) %>%
  print(n=Inf)
#==========================================================================================================