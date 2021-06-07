rm(list = ls())

# INÍCIO
#==============================================================================
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


#==============================================================================


# FUNÇÕES
#==============================================================================
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


#==============================================================================


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


# EFP trimestral nominal --------------------------------------------------

esferas <- 
  list(Esfera = c("GC-O", "GC-C", "GE", "GM", "GG"), 
       Range  = c("A4:AT33", "A39:AT68", "A74:AT103", "A109:AT138", "A144:AT173"))

efp.tn <- vector("list", length(esferas[["Esfera"]])); names(efp.tn) <- esferas[["Esfera"]]; for (i in seq_along(efp.tn)) {
  
  x <- read_excel("Demonstrativos GG_29_04_21.xlsx", 
                  col_names = TRUE,
                  sheet     = "1.3", 
                  range     = esferas[["Range"]][[i]], 
                  na        = c("n.d.", ""))
  
  x1 <- x %>% colnames()
  x2 <-
    x %>%
    pivot_longer(`2010-I`:`2020-IV`, names_to = "t", values_to = "y") %>%
    rename(Num = x1[1], Descr = x1[2]) %>% 
    select(Num, Descr) %>% 
    filter(is.na(Num)) %>%
    pull(Descr) %>% 
    unique()
  
  x <- 
    x %>%
    pivot_longer(`2010-I`:`2020-IV`, names_to = "t", values_to = "y") %>%
    rename(Num = x1[1], Descr = x1[2]) %>%
    mutate(Num = case_when(Descr == x2[1] ~ "41", Descr == x2[2] ~ "42", TRUE ~ Num), Descr = paste(Num, Descr)) %>%
    select(-Num) %>%
    group_by(t) %>%
    mutate(Serie  = seq_along(Descr),
           Esfera = esferas[["Esfera"]][[i]], 
           Descr  = trimws(sub("\\d{1}/", "", Descr)), 
           t      = case_when(str_detect(t, "-I{1}$") ~ paste0(str_extract(t, "^\\d{4}"),":1"), 
                              str_detect(t, "-I{2}$") ~ paste0(str_extract(t, "^\\d{4}"),":2"), 
                              str_detect(t, "-I{3}$") ~ paste0(str_extract(t, "^\\d{4}"),":3"),
                              str_detect(t, "-IV$")   ~ paste0(str_extract(t, "^\\d{4}"),":4"), 
                              TRUE ~ t), 
           t = t %>% as.yearqtr(format = "%Y:%q") %>% yearquarter()) %>%
    relocate(Esfera, Serie, t, y)
  
  efp.tn[[i]] <- x
  
}
efp.tn <- efp.tn %>% reduce(rbind) %>% mutate(Esfera = factor(Esfera, levels = c("GG", "GC-O", "GC-C", "GE", "GM")))

# x  <- read_excel("Demonstrativos GG_29_04_21.xlsx", sheet = "1.3", range = "A4:AT33", na = c("n.d.", ""), col_names = TRUE)
# x1 <- x %>% colnames()
# x2 <-
#   x %>%
#   pivot_longer(`2010-I`:`2020-IV`, names_to = "t", values_to = "y") %>%
#   rename(Num = x1[1], Descr = x1[2]) %>% 
#   select(Num, Descr) %>% 
#   filter(is.na(Num)) %>%
#   pull(Descr) %>% 
#   unique()
# 
# gfo.tn <- 
#   x %>%
#   pivot_longer(`2010-I`:`2020-IV`, names_to = "t", values_to = "y") %>%
#   rename(Num = x1[1], Descr = x1[2]) %>%
#   mutate(Num = case_when(Descr == x2[1] ~ "41", Descr == x2[2] ~ "42", TRUE ~ Num), Descr = paste(Num, Descr)) %>%
#   select(-Num) %>%
#   group_by(t) %>%
#   mutate(Serie  = seq_along(Descr),
#          Esfera = "GF-O", 
#          Descr  = trimws(sub("\\d{1}/", "", Descr)), 
#          t      = case_when(str_detect(t, "-I{1}$") ~ paste0(str_extract(t, "^\\d{4}"),":1"), 
#                             str_detect(t, "-I{2}$") ~ paste0(str_extract(t, "^\\d{4}"),":2"), 
#                             str_detect(t, "-I{3}$") ~ paste0(str_extract(t, "^\\d{4}"),":3"),
#                             str_detect(t, "-IV$")   ~ paste0(str_extract(t, "^\\d{4}"),":4"), 
#                             TRUE ~ t), 
#          t = t %>% as.yearqtr(format = "%Y:%q") %>% yearquarter()) %>%
#   relocate(Esfera, Serie, t, y)
# 
# x  <- read_excel("Demonstrativos GG_29_04_21.xlsx", sheet = "1.3", range = "A39:AT68",   na = c("n.d.", ""), col_names = TRUE)
# x1 <- x %>% colnames()
# x2 <- 
#   x %>%
#   pivot_longer(`2010-I`:`2020-IV`, names_to = "t", values_to = "y") %>%
#   rename(Num = x1[1], Descr = x1[2]) %>% 
#   select(Num, Descr) %>% 
#   filter(is.na(Num)) %>%
#   pull(Descr) %>% 
#   unique()
# 
# gfc.tn <- 
#   x %>%
#   pivot_longer(`2010-I`:`2020-IV`, names_to = "t", values_to = "y") %>%
#   rename(Num = x1[1], Descr = x1[2]) %>%
#   mutate(Num = case_when(Descr == x2[1] ~ "41", Descr == x2[2] ~ "42", TRUE ~ Num), Descr = paste(Num, Descr)) %>%
#   select(-Num) %>%
#   group_by(t) %>%
#   mutate(Serie  = seq_along(Descr),
#          Esfera = "GF-C", 
#          Descr  = trimws(sub("\\d{1}/", "", Descr)), 
#          t      = case_when(str_detect(t, "-I{1}$") ~ paste0(str_extract(t, "^\\d{4}"),":1"), 
#                             str_detect(t, "-I{2}$") ~ paste0(str_extract(t, "^\\d{4}"),":2"), 
#                             str_detect(t, "-I{3}$") ~ paste0(str_extract(t, "^\\d{4}"),":3"),
#                             str_detect(t, "-IV$")   ~ paste0(str_extract(t, "^\\d{4}"),":4"), 
#                             TRUE ~ t), 
#          t = t %>% as.yearqtr(format = "%Y:%q") %>% yearquarter()) %>%
#   relocate(Esfera, Serie, t, y)
# 
# x  <- read_excel("Demonstrativos GG_29_04_21.xlsx", sheet = "1.3", range = "A74:AT103",  na = c("n.d.", ""), col_names = TRUE)
# x1 <- x %>% colnames()
# x2 <- 
#   x %>%
#   pivot_longer(`2010-I`:`2020-IV`, names_to = "t", values_to = "y") %>%
#   rename(Num = x1[1], Descr = x1[2]) %>% 
#   select(Num, Descr) %>% 
#   filter(is.na(Num)) %>%
#   pull(Descr) %>% 
#   unique()
# 
# ge.tn <- 
#   x %>%
#   pivot_longer(`2010-I`:`2020-IV`, names_to = "t", values_to = "y") %>%
#   rename(Num = x1[1], Descr = x1[2]) %>%
#   mutate(Num = case_when(Descr == x2[1] ~ "41", Descr == x2[2] ~ "42", TRUE ~ Num), Descr = paste(Num, Descr)) %>%
#   select(-Num) %>%
#   group_by(t) %>%
#   mutate(Serie  = seq_along(Descr),
#          Esfera = "GE", 
#          Descr  = trimws(sub("\\d{1}/", "", Descr)), 
#          t      = case_when(str_detect(t, "-I{1}$") ~ paste0(str_extract(t, "^\\d{4}"),":1"), 
#                             str_detect(t, "-I{2}$") ~ paste0(str_extract(t, "^\\d{4}"),":2"), 
#                             str_detect(t, "-I{3}$") ~ paste0(str_extract(t, "^\\d{4}"),":3"),
#                             str_detect(t, "-IV$")   ~ paste0(str_extract(t, "^\\d{4}"),":4"), 
#                             TRUE ~ t), 
#          t = t %>% as.yearqtr(format = "%Y:%q") %>% yearquarter()) %>%
#   relocate(Esfera, Serie, t, y)
# 
# x  <- read_excel("Demonstrativos GG_29_04_21.xlsx", sheet = "1.3", range = "A109:AT138", na = c("n.d.", ""), col_names = TRUE)
# x1 <- x %>% colnames()
# x2 <- 
#   x %>%
#   pivot_longer(`2010-I`:`2020-IV`, names_to = "t", values_to = "y") %>%
#   rename(Num = x1[1], Descr = x1[2]) %>% 
#   select(Num, Descr) %>% 
#   filter(is.na(Num)) %>%
#   pull(Descr) %>% 
#   unique()
# 
# gm.tn <- 
#   x %>%
#   pivot_longer(`2010-I`:`2020-IV`, names_to = "t", values_to = "y") %>%
#   rename(Num = x1[1], Descr = x1[2]) %>%
#   mutate(Num = case_when(Descr == x2[1] ~ "41", Descr == x2[2] ~ "42", TRUE ~ Num), Descr = paste(Num, Descr)) %>%
#   select(-Num) %>%
#   group_by(t) %>%
#   mutate(Serie  = seq_along(Descr),
#          Esfera = "GM", 
#          Descr  = trimws(sub("\\d{1}/", "", Descr)), 
#          t      = case_when(str_detect(t, "-I{1}$") ~ paste0(str_extract(t, "^\\d{4}"),":1"), 
#                             str_detect(t, "-I{2}$") ~ paste0(str_extract(t, "^\\d{4}"),":2"), 
#                             str_detect(t, "-I{3}$") ~ paste0(str_extract(t, "^\\d{4}"),":3"),
#                             str_detect(t, "-IV$")   ~ paste0(str_extract(t, "^\\d{4}"),":4"), 
#                             TRUE ~ t), 
#          t = t %>% as.yearqtr(format = "%Y:%q") %>% yearquarter()) %>%
#   relocate(Esfera, Serie, t, y)
# 
# efp.tn <- 
#   rbind(gfo.tn, gfc.tn, ge.tn, gm.tn) %>% pull(Esfera) %>% unique()
#   mutate(Esfera = factor(Esfera, levels = c("GF-O", "GF-C", "GE", "GM")))



# EFP trimestral real -----------------------------------------------------

efp.tr <-
  efp.tn %>% 
  group_by(Esfera, Serie, Descr) %>%
  left_join(ipcatrimestral, by = "t") %>%
  mutate(Real = Real(IPCA/100, y, TRUE)) %>%
  transmute(t, y = Real) %>%
  relocate(Esfera, Serie, t,y) %>%
  ungroup()


# EFP trimestral % PIB ---------------------------------------------------

efp.tny <- 
  efp.tn %>%
  group_by(Esfera, Serie, Descr) %>%
  left_join(rename(pib.tn, PIB = y), by = "t") %>%
  transmute(t, y = (y/PIB)*100) %>%
  relocate(Esfera, Serie, t, y)


ipcamensal %>% 
  filter(year(t) > 2009 & year(t) <= 2011) %>% 
  mutate(tt = quarter(t)) %>%
  group_by(year(t), tt) %>%
  mutate(Ind = Ind(IPCA/100,1,FALSE)) %>%
  filter(y = last(y)) %>%
  print(n=Inf)


# EFP anual nominal -------------------------------------------------------

efp.an <- 
  efp.tn %>%
  mutate(aa = year(t)) %>%
  group_by(Esfera, Serie, Descr, aa) %>%
  summarise(y = sum(y, na.rm = TRUE)) %>%
  transmute(t = aa, y) %>%
  relocate(Esfera, Serie, t, y) %>%
  ungroup()


# EFP anual real ----------------------------------------------------------

efp.ar <- 
  efp.an %>%
  filter(t != 2021) %>%
  left_join(ipcaanual, by = "t") %>%
  group_by(Esfera, Serie, Descr) %>%
  mutate(Real = Real(IPCA/100, y, TRUE)) %>%
  transmute(t, y = Real) %>%
  relocate(Esfera, Serie, t, y)



# Mudança GGPLOT ----------------------------------------------------------

# temasggplot(fonte = "Arial", cor = paletacinza[1], titlesize = 12, subtitlesize  = 10, captionsize = 8,axistextsize = 10, axistitlesize = 9)


#==========================================================================================================


# GRÁFICOS BÁSICOS EFP
#==========================================================================================================
# Resultado primário anual (% PIB) ----------------------------------

graf <- function(esfera = "GF-C", aa = 2010, nudge = 0, dig = 0) {
  
  x <-
    efp.an %>%
    filter(Serie == 29) %>%
    filter(t >= aa) %>%
    filter(Esfera == esfera) %>%
    left_join(rename(pib.an, PIB = y), by = "t") %>%
    transmute(Esfera, t, y = (y/PIB)*100) %>%
    mutate(Neg = if_else(y <  0, paste0(format(round(y, digits = 1), decimal.mark = ",", big.mark = "."), "%"), NA_character_),
           Pos = if_else(y >= 0, paste0(format(round(y, digits = 1), decimal.mark = ",", big.mark = "."), "%"), NA_character_), 
           Def = if_else(y < 0, "S", "N"), 
           Def = factor(Def, levels = c("S", "N"))) %>%
    ungroup() 
  
  limx <- range(x$t)
  limy <- range(x$y, na.rm = TRUE)
  
  if (esfera == "GG") { 
    titulo <- paste0("RESULTADO PRIMÁRIO DO GOVERNO CONSOLIDADO (", limx[1], "-", limx[2], ")")
  } else if (esfera == "GC-O") {
    titulo <- paste0("RESULTADO PRIMÁRIO DO GOVERNO FEDERAL ORÇAMENTÁRIO (", limx[1], "-", limx[2],")")
  } else if (esfera == "GC-C") {
    titulo <- paste0("RESULTADO PRIMÁRIO DO GOVERNO FEDERAL CONSOLIDADO (", limx[1], "-", limx[2],")")
  } else if (esfera == "GE") {
    titulo <- paste0("RESULTADO PRIMÁRIO DOS GOVERNOS ESTADUAIS (", limx[1], "-", limx[2],")")
  } else if (esfera == "GM") {
    titulo <- paste0("RESULTADO PRIMÁRIO DOS GOVERNOS MUNICIPAIS (", limx[1], "-", limx[2],")")
  } else {
    stop("escolha esfera corretamente!")
  }
  
  x %>%
    ggplot(aes(t, y, fill = Def)) + 
    geom_bar(stat = "identity", alpha = 0.8) + 
    geom_text(aes(label = Neg, color = Def), size = 3, fontface = "bold", nudge_y = -(nudge/100)*limy[2]) +
    geom_text(aes(label = Pos, color = Def), size = 3, fontface = "bold", nudge_y = +(nudge/100)*limy[2]) +
    scale_x_continuous(NULL, expand = c(0,0), breaks = round(seq(min(limx), max(limx)), digits = 0)) + 
    scale_y_continuous(NULL, breaks = round(seq(limy[1], limy[2], length.out = 7), digits = dig), labels = function(y) paste0(y,"%")) + 
    scale_fill_manual(NULL,  values = c(pal_v[1], pal_a[1])) + 
    scale_color_manual(NULL, values = c(pal_v[1], pal_a[1])) + 
    labs(title = titulo, subtitle = "Proporção do PIB anual", caption = "Fonte: STN/IBGE") + 
    g4 + theme(axis.ticks.x.bottom = element_line(color = pal_c[1]),  
               axis.text.x.bottom  = element_text(color = pal_c[1], size = 9))
}
graf(aa = 2010, nudge = 8, dig = 0, esfera = "GG")
graf(aa = 2010, nudge = 8, dig = 0, esfera = "GC-O")
graf(aa = 2010, nudge = 8, dig = 0, esfera = "GC-C")
graf(aa = 2010, nudge = 2, dig = 1, esfera = "GE")
graf(aa = 2010, nudge = 2, dig = 2, esfera = "GM")

# Resultado primário anual (% PIB) -------------------------------------------

graf <- function(aa = NULL, nudgex = c(rep(-0.8,2), rep(-0.5,2)), nudgey = c(0.5, 0.5, 0.5, -0.5)) {
  
  if (is.null(aa)) {aa <- efp.an %>% pull(t) %>% unique() %>% first()} else{}
  
  x <- 
    efp.an %>%
    mutate(Nome = case_when(Esfera == "GG" ~   "Governo geral", 
                            Esfera == "GC-O" ~ "Governo central", 
                            Esfera == "GC-C" ~ "Governo central", 
                            Esfera == "GE" ~   "Governos estaduais", 
                            Esfera == "GM" ~   "Governos municipais")) %>%
    filter(Esfera != "GC-O") %>%
    filter(Serie == 29) %>%
    filter(t >= aa) %>%
    left_join(rename(pib.an, PIB = y), by = "t") %>%
    transmute(Esfera, Nome, t, y = (y/PIB)*100) %>%
    group_by(Esfera) %>%
    mutate(Last = if_else(t == last(t), paste0(format(round(y, digits = 1), decimal.mark = ",", big.mark = "."), "%"), NA_character_)) %>%
    ungroup() 
  
  limx <- range(x$t)
  limy <- range(x$y, na.rm = TRUE)
  
  x %>%
    filter(Esfera != "GF-O") %>%
    ggplot(aes(t, y, color = Esfera)) + 
    geom_hline(yintercept = 0, size = 0.2, color = pal_c[1], alpha = 1, linetype = 1) + 
    geom_line(size = 0.3) + 
    geom_point(size = 1) + 
    geom_label_repel(aes(label = Last, fill = Esfera),
                     show.legend = FALSE,
                     color = pal_c[9], 
                     size = 3, 
                     fontface = "bold", 
                     segment.size = 0.3,
                     segment.color = pal_c[1],
                     nudge_x = nudgex,
                     nudge_y = nudgey) + 
    scale_x_continuous(NULL, expand = c(0,0), breaks = round(seq(limx[1], limx[2], 2), digits = 0)) + 
    scale_y_continuous(NULL, breaks = round(seq(limy[1], limy[2], length.out = 7), digits = 0), labels = function(y) paste0(y,"%")) + 
    scale_color_manual(NULL, values = c(pal_c[1], pal_v[1], pal_a[2], pal_vd[1]), labels = unique(x$Nome)) +  
    scale_fill_manual(NULL,  values = c(pal_c[1], pal_v[1], pal_a[2], pal_vd[1]), labels = unique(x$Nome)) +  
    labs(title    = paste0("RESULTADO PRIMÁRIO POR ESFERAS DE GOVERNO (", limx[1],"-", limx[2], ")"),
         subtitle = "Proporção do PIB anual",
         caption  = "Fonte: STN/IBGE") + 
    guides(color = guide_legend(override.aes = list(size = 0.8))) + 
    g4 + leg00 
  
}
graf()
graf(aa = 2014, nudgex = c(rep(-0.5,2),  rep(-0.3,2)))
graf(aa = 2019, nudgex = c(rep(-0.06,2), rep(-0.03,2)))


# Resultado primário trimestral (% PIB) --------------------------------------

graf <- function(esfera = "GC-C", aa = 2010, nudge = 0, dig = 0, by = 2, between = c(2020, 2021)) {
  
  x <-
    efp.tn %>%
    filter(Serie == 29) %>%
    filter(year(t) >= aa) %>%
    filter(Esfera == esfera) %>%
    left_join(rename(pib.tn, PIB = y), by = "t") %>%
    transmute(Esfera, t, y = (y/PIB)*100) %>%
    mutate(Neg  = if_else(between(year(t), between[1], between[2]) & y < 0,
                          paste0(format(round(y, digits = 1), decimal.mark = ",", big.mark = "."), "%"), 
                          NA_character_),
           Pos  = if_else(between(year(t), between[1], between[2]) & y >= 0,
                          paste0(format(round(y, digits = 1), decimal.mark = ",", big.mark = "."), "%"), 
                          NA_character_), 
           Def  = if_else(y < 0, "S", "N"), 
           Def  = factor(Def, levels = c("S", "N"))) %>%
    ungroup() 
  
  limx <- range(x$t)
  limy <- range(x$y, na.rm = TRUE)
  
  inicio <- paste0(year(limx[1]), ":", quarter(limx[1]))
  fim    <- paste0(year(limx[2]), ":", quarter(limx[2]))
  
  
  if (esfera == "GG") {
    titulo <- paste0("RESULTADO PRIMÁRIO DO GOVERNO GERAL (", year(limx[1]), "-", year(limx[2]), ")")
  } else if (esfera == "GC-O") {
    titulo <- paste0("RESULTADO PRIMÁRIO DO GOVERNO FEDERAL ORÇAMENTÁRIO (",year(limx[1]),"-",year(limx[2]),")")
  } else if (esfera == "GC-C") {
    titulo <- paste0("RESULTADO PRIMÁRIO DO GOVERNO FEDERAL CONSOLIDADO (",year(limx[1]),"-",year(limx[2]),")")
  } else if (esfera == "GE") {
    titulo <- paste0("RESULTADO PRIMÁRIO DOS GOVERNOS ESTADUAIS (",year(limx[1]),"-",year(limx[2]),")")
  } else if (esfera == "GM") {
    titulo <- paste0("RESULTADO PRIMÁRIO DOS GOVERNOS MUNICIPAIS (",year(limx[1]),"-",year(limx[2]),")")
  } else {
    stop("escolha esfera corretamente!")
  }
  
  x %>%
    ggplot(aes(t, y, fill = Def)) + 
    geom_bar(stat = "identity", alpha = 0.8) + 
    geom_text(aes(label = Neg, color = Def), size = 3, fontface = "bold", nudge_y = -(nudge/100)*limy[2]) +
    geom_text(aes(label = Pos, color = Def), size = 3, fontface = "bold", nudge_y = +(nudge/100)*limy[2]) +
    scale_x_yearquarter(NULL, expand = c(0,0), breaks = round(seq(limx[1], limx[2], by), digits = 0), date_labels = "%Y:%q") + 
    scale_y_continuous(NULL,  breaks = round(seq(limy[1], limy[2], length.out = 7), digits = dig), labels = function(y) paste0(y,"%")) + 
    scale_fill_manual(NULL,   values = c(pal_v[1], pal_a[1])) + 
    scale_color_manual(NULL,  values = c(pal_v[1], pal_a[1])) + 
    labs(title = titulo, subtitle = "Proporção do PIB anual", caption = "Fonte: STN/IBGE") + 
    g4 + theme(axis.ticks.x.bottom = element_line(color = pal_c[1]),  
               axis.text.x.bottom  = element_text(color = pal_c[1], size = 9))
  
}
graf(aa = 2010, nudge = 5,  dig = 0, by = 6, esfera = "GG")
graf(aa = 2010, nudge = 10, dig = 0, by = 6, esfera = "GC-O")
graf(aa = 2010, nudge = 10, dig = 0, by = 6, esfera = "GC-C")
graf(aa = 2010, nudge = 2,  dig = 0, by = 6, esfera = "GE")
graf(aa = 2010, nudge = 3,  dig = 1, by = 6, esfera = "GM")


# Receitas e despesas trimestrais (% PIB) ---------------------------------------------

graf <- function(esferas = "federal", aa = NULL) { 
  
  if (is.null(aa)) {aa <- efp.tn %>% pull(t) %>% year() %>% first()} else{}
  
  if (esferas == "consolidado") { 
    esf <- "GG"
  } else if (esferas == "federal") {
    esf <- "GC-C"
  } else if (esferas == "outros") {
    esf <- c("GE", "GM")
  } else { 
    stop("Escolha corretamente as esferas!")
  }
  
  x <- 
    efp.tn %>%
    filter(year(t) >= aa) %>%
    group_by(Esfera, t) %>%
    filter(Serie %in% c(1,12,14,19)) %>%
    select(-Descr) %>%
    pivot_wider(names_from = Serie, values_from = y) %>%
    transmute(Rprim = `1` - `12`, Dprim = `14` - `19`) %>%
    pivot_longer(c(Rprim, Dprim), names_to = "k", values_to = "y") %>%
    group_by(Esfera, k) %>%
    left_join(rename(pib.tn, PIB = y), by = "t") %>%
    ungroup() %>%
    transmute(Esfera, t, k, y = (y/PIB)*100) %>%
    as_tsibble(index = t, key = c("Esfera", "k")) %>%
    model(X11 = X_13ARIMA_SEATS(y ~ x11())) %>%
    components() %>%
    select(Esfera, t, k, y = season_adjust) %>%
    mutate(k       = factor(k, levels = c("Rprim", "Dprim")), 
           Esfera2 = Esfera %>% fct_recode("Governo federal (orçamentário)" = "GC-O", 
                                           "Governo federal (consolidado)"  = "GC-C", 
                                           "Governos estaduais"  = "GE", 
                                           "Governos municipais" = "GM", 
                                           "Governo consolidado" = "GG"))
  
  if (length(esf) == 1) { 
    x <- x %>% filter(Esfera == esf)
  } else {
    x <- x %>% filter(Esfera %in% esf)
  }
  
  limx <- range(x$t)
  limy <- range(x$y, na.rm = TRUE)
  
  titulos <- 
    tibble("consolidado" = c(paste0("RECEITAS E DESPESAS PRIMÁRIAS DO GOVERNO CONSOLIDADO (", 
                                    year(limx[1]), "-", 
                                    year(limx[2]), ")")), 
           "federal"     = c(paste0("RECEITAS E DESPESAS PRIMÁRIAS DO GOVERNO FEDERAL (", 
                                    year(limx[1]), "-", 
                                    year(limx[2]), ")")), 
           "outros"      = c(paste0("RECEITAS E DESPESAS PRIMÁRIAS DOS GOVERNOS SUBNACIONAIS (", 
                                    year(limx[1]), "-", 
                                    year(limx[2]), ")"))) 
  
  
  x %>%
    ggplot(aes(t,y, color = k, linetype = Esfera2)) +
    geom_line(alpha = 0.7) + 
    geom_point(alpha = 0.7) + 
    scale_x_yearquarter(NULL, expand = c(0,0), date_labels = "%Y:%q", breaks = seq(limx[1], limx[2], length.out = 8)) + 
    scale_y_continuous(NULL, labels = function(y) paste(y, "%")) + 
    scale_color_manual(NULL, values = c(pal_a[1], pal_v[1]), labels = c("Receita", "Despresa")) + 
    scale_linetype_discrete(NULL) + 
    labs(title = titulos[esferas][[1]], subtitle = "Proporção do PIB", caption = "Fonte: STN/IBGE") + 
    g2 + 
    theme(axis.text.x.bottom = element_text(margin = margin(t = 5, b = 5)), 
          plot.margin = margin(r = 16))
}
graf(esferas = "federal")
graf(esferas = "outros")
graf(esferas = "consolidado")


# Receitas e despeas anuais (% PIB) -----------------------------------------------

graf <- function(esferas = "federal", aa = NULL) { 
  
  if (is.null(aa)) {aa <- efp.an %>% pull(t) %>% first()} else{}
  
  if (esferas == "consolidado") { 
    esf <- "GG"
  } else if (esferas == "federal") {
    esf <- "GC-C"
  } else if (esferas == "outros") {
    esf <- c("GE", "GM")
  } else { 
    stop("Escolha corretamente as esferas!")
  }
  
  x <- 
    efp.an %>%
    filter(t >= aa) %>%
    group_by(Esfera, t) %>%
    filter(Serie %in% c(1,12,14,19)) %>%
    select(-Descr) %>%
    pivot_wider(names_from = Serie, values_from = y) %>%
    transmute(Rprim = `1` - `12`, Dprim = `14` - `19`) %>%
    pivot_longer(c(Rprim, Dprim), names_to = "k", values_to = "y") %>%
    group_by(Esfera, k) %>%
    left_join(rename(pib.an, PIB = y), by = "t") %>%
    ungroup() %>%
    transmute(Esfera, t, k, y = (y/PIB)*100) %>%
    mutate(k       = factor(k, levels = c("Rprim", "Dprim")), 
           Esfera2 = Esfera %>% fct_recode("Governo federal (orçamentário)" = "GC-O", 
                                           "Governo federal (consolidado)"  = "GC-C", 
                                           "Governos estaduais"  = "GE", 
                                           "Governos municipais" = "GM", 
                                           "Governo consolidado" = "GG"))
  
  if (length(esf) == 1) { 
    x <- x %>% filter(Esfera == esf)
  } else {
    x <- x %>% filter(Esfera %in% esf)
  }
  
  limx <- range(x$t)
  limy <- range(x$y, na.rm = TRUE)
  
  titulos <- 
    tibble("consolidado" = c(paste0("RECEITAS E DESPESAS PRIMÁRIAS DO GOVERNO CONSOLIDADO (", 
                                    (limx[1]), "-", 
                                    (limx[2]), ")")), 
           "federal"     = c(paste0("RECEITAS E DESPESAS PRIMÁRIAS DO GOVERNO FEDERAL (", 
                                    (limx[1]), "-", 
                                    (limx[2]), ")")), 
           "outros"      = c(paste0("RECEITAS E DESPESAS PRIMÁRIAS DOS GOVERNOS SUBNACIONAIS (", 
                                    (limx[1]), "-", 
                                    (limx[2]), ")"))) 
  
  x %>%
    ggplot(aes(t,y, color = k, linetype = Esfera2)) +
    geom_line(alpha = 0.7) + 
    geom_point(alpha = 0.7) + 
    scale_x_continuous(NULL, expand = c(0,0), breaks = round(seq(limx[1], limx[2], 1), digits = 0)) + 
    scale_y_continuous(NULL, labels = function(y) paste(y, "%"), breaks = round(seq(limy[1], limy[2], length.out = 6), digits = 0)) + 
    scale_color_manual(NULL, values = c(pal_a[1], pal_v[1]), labels = c("Receita", "Despresa")) + 
    scale_linetype_discrete(NULL) + 
    labs(title = titulos[esferas][[1]], subtitle = "Proporção do PIB", caption = "Fonte: STN/IBGE") + 
    g2 + 
    theme(axis.text.x.bottom = element_text(margin = margin(t = 5, b = 5)), 
          plot.margin = margin(r = 16))
}
graf(esferas = "federal")
graf(esferas = "outros")
graf(esferas = "consolidado")


# Evolução das receitas e despesas reais ----------------------------------

graf <- function(trim = "2019 Q4", nudge1 = c(0,0), nudge2 = c(0,0)) {
  
  x <- 
    efp.tr %>%
    group_by(Esfera,t) %>%
    filter(Serie %in% c(1,12,14,19)) %>%
    select(-Descr) %>%
    pivot_wider(names_from = Serie, values_from = y) %>%
    transmute(Rprim = `1` - `12`, Dprim = `14` - `19`) %>%
    filter(Esfera %in% c("GC-C", "GE", "GM")) %>%
    mutate(Ente = case_when(Esfera == "GC-C" ~ "Federal", TRUE ~ "Sub")) %>%
    group_by(Ente, t) %>%
    summarise(Rprim = sum(Rprim), Dprim = sum(Dprim)) %>%
    pivot_longer(c(Rprim, Dprim), names_to = "k", values_to = "y") %>%
    as_tsibble(index = t, key = c(Ente, k)) %>%
    model(X13 = X_13ARIMA_SEATS(log(y) ~ seats())) %>%
    components() %>% 
    as_tsibble() %>%
    transmute(Ente, k, t, y = exp(season_adjust)) %>%
    mutate(k = factor(k, levels = c("Rprim", "Dprim"))) %>%
    filter_index(trim ~ .) %>%
    group_by(Ente, k) %>%
    mutate(y = Dif(y,T), y = Ind(y/100,100,FALSE)) %>%
    group_by(Ente) %>%
    mutate(label1 = if_else(Ente == "Federal" & (y == min(y) | y == max(y)), 
                            format(round(y, digits = 1), decimal.mark = ",", big.mark = "."), 
                            NA_character_),
           label2 = if_else(Ente == "Federal" & k == "Rprim" & y == last(y), 
                            format(round(y, digits = 1), decimal.mark = ",", big.mark = "."), 
                            NA_character_)) %>%
    group_by(Ente,k) %>%
    mutate(label3 = if_else(Ente == "Sub" & k == "Dprim" & y == last(y), 
                            format(round(y, digits = 1), decimal.mark = ",", big.mark = "."), 
                            NA_character_))
  
  limx <- range(x$t) 
  limy <- range(x$y, na.rm = TRUE)
  
  titulo    <- paste0("EVOLUÇÃO DAS RECEITAS E DESPESAS REAIS (", year(limx[1]), "-", year(limx[2]), ")")
  subtitulo <- paste0("Número-Índice (", format(limx[1], "%Y:%q"), " = 100)")
  fonte     <- "Fonte: STN/IBGE"
  
  x %>%
    ggplot(aes(t, y , color = Ente, linetype = k)) + 
    geom_line() + 
    geom_point() +
    geom_hline(yintercept = 100, size = 0.3, color = pal_c[1]) + 
    geom_label_repel(aes(label = label1), 
                     size = 3,
                     fill = pal_a[1],
                     color = pal_c[9],
                     fontface = "bold") + 
    geom_label_repel(aes(label = label2), 
                     size = 3,
                     fill = pal_a[1],
                     color = pal_c[9],
                     fontface = "bold", 
                     segment.size = 0.3, 
                     segment.colour = pal_c[1],
                     nudge_x = nudge1[1], 
                     nudge_y = nudge1[2]) + 
    geom_label_repel(aes(label = label3), 
                     size = 3,
                     fill = pal_v[1],
                     color = pal_c[9],
                     fontface = "bold", 
                     segment.size = 0.3, 
                     segment.colour = pal_c[1],
                     nudge_x = nudge2[1], 
                     nudge_y = nudge2[2]) + 
    scale_x_yearquarter(NULL, expand = c(0,0), date_labels = "%Y:%q") + 
    scale_y_continuous(NULL, labels = function(y) round(y, digits = 0)) + 
    scale_color_manual(NULL, values = c(pal_a[1], pal_v[1]), labels = c("Governo central", "Governos sub-nacionais")) + 
    scale_linetype_discrete(NULL, labels = c("Receita primária", "Despesa primária")) + 
    labs(title = titulo, subtitle = subtitulo, caption = fonte) + 
    g2 + leg00 + 
    theme(plot.margin        = margin(r = 16),
          axis.line.x.bottom = element_blank(), 
          axis.line.y.left   = element_blank(), 
          axis.text.x.bottom = element_text(margin = margin(t = 5, b = 5)))
  
}
graf(nudge1 = c(-1,-5), nudge2 = c(-1,5))


# Paineis ----------------------------------------------------------------

graf <- function(trim = "2019 Q4", nudge1 = c(0,0), nudge2 = c(0,0)) {
  
  x <- 
    efp.tr %>%
    group_by(Esfera,t) %>%
    filter(Serie %in% c(1,12,14,19)) %>%
    select(-Descr) %>%
    pivot_wider(names_from = Serie, values_from = y) %>%
    transmute(Rprim = `1` - `12`, Dprim = `14` - `19`) %>%
    filter(Esfera %in% c("GC-C", "GE", "GM")) %>%
    mutate(Ente = case_when(Esfera == "GC-C" ~ "Federal", TRUE ~ "Sub")) %>%
    group_by(Ente, t) %>%
    summarise(Rprim = sum(Rprim), Dprim = sum(Dprim)) %>%
    pivot_longer(c(Rprim, Dprim), names_to = "k", values_to = "y") %>%
    as_tsibble(index = t, key = c(Ente, k)) %>%
    model(X13 = X_13ARIMA_SEATS(log(y) ~ seats())) %>%
    components() %>% 
    as_tsibble() %>%
    transmute(Ente, k, t, y = exp(season_adjust)) %>%
    mutate(k = factor(k, levels = c("Rprim", "Dprim"))) %>%
    filter_index(trim ~ .) %>%
    group_by(Ente, k) %>%
    mutate(y = Dif(y,T), y = Ind(y/100,100,FALSE)) %>%
    group_by(Ente) %>%
    mutate(label1 = if_else(Ente == "Federal" & (y == min(y) | y == max(y)), 
                            format(round(y, digits = 1), decimal.mark = ",", big.mark = "."), 
                            NA_character_),
           label2 = if_else(Ente == "Federal" & k == "Rprim" & y == last(y), 
                            format(round(y, digits = 1), decimal.mark = ",", big.mark = "."), 
                            NA_character_)) %>%
    group_by(Ente,k) %>%
    mutate(label3 = if_else(Ente == "Sub" & k == "Dprim" & y == last(y), 
                            format(round(y, digits = 1), decimal.mark = ",", big.mark = "."), 
                            NA_character_))
  
  limx <- range(x$t) 
  limy <- range(x$y, na.rm = TRUE)
  
  titulo    <- paste0("(a) receitas e despesas (", year(limx[1]), "-", year(limx[2]), ")")
  subtitulo <- paste0("Número-Índice (", format(limx[1], "%Y:%q"), " = 100)")
  fonte     <- "Fonte: STN/IBGE"
  
  gr1 <<- 
    x %>%
    ggplot(aes(t, y , color = Ente, linetype = k)) + 
    geom_line() + 
    geom_point() +
    geom_hline(yintercept = 100, size = 0.3, color = pal_c[1]) + 
    geom_label_repel(aes(label = label1), 
                     size = 3,
                     fill = pal_a[1],
                     color = pal_c[9],
                     fontface = "bold") + 
    geom_label_repel(aes(label = label2), 
                     size = 3,
                     fill = pal_a[1],
                     color = pal_c[9],
                     fontface = "bold", 
                     segment.size = 0.3, 
                     segment.colour = pal_c[1],
                     nudge_x = nudge1[1], 
                     nudge_y = nudge1[2]) + 
    geom_label_repel(aes(label = label3), 
                     size = 3,
                     fill = pal_v[1],
                     color = pal_c[9],
                     fontface = "bold", 
                     segment.size = 0.3, 
                     segment.colour = pal_c[1],
                     nudge_x = nudge2[1], 
                     nudge_y = nudge2[2]) + 
    scale_x_yearquarter(NULL, expand = c(0,0), date_labels = "%Y:%q") + 
    scale_y_continuous(NULL, labels = function(y) round(y, digits = 0)) + 
    scale_color_manual(NULL, values = c(pal_a[1], pal_v[1]), labels = c("Central", "Sub-nacionais")) + 
    scale_linetype_discrete(NULL, labels = c("Receita", "Despesa")) + 
    labs(title = titulo, subtitle = subtitulo) + 
    g1 + leg01 + 
    theme(plot.margin        = margin(r = 16),
          axis.line.x.bottom = element_blank(), 
          axis.line.y.left   = element_blank(), 
          axis.text.x.bottom = element_text(margin = margin(t = 5, b = 5)))
  
}
graf(nudge1 = c(-1,-5), nudge2 = c(-1,5))
graf <- function(aa = NULL, nudgex = c(rep(-0.8,2), rep(-0.5,2)), nudgey = c(0.5, 0.5, 0.5, -0.5)) {
  
  if (is.null(aa)) {aa <- efp.an %>% pull(t) %>% unique() %>% first()} else{}
  
  x <- 
    efp.an %>%
    mutate(Nome = case_when(Esfera == "GG" ~   "Geral", 
                            Esfera == "GC-O" ~ "Central", 
                            Esfera == "GC-C" ~ "Central", 
                            Esfera == "GE" ~   "Estaduais", 
                            Esfera == "GM" ~   "Municipais")) %>%
    filter(Esfera != "GC-O") %>%
    filter(Serie == 29) %>%
    filter(t >= aa) %>%
    left_join(rename(pib.an, PIB = y), by = "t") %>%
    transmute(Esfera, Nome, t, y = (y/PIB)*100) %>%
    group_by(Esfera) %>%
    mutate(Last = if_else(t == last(t), paste0(format(round(y, digits = 1), decimal.mark = ",", big.mark = "."), "%"), NA_character_)) %>%
    ungroup() 
  
  limx <- range(x$t)
  limy <- range(x$y, na.rm = TRUE)
  
  gr2 <<- 
    x %>%
    filter(Esfera != "GF-O") %>%
    ggplot(aes(t, y, color = Esfera)) + 
    geom_hline(yintercept = 0, size = 0.2, color = pal_c[1], alpha = 1, linetype = 1) + 
    geom_line(size = 0.3) + 
    geom_point(size = 1) + 
    geom_label_repel(aes(label = Last, fill = Esfera),
                     show.legend = FALSE,
                     color = pal_c[9], 
                     size = 3, 
                     fontface = "bold", 
                     segment.size = 0.3,
                     segment.color = pal_c[1],
                     nudge_x = nudgex,
                     nudge_y = nudgey) + 
    scale_x_continuous(NULL, expand = c(0,0), breaks = round(seq(limx[1], limx[2], 2), digits = 0)) + 
    scale_y_continuous(NULL, breaks = round(seq(limy[1], limy[2], length.out = 7), digits = 0), labels = function(y) paste0(y,"%")) + 
    scale_color_manual(NULL, values = c(pal_c[1], pal_v[1], pal_a[2], pal_vd[1]), labels = unique(x$Nome)) +  
    scale_fill_manual(NULL,  values = c(pal_c[1], pal_v[1], pal_a[2], pal_vd[1]), labels = unique(x$Nome)) +  
    labs(title    = paste0("(b) resultado primário (", limx[1],"-", limx[2], ")"),
         subtitle = "Proporção do PIB anual") + 
    guides(color = guide_legend(override.aes = list(size = 0.8))) + 
    g1 + leg00 
  
}
graf()

ggarrange(gr1, gr2, ncol = 2)

#==========================================================================================================

