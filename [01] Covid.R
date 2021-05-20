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
               
}; temasggplot()

leg01 <- theme(legend.justification = c(0,1), legend.position = c(0,1))
leg00 <- theme(legend.justification = c(0,0), legend.position = c(0,0))
leg11 <- theme(legend.justification = c(1,1), legend.position = c(1,1))
leg10 <- theme(legend.justification = c(1,0), legend.position = c(1,0))


g_3 <- theme_bw() + theme(panel.border      = element_rect(size = 1.3, color = "#000000"),
                          panel.grid.major  = element_blank(), 
                          panel.grid.minor  = element_blank(), 
                          plot.title        = element_text(size = 14, hjust = 0, face = "bold"),
                          plot.subtitle     = element_text(size = 11, hjust = 0),
                          panel.grid        = element_line(size = 1, color = "#000000"),
                          axis.ticks        = element_line(colour = "black"), 
                          axis.line         = element_line(color = "#000000"),
                          axis.title.y      = element_text(colour = "black", size = 10, hjust = 0.5, face = "bold"), 
                          axis.title.x      = element_blank(), 
                          axis.text.y       = element_text(size = 10, colour = "black", face = "bold"), 
                          axis.text.x       = element_text(size = 10, colour = "black", face = "bold", angle = 0, vjust = 0.5, hjust = 0.5),
                          legend.title      = element_text(size = 13, colour = "black", face = "bold"),
                          legend.background = element_blank(),
                          legend.key        = element_rect(colour = "black"),
                          legend.text       = element_text(size = 11, face = "bold"), 
                          plot.margin       = margin(l = 5, t = 5, b = 5, r = 20, unit = "pt")) 

g4 <- g_4 <- theme_bw() + theme(panel.border      = element_blank(),
                                panel.grid.major  = element_line(size = 0.5, color = "#C0C0C0", linetype = 3),
                                panel.grid.minor  = element_line(size = 0.5, color = "#C0C0C0", linetype = 3),
                                plot.title        = element_text(size = 10, hjust = 0, face = "bold"),
                                plot.subtitle     = element_text(size = 10, hjust = 0),
                                plot.caption      = element_text(size = 9,  color = "#202020", hjust = 1, face = "bold"),
                                axis.ticks        = element_line(colour = "black"), 
                                axis.line         = element_line(color = "#000000"),
                                axis.title.y      = element_text(colour = "#000000", size = 9, hjust = 0.1, face = "bold"), 
                                axis.title.x      = element_text(colour = "#808080", size = 9, hjust = 0.1, face = "bold"), 
                                axis.text.y       = element_text(size = 9, colour = "black", face = "bold"), 
                                axis.text.x       = element_text(size = 9, 
                                                                 colour = "black", 
                                                                 face = "bold", 
                                                                 angle = 0, 
                                                                 vjust = 0.5, 
                                                                 hjust = 0.5, 
                                                                 margin = margin(b = 5)),
                                legend.title         = element_text(size = 9, colour = "black", face = "bold"),
                                legend.background    = element_blank(),
                                legend.key           = element_rect(colour = "black"),
                                legend.text          = element_text(size = 9.2, face = "bold"), 
                                legend.justification = c(0,1), 
                                legend.position      = c(0,1),
                                plot.margin       = margin(l = 5, t = 5, b = 5, r = 20, unit = "pt")) 

g5 <- g_5 <- theme_bw() + theme(panel.border      = element_blank(),
                                panel.grid.major  = element_blank(),
                                panel.grid.minor  = element_blank(),
                                plot.title        = element_text(size = 10, hjust = 0, face = "bold"),
                                plot.subtitle     = element_text(size = 10, hjust = 0),
                                plot.caption      = element_text(size = 9,  color = "#202020", hjust = 1, face = "bold"),
                                axis.ticks        = element_line(colour = "black"), 
                                axis.line         = element_line(color = "#000000"),
                                axis.title.y      = element_blank(),
                                axis.title.x      = element_blank(),
                                axis.text.y       = element_text(size = 9, colour = "black", face = "bold"), 
                                axis.text.x       = element_text(size = 9, 
                                                                 colour = "black", 
                                                                 face = "bold", 
                                                                 angle = 0, 
                                                                 vjust = 0.5, 
                                                                 hjust = 0.5, 
                                                                 margin = margin(b = 5)),
                                legend.title         = element_text(size = 9, colour = "black", face = "bold"),
                                legend.background    = element_blank(),
                                legend.key           = element_rect(colour = "black"),
                                legend.text          = element_text(size = 9.2, face = "bold"), 
                                legend.justification = c(0,1), 
                                legend.position      = c(0,1),
                                plot.margin       = margin(l = 5, t = 5, b = 5, r = 20, unit = "pt")) 



# geom_label_repel


# CNT: Tabela de índices de volume (1620) -------------------------------------------------

sidra_1620 <- get_sidra(api = "/t/1620/n1/all/v/all/p/all/c11255/all/d/v583%202")
x <- 
  sidra_1620 %>%
  as_tibble() %>%
  select(Trimestre, `Setores e subsetores`, Valor) %>%
  rename(t = Trimestre, `Descrição` = `Setores e subsetores`, y = Valor) %>%
  mutate(tt = str_extract(t, "^\\d"), 
         aa = trimws(str_extract(t, "\\s\\d+$")),
         t  = as.yearqtr(paste(aa,tt, sep = ":"), format = "%Y:%q") %>% yearquarter()) %>%
  select(t, `Descrição`, y)

descr_1620 <- x %>% pull(`Descrição`) %>% unique()

indices <- 
  x %>% 
  mutate(`Acrônimo` = factor(`Descrição`, levels = descr_1620) %>% 
           fct_recode("Agro"     = "Agropecuária - total",
                      "Ind"      = "Indústria - total",
                      "Extr"     = "Indústrias extrativas",
                      "Transf"   = "Indústrias de transformação", 
                      "SIUP"     = "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos",
                      "Serviços" = "Serviços - total", 
                      "Transp"   = "Transporte, armazenagem e correio",  
                      "Info"     = "Informação e comunicação",  
                      "Fin"      = "Atividades financeiras, de seguros e serviços relacionados",
                      "Imob"     = "Atividades imobiliárias",
                      "ServOtr"  = "Outras atividades de serviços",
                      "Adm"      = "Administração, saúde e educação públicas e seguridade social",
                      "VA"       = "Valor adicionado a preços básicos", 
                      "Imp"      = "Impostos líquidos sobre produtos",
                      "PIB"      = "PIB a preços de mercado",     
                      "Consumo"  = "Despesa de consumo das famílias",
                      "Governo"  = "Despesa de consumo da administração pública",
                      "FBCF"     = "Formação bruta de capital fixo",    
                      "Export"   = "Exportação de bens e serviços",       
                      "Import"   = "Importação de bens e serviços (-)"), 
         Nome = `Acrônimo` %>% 
           fct_recode("Agropecuária"         = "Agro", 
                      "Indústria"            = "Ind", 
                      "Ind. Extrativas"      = "Extr", 
                      "Ind. Transformação"   = "Transf",
                      "Transporte e outros"  = "Transp", 
                      "Informação e outros"  = "Info", 
                      "Financeiro e outros"  = "Fin",
                      "Imobiliário e outros" = "Imob",
                      "Outros serviços"      = "ServOtr", 
                      "Adm pública e outros" = "Adm",
                      "Valor adicionado"     = "VA",
                      "Impostos líquidos"    = "Imp",
                      "Consumo das famílias" = "Consumo", 
                      "Consumo do governo"   = "Governo", 
                      "Exportação"           = "Export", 
                      "Importação"           = "Import"), 
         Setor = `Acrônimo` %>% fct_collapse(`Agropecuária` = "Agro", 
                                             `Indústria`    = c("Ind", "Extr", "Transf", "SIUP", "Construção"), 
                                             `Serviços`     = c("Serviços", "Comércio", "Transp", "Info", "Fin", "Imob", "ServOtr", "Adm"), 
                                             Impostos       = "Imp", 
                                             C = "Consumo", 
                                             G = "Governo", 
                                             I = "FBCF", 
                                             X = "Export", 
                                             M = "Import")) %>%
  group_by(t) %>%
  mutate(k = 1:n()) %>%
  relocate(t,k, Setor, Nome, y, `Acrônimo`,`Descrição`) %>%
  ungroup()

grupos_1620 <- indices %>% select(k, Setor, Nome, `Acrônimo`, Nome) %>% unique() 


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


#==============================================================================


# MODELO DE PREVISÃO DO PIB: 2020-21
#==========================================================================================================================
# Estimando (2019-2020) -----------------------------------------

decomp <- 
  indices %>% 
  select(t, Setor, Nome, `Acrônimo`, y) %>%
  as_tsibble(index = t, key = c(Nome, `Acrônimo`)) %>%
  filter(Nome == "PIB") %>%
  model(X13 = X_13ARIMA_SEATS(y ~ seats())) %>%
  components() %>%
  as_tsibble() %>%
  ungroup()

completa <- 
  decomp %>%
  transmute(t, 
            y = season_adjust, 
            d1  = if_else(year(t) == 2020 & quarter(t) == 1, 1, 0), 
            d2  = if_else(year(t) == 2020 & quarter(t) == 2, 1, 0), 
            d3  = if_else(year(t) == 2008 & quarter(t) == 4, 1, 0), 
            d4  = if_else(year(t) >= 2014, 1, 0))

# Estimando PIB futuro em 2019
training  <- completa %>% filter(year(t) <= 2019)
modelos   <- 
  training %>% 
  model(mod1  = ARIMA(log(y) ~ pdq(d = 1)), 
        mod2  = ARIMA(log(y) ~ pdq(d = 1) + d3),
        mod3  = ARIMA(log(y) ~ pdq(d = 1) + d4),
        mod4  = ARIMA(log(y) ~ pdq(d = 1) + d3 + d4))

modelos %>% 
  glance() %>%
  select(.model, AIC:BIC) %>%
  arrange(BIC) %>%
  arrange(AICc)

modelos %>% select(mod2) %>% report()

modelos %>% 
  select(mod2) %>%
  residuals() %>% 
  features(.resid, ljung_box, lag = 10, dof = 3) %>% # como especificar número de parâmetros automaticamente?
  arrange(.model)

cenarios <- scenarios(d1 = new_data(training,8) %>% mutate(d3 = rep(0,8)), names_to = "Cenários")

fct  <- modelos %>% select(mod2) %>% forecast(h = 8, new_data = cenarios)


# Previsão (2020-21) --------------------

anot <- tsibble(t    = max(fct$t), 
                y    = min(completa$y), 
                label= paste0("Estimação: ", 
                              min(year(training$t)), "-", max(year(training$t)), "; \n Previsão: ", 
                              paste(year(min(fct$t)), quarter(min(fct$t)), sep = ":"), "-", 
                              paste(year(max(fct$t)), quarter(max(fct$t)), sep = ":")))

lab <- training %>% filter(row_number() == length(t) %/% 1.2)

fct %>%
  select(-`Cenários`) %>%
  autoplot(data = training, level = seq(10,90,10), show_gap = FALSE) + 
  autolayer(completa) + 
  geom_text(data = anot, aes(label = label, x = t, y = y), hjust = "right", vjust = "bottom", size = 3.5, fontface = "italic", nudge_x = -100, nudge_y = 0) + 
  geom_label_repel(data = lab, aes(x = t, y = y, label = "PIB efetivo"), 
                   color = pal_c[9], 
                   fill  = pal_c[1], 
                   size  = 3, 
                   nudge_y  = -10, 
                   fontface = "bold", 
                   label.size   = 0,
                   segment.size = 0.3, 
                   segment.color = pal_c[1]) + 
  scale_x_yearquarter(NULL, expand = c(0,0), date_labels = "%Y:%q", breaks = seq(min(completa$t), max(completa$t), length.out = 8)) + 
  scale_y_continuous(NULL) + 
  labs(title = paste0("BRASIL: PRODUTO INTERNO BRUTO (", min(year(completa$t)), "-", max(year(completa$t)), ")"), 
       subtitle = "PIB: Número-Índice (média 1995 = 100)")+ 
  guides(color = guide_legend(title = "Modelos"), 
         fill  = guide_legend(title = "Modelos")) + 
  g1 + theme(plot.margin = margin(r = 16, unit = "pt"))


# Previsão alternativa (2020-21) ------------------------------------------

completa <- 
  decomp %>%
  transmute(t, 
            y = season_adjust, 
            d1  = if_else(year(t) == 2020 & quarter(t) == 1, 1, 0), 
            d2  = if_else(year(t) == 2020 & quarter(t) == 2, 1, 0), 
            d3  = if_else(year(t) == 2008 & quarter(t) == 4, 1, 0), 
            d4  = if_else(year(t) >= 2014, 1, 0))

training2  <- completa %>% filter(year(t) <= 2020)
modelos2   <- 
  training2 %>% 
  model(mod1  = ARIMA(log(y) ~ 1 + pdq(1,1,0) + PDQ(0,0,0) + d3),
        mod2  = ARIMA(log(y) ~ 1 + pdq(1,1,0) + PDQ(0,0,0) + d1 + d2 + d3), 
        mod3  = ARIMA(log(y) ~ 1 + pdq(1,1,0) + PDQ(0,0,0) + d1 + d2 + d3 + lag(d2,1)), 
        mod4  = ARIMA(log(y) ~ 1 + pdq(1,1,0) + PDQ(0,0,0) + d1 + d2 + d3 + lag(d2,1) + lag(d2,2)),
        mod5  = ARIMA(log(y) ~ 1 + pdq(1,1,0) + PDQ(0,0,0) + d1 + d2 + d3 + lag(d2,1) + lag(d2,2) + lag(d2,3)))

modelos2 %>% 
  glance() %>%
  select(.model, AIC:BIC) %>%
  arrange(BIC) %>%
  arrange(AICc)

modelos2 %>% select(mod1) %>% report()
modelos2 %>% select(mod2) %>% report()
modelos2 %>% select(mod3) %>% report()
modelos2 %>% select(mod4) %>% report()
modelos2 %>% select(mod5) %>% report()

modelos2 %>% select(mod2) %>% residuals() %>% features(.resid, ljung_box, lag = 10, dof = 5)

cen1 <- 0
cen2 <- 0.05
cen3 <- 0.10

cenarios <- 
  scenarios(
    c1 = new_data(training2, 4) %>% mutate(d1 = c(0,0,0,0), d2 = c(cen1,0,0,0), d3 = c(0,0,0,0), d4 = c(1,1,1,1)), 
    c2 = new_data(training2, 4) %>% mutate(d1 = c(0,0,0,0), d2 = c(cen2,0,0,0), d3 = c(0,0,0,0), d4 = c(1,1,1,1)),
    c3 = new_data(training2, 4) %>% mutate(d1 = c(0,0,0,0), d2 = c(cen3,0,0,0), d3 = c(0,0,0,0), d4 = c(1,1,1,1)),
    names_to = "Cenários")

# cenarios <-
#   scenarios(
#     c1 = new_data(training2, 4) %>% mutate(d1 = c(0,0,0,0), d2 = c(cen1,0,0,0)),
#     c2 = new_data(training2, 4) %>% mutate(d1 = c(0,0,0,0), d2 = c(cen2,0,0,0)),
#     c3 = new_data(training2, 4) %>% mutate(d1 = c(0,0,0,0), d2 = c(cen3,0,0,0)),
#     names_to = "Cenários")

fct2  <- modelos2 %>% select(mod2) %>% forecast(h = 4, new_data = cenarios)


# Gráfico: previsão alternativa 2021 --------------------------------------

graf <- function(aa = 2019) {
  
  anot <- tsibble(t = max(fct2$t), y = min(completa$y), 
                  label= paste0("Estimação: ", 
                                min(year(training$t)), "-", max(year(training$t)),
                                "; \n Previsão: ", 
                                paste(year(min(fct$t)), quarter(min(fct$t)), sep = ":"), 
                                "-", 
                                paste(year(max(fct$t)), quarter(max(fct$t)), sep = ":")))
  
  lab <- training2 %>% filter(row_number() == length(t) %/% 1.2)
  
  x <- 
    fct %>%
    as_tsibble() %>%
    select(t, .mean) %>%
    rename(fct = .mean) %>% 
    left_join(pivot_wider(select(as_tsibble(fct2), `Cenários`, t, .mean), names_from = `Cenários`, values_from = .mean), by = "t") %>%
    full_join(select(completa, t,y), by = "t") %>%
    relocate(t,y) %>%
    mutate(fct = if_else(year(t) == 2019 & quarter(t) == 4, y, fct),
           c1  = if_else(year(t) == 2020 & quarter(t) == 4, y, c1),
           c2  = if_else(year(t) == 2020 & quarter(t) == 4, y, c2),
           c3  = if_else(year(t) == 2020 & quarter(t) == 4, y, c3))
  
  calculando <<- x
  
  x     <- x %>% filter(year(t) >= aa) 
  lim.x <- range(x$t)
  lim.y <- range(x[,c("y", "fct")], na.rm = TRUE)
  pib   <- x %>% filter(y == min(y, na.rm = TRUE)) %>% mutate(lb = "PIB efetivo")
  
  x %>%
    ggplot() + 
    geom_line(aes(t, fct, color = "fct"), size = 0.7) + 
    geom_line(aes(t, c1,  color = "c1"),  size = 0.7) + 
    geom_line(aes(t, c2,  color = "c2"),  size = 0.7) + 
    geom_line(aes(t, c3,  color = "c3"),  size = 0.7) + 
    geom_line(aes(t, y),  color = pal_c[1]) + 
    geom_point(aes(t,y),  color = pal_c[1]) + 
    geom_label(data = pib, mapping = aes(t,y, label = lb), 
               color = pal_c[9], 
               fill  = pal_c[1], 
               size  = 3, 
               nudge_y = 2,
               nudge_x = -0,  
               fontface = "bold", 
               label.size   = 0,
               segment.size = 0.3, 
               segment.color = pal_c[1]) + 
    geom_vline(xintercept = ymd(20191001), size = 0.5, linetype = 3) + 
    scale_x_yearquarter(NULL, expand = c(0,0), breaks = seq(lim.x[1], lim.x[2], length.out = 7), date_labels = "%Y:%q") + 
    scale_y_continuous(NULL, breaks = round(seq(lim.y[1], lim.y[2], length.out = 5), digits = 0)) + 
    scale_color_manual(NULL,
                       breaks = c("fct", "c1", "c2", "c3"), 
                       values = c(pal_a[2], pal_v[3:1]),
                       labels = c("Previsão inicial (2020-2021)", 
                                  "Previsão 2021: I", 
                                  "Previsão 2021: II", 
                                  "Previsão 2021: III")) + 
    labs(title = "PREVISÃO DO PIB: CENÁRIOS PARA 2021", subtitle = "Índice de volume: 1995 = 100", caption  = "Elaboração própria/IBGE") + 
    g1 + theme(plot.margin = margin(r = 16, unit = "pt"))
  
}
graf(); calculando %>% filter(year(t) >= 2019) %>% print(n=Inf)

# Perda acumulada, variações e gaps (2020-21) --------------

x1 <- 
  calculando %>% 
  mutate(pib_c1 = if_else(is.na(c1) & year(t) == 2020, y,c1), 
         pib_c2 = if_else(is.na(c2) & year(t) == 2020, y,c2), 
         pib_c3 = if_else(is.na(c3) & year(t) == 2020, y,c3)) %>%
  select(-c1,-c2,-c3) %>%
  pivot_longer(pib_c1:pib_c3, names_to = "Cenários", values_to = "PIB_c") %>%
  group_by(`Cenários`) %>%
  arrange(`Cenários`, t) %>%
  mutate(Perdas     = PIB_c - fct, 
         PIB2019    = sum(subset(y, year(t) == 2019))) %>%
  as_tibble() %>%
  group_by(`Cenários`) %>%
  mutate(perdas_PIB = (Perdas/PIB2019)*100) %>% 
  summarise(Perdas_PIB = sum(perdas_PIB, na.rm = T)) %>%
  mutate(`Cenários` = case_when(`Cenários` == "pib_c1" ~ "I", 
                                `Cenários` == "pib_c2" ~ "II", 
                                `Cenários` == "pib_c3" ~ "III", 
                                TRUE ~ `Cenários`)) %>%
  data.frame() %>%
  mutate(Perdas_PIB = round(Perdas_PIB, digits = 2)) %>%
  rename(`Perda (%)` = Perdas_PIB) 

x2 <- 
  calculando %>%
  select(-y) %>%
  filter(year(t) >= 2020, quarter(t) == 4) %>%
  pivot_longer(c(fct, c1,c2,c3), names_to = "Modelo", values_to = "y") %>%
  as_tibble() %>%
  arrange(Modelo) %>%
  group_by(Modelo) %>%
  mutate(Var = diflog(y,T)) %>%
  group_by(t) %>%
  mutate(Gap    = (1 - (y/subset(y, Modelo == "fct")))*100, 
         Modelo = factor(Modelo, levels = c("fct", "c1", "c2", "c3")) %>% 
           fct_recode("I" = "c1", 
                      "II"  = "c2", 
                      "III" = "c3", 
                      "P. 2019"  = "fct")) %>%
  filter(year(t) == 2021) %>%
  select(-y) %>%
  data.frame() %>%
  transmute(t, Modelo, Var = round(Var, digits = 2), Gap_P2019 = round(Gap, digits = 3)) 

x3 <- 
  calculando %>% 
  as_tibble() %>%
  filter(year(t) >= 2019) %>% 
  pivot_longer(c(y:c3), names_to = "k", values_to = "y") %>%
  mutate(aa = year(t)) %>%
  group_by(aa,k) %>%
  summarise(y = sum(y, na.rm = T)) %>% 
  filter(aa != 2019) %>%
  filter(k != "fct") %>%
  group_by(k) %>%
  filter(!(aa == 2021 & k == "y")) %>%
  filter(!(aa == 2020 & k != "y")) %>%
  ungroup() %>%
  mutate(d = ((y/subset(y, k == "y"))-1)*100) %>%
  rename(t = aa, PIB = k, Vol = y, Var = d)

list("Perda acumulada (2020-2021): porcentagem do PIB 2019" = x1, 
     "Previsões: Variação e Gaps dos cenários em 2021:4"    = x2, 
     "Taxa de crescimento em 2021" = x3); rm(x1,x2,x3)


#==========================================================================================================================

