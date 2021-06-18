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
library(zoo)

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


# Regiões, Estados -----------------------------------------------------------------

Estados <- 
  tibble(UF = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", 
                "MG", "ES", "RJ", "SP", 
                "PR", "SC", "RS", 
                "MS", "MT", "GO", "DF"), 
         Nome = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins", 
                  "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", 
                  "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo",
                  "Paraná", "Santa Catarina", "Rio Grande do Sul", 
                  "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"), 
         CodUF = c(c(11:17), 
                   c(21:29),
                   c(31:33, 35), 
                   c(41:43), 
                   c(50:53))) %>%
  mutate(Reg = fct_collapse(UF, 
                            Norte    = c("RO", "AC", "AM", "RR", "PA", "AP", "TO"), 
                            Nordeste = c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"),
                            Sudeste  = c("MG", "ES", "RJ", "SP"),
                            Sul      = c("PR", "SC", "RS"), 
                            Centro   = c("MS", "MT", "GO", "DF"))) %>%
  select(Reg,UF, CodUF, Nome)


Norte    <- Estados %>% filter(Reg == "Norte")
Nordeste <- Estados %>% filter(Reg == "Nordeste")
Sudeste  <- Estados %>% filter(Reg == "Sudeste")
Sul      <- Estados %>% filter(Reg == "Sul")
Centro   <- Estados %>% filter(Reg == "Centro")
Regioes  <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro")


# Países ------------------------------------------------------------------

paises <- 
  codelist %>% 
  select(un.region.name, un.regionsub.name, region23, iso3c, cldr.name.pt, iso.name.en) %>%
  rename(cont    = un.region.name, 
         subcont = un.regionsub.name, 
         reg     = region23,
         id      = iso3c, 
         nome    = cldr.name.pt, 
         nome2   = iso.name.en) %>%
  select(cont,subcont,everything()) %>%
  select(-nome2) %>%
  mutate(cont    = if_else(nome    == "Kosovo", "Europe", cont),
         cont    = if_else(nome    == "Taiwan", "Asia", cont),
         reg     = if_else(subcont == "Australia and New Zealand", "Australia and New Zealand", reg), 
         reg     = if_else(nome    == "Taiwan", "Eastern Asia",    reg), 
         reg     = if_else(nome    == "Kosovo", "Southern Europe", reg), 
         subcont = if_else(nome    == "Taiwan", "Eastern Asia",    subcont), 
         subcont = if_else(nome    == "Kosovo", "Southern Europe", subcont), 
         id      = if_else(nome    == "Kosovo", "RKS", id)) %>%
  filter(!is.na(nome), !is.na(reg)) %>%
  mutate(gr = "g0") %>%
  mutate(gr = if_else(id %in% c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "PRY", "PER", "URY", "VEN"), "g1", gr), 
         gr = if_else(id %in% c("CAN", "MEX", "USA"), "g2", gr), 
         gr = if_else(id %in% c("DNK", "FIN", "ISL", "NOR", "SWE"), "g3", gr),
         gr = if_else(id %in% c("DEU", "AUT", "BEL", "FRA", "ESP", "ITA", "LIE", "LUX", "MCO", "PRT", "NLD", "CHE", "GBR"), "g4", gr),
         gr = if_else(id %in% c("CHN", "KOR", "JPN", "TWN"), "g5", gr)) %>%
  arrange(cont,subcont,reg,nome) %>%
  select(cont, subcont,reg, id, gr, nome) %>%
  rename(Cont = cont, Subcont = subcont, Reg = reg, Nome = nome)

Europa   <- paises %>% filter(Cont == "Europe")   %>% pull(id)
Americas <- paises %>% filter(Cont == "Americas") %>% pull(id)
Africa   <- paises %>% filter(Cont == "Africa")   %>% pull(id)
Asia     <- paises %>% filter(Cont == "Asia")     %>% pull(id)


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


# INÍCIO
#=======================================================================================================================================
# Packages ----------------------------------------------------------------

library(countrycode)
library(remotes)
library(ggrepel)


# Pacote COVID19 ----------------------------------------------------------

install.packages("COVID19")
library(COVID19)

coviddata <- 
  covid19() %>%
  select(id, date, confirmed, deaths, population) %>%
  rename(t = date, ca = confirmed, ob = deaths, po = population) %>%
  arrange(id,t) %>%
  left_join(paises, by = "id") %>%
  select(Cont, Subcont, Reg, gr,id, Nome, t, ca, ob, po) %>%
  mutate(ca = ca %>% as.double(), ob = ob %>% as.double(), po = po %>% as.double(), 
         id = factor(id)) %>%
  filter(!is.na(Cont)) %>%
  mutate(ca2 = ca - lag(ca), 
         ob2 = ob - lag(ob),
         ca3 = (ca2 + lag(ca2,1) + lag(ca2,2) + lag(ca2,3) + lag(ca2,4) + lag(ca2,5) + lag(ca2,6))/7,
         ob3 = (ob2 + lag(ob2,1) + lag(ob2,2) + lag(ob2,3) + lag(ob2,4) + lag(ob2,5) + lag(ob2,6))/7, 
         ca4 = ca/(po/100000), 
         ob4 = ob/(po/100000), 
         ca5 = ca4 - lag(ca4), 
         ob5 = ob4 - lag(ob4),
         ca6 = (ca5 + lag(ca5,1) + lag(ca5,2) + lag(ca5,3) + lag(ca5,4) + lag(ca5,5) + lag(ca5,6))/7,
         ob6 = (ob5 + lag(ob5,1) + lag(ob5,2) + lag(ob5,3) + lag(ob5,4) + lag(ob5,5) + lag(ob5,6))/7, 
         tx  = (ob/ca)*100,) %>%
  ungroup()

vacinas <- 
  covid19() %>%
  select(date, id, vaccines, population) %>%
  left_join(paises, by = "id") %>%
  rename(vac = vaccines, t = date, po = population) %>%
  relocate(id, t, vac) %>%
  group_by(id) %>%
  mutate(id   = factor(id), 
         vac2 = (vac - lag(vac)), 
         vac3 = (vac2 + lag(vac2,1) + lag(vac2,2) + lag(vac2,3) + lag(vac2,4) + lag(vac2,5) + lag(vac2,6))/7,
         vac4 = (vac/(po/100000)), 
         vac5 = (vac4 - lag(vac4)), 
         vac6 = (vac5 + lag(vac5,1) + lag(vac5,2) + lag(vac5,3) + lag(vac5,4) + lag(vac5,5) + lag(vac5,6))/7) %>%
  ungroup() 



# Pacote covid19br -------------------------------------------------------------------

# install_github("liibre/coronabr"); library(coronabr)
# corona <- get_corona_minsaude()
# coronabr <-
# corona %>%
#   tibble() %>%
#   transmute(reg = regiao,
#             uf  = estado,
#             cod = codmun,
#             mun = municipio,
#             t   = as_date(data),
#             sem = semanaEpi,
#             ca  = casosAcumulado,
#             ob  = obitosAcumulado,
#             po  = populacaoTCU2019) %>%
#   filter(!(po %in% str_subset(po, "[\\C\\)]")), !is.na(po)) %>%
#   mutate(po  = po %>% as.double(), 
#          reg = fct_recode(reg, "Centro" = "Centro-Oeste")) %>%
#   left_join(Estados, by = c("reg", "uf")) %>%
#   mutate(nome = case_when(reg == "Brasil" ~ "Brasil", 
#                           !is.na(uf) & !is.na(cod) ~ mun, 
#                           TRUE ~ nome), 
#          cod  = case_when(is.na(cod) & !is.na(coduf) ~ coduf, 
#                           TRUE ~ cod), 
#          nome = case_when(uf == "PA" & mun == "Belém" ~ "Belém (PA)", 
#                           uf == "SP" & nchar(cod) > 2 & nome == "São Paulo" ~ "São Paulo (SP)", 
#                           uf == "RJ" & nchar(cod) > 2 & nome == "Rio de Janeiro" ~ "Rio de Janeiro (RJ)",
#                           uf == "RN" & nome == "Paraná" ~ "Paraná (RN)",
#                           TRUE ~ nome)) %>%
#   select(reg, uf, cod, nome, t, sem, ca, ob, po) %>%
#   arrange(reg, uf, cod, nome, t) %>%
#   group_by(reg, uf, cod) %>%
#   mutate(ca2 = ca/(po/1000000),
#          ob2 = ob/(po/1000000),
#          tx  = (ob/ca)*100,
#          ca3 = ca2 - lag(ca2),
#          ob3 = ob2 - lag(ob2),
#          ca4 = (ca3 + lag(ca3,1) + lag(ca3,2) + lag(ca3,3) + lag(ca3,4) + lag(ca3,5) + lag(ca3,6))/7,
#          ob4 = (ob3 + lag(ob3,1) + lag(ob3,2) + lag(ob3,3) + lag(ob3,4) + lag(ob3,5) + lag(ob3,6))/7)

# install_github("fndemarqui/covid19br")
# 
# library(covid19br)
# 
# # REGIÕES
# regioes <- downloadCovid19(level = "regions")
# regioes <- 
#   regioes %>%
#   ungroup() %>%
#   transmute(Reg = region, 
#             t   = date, 
#             ca  = accumCases, 
#             ob  = accumDeaths, 
#             po  = pop) %>%
#   mutate(po = na.locf(po)) %>%
#   mutate(Reg = case_when(Reg == "Midwest" ~ "Centro-Oeste", 
#                          Reg == "North" ~ "Norte", 
#                          Reg == "Northeast" ~ "Nordeste", 
#                          Reg == "South" ~ "Sul", 
#                          Reg == "Southeast" ~ "Sudeste", 
#                          TRUE ~ Reg))
# 
# bra <- 
#   regioes %>%
#   group_by(t) %>%
#   summarise(ca = sum(ca),  ob = sum(ob), po = sum(po)) %>%
#   mutate(Reg ="Brasil") %>%
#   select(Reg, everything())
# 
# regioes <- 
#   rbind(regioes,bra) %>%
#   group_by(Reg) %>% 
#   mutate(Reg = factor(Reg, levels = c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")),
#          ca2 = ca - lag(ca), 
#          ob2 = ob - lag(ob),
#          ca3 = (ca2 + lag(ca2,1) + lag(ca2,2) + lag(ca2,3) + lag(ca2,4) + lag(ca2,5) + lag(ca2,6))/7,
#          ob3 = (ob2 + lag(ob2,1) + lag(ob2,2) + lag(ob2,3) + lag(ob2,4) + lag(ob2,5) + lag(ob2,6))/7, 
#          ca4 = ca/(po/100000), 
#          ob4 = ob/(po/100000), 
#          ca5 = ca4 - lag(ca4), 
#          ob5 = ob4 - lag(ob4),
#          ca6 = (ca5 + lag(ca5,1) + lag(ca5,2) + lag(ca5,3) + lag(ca5,4) + lag(ca5,5) + lag(ca5,6))/7,
#          ob6 = (ob5 + lag(ob5,1) + lag(ob5,2) + lag(ob5,3) + lag(ob5,4) + lag(ob5,5) + lag(ob5,6))/7, 
#          tx  = (ob/ca)*100,) %>%
#   ungroup() 
# 
# 
# # ESTADOS
# x <- downloadCovid19(level = "states")
# 
# estados <-
#   x %>% 
#   ungroup() %>%
#   transmute(Reg = region, 
#             Est = state,
#             Cod = state_code,
#             t   = date, 
#             c   = accumCases, 
#             o   = accumDeaths, 
#             p   = pop) %>%
#   group_by(Est) %>%
#   filter(!duplicated(t)) %>%
#   mutate(p = if_else(is.na(p), na_locf(p), p)) %>%
#   ungroup() %>%
#   mutate(Reg = case_when(Reg == "Midwest" ~ "Centro", 
#                          Reg == "North" ~ "Norte", 
#                          Reg == "Northeast" ~ "Nordeste", 
#                          Reg == "South" ~ "Sul", 
#                          Reg == "Southeast" ~ "Sudeste", 
#                          TRUE ~ Reg)) %>%
#   mutate(Reg = factor(Reg, levels = c("Norte", "Nordeste", "Centro", "Sudeste", "Sul")))
# 
# x <- 
#   estados %>%
#   group_by(t) %>%
#   summarise(c = sum(c),  o = sum(o), p = sum(p)) %>%
#   mutate(Reg = "Brasil", Est = "BRA", Cod = NA_character_) %>%
#   select(Reg, Est, Cod, everything())
# 
# estados <- 
#   rbind(estados,x) %>%
#   group_by(Est) %>%
#   mutate(c2 = c/(p/1000000), 
#          o2 = o/(p/1000000), 
#          tx = (o/c)*100, 
#          c3 = c2 - lag(c2), 
#          o3 = o2 - lag(o2), 
#          c4 = (c3 + lag(c3,1) + lag(c3,2) + lag(c3,3) + lag(c3,4) + lag(c3,5) + lag(c3,6))/7, 
#          o4 = (o3 + lag(o3,1) + lag(o3,2) + lag(o3,3) + lag(o3,4) + lag(o3,5) + lag(o3,6))/7) %>%
#   gather(c:o4, key = "k", value = "y") 
# 
# x <- Estados %>% select(UF, Nome)
# x <- rbind(x, tibble(UF = "BRA", Nome = "Brasil")) %>% rename(Est = UF)
# 
# estados <- 
#   estados %>% 
#   group_by(Est) %>% 
#   left_join(x, by = "Est") %>%
#   select(Reg,Est,Nome, everything())
# 
# x <- downloadCovid19(level = "cities")
# 
# cidades <- 
#   x %>% 
#   ungroup() %>%
#   transmute(Reg     = region, 
#             Est     = state,
#             Mun     = city,
#             Cod_UF  = state_code,
#             Cod_Mun = city_code,
#             t       = date, 
#             c       = accumCases, 
#             o       = accumDeaths, 
#             p       = pop) %>%
#   mutate(Reg = case_when(Reg == "Midwest" ~ "Centro", 
#                          Reg == "North" ~ "Norte", 
#                          Reg == "Northeast" ~ "Nordeste", 
#                          Reg == "South" ~ "Sul", 
#                          Reg == "Southeast" ~ "Sudeste", 
#                          TRUE ~ Reg)) %>%
#   mutate(Mun = case_when(Mun == "" ~ NA_character_, TRUE ~ Mun))%>%
#   mutate(Reg = factor(Reg, levels = c("Norte", "Nordeste", "Centro", "Sudeste", "Sul"))) %>%
#   filter(!is.na(p))
# 
# x <- 
#   cidades %>%
#   group_by(t) %>%
#   summarise(c = sum(c),  o = sum(o), p = sum(p)) %>%
#   mutate(Reg = "Brasil", Est = "BRA", Mun = "Brasil", Cod_UF = NA_integer_, Cod_Mun = NA_integer_) %>%
#   select(Reg,Est,Mun,Cod_UF, Cod_Mun, everything())
# 
# cidades <- 
#   rbind(cidades,x) %>%
#   group_by(Cod_Mun) %>%
#   mutate(c2 = c/(p/1000000), 
#          o2 = o/(p/1000000), 
#          tx = (o/c)*100, 
#          c3 = c2 - lag(c2), 
#          o3 = o2 - lag(o2), 
#          c4 = (c3 + lag(c3,1) + lag(c3,2) + lag(c3,3) + lag(c3,4) + lag(c3,5) + lag(c3,6))/7, 
#          o4 = (o3 + lag(o3,1) + lag(o3,2) + lag(o3,3) + lag(o3,4) + lag(o3,5) + lag(o3,6))/7) %>%
#   gather(c:o4, key = "k", value = "y") 
# 
# x <- Estados %>% select(UF, Nome)
# x <- rbind(x, tibble(UF = "BRA", Nome = "Brasil")) %>% rename(Est = UF)
# 
# cidades <- 
#   cidades %>% 
#   group_by(Cod_Mun) %>% 
#   left_join(x, by = "Est") %>%
#   select(Reg,Est,Nome,everything())


# Discrepância: covid19() & coronabr() ------------------------------------

# x1 <- 
#   corona %>%
#   transmute(reg = regiao, uf = estado, t = date, c = casosAcumulado, d = obitosAcumulado, p = populacaoTCU2019) %>%
#   filter(reg == "Brasil") %>%
#   arrange(t) %>%
#   ungroup() %>%
#   select(-reg, -uf)
# 
# x2<- covid19("BRA")
# x2 <- 
#   x2 %>%
#   transmute(t = date, c2 = confirmed, d2 = deaths, p2 = population) %>%
#   arrange(t) %>%
#   ungroup() %>%
#   select(-id)
# 
# discrep <-
#   inner_join(x1,x2, by = "t") %>%
#   mutate(cR = c - c2, dR= d - d2, pR = p - p2) %>%
#   print(n=Inf)
# 
# rm(x1,x2)
# 

# Grupos de países --------------------------------------------------------

gr0 <- paises %>% filter(gr == "g0") %>% arrange(id) %>% pull(id)
gr1 <- paises %>% filter(gr == "g1") %>% arrange(id) %>% pull(id)
gr2 <- paises %>% filter(gr == "g2") %>% arrange(id) %>% pull(id)
gr3 <- paises %>% filter(gr == "g3") %>% arrange(id) %>% pull(id)
gr4 <- paises %>% filter(gr == "g4") %>% arrange(id) %>% pull(id)
gr5 <- paises %>% filter(gr == "g5") %>% arrange(id) %>% pull(id)


#=======================================================================================================================================


# COMPARAÇÃO ENTRE PAÍSES: GRÁFICOS EM BARRAS
#=======================================================================================================================================
graf <- function(ID = NULL, ESTAT = "ca", TRIM = 1, dig = 0, limit = 0, nudge = 0, p = 10) {
  
  dia    <- today() - TRIM
  
  if (is.null(ID)) {
    paises <- coviddata %>% pull(id) %>% unique() 
    parentese <- "TOP-35"
  } else {
    paises <- ID
    parentese <- "PAÍSES SELECIONADOS"
  }
  
  if (ESTAT == "ca") {
    Estat <- "ca"
    title    <- paste0("COVID-19: NÚMERO DE CASOS CONFIRMADOS (", parentese, ")")
    subtitle <- paste0("Registros totais em ", format(dia, "%d/%m/%Y"))
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else if (ESTAT == "ob") {
    Estat <- "ob"
    title    <- paste0("COVID-19: NÚMERO DE MORTES CONFIRMADAS (", parentese, ")")
    subtitle <- paste0("Registros totais em ", mday(dia),"/", month(dia), "/", year(dia))
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else if (ESTAT == "ca2") {
    Estat <- "ca2"
    title    <- paste0("COVID-19: NÚMERO DE CASOS DIÁRIOS (", parentese, ")")
    subtitle <- paste0("Novos registros em ", mday(dia),"/", month(dia), "/", year(dia))
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else if (ESTAT == "ob2") {
    Estat    <- "ob2"
    title    <- paste0("COVID-19: NÚMERO DE MORTES DIÁRIAS (", parentese, ")")
    subtitle <- paste0("Novos registros em ", mday(dia),"/", month(dia), "/", year(dia))
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else if (ESTAT == "ca3") { 
    Estat    <- "ca3"
    title    <- paste0("COVID-19: NÚMERO DE CASOS DIÁRIOS (", parentese, ")")
    subtitle <- paste0("Novos registros [média 7 dias] em ", mday(dia),"/", month(dia), "/", year(dia))
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else if (ESTAT == "ob3") {
    Estat    <- "ob3"
    title    <- paste0("COVID-19: NÚMERO DE MORTES DIÁRIAS (", parentese, ")")
    subtitle <- paste0("Novos registros [média 7 dias] em ", mday(dia),"/", month(dia), "/", year(dia))
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else if (ESTAT == "ca4") {
    Estat    <- "ca4"
    title    <- paste0("COVID-19: NÚMERO DE CASOS CONFIRMADOS (", parentese, ")")
    subtitle <- paste0("Registros por 100 mil habitantes em ", mday(dia),"/", month(dia), "/", year(dia))
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else if (ESTAT == "ob4") {
    Estat    <- "ob4"
    title    <- paste0("COVID-19: NÚMERO DE MORTES CONFIRMADAS (", parentese, ")")
    subtitle <- paste0("Registros por 100 mil habitantes em ", mday(dia),"/", month(dia), "/", year(dia))
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else if (ESTAT == "ca5") {
    Estat    <- "ca5"
    title    <- paste0("COVID-19: NÚMERO DE CASOS DIÁRIOS (", parentese, ")")
    subtitle <- paste0("Novos registros por 100 mil habitantes em ", mday(dia),"/", month(dia), "/", year(dia))
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else if (ESTAT == "ob5") {
    Estat    <- "ob5"
    title    <- paste0("COVID-19: NÚMERO DE MORTES DIÁRIAS (", parentese, ")")
    subtitle <- paste0("Novos registros por 100 mil habitantes em ", mday(dia),"/", month(dia), "/", year(dia))
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else if (ESTAT == "ca6") {
    Estat    <- "ca6"
    title    <- paste0("COVID-19: NÚMERO DE CASOS DIÁRIOS (", parentese, ")")
    subtitle <- paste0("Novos registros [média 7 dias] por 100 mil habitantes em ", mday(dia),"/", month(dia), "/", year(dia))
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else if (ESTAT == "ob6") {
    Estat    <- "ob6"
    title    <- paste0("COVID-19: NÚMERO DE MORTES DIÁRIAS (", parentese, ")")
    subtitle <- paste0("Novos registros [média 7 dias] por 100 mil habitantes em ", mday(dia),"/", month(dia), "/", year(dia))
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else if (ESTAT == "tx") {
    Estat    <- "tx"
    title    <- paste0("COVID-19: TAXA DE MORTALIDADE (", parentese, ")")
    subtitle <- paste0("Percentual de mortes em relação aos casos confirmados (%) [", mday(dia),"/", month(dia), "/", year(dia),"]")
    caption  <- "Fonte: Guidotti e Ardia (2020)"
  } else {
    stop("Escolha a estatística corretamente!")
  }
  
  x <- 
    coviddata %>%
    group_by(id) %>%
    mutate(d = seq_along(t)) %>%
    filter(ca > 0) %>%
    pivot_longer(c(ca, ob, ca2:tx), names_to = "k", values_to = "y") %>%
    ungroup() %>%
    filter(id %in% paises) %>%
    filter(t == dia, k == Estat) %>%
    filter(!is.na(y)) %>%
    mutate(r = rank(desc(y))) %>%
    filter(r <= 35)
  
  limy <- range(x$y, na.rm = TRUE); if(limy[1] > 0) {limy[1] <- 0} else {}
  med  <- mean(limy)
  
  x <- x %>% mutate(label = if_else(y >= (p/100)*limy[2], format(round(y, digits = dig), decimal.mark = ",", big.mark = "."), NA_character_))
  
  g <- 
    x %>%
    ggplot(aes(fct_reorder(id, y), y, fill = y)) + 
    geom_bar(stat = "identity", alpha = 0.9) + 
    geom_bar(stat = "identity", data = x %>% filter(id == "BRA"), fill = pal_vd[1]) + 
    geom_text(aes(label = label), 
              size = 2.8, 
              color = pal_c[9], 
              fontface = "bold", 
              nudge_y = -(nudge/100)*med, 
              alpha = 0.9) + 
    scale_y_continuous(NULL, limits = c(limy[1], limy[2]), expand = c(0,0)) + 
    scale_fill_gradient(low = pal_v[1], high = pal_c[1]) + 
    labs(title = title, subtitle = subtitle, caption = caption) + 
    coord_flip() + 
    g4
  
  x %>% 
    select(t, id, Nome, y) %>% 
    arrange(-y) %>% 
    print(n=Inf)
  g
  
}; 
# ID:     Sigla do país: c(BRA, USA, ARG, ...)
# ESTAT:  Estatística: c("ca, ob, ca2, ob2, ca3, ob3, ca4, ob4, ca5, ob5, ca6, ob6, tx)
# TRIM:   Controla a data (diminui da mais recente)
# Demais: Controle de detalhes - casas decimais, limites do gráficos e distância do label



id <- c(gr1,gr2,gr3,gr4,gr5)
id <- NULL

trim <- 1
graf(ID = id, ESTAT = "ca",  TRIM = trim, nudge = 6.8, dig = 0, p = 7) # casos totais
graf(ID = id, ESTAT = "ob",  TRIM = trim, nudge = 5,   dig = 0, p = 6) # mortes totais
graf(ID = id, ESTAT = "ca2", TRIM = trim, nudge = 5,   dig = 0, p = 5) # casos diários
graf(ID = id, ESTAT = "ob2", TRIM = trim, nudge = 3.5, dig = 0, p = 6) # mortes diárias
graf(ID = id, ESTAT = "ca3", TRIM = trim, nudge = 5,   dig = 0, p = 4) # casos diários (7 dias)
graf(ID = id, ESTAT = "ob3", TRIM = trim, nudge = 3.5, dig = 0, p = 5) # mortes diárias (7 dias)
graf(ID = id, ESTAT = "ca4", TRIM = trim, nudge = 4.5, dig = 0, p = 5) # casos por 100 mil
graf(ID = id, ESTAT = "ob4", TRIM = trim, nudge = 3,   dig = 0, p = 5) # mortes por 100 mil
graf(ID = id, ESTAT = "ca5", TRIM = trim, nudge = 2,   dig = 0, p = 3) # casos diários por 100 mil
graf(ID = id, ESTAT = "ob5", TRIM = trim, nudge = 2.5, dig = 1, p = 5) # mortes diárias por 100 mil
graf(ID = id, ESTAT = "ca6", TRIM = trim, nudge = 2,   dig = 0, p = 5) # casos diários por 100 mil (7 dias)
graf(ID = id, ESTAT = "ob6", TRIM = trim, nudge = 2.5, dig = 1, p = 5) # mortes diárias por 100 mil (7 dias)
graf(ID = id, ESTAT = "tx",  TRIM = trim, nudge = 2.5, dig = 1, p = 0) # taxa de mortalidade
#=======================================================================================================================================


# COMPARAÇÃO ENTRE PAÍSES: DINÂMICA
#=======================================================================================================================================
graf <- function(ID = c("ARG", "USA", "CHL"), REF = "BRA", VAR = "ca4", div = 2, nudge = c(0,0)) {
  
  if (is.null(ID))  {stop("Escolha uma seleção de países!")} else {}
  if (is.null(REF)) {stop("Escolha um país de referência!")} else {}
  
  if (VAR == "ca4") {
    titulo    <- "COVID-19: TOTAL DE CASOS CONFIRMADOS (PAÍSES SELECIONADOS)"
    subtitulo <- "Registros por 100 mil habitantes"
  } else if (VAR == "ob4") {
    titulo    <- "COVID-19: TOTAL DE MORTES CONFIRMADAS (PAÍSES SELECIONADOS)"
    subtitulo <- "Registros por 100 mil habitantes"
  } else if (VAR == "ca6") {
    titulo    <- "COVID-19: CASOS DIÁRIOS CONFIRMADOS (PAÍSES SELECIONADOS)"
    subtitulo <- "Média móvel [7 dias] dos registros por 100 mil habitantes"
  } else if (VAR == "ob6") {
    titulo    <- "COVID-19: MORTES DIÁRIAS CONFIRMADAS (PAÍSES SELECIONADOS)"
    subtitulo <- "Média móvel [7 dias] dos registros por 100 mil habitantes"
  } else if (VAR == "tx") {
    titulo    <- "COVID-19: TAXA DE MORTALIDADE (PAÍSES SELECIONADOS)"
    subtitulo <- "Proporção de mortes em relação aos casos confirmados (%)"
  } else {
    stop("escolha entre: c(ca4, ob4, ca6, ob6, tx)")
  }
  
  x <<- 
    coviddata %>%
    filter(id %in% ID) %>%
    group_by(id) %>%
    mutate(d = seq_along(t)) %>%
    filter(ca > 0) %>%
    pivot_longer(c(ca, ob, ca2:tx), names_to = "k", values_to = "y") %>%
    filter(k == VAR)
  
  ref <- 
    coviddata %>%
    filter(id == REF) %>%
    mutate(d = seq_along(t)) %>%
    filter(ca > 0) %>%
    pivot_longer(ca:tx, names_to = "k", values_to = "y") %>%
    filter(k == VAR)
  
  limx <- range(x$d)
  limy <- range(x$y, na.rm = T)
  
  x %>% 
    ggplot() + 
    geom_line(aes(d, y, color = Nome)) + 
    geom_line(ref, mapping = aes(d, y), color = pal_c[1], size = 0.7) + 
    geom_label_repel(data = ref %>% filter(row_number() == length(t) %/% div), 
                     mapping = aes(d, y, label = Nome), 
                     size = 3, 
                     fill = pal_c[1], 
                     color = pal_c[9], 
                     fontface = "bold", 
                     nudge_x = nudge[1], 
                     nudge_y = nudge[2], 
                     segment.colour = pal_c[1], 
                     segment.size = 0.3, 
                     label.size = 0) +
    scale_x_continuous("Dias desde o 1º caso", expand = c(0,0), breaks = round(seq(limx[1], limx[2], length.out = 8), digits = 0)) + 
    scale_y_continuous(NULL, labels = function(y) format(round(y, digits = 0), decimal.mark = ",", big.mark = ".")) + 
    scale_color_hue(NULL) + 
    labs(title = titulo, subtitle = subtitulo, caption = "Fonte: Guidotti e Ardia (2020)") + 
    g2 + 
    guides(color = guide_legend(override.aes = list(size = 1.1))) + 
    theme(plot.margin        = margin(l = 3, r = 16, unit = "pt"),
          axis.line.x.bottom = element_blank(), 
          axis.line.y.left   = element_blank(),
          axis.text.y.left   = element_text(margin = margin(r = 5)),
          panel.grid.major.y = element_line(color = pal_c[5], linetype = 3, size = 0.2), 
          axis.text.x.bottom = element_text(margin = margin(t = 5, b = 5)))
}
# ID:     Vetor de siglas dos países (legenda) - c(USA, ARG, ...)
# REF:    Sigla do país de referência (linha preta)
# VAR:    Estatística - c(ca4, ob4, ca5, ob6)
# Demais: Controle de posição do label

id <- c("ARG", "USA", "CHL", "URY")
id <- c("GBR", "FRA", "ITA", "ESP")
id <- c("ARG", "BOL", "CHL", "COL", "ECU", "PER", "PRY", "URY")
id <- coviddata %>% filter(Reg == "Western Europe" | id == "GBR") %>% select(Reg, id, Nome) %>% unique() %>% pull(id)
graf(ID = id, div = 1.2,  nudge = c(-40,0),  VAR = "ca4")
graf(ID = id, div = 1.05, nudge = c(-30,0),  VAR = "ob4")
graf(ID = id, div = 5,    nudge = c(-40,20), VAR = "ca6")
graf(ID = id, div = 4,    nudge = c(0,0.3),  VAR = "ob6")
#=======================================================================================================================================


# DINÂMICA INDIVIDUAL DOS PAÍSES
#=======================================================================================================================================
graf <- function(ID = "BRA", ESTAT = "casos1", nudge = c(0,-50))  {
  
  pais <- coviddata %>% filter(id == ID) %>% pull(Nome) %>% unique()
  
  if (ESTAT == "casos1") {
    var1      <- "ca2"
    var2      <- "ca3"
    titulo    <- paste0("COVID-19: CASOS DIÁRIOS (", str_to_upper(pais), ")")
    subtitulo <- "Registros totais"
  } else if (ESTAT == "casos2") {
    var1      <- "ca5"
    var2      <- "ca6"
    titulo    <- paste0("COVID-19: CASOS DIÁRIOS (", str_to_upper(pais), ")")
    subtitulo <- "Registros por 100 mil habitantes"
  } else if (ESTAT == "mortes1") {
    var1      <- "ob2"
    var2      <- "ob3"
    titulo    <- paste0("COVID-19: MORTES DIÁRIAS (", str_to_upper(pais), ")")
    subtitulo <- "Registros totais"
  }  else if (ESTAT == "mortes2") {
    var1      <- "ob5"
    var2      <- "ob6"
    titulo    <- paste0("COVID-19: MORTES DIÁRIAS (", str_to_upper(pais), ")")
    subtitulo <- "Registros por 100 mil habitantes"
  } else {
    stop("Escolha estatística corretamente!")
  }
  
  x <- 
    coviddata %>%
    filter(id == ID) %>%
    filter(ca > 0) %>%
    mutate(d = seq_along(t)) %>%
    pivot_longer(c(ca,ob,ca2:tx), names_to = "k", values_to = "y")
  
  limx <- range(x$t)
  limy <- range(x$y, na.rm = T)
  
  label <- x %>% ungroup() %>% filter(k == var2) %>% filter(t == last(t))
  
  
  x %>%
    ggplot() + 
    geom_bar(data  = x %>% filter(k == var1), mapping = aes(t, y, fill  = "ca5"), stat = "identity", alpha = 0.4, show.legend = FALSE) + 
    geom_line(data = x %>% filter(k == var2), mapping = aes(t,y,  color = "ca6"), size = 0.8) + 
    geom_label_repel(label, mapping = aes(t, y, label = format(round(y, digits = 0), big.mark = ".", decimal.mark = ",")), 
                     color          = pal_c[9], 
                     fill           = pal_c[1], 
                     size           = 3, 
                     fontface       = "bold", 
                     segment.colour = pal_c[1], 
                     segment.size   = 0.4, 
                     nudge_x        = nudge[1], 
                     nudge_y        = nudge[2]) + 
    scale_x_date(NULL, expand = c(0,0), breaks = seq(limx[1], limx[2], length.out = 10),date_labels = "%b-%y") + 
    scale_y_continuous(NULL, expand = c(0,0), labels = function(y) format(round(y,digits = 0), big.mark = ".", decimal.mark = ",")) +
    scale_fill_manual(NULL, values = pal_v[1]) + 
    scale_color_manual(NULL, values = pal_c[1], labels = c("Média móvel 7 dias")) + 
    g4 + leg01 + 
    labs(title = titulo, subtitle = subtitulo, caption  = "Fonte: Guidotti e Ardia (2020)") + 
    theme(plot.margin        = margin(l = 3, r = 16, unit = "pt"),
          axis.text.y.left   = element_text(margin = margin(r = 5)),
          panel.grid.major.y = element_line(color = pal_c[5], linetype = 3, size = 0.2), 
          axis.text.x.bottom = element_text(margin = margin(t = 5, b = 5)))
}
# ID:    Sigla do país: c(BRA, USA, ARG, ...)
# ESTAT: Estatística: c("casos1, casos2, mortes1, mortes2)
# Demais parâmetros controlam posições do texto e label no gráfico

id <- "BRA"
id <- "URY"
id <- "ESP"

graf(ID = id, ESTAT = "casos1",  nudge = c(-20,3500)) # casos totais
graf(ID = id, ESTAT = "mortes1", nudge = c(-20,150))  # mortes totais
graf(ID = id, ESTAT = "casos2",  nudge = c(-15,4))    # casos 100 mil hab
graf(ID = id, ESTAT = "mortes2", nudge = c(-8,0.1))   # mortes 100 mil hab
#=======================================================================================================================================


# VACINAS: GRÁFICOS EM BARRAS
#=======================================================================================================================================
graf <- function(ID = NULL, ESTAT = 1, TRIM = 5, p = 0.08, nudgey = 1, rank = 30, pq = TRUE) {
  
  dia <- today() - TRIM
  
  if (is.null(ID)) {ID <- vacinas %>% pull(id) %>% unique()} else{}
  
  if (ESTAT == 1) {
    Estat     <- "vac"
    subtitulo <- paste0("Doses totais em ", format(dia, "%d/%m/%Y"))
  } else if (ESTAT == 2) {
    Estat     <- "vac2"
    subtitulo <- paste0("Doses diárias em ", format(dia, "%d/%m/%Y")) 
  } else if (ESTAT == 3) {
    Estat     <- "vac3"
    subtitulo <- paste0("Doses diárias em ", format(dia, "%d/%m/%Y"), " [média móvel 7 dias]") 
  } else if (ESTAT == 4) {
    Estat     <- "vac4"
    subtitulo <- paste0("Doses por 100 mil habitantes em ", format(dia, "%d/%m/%Y")) 
  } else if (ESTAT == 5) {
    Estat <- "vac5"
    subtitulo <- paste0("Doses diárias por 100 mil habitantes em ", format(dia, "%d/%m/%Y")) 
  } else if (ESTAT == 6) {
    Estat <- "vac6"
    subtitulo <- paste0("Doses diárias por 100 mil habitante habitantes em ", format(dia, "%d/%m/%Y"), " [média móvel 7 dias]") 
  } else {stop("Escolher Estatística corretamente")}
  
  x <- 
    vacinas %>%
    pivot_longer(c(vac, vac2:vac6), names_to = "k", values_to = "y") %>%
    relocate(id, t, k, y) %>%
    filter(t == dia) %>%
    filter(k == Estat) %>%
    filter(id %in% ID) %>%
    filter(!is.na(y)) %>%
    mutate(small = if_else(po > 1000000, TRUE, FALSE))
  
  limx <- range(x$t)
  limy <- range(x$y); if (limy[1] > 0) {limy[1] <- 0} else {}
  
  x <- x %>% mutate(r = rank(-y)) %>% filter(r <= rank)
  
  if (pq == FALSE) {x <- x %>% filter(small != FALSE)} else {}
  
  x <- x %>% mutate(label = if_else(y >= p*limy[2], format(round(y, digits = 0), decimal.mark = ",", big.mark = "."), NA_character_))
  
  titulo    <- paste0("COVID-19: VACINAS ADMINISTRADAS (RANKING, ", rank, " PAÍSES)")
  caption   <- "Fonte: Guidotti e Ardia (2020)"
  
  g <- 
    x %>% 
    ggplot(aes(fct_reorder(id, y), y, fill = y)) + 
    geom_bar(stat = "identity", alpha = 0.9) + 
    geom_bar(stat = "identity", data = x %>% filter(id == "BRA"), fill = pal_vd[1]) + 
    geom_text(aes(label = label), 
              size  = 3, 
              color = pal_c[9], 
              nudge_y = -(nudgey/100)*limy[2], 
              fontface = "bold") + 
    scale_x_discrete(NULL) + 
    scale_y_continuous(NULL, limits = limy, expand = c(0,0)) + 
    scale_fill_gradient(NULL, low = pal_c[1], high = pal_a[1]) + 
    labs(title = titulo, subtitle = subtitulo, caption = caption) + 
    coord_flip() + 
    g4
  
  x %>% 
    select(id, Nome, y, r) %>% 
    arrange(r) %>%
    select(-r) %>% 
    rename(Quantidade = y) %>% 
    data.frame() %>%
    mutate(Quantidade = format(round(Quantidade, digits = 0), big.mark = ".", decimal.mark = ",")) %>%
    list() %>%
    set_names(subtitulo) %>% 
    print()
  
  g
  
}; 
# ID:     Sigla do país: c(BRA, USA, ARG, ...)
# ESTAT:  Estatística: c(1,2,3,4,5,6)
# TRIM:   Número de dias atrás
# p:      Threshold para exibir número (em relação ao máximo)
# nudgey: Correção da posição do número
# rank:   Define número de países
# pq:     Incluir países com população reduzida?

trim <- 2
graf(ESTAT = 1,TRIM = trim, nudgey = 4.5, rank = 30, pq = TRUE) # doses totais 
graf(ESTAT = 4,TRIM = trim, nudgey = 2.8, rank = 35, pq = TRUE) # doses por 100 mil hab
graf(ESTAT = 2,TRIM = trim, nudgey = 3.5, rank = 30, pq = TRUE) # doses diárias
graf(ESTAT = 3,TRIM = trim, nudgey = 3.5, rank = 30, pq = TRUE) # doses diárias (média móvel)
graf(ESTAT = 5,TRIM = trim, nudgey = 2,   rank = 45, pq = TRUE) # doses diárias 100 mil hab
graf(ESTAT = 6,TRIM = trim, nudgey = 2,   rank = 35, pq = TRUE) # doses diárias 100 mil hab (média móvel)
#=======================================================================================================================================


# VACINAS: GRÁFICOS EM LINHA (DINÂMICA)
#=======================================================================================================================================
graf <- function(ID = c("USA", "BRA"), ESTAT = 1, REF = "BRA", div = 2) {
  
  if (ESTAT == 1) {
    Estat     <- "vac"
    subtitulo <- paste0("Doses totais (milhões)")
    vacinas   <- vacinas %>% mutate(vac = vac/1000000)
  } else if (ESTAT == 2) {
    Estat     <- "vac2"
    subtitulo <- paste0("Doses diárias (milhões)")
    vacinas   <- vacinas %>% mutate(vac2 = vac2/1000000)
  } else if (ESTAT == 3) {
    Estat     <- "vac3"
    subtitulo <- paste0("Doses diárias: média móvel 7 dias (milhões)") 
    vacinas   <- vacinas %>% mutate(vac3 = vac3/1000000)
  } else if (ESTAT == 4) {
    Estat     <- "vac4"
    subtitulo <- paste0("Doses por 100 mil habitantes")
  } else if (ESTAT == 5) {
    Estat <- "vac5"
    subtitulo <- paste0("Doses diárias por 100 mil habitantes")
  } else if (ESTAT == 6) {
    Estat <- "vac6"
    subtitulo <- paste0("Doses diárias por 100 mil habitantes [média móvel 7 dias]") 
  } else {stop("Escolher Estatística corretamente")}
  
  x <- 
    vacinas %>% 
    pivot_longer(c(vac, vac2:vac6), names_to = "k", values_to = "y") %>%
    filter(k == Estat) %>%
    filter(!is.na(y))
  
  limx <- range(x$t)
  limy <- range(x$y)
  
  ref <- x   %>% filter(id == REF)
  x   <- x   %>% filter(id %in% ID) %>% filter(id != REF)
  lab <- ref %>% filter(row_number() == length(row_number()) %/% div)
  
  titulo    <- paste0("COVID-19: VACINAS ADMINISTRADAS (PAÍSES SELECIONADOS)")
  subtitulo <- paste0(subtitulo, "\n Fonte: Guidotti e Ardia (2020)")
  
  x %>% 
    ggplot() + 
    geom_line(aes(t, y, color = Nome), size = 0.6) + 
    geom_line(ref, mapping = aes(t,y), color = pal_c[1], size = 0.9) + 
    geom_label_repel(lab, 
                     mapping = aes(t, y, label = Nome), 
                     size  = 3.2, 
                     fill  = pal_c[1], 
                     color = pal_c[9]) + 
    scale_x_date(NULL, expand = c(0,0), breaks = breaks_width("month"), date_labels = "%b") + 
    scale_y_continuous(NULL, labels = function(y) format(round(y, digits = 0), decimal.mark = ",", big.mark = ".")) + 
    scale_color_hue(NULL) + 
    guides(color = guide_legend(override.aes = list(size = 1))) + 
    labs(title = titulo, subtitle = subtitulo) + 
    g2 + theme(axis.line.x.bottom = element_blank(), 
               axis.line.y.left   = element_blank(), 
               panel.grid.major.y = element_line(color = pal_c[6], linetype = 3, size = 0.7))
  
}

id <- c("ARG", "USA", "CHL", "URY")
id <- c("GBR", "FRA", "ITA", "ESP")
id <- c("ARG", "BOL", "CHL", "COL", "ECU", "PER", "PRY", "URY")
id <- c(gr1,gr2,gr3,gr4,gr5)

graf(ID = id, ESTAT = 1) # total
graf(ID = id, ESTAT = 3) # diárias (média móvel)
graf(ID = id, ESTAT = 4) # doses por 100 mil hab
graf(ID = id, ESTAT = 6) # diárias por 100 mil hab (média móvel)
#=======================================================================================================================================

