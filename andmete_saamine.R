library(readxl)
library(dplyr)
library(rvest)
library(stringr)
library(ggplot2)
library(gridExtra)
library(plotly)
library(ggthemes)


# algne andmetabel

andmed <- read_xlsx("CO2_emissions_by_state.xlsx", sheet="DATA")

# tabeliga tutvumine

sum(is.na(andmed)) # puuduvaid andmeid pole

unique(andmed$STATE_NAME)

length(unique(andmed$STATE_NAME)) # 44

#miinimumid ja maksimumid
summary(andmed$CO2_QTY_TONNES)
summary(andmed$TF)

#### Kohendused ja lisandused


# kuu ja aasta faktortunnusteks

andmed$YEAR <- factor(andmed$YEAR)

andmed$MONTH <- factor(andmed$MONTH, levels = c(1:12), labels = c("jaanuar", "veebruar", "märts", "aprill", "mai",
                                                                "juuni", "juuli", "august", "september", "oktoober", "november", "detsember") )

# tärni eemaldamine riiginimedest
andmed$STATE_NAME<- str_remove(andmed$STATE_NAME, "[*]")

# Kaanari saarte (Canary island) andmete liitmine ülejäänud Hispaaniaga

andmed$STATE_NAME <- ifelse(andmed$STATE_NAME == "CANARY ISLANDS", "SPAIN", andmed$STATE_NAME )

andmed <- andmed %>% 
  group_by(YEAR, MONTH, STATE_NAME) %>%
  summarize(CO2_QTY_TONNES = sum(CO2_QTY_TONNES), TF = sum(TF)) %>%
  data.frame()

#uus tunnus: Co2 kogus lennu kohta

andmed$CO2_kogus_lennu_kohta <- round(andmed$CO2_QTY_TONNES/andmed$TF, 1)

# CO2 koguste ümardamine (üks koht peale koma)

andmed$CO2_QTY_TONNES <- round(andmed$CO2_QTY_TONNES, 1)

#nimede eestindamine

andmed$STATE_NAME <- factor(andmed$STATE_NAME)

levels(andmed$STATE_NAME)

nimed <- c("Albaania",  "Armeenia",  "Austria",  "Belgia", 
           "Bosnia and Hertsegoviina", "Bulgaaria",  "Horvaatia", 
           "Küpros",   "Tšehhi",  "Taani", "Eesti", "Soome",  "Prantsusmaa",   "Gruusia",  "Saksamaa", 
           "Kreeka",   "Ungari",  "Island",  "Iirimaa", "Itaalia",  "Kosovo",   "Läti",   "Liechtenstein",         
           "Leedu","Luksemburg", "Malta", "Moldova",   "Monaco",   
           "Montenegro", "Holland", "Põhja-Makedoonia", "Norra", "Poola", "Portugal", "Rumeenia", "Serbia" ,  "Slovakkia", "Sloveenia", "Hispaania", "Rootsi", "Šveits", "Türgi", "Suurbritannia"  
)

andmed$riik <- factor(andmed$STATE_NAME, labels=nimed)

# ISO-3166-1 koodide lisamine andmetele

#Koodid Wikipeediast

koodid <- read_html("https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3")

iso <- koodid %>%
  html_nodes(".plainlist") %>%
  html_text()

iso_koodid <- data.frame(str_split(iso, '\n')) 
iso_koodid <- data.frame(iso_koodid[-c(1:2),]) 
colnames(iso_koodid) <- c("kokku")

# Saadud andmetest riigi ja koodi eraldamine

iso_koodid$kood <- str_split_fixed(iso_koodid$kokku, "\\s", 2)[, 1]
iso_koodid$riik <- str_split_fixed(iso_koodid$kokku, "[A-Z]{3}", 2)[, 2]
iso_koodid <- iso_koodid[-c(1)] 

# Koodide lisamine algsele andmetabelile

iso_koodid$riik <- toupper(str_trim(iso_koodid$riik, side ="left")) # tühikute eemaldamine  + suured tähed

andmed[1, 3] == iso_koodid[5,2] # igaks juhuks kontroll, et sõned on võrdsed

andmed <- left_join(andmed, iso_koodid, by = join_by("STATE_NAME" == riik), copy=TRUE)

# puuduvate väärtuste kontroll ja käsitsi juurde lisamine

unique(andmed[is.na(andmed$kood),]$STATE_NAME) 

andmed$kood <- ifelse(andmed$STATE_NAME == "NETHERLANDS", "NLD", 
                      ifelse(andmed$STATE_NAME == "UNITED KINGDOM", "GBR",
                             ifelse(andmed$STATE_NAME == "KOSOVO", "XXK", andmed$kood))) #kõigist katsetest hoolimata Kosovot siiski kaardile ei saa

sum(is.na(andmed$kood)) 



# Andmestiku salvestamine 

save(andmed,file="shiny/andmed.Rda")

#################################################################################

# See osa on juba shine'i app'is olemas (nii enam-vähem)

###### Funktsioonid
# Graafikud riikide kohta

riik_graafikud <- function(maa, aasta, kuu) { 
  
  # vastavad väärtused
  #Tabelid jooniste jaoks
  
   tabel_a <- andmed %>%
    filter(riik == maa & YEAR == aasta) %>%
    select(MONTH, CO2_QTY_TONNES, CO2_kogus_lennu_kohta)
  
  tabel_k <- andmed %>%
    filter(riik == maa & MONTH == kuu) %>%
    select(YEAR, CO2_QTY_TONNES, CO2_kogus_lennu_kohta)
  
  # väärtused x teljele
  
  if (maa== "Eesti" & aasta == "2017") {
    rooma_nr <- c("IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII")
  } else {
  rooma_nr <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII")}
  

  # Joonised
  
  theme_set(theme_few())
  
  joonis_a1 <- tabel_a %>%
    ggplot(aes(x = MONTH, y = CO2_QTY_TONNES/1000, fill = ifelse(MONTH == kuu, "esil", "tavaline"), color=ifelse(MONTH == kuu, "esil", "tavaline"))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(name = "MONTH", values = c("red","grey50")) +
    scale_color_manual(name = "MONTH", values = c("red","grey50")) + #triipude vältimiseks
    scale_x_discrete(labels = rooma_nr) +
    labs(title = paste0("CO2 üldine heitkogus ", aasta, ". aastal"), x = "", y = "CO2 kogus (1000 t)")
  
  joonis_a2 <- tabel_a %>%
    ggplot(aes(x = MONTH, y = CO2_kogus_lennu_kohta, fill = ifelse(MONTH == kuu, "esil", "tavaline"), color = ifelse(MONTH == kuu, "esil", "tavaline"))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(name = "MONTH", values = c("red","grey50")) +
    scale_color_manual(name = "MONTH", values = c("red","grey50")) +
    scale_x_discrete(labels = rooma_nr) +
    labs(title = paste0("CO2 heitkogus lennu kohta ", aasta, ". aastal"), x = "", y = "CO2 kogus (t)")
  
  joonis_k1 <- tabel_k %>%
    ggplot(aes(x = YEAR, y = CO2_QTY_TONNES/1000, fill = ifelse(YEAR == aasta, "esil", "tavaline"), color = ifelse(YEAR == aasta, "esil", "tavaline"))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(name = "MONTH", values = c("red","grey50")) +
    scale_color_manual(name = "MONTH", values = c("red","grey50")) +
    labs(title = paste0("CO2 üldine heitkogus aastate lõikes (", kuu, ")"), x = "", y = "")
  
  joonis_k2 <- tabel_k %>%
    ggplot(aes(x = YEAR, y = CO2_kogus_lennu_kohta, fill = ifelse(YEAR == aasta, "esil", "tavaline"), color = ifelse(YEAR == aasta, "esil", "tavaline"))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(name = "MONTH", values = c("red","grey50")) +
    scale_color_manual(name = "MONTH", values = c("red","grey50")) +
    labs(title = paste0("CO2 heitkogus lennu kohta aastate lõikes (", kuu, ")"), x = "", y = "") 
  
  # Jooniste liitmine
  
  tulem <- grid.arrange(joonis_a1, joonis_k1, joonis_a2, joonis_k2)
  
  return(tulem)
}

#katsetus

riik_graafikud(maa="Eesti", kuu="mai", aasta="2017") 


# Kaart terve Euroopa kohta


euroopa_graafik <- function(aasta, kuu) {
  
  #Andmetabel
  
  tabel <- andmed %>%
    filter(YEAR == aasta, MONTH == kuu) %>%
    mutate(i_text = paste0(riik, "\n", CO2_QTY_TONNES)) %>%
    select(riik, kood, CO2_QTY_TONNES, i_text)
  
  #Joonis
  
  joonis <- plot_geo(tabel, locations = tabel$kood) %>% 
    add_trace(
      z = ~CO2_QTY_TONNES,
      text = ~i_text,
      color = ~CO2_QTY_TONNES,
      colorscale = 'Reds',
      reversescale = FALSE,
      hoverinfo = "text"
    ) %>%
    layout(
      geo = list(lonaxis = list(range = c(-30, 50)),
                 lataxis = list(range = c(30, 90)),
                 resolution = 50,
                 showland = TRUE, 
                 landcolor = "rgb(229, 229, 229)",
                 showframe = TRUE
      ),
      font = list(family= "Arial", color = "black")) %>%
   colorbar(title="CO2 heitkogus (t)") %>%
    style(hoverlabel = list(bgcolor = "rgb(238,238,238)", font = list(color = "black")))
 
 
  return(joonis)
}

euroopa_graafik("2016", "mai")


### Lõplik funktisoon... mida ei saa kasutada :)

tulemus <- function(riik, aasta, kuu){
  if (riik == "kogu Euroopa"){
    euroopa_graafik(kuu, aasta)
  }
  else {
    riik_graafikud(riik, aasta, kuu)
  }
  
}

