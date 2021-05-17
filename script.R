
##### UMFRAGENAGGREGATOR FUER DIE SONNTAGSFRAGE IN DEUTSCHLAND #####
###### "Wen wuerden Sie waehlen, wenn am naechsten Sonntag Bundestagswahl waere?" #######
##### POLL OF ELECTION POLLS IN GERMANY #####

library(rvest)
library(tidyverse)
library(lubridate)
library(plotly)


###### Step 1: Data collection from wahlrecht.de

institut<-c("allensbach","emnid","forsa","politbarometer","gms","dimap","insa","yougov")
pb = txtProgressBar(min = 0, max = length(institut), initial = 0,style = 3) 
pop<-data.frame()
i<-1

for (i in 1:length(institut)){
  url <- paste0("https://www.wahlrecht.de/umfragen/",institut[i],".htm")
  html<- read_html(httr::GET(url)) # force-coding to httr::GET() so proxy settings from httr_config() apply 
  
  tab<- html %>%
    html_nodes(css='.wilko') %>%
    html_table(fill=T)%>%
    pluck(1)
  
  names(tab)[institut[i]=="politbarometer"]<-tab[1,]
  names(tab)[1]<-"Datum"
  tab<-tab[names(tab)!="" & !is.na(names(tab)) & names(tab)!= ".1"]%>%
    drop_na(Zeitraum)%>%
    filter( Zeitraum != "Zeitraum" )%>%
    mutate_all(~gsub(" %", "", .))%>%
    mutate(across(2:8,~gsub(",", ".", .)))%>%
    mutate(Institut=ifelse(Zeitraum == "Bundestagswahl","Bundestagswahl", institut[i]))
  pop<-bind_rows(pop,tab)
  setTxtProgressBar(pb, i)
  
}

####### Step 2: Data Wrangling

pop<-pop%>%
  #select( -PIRATEN,-FW)%>% # choice to exclude small parties
  mutate(Datum=dmy(Datum))%>%  # drops observations where 'Datum' is text
  gather("Partei","Wert",-Datum,-Befragte,-Zeitraum,-Institut)%>%
  mutate(Wert=as.numeric(Wert))%>%  # drops observations where 'Wert' is text
  drop_na(Datum,Wert)

####### Step 3: Simple Visualization with ggplot
p<-ggplot(pop,aes(Datum,Wert,colour=Partei,fill=Partei,group=Partei,
text = paste(Partei,
"<br>", Datum,
"<br>", Wert,
"<br>", Institut)))+ # formatting for plotly-popup
geom_point(alpha=0.2) +
stat_smooth(method="loess",span = 0.1, se=T) +
scale_fill_manual(values=c("SPD"="red2", "AfD"="deepskyblue", "PIRATEN"="orange", "LINKE"="purple1", "GRÜNE"="springgreen4", "Sonstige"="snow3","CDU/CSU"="grey8","FDP"="yellow2","FW"="wheat","Nichtwähler/Unentschl."="grey85"))+
scale_colour_manual(values=c("SPD"="red2", "AfD"="deepskyblue", "PIRATEN"="orange", "LINKE"="purple1", "GRÜNE"="springgreen4", "Sonstige"="snow3","CDU/CSU"="grey8","FDP"="yellow2","FW"="wheat","Nichtwähler/Unentschl."="white"))+
theme_minimal()+
theme(legend.position = "none")+
scale_x_date(limits = as.Date(c("2019-01-01", "2021-04-30")))

pply<-ggplotly(p,tooltip="text")

# ###### Step 3: Interactive Visualization with plot_ly
# ###### 3a: Generate colour scales
# partypal <- c("red2", "deepskyblue", "orange", "purple1", "springgreen4", "snow3","grey8","yellow2","wheat","grey85")
# partypal <- setNames(partypal, c("SPD","AfD","PIRATEN","LINKE","GRÜNE","Sonstige","CDU/CSU","FDP","FW","Nichtwähler/Unentschl."))
# 
# ###### 3b: Aux-variable for loess-smoothing
# pop$datumz<-as.numeric(pop$Datum-min(pop$Datum))
# ###### 3c: Visualization with native plotly code
# pop%>%
#   group_by(Partei)%>%
#   mutate(fit = fitted(loess(Wert ~ datumz,span=0.2))) %>%
#   plot_ly(x = ~Datum)%>%
#   add_markers(y= ~Wert, color= ~Partei, alpha=0.5,colors= partypal)%>%
#   add_lines(y = ~fit, color= ~Partei)
# 
# plot_ly(data = pop, x = ~Datum, y = ~Wert, color = ~Partei,
#         colors=partypal)
# 
# 
# ## Add month selection buttons on top and slider selection on bottom
# %>% layout(
#   title = "Poll of Polls",
#   xaxis = list(
#     rangeselector = list(
#       buttons = list(
#         list(
#           count = 3,
#           label = "3",
#           step = "month",
#           stepmode = "backward"),
#         list(
#           count = 6,
#           label = "6",
#           step = "month",
#           stepmode = "backward"),
#         list(
#           count = 1,
#           label = "1 J",
#           step = "year",
#           stepmode = "backward"),
#         list(
#           count = 1,
#           label = "akt. J.",
#           step = "year",
#           stepmode = "todate"),
#         list(step = "all"))),
#     
#     rangeslider = list(type = "date")))

