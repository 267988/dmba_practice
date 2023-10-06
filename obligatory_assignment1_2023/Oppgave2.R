#Oppgave 2. I denne oppgaven bruker vi NIST-data:https://pvdata.nist.gov/
#Last ned Ground-data (Bulk Download) med 1 minutt oppløsning for 2015. 
#Her kan dere gjenbruke løsningen på oppgavene fra tredje forelesning i github-repoet for å lage en stor Parquet-fil. 
#Les deretter denne inn i R og gjennomfør selve oppgaven i R.

# install.packages("lubridate")
# install.packages("dplyr")
# install.packages("arrow")
# install.packages("tidyverse")
# install.packages("ggplot2")
library(dplyr)
library(arrow)
library(lubridate)
library(tidyverse)

df = read_parquet("solar.parquet")
df
# Allerede parse TIMESTAMP i python, men trenger fortsatt noen kolonner med måned, dato, år
# Løser timezone -05:00 feilmelding ved å ignorere tidssonen siden den er uvesentlig for oppgaven

df$TIMESTAMP <- with_tz(df$TIMESTAMP, tzone = "UTC")
df <- df %>% mutate(year=year(TIMESTAMP), month=month(TIMESTAMP), day=day(TIMESTAMP), hour = hour(TIMESTAMP))
df
# Først lager vi ny kolonne med uke nummer, bruker lubridate funksjonen "week"
df$week_nr <- week(df$TIMESTAMP)
# Ønsker å flytte year, month, week, day og hour helt til vesntre.
df <- df %>% select(year,month,week_nr,day,hour, everything())
df <- df %>% filter(InvPDC_kW_Avg >= 0.0, Pyra1_Wm2_Avg >= 0.0)


# A) Et pyranometer måler innstrålingen av sol som et gitt antall Watt pr.kvadratmeter. 
# Velg en tilfeldig uke på sommeren 2015, og plott tidsstempel på X-aksen, og verdien til “Pyra1_Wm2_Avg” på Y-aksen.
# Lager ny dataframe med uke 30 (slutten av juli) og beregner daglig snitt innstråling pr kvm
df_a <- df %>% filter(week_nr==30)
df_a
plot_a <- ggplot(df_a, aes(x=TIMESTAMP, y=Pyra1_Wm2_Avg)) + geom_line() +
  labs(title="Solar radiance given as Watt per square meter",
       x = "Dates during july 2015", y = "W/sq.m")
plot_a

# B) “InvPDC_kW_Avg” er gjennomsnittlig effekt produsert av solcelleanlegget pr.minutt. Legg til denne verdien på Y-aksen.
df_b <- df %>% filter(week_nr==30) 
df_b <- as.data.frame(df_b)
df_b

plot_b <- ggplot(df_b, aes(x=TIMESTAMP)) + 
  geom_line(aes(y=Pyra1_Wm2_Avg, color="pyra")) +
  geom_line(aes(y=InvPDC_kW_Avg, color="inv")) +
  
  scale_color_manual(values = c("pyra" = "red", "inv" = "skyblue"),
    name = "",
    labels = c("pyra" = "Solar Radiance pr square meter", "inv" = "Effect produced per minute")) +
  labs(title="Solar radiance vs Effect produced",
       x = "Days during week 30 in year 2015", y = "Watt per sq.m per min")
plot_b 


# C) Er “Pyra1_Wm2_Avg” og “InvPDC_kW_Avg” avhengige eller uavhengige variabler? 
# Beregn Pearson Correlation mellom de to variablene. Begrunn svaret ditt. Hva tenker du om årsak og virkning?

cor(df_b$Pyra1_Wm2_Avg, df_b$InvPDC_kW_Avg)

# D) Plott sammenhengen mellom de to variablene (scatterplot), med “InvPDC_kW_Avg” på X-aksen, “Pyra1_Wm2_Avg” på Y-aksen.
d_plot <- ggplot(df_b,aes(x=Pyra1_Wm2_Avg, y=InvPDC_kW_Avg)) +
  geom_point(alpha=0.7, size=0.1) +
  labs(title = "Correlation between radiance and effect", x = "Solar Radiance", y = "Effect produced")
d_plot

# E) Estimer parametrene til en lineær regresjonsmodell med kun data fra 2015 hvor: InvPDC_kW_Avg = ⍺ + 𝛃*Pyra1_Wm2_Avg + 𝛜
# Vi antar at 𝛜 er i.i.d. normalt distribuert. Hvor stor andel av variansen i “InvPDC_kW_Avg” gjør vi rede for med “Pyra1_Wm2_Avg”? 
# Er dette en god modell? Finn et 95% konfidensintervall for 𝛃. Forklar hva det betyr at 𝛃 har dette konfidensintervallet.

# Kjører en lineær modell for å få info om statistisk modell output
mod_e <- lm(InvPDC_kW_Avg ~ Pyra1_Wm2_Avg, data=df_b)
summary(mod_e)
# Multiple R-squared har et veldig høy verdi på 0.99. Dette vil si at 99% av variansen i den gjennomsnitlige effekten
# produsert av solcellene kan forklares med endring i solstrålingen. 
# Om dette er en god modell: Selv om kvartilene viser stor variasjon (fra ca -154 til 28), er dette
# virker dette som en god modell fordi variansen til residuals er bare 8.285.
# Dette er forholdsvis lavt gitt at effekten produsert av solcellene strekker seg nesten opp til 400. 
# Mesteparten av residualene ligger i 3Q (3.567). Dette tyder på at ikke alt solstråling er reflektert
# i energi produksjonen. Dette kan skyldes f.eks vedlikehold eller at deler av anlegget var skrudd av.
# P-verdien er på mindre enn 2e-16. Dette er ekstremt lavt og tyder på at regresjonskoeffisientetn
# Beta_1 (stigningstallet) er langt ifra 0.

# "Finn et 95% konfidensintervall for 𝛃.
#  Forklar hva det betyr at 𝛃 har dette konfidensintervallet."

confint(mod_e, level=0.95)

# Da får jeg at konfidens intervallet for interseptet er den ekte verdien ligger mellom
# 4.6101428 og 5.5775146, 95% av tiden. Interseptet er startverdien for y, når X=0.

# Konfidensintervallet for koeffisienten (!) av Solstrålingen er mellom 0.2362031 og 0.2378831
# Denne koeffisienten forteller noe om hvor mye den energi produksjonen (avhengig variabel) endrer seg
# når soltrålingen endrer seg med en enhet. Forutsetningen er at alle andre uavhengige variabler
# er konstante - ikke aktuelt i denne oppgaven men generelt. 

# F) Finn 95% konfidensintervallet til InvPDC_kW_Avg når Pyra1_Wm2_Avg=400.
# Her snakker vi om estimering. Da trenger vi å lage ny data frame der Pyra = 400.
mod_f <- lm(InvPDC_kW_Avg ~ Pyra1_Wm2_Avg, data=df_b)
pyra_400 <- data.frame(Pyra1_Wm2_Avg = 400)
predict_f <- predict(mod_f, newdata = pyra_400, interval = "prediction", level=0.95)
predict_f
# fit      lwr      upr
# 99.91106 78.77772 121.0444
# fit: Dette vil si at den predikerte verdien av InvPDC_kW_Avg, når strålingen er 400,
# er 99.91106 ifølge modellen.

# lwr&upr: Ved gjentatte målinger/eksperimenter vil 95% av de predikerte konfidensintervallene 
# inneholde den sanne verdien av InvPDC_kW_Avg (parameteren vi estimerer).
# Konfidensintervaller beskriver et område der vi forventer den sanne verdier å ligge
# I dette tilfellet : mellom 78.77772 og 121.0444

# G) Lag et plott for verdier mellom 0 og Pyra1_Wm2_Avg på X-aksen, og på Y-aksen:
#   - De faktiske verdiene for InvPDC_kW_Avg (scatterplot)
# - Estimatet av E[InvPDC_kW_Avg | Pyra1_Wm2_Avg ] som en linje
# - Nedre og øvre grenseverdi for 95%-konfidensintervallet til estimatet av InvPDC_kW_Avg som linjer.

# Begynner med å lage en kombinert dataframe med scatterplottet fra tidligere og verdiene fra 
# prediksjonsmodellen av den enkle lineære regresjonen fra deloppgave f)
df_g <- data.frame(Pyra1_Wm2_Avg = df_b$Pyra1_Wm2_Avg, InvPDC_kW_Avg = df_b$InvPDC_kW_Avg)
df_g

predict_g <- predict(mod_f, newdata = df_g, interval = "prediction", level=0.95)
predict_g

# Her tar vi kun kolonnen Pyra på x-aksen og prediksjonsverdiene på y-aksen
df_kombo <- cbind(df_g, predict_g)
df_kombo

g_plot <- ggplot(df_kombo,aes(x=Pyra1_Wm2_Avg, y=InvPDC_kW_Avg)) +
  geom_point(alpha=0.5, size=0.1) +
  geom_line(aes(y=fit, color="Estimated effect")) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill = "Confidence band"), alpha = 0.4) +
  labs(title = "Actual vs Predicted",
       subtitle = "Analyzing the solar radiance's influence on the effect produced",
       x = "Solar Radiance", y = "Effect produced") +
  scale_colour_manual("", values="blue") +
  scale_fill_manual("", values="orange")
g_plot

# H) Finn residualen (𝛜) for alle tidsstemplene og lag et histogram som viser distribusjonen
# til 𝛜. Ser 𝛜 normaldistribuert ut?
df_kombo

# Residualene er jo forskjellen mellom de observerte verdiene (InvPDC_kW_Avg) og
# verdiene predikert av modellen (fit linjen)
# Begynner med å lage ny kolonne i dataframen df_kombo
df_kombo$res <- df_kombo$InvPDC_kW_Avg - df_kombo$fit
min(df_kombo$res)

h_plot <- ggplot(df_kombo,aes(x=res)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color="black", alpha=0.6) +
  labs(title = "Residuals - effect produced based on radiance",
       x = "Residuals", y = "Frequency")
h_plot

# Error ser ikke normal distribuert ut.

# Ønsker å sjekke hva som skal til for å få den til å bli det
minte <- sort(df_kombo$res)
minte[1:50]

h1_plot <- ggplot(df_kombo,aes(x=res-minte)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color="black", alpha=0.6) +
  labs(title = "Residuals - effect produced based on radiance",
       x = "Residuals", y = "Frequency")
h1_plot

# Hvis vi fjerner de 50 minste største residualene blir residualene ca normal distribuert.
# Dette er en upraksis/uvane.




