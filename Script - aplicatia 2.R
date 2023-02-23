# Modele cu date de tip panel
# Install packages
PackageNames <- c("tidyverse","gplots","plm","readxl","foreign","lmtest")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

directory <- "G:/Other computers/ASUS/An 3 sem 1/Econometrie E/Seminar/Proiect/Aplicatia 2"
data <- read.csv("G:/Other computers/ASUS/An 3 sem 1/Econometrie E/Seminar/Proiect/Aplicatia 2/Life_Expectancy.csv")

summary(data)

data <- data %>% mutate(Population = Population / 1000000)
data <- data %>% filter(Year > 2005)
pd.df <- pdata.frame(data, index = c("Country","Year"), drop.index = TRUE)
View(pd.df)

# Corelatia dintre Life_Expectancy/spatiu/timp
coplot(Life_Expectancy ~ Year|Country, type="l", data=data)

# Heterogeneitatea presupune ca exista diferente intre unitatile studiate
plotmeans(Life_Expectancy ~ Country, main = 'Heterogeneitate in randul tarilor', data = data)
#=> heterogeneitate - mediile pentru fiecare tara in parte nu se aliniaza

# Explorarea heterogeneitatii in sectiunea temporala
plotmeans(Life_Expectancy ~ Year, main = 'Heterogeneitate in timp', data = data)
#=> heterogeneitate - mediile pentru fiecare an in parte nu se aliniaza

# Model OLS - model clasic de regresie liniara
# Nu ia in calcul heterogeneitatea intre spatiu si timp
ols <- lm(Life_Expectancy ~ Population + CO2_emissions + Health_expenditure + Electric_power_consumption, data)
summary(ols) 
# modelul este valid
# R^2=41.15
# toate variabilele sunt semnificative 99%

# Grafic - Life_Expectancy in raport cu CO2_emissions
yhat <- ols$fitted # valori estimate
ggplot(data, aes(x=CO2_emissions, y=Life_Expectancy))+ #Life_Expectancy~CO2_emissions
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  theme_bw()

# Model FE (cu efecte fixe) 
fe <- plm(Life_Expectancy ~ Population + CO2_emissions + Health_expenditure + Electric_power_consumption, data, index = c('Country','Year'),
          model = 'within')
summary(fe)
# Unbalanced Panel: n = nr de paneluri => 38 tari
#                   T = ani => perioada 10 ani
#                   N = nr total de observatii => 380 observatii
# modelul este valid 
# R^2 = 14.97

# Alegerea celei mai adecvate variante de model prin testarea intre regresie OLS vs fixed effects panel model
# H0: FE 
# H1: OLS
pFtest(fe, ols) # p-value < 2.2e-16 < 0.05 => se recomanda model de panel data FE

# Model cu efecte aleatorii RE (random effects)
re <- plm(Life_Expectancy ~ Population + CO2_emissions + Health_expenditure + Electric_power_consumption, data, index = c('Country','Year'),
          model = 'between')
summary(re)
# modelul este semnificativ d p d v statistic (p-value: 0.00076589)
# R^2 = 42.96

# Testul Hausmann il utilizam pentru a decide intre FE si RE
# H0: model cu efecte random 
# H1: model cu efecte fixe
phtest(fe,re) # p-value = 3.789e-09 < 0.05 => se recomanda model cu efecte fixe 

# TESTARE IPOTEZE MODEL
# In urma testului Hausmann am decis sa utilizam modelul FE

# 1.Testarea efectelor fixe in timp
fixed.time <- plm(Life_Expectancy ~ Population + CO2_emissions + Health_expenditure + Electric_power_consumption + factor(Year), data=data, 
                  index=c("Country","Year"), model="within")
summary(fixed.time) # imi face cate o variabila pentru fiecare an

# H0:  nu sunt necesare efectele fixe in timp
# H1:  sunt necesare efectele fixe in timp
pFtest(fixed.time, fe) # p-value < 2.2e-16 < 0.05 => se recomanda folosirea efectelor fixe in timp
plmtest(fe, c('time'), type = 'bp') # p-value = 0.5061 > 0.05  => nu este nevoie sa se utilizeze efecte fixe in timp 
# Cele doua teste sunt inconcluzive => vom alege varianta in care nu avem efecte fixe in timp

# 2.Testarea efectelor aleatorii cu Breusch-Pagan Lagrange Multiplier
# Testul ne ajuta sa decidem intre RE si OLS 
pool <- plm(Life_Expectancy ~ Population + CO2_emissions + Health_expenditure + Electric_power_consumption, data=data, index=c("Country", "Year"), model="pooling")
summary(pool)

# H0: variatiile in timp sunt 0
# H1: variatiile in timp sunt diferite de 0
plmtest(pool, type=c("bp")) # p-value < 2.2e-16 < 0.05 => respingem ipoteza nula
# variatiile in timp sunt diferite de 0 => efectele aleatorii sunt adecvate pt ca exista diferente semnificative intre tari

# 3.Testarea dependentei transversale folosind testul Breusch-Pagan LM si testul Parasan CD
# H0: reziduurile intre tari nu sunt corelate
# H1: reziduurile intre tari sunt corelate
pcdtest(fe, test = 'lm') # p-value < 2.2e-16 < 0.05 => dependenta transversala
pcdtest(fe, test = 'cd') # p-value < 2.2e-16 < 0.05 => dependenta transversala
# Nu corectam pt ca avem panel mic. 

# 4.Testarea autocorelarii - Breusch-Godfrey / Wooldridge test 
# H0: Nu exista autocorelate
# H1: autocorelarea este prezenta
pbgtest(fe) # p-value < 2.2e-16 < 0.05 => avem autocorelare, dar fiind panelul mic o vom ignora

# 5.Testarea heteroschedasticitatii cu testul Breusch-Pagan
# H0: homoschedasticitate
# H1: heteroschedasticitate
bptest(Life_Expectancy ~ Population + CO2_emissions + Health_expenditure + Electric_power_consumption + factor(Country), data = data, studentize=F)
# p-value < 2.2e-16 < 0.05 => avem heteroschedasticitate
# Nu vom corecta, doar o vom testa.