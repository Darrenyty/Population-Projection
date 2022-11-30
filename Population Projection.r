## R tutorial 9 - Population projection
## DEMO2002 
## 04-May-2022

## Note: this code is INCOMPLETE, 
## We are going to complete it together during the tutorial


library(dplyr)   # for data manipulation 
library(ggplot2) # for data visualization


## PREPARE DATA ####

# country: Japan
Names <- c("JPN")
Names2 <- c("Japan")

### iso3 country code
t <- 1
  
### name of country
t2 <- t

### initial population
YEAR <- 2010

# locate my working directory (FOLDER1), and my data folder: HFD (CountryF) and HMD (CountryM) 
FOLDER1<-"D:/.DEMO2002/W9 DEMO"
CountryF<-"D:/.DEMO2002/W9 DEMO/hfd_jpn"
CountryM<-"D:/.DEMO2002/W9 DEMO/hmd_jpn"


# import fertility statistics from HFD folder
setwd(CountryF)

#period asfr by year and age (Lexis squares) for all countries
f0 <- read.table("JPNasfrRR.txt",header=TRUE,fill=TRUE,skip=2,as.is=TRUE )             
f<-f0[f0$Year==YEAR,]                   # select year. We hold asfr constant at YEAR(2020) level in projection
Fe<-c(rep(0,12),f$ASFR,rep(0,(110-55))) # apply asfr to reproductive ages
# Note that Fe[1]=0 is for age 0, Fe[12]=0 is for age 11, Fe[13]=f$ASFR[1] is for age 12  



# import mortality, population, and births statistics from HMD folder
setwd(CountryM)


l<-read.table("fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1,as.is=TRUE)  # female life table
lm<-read.table("mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1,as.is=TRUE) # male life table


Pop<-read.table("Population.txt",header=TRUE,fill=TRUE,skip=1,as.is=TRUE) # population

B<-read.table("Births.txt",header=TRUE,fill=TRUE,skip=1,as.is=TRUE) # birth
B<-B[B$Year==YEAR,] # birth in YEAR(2020)

# now go to the working directory for week 9
setwd(FOLDER1)


## PREPARE FOR PROJECTION ####
#### To run this program you need to calculate the following:

### 1) Sx and Sxm: survival ratios for females and males respectively 
#### to run the program they should have a length of (110 not 111)!!! 
#### you should get them from the corresponding life tables from the 
#### Human Mortality Database 

l<- l[l$Year == YEAR,]              # female life table for the defined YEAR, age 0-110+ 
lm<- lm[lm$Year == YEAR,]           # male life table for the defined YEAR, age 0-110+

# we minus the first entry cuz we will be calculating later
# we dont wan zero in our denominator
Sx<- ifelse(l$Lx[-111]>0,l$Lx[-1]/l$Lx[-111],0)     # Survival ratio calculated from female life table, by age 
Sxm<-ifelse(lm$Lx[-111]>0,lm$Lx[-1]/lm$Lx[-111],0)  # Survival ratio calculated from male life table, by age


### 2) L111 and L111m the survival for the last two age groups 
### (one number each)

L111<- ifelse(l$Tx[110]>0,l$Tx[111]/l$Tx[110],0)   # use l$Tx
L111m<- ifelse(lm$Tx[110]>0,lm$Tx[111]/lm$Tx[110],0)


### (replace the last value in Sx and Sxm)
Sx[110]<- L111
Sxm[110]<- L111m


### 3) line1: a combination of fertility and survival information
### for the first row of the matrix which returns the number 
### of babies when multiplied by the population it is also (110 long ### i.e. skip the last value)

SRB<-B$Male/B$Female

L1<-l$Lx[1]/(2*l$lx[1])
L1m<-lm$Lx[1]/(2*lm$lx[1])

#p50
k<-L1/(1+SRB) # this is for female babies

line1<-k*(Fe[-1]*Sx + Fe[-111]) # this is for female survivor and baby survivors

### 4) Ipop and Ipopm the base population of females and males 
### respectively (111 entries each)

Ipop<- as.numeric(Pop[Pop$Year==YEAR,]$Female)
Ipopm<- as.numeric(Pop[Pop$Year==YEAR,]$Male)


############################################################
###########  the rest of the program is already written
############################################################

Proj<-101			# let's look at a projection for 50 years

Mf<-cbind(rbind(line1,diag(Sx)),c(rep(0,110),L111))
Mm<-cbind(rbind(rep(0,length(line1)),diag(Sxm)),c(rep(0,110),L111m))

pop<-matrix(0,Proj,length(Ipop)) 	# creates a matrix whit each new line 
# for each new year of the female population
popm<-matrix(0,Proj,length(Ipop))   # same for males

pop[1,]<- Ipop  	 #initialize the first row of female population
colnames(pop)=0:110
rownames(pop)=1:Proj
popm[1,]<- Ipopm   #initialize the first row of male population
colnames(popm)=0:110
rownames(popm)=1:Proj

K<-SRB*L1m/L1 ## factor for calculating and surviving the male babies 



# How to visualize the projection? 
# Plot population pyramid 
# The pyramid function uses similar code from week 6
Pyramid<-function(Po,Ye){  # Po for population data table, Ye for year 
  p <- ggplot(data = filter(Po, Year == (YEAR+Ye-1)), aes(x = Age,  y = pct, fill = Sex)) + 
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = abs, limits = max(Po$pct) * c(-1.2,1.2)) +
    labs(x = "Age", y = "Percentage of Population",
         title = paste("Population pyramid of",unique(Po$Code)),
         subtitle = (YEAR+Ye-1),
         caption = "Data source: HMD, HFD")+
    coord_flip()+
    scale_fill_brewer(palette= "Set1")
  
  print(p) 
  
  ggsave(paste0("population pyramid_",unique(Po$Code),"_",(YEAR+Ye-1),".png"),width=15, height=12,units="cm")
  
  
  if((YEAR+Ye)==Proj+min(Po$Year))  {   # for the last year in projection, we animate the plot
    
    for (year in min(Po$Year):max(Po$Year)){
      
      print(ggplot(filter(Po, Year == year), aes(x = Age,  y = pct, fill = Sex)) + 
              geom_bar(stat = "identity") +
              scale_y_continuous(labels = abs, limits = max(Po$pct) * c(-1.2,1.2)) +
              labs(x = "Age", y = "Percentage of Population",
                   title = paste("Population pyramid of",Names2[t]),
                   subtitle = paste("Year:",year),
                   caption = "Data source: HMD, HFD")+
              coord_flip()+
              scale_fill_brewer(palette= "Set1"))
      
      Sys.sleep(2)
      
    }
  }
}


############
#### Pyramid for the initial population in 2020
############

### use the code from week 6 to calculate percentages by age and sex
### to prepare us for population pyramid plot 
### ipop for population pyramid (percentage)

ipop=Pop[Pop$Year==YEAR,]
unique(ipop$Age)
ipop$Age[ipop$Age=="110+"] <- "110"
ipop$Age=as.numeric(ipop$Age)

ipop <- ipop %>% 
  group_by(Year) %>%                     
  mutate(pctf=Female/sum(Total)*100,
         pctm=Male/sum(Total)*-100) %>%  
  select(Year, Age, pctf, pctm)

ipopf <- ipop %>%  select(Year, Age, pctf)
ipopf$Sex <- c("Female")

ipopm <- ipop %>%  select(Year,Age,pctm)
ipopm$Sex <- c("Male")

sum(ipopf$pctf)-sum(ipopm$pctm) # 100

names(ipopf)[names(ipopf)=="pctf"]="pct"
names(ipopm)[names(ipopm)=="pctm"]="pct"

ipop <- rbind(ipopf, ipopm) 
ipop$Code <- Names[t]
ipop$Year <- as.numeric(ipop$Year)

ipop1=ipop

Pyramid(ipop1,1)

############
#### Pyramid for the projections
############

# reset the population projection to year 1 (ipop1 is where the year 1 population stores)
ipop=ipop1 

for (year in 2:Proj){
  
  # t() is transpose - transpose the row to col
  pop[year,]<-t(Mf%*%pop[(year-1),])
  # male dh first row 
  popm[year,]<-c(pop[year,1]*K,t(Mm%*%popm[(year-1),])[-1])
  To<-sum(pop[year,]+popm[year,])
  
  ipop_new <- data.frame(Year=(year+YEAR-1),Age=0:110,Female=pop[year,],Male=popm[year,],Total=To)
  
  ipop_new <- ipop_new %>% 
    group_by(Year) %>%                     
    mutate(pctf=Female/To*100,
           pctm=Male/To*-100) %>%  
    select(Year, Age, pctf, pctm)
  
  ipopf <- ipop_new %>%  select(Year, Age, pctf)
  ipopf$Sex <- c("Female")
  
  ipopm <- ipop_new %>%  select(Year,Age,pctm)
  ipopm$Sex <- c("Male")
  
  sum(ipopf$pctf)-sum(ipopm$pctm) 
  
  names(ipopf)[names(ipopf)=="pctf"]="pct"
  names(ipopm)[names(ipopm)=="pctm"]="pct"
  
  ipop_new <- rbind(ipopf, ipopm) 
  ipop_new$Code <- Names[t]
  
  # add to the beginning population ipop
  ipop <- rbind(ipop,ipop_new)
  
  Pyramid(ipop,year)
}


## The gganimate option ####
## code will be provided in the complete code


## SAVE YOUR DATA IN R ####

save.image("Week9 Tutorial.RData")

