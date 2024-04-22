###########
#This code takes data from the Seagrass and the Water Column Experiments
#It preps the data, calculates growth (and other responses by treatment), and combines the datasets
#Created by Bridget Shayka
##########


##Load libraries -------------
library(tidyverse)


##Load functions ------------
stand <- function(X) { (X-mean(X,na.rm=T))/(2*sd(X,na.rm=T)) }#function


##Load data -----------------

#Ratio Exp data
ratiodata <- read_csv('data/Ratio_growth_2021.csv')
#Growth, Length, and Width are in mm
#Blade_dry_weight is in grams

ratiotreatments <- read_csv('data/Ratio_treatments.csv')
#N and P are in  g m-2 d-1
#NPratio is the molar ratio

bbdata <- read_csv('data/Ratio_BB_2021.csv') %>%
  separate_rows(c(Thalassia, Syringodium), sep = ",", convert = TRUE) #this separates the four values in each of Thalassia and Syringodium (separated by commas) into new rows
#convert=TRUE converts the split up numbers into numbers instead of leaving them as characters
#all data are from quad B (south side)
#"Thalassia" and "Syringodium" are shoot counts per 10x10cm area, four per quad
#all other data are % cover of seagrass or algae according to Braun_Blanquette methods (0.1=1 to few; 0.5=many; 1=10-20%; 2=20-40%; 3=40-60%; 4=60-80%, 5=80-100%)
#animal counts are notes with actual numbers of animals

nutdata <- read_csv('data/Ratio_nutrients_year2_2021.csv') %>%
  mutate(Site = Sample_ID) %>%
  select(Site, N_percent, C_percent, d15N, d13C, P) %>%
  group_by(Site) %>%
  summarise(meanC = mean(C_percent,na.rm=T),
            meandC = mean(d13C,na.rm=T),
            meanN = mean(N_percent,na.rm=T),
            meandN = mean(d15N,na.rm=T),
            meanP = mean(P,na.rm=T)) %>%
  mutate(CN = (meanC/12.01)/(meanN/14.01),
         CP = (meanC/12.01)/(meanP/30.97),
         NP = (meanN/14.01)/(meanP/30.97))


#Water Column Exp data
wcdata <- read_csv('data/Water column expt.csv',
                   col_type=cols(day = col_character())) %>%
  filter(chla != "")
#n and p are in ug/L
#np is N:P molar ratio
#chla is in ug/L
#nh4 is in ug/L
#SRP is in ug/L




##Data Prep ----------------

##Ratio Exp data prep ---------

ratiogrowthdata <- ratiodata %>%
  select(Days, Site, Shoot, Blade, Growth, Length, Width, Blade_dry_weight, Epiphyte_dry_weight, Bitten, Bites) %>%
  mutate(garea = Growth * Width, #in mm2 #area of new growth during Days time period
         totarea = Length * Width) %>% #in mm2
  group_by(Site, Shoot) %>%
  summarise(gshootarea = sum(garea, na.rm=T), #in mm2
            totshootarea = sum(totarea, na.rm=T), #in mm2
            totshootweight = mean(Blade_dry_weight, na.rm=T), #in g #this is mean b/c all blades were weighed together from a shoot and same shoot weight value is recorded for every blade
            days = mean(Days, na.rm=T), #removes NA values where there are some numbers and some NAs; otherwise just get NAs
            epweight = mean(Epiphyte_dry_weight), #in g #total epiphyte weight by shoot; use mean b/c epiphyte weights are copied across all blades of a shoot (DO NOT SUM)
            shootheight = max(Length), #in mm #height of tallest blade in each shoot (even if bitten)
            notop = sum(Bitten == "y", na.rm=T), #number of blades missing top (by shoot)
            blades = n(), #number of blades (by shoot)
            bites = mean(Bites)) %>% #number of bites (in each shoot); use mean b/c bites are copied across all blades of a shoot (DO NOT SUM)
  mutate(growthratearea = gshootarea/days, #in mm2/day
         gshootweight = gshootarea* (totshootweight/totshootarea), #in g #weight of new growth during Days time period
         growthrateweight = gshootweight/days,  #in g/day
         notopratio = notop/blades,
         epweightperarea = epweight/totshootarea, #in g/mm2
         biteperarea = bites/totshootarea) %>% #in bites/mm2
  filter(Site != "R10" | Shoot != "8") #use the OR symbol here b/c you are actually dealing with "nor" b/c you want rows that are NOT something
#remove site R10 shoot 8 because lost a blade from this shoot before scraping and weighing

ratiodatawtreatments <- left_join(ratiogrowthdata, ratiotreatments)

ratiodatawoctrl <- ratiodatawtreatments %>%
  filter(!is.na(N)) #need to use is.na to filter by NA values


#shoot count data from BBs
shootcount <- bbdata %>%
  group_by(Site) %>%
  summarise(tshoots = mean(Thalassia) * 100, #also converts from "per 100cm2 (10x10cm)" to "per 1m2", so this is per m2
            sshoots = mean(Syringodium)* 100)


#richness and evenness
rande <- read_csv('data/Ratio_BB_2021.csv') %>%
  filter(!is.na(Site)) %>%
  select(-(Thalassia:Syringodium),-AnimalCounts,-Notes) %>%
  rowwise() %>%
  dplyr::mutate(species = sum(!is.na(c_across(Thalassia.per:Batophora.oerstedii)))) %>%
  dplyr::mutate(across(everything(), ~replace_na(.x, 0))) #replaces all NAs with zeros in all columns
#this includes sponges but not coral (because there wasn't any) or any other animals  

#n = the tot number of org of a species
#N = tot number of organisms of all species
#d.1 = (sum(n(n-1))) / (N(N-1)) different type of simpsons (evenness)
#d.2 = sum((n/N)^2) different type of simpsons (evenness)
ehs <- read_csv('data/Ratio_BB_2021.csv') %>%
  filter(!is.na(Site)) %>%
  select(-(Site:Syringodium),-AnimalCounts,-Notes) %>%
  dplyr::mutate(across(everything(), ~replace_na(.x, 0))) %>% #replaces all NAs with zeros in all columns
  as.data.frame()
d.1 = matrix(ncol = 1,nrow = length(ehs[,1])) #[row,column]
d.2 = matrix(ncol = 1,nrow = length(ehs[,1]))
for(i in 1:length(ehs[,1])){
  
  dnow.1 = matrix(ncol = 1, nrow = length(ehs[1,]))
  dnow.2 = matrix(ncol = 1, nrow = length(ehs[1,]))
  for(j in 1:length(ehs[1,])){
    dnow.1[j,] = ehs[i,j] * (ehs[i,j]-1)
    dnow.2[j,] = (ehs[i,j]/sum(ehs[i,]))^2
  }
  
  d.1[i,] = colSums(dnow.1) #Don't use this one right now b/c the equation is weird above
  d.2[i,] = colSums(dnow.2) # I use this one  
}

rande[, "d.1"] <- as.data.frame(d.1)
rande[, "d.2"] <- as.data.frame(d.2) #these add the simpson's diversity metrics as columns at the end

re <- rande %>%
  select(Site, species, d.2) %>%
  mutate(D = 1-d.2)

#data by site
ratiodatabysite <- ratiodatawoctrl %>%
  group_by(Site) %>%
  summarise(avggrowtharea = mean(growthratearea), # mm2/day/shoot
            avggrowthweight = mean(growthrateweight), # g/day/shoot
            avgepweightperarea = mean(epweightperarea), # g/mm2
            avgbites = mean(bites), #avg bites/shoot by site
            avgbitespersa = mean(biteperarea), #avg bites/mm2 by site
            avgnotop = mean(notopratio), #average percent of blades missing top per shoot (aka ratio of top missing to full blades per shoot) per site
            avgbladearea = mean(totshootarea), #average blade area per shoot, by site
            avgheight = mean(shootheight), #average height of tallest blade in each shoot, by site
            ntrt = mean(N), #unique does the same thing b/c they are numbers, but if they were chrs, use unique
            nfac = as.factor(unique(nfac)), #makes the N treament categories a factor
            p = mean(P),
            np = mean(NPratio),
            .groups = "drop") %>% #explicitly tells summarize to drop groups at the end
  mutate(nstand = stand(ntrt),
         p.s = stand(p)) %>%
  mutate (n = as.factor(ntrt),
          n.s = as.factor(round(nstand, digits=3)),
          np.s = stand(np)) %>%
  left_join(shootcount) %>% #adds shoot count data as new columns, joins by Site #per m2
  left_join(nutdata) %>% #adds nutrient data (%N, %C, %P, N and C isotopes, and ratios)
  left_join(re)
#the last mutate function reduces the number of decimals to whatever "digits" equals and then converts the column to a factor so you can analyze the N treatment as a categorical variable

write_csv(x = ratiodatabysite, file = "processed_data/ratio_data.csv")



##Water Column Exp data prep ---------

wcwoutctrls <- wcdata %>%
  dplyr::filter(n != "Control") %>%
  mutate(ntrt = as.numeric(n),
         p = as.numeric(p),
         np = as.numeric(np),
         n = as.factor(n),
         nfac = as.factor(case_when(n == 10 ~ "a",
                                    n == 30 ~ "b",
                                    n == 50 ~ "c"))) %>%
  mutate(nstand = stand(ntrt),
         p.s = stand(p),
         np.s = stand(np)) %>%
  mutate (n.s = as.factor(round(nstand, digits=3))) %>%
  mutate(srp = replace(srp, srp > 100, NA)) #this replaces srp values greater than 100 with NA b/c they are likely contaminated

wcgrowthrates <- wcwoutctrls %>%
  pivot_wider(names_from = day, values_from = c(hrs, chla.init, chla, nh4.init, nh4, srp.init, srp)) %>%
  select(!c(chla.init_2, chla.init_3, nh4.init_2, nh4.init_3, srp.init_2, srp.init_3)) %>%
  rename(chla.init = chla.init_1, nh4.init = nh4.init_1, srp.init = srp.init_1) %>% #new name = old name
  mutate(growth_1 = (chla_1 - chla.init)/hrs_1, #initial to day 1
         growth_2 = (chla_2 - chla_1)/hrs_2, #day 1 to day 2
         growth_3 = (chla_3 - chla_2)/hrs_3, #day 2 to day 3
         day1to3growth = (chla_3 - chla_1)/(hrs_2 + hrs_3), #day 1 to day 3 #in ug L-1 hr-1
         day1to2growth = (chla_2 - chla_1)/(hrs_2),
         day2to3growth = (chla_3 - chla_2)/(hrs_3),
         day1to2srp = (srp_2 - srp_1)/(hrs_2),
         day2to3srp = (srp_3 - srp_2)/(hrs_3),
         day1to3srp = (srp_3 - srp_1)/(hrs_2 + hrs_3),
         day1to2nh4 = (nh4_2 - nh4_1)/(hrs_2),
         day2to3nh4 = (nh4_3 - nh4_2)/(hrs_3),
         day1to3nh4 = (nh4_3 - nh4_1)/(hrs_2 + hrs_3))

wclonggrowthrates <- wcgrowthrates %>%
  pivot_longer(cols = c(hrs_1:hrs_3, chla_1:chla_3, nh4_1:nh4_3, srp_1:srp_3, growth_1:growth_3),
               names_to = c(".value", "day"), #special .value sentinel, the name of the value column will be derived from part of the existing column names
               names_pattern = "(.+)_(.)$") #()() = two parts, .+ = at least one, . = exactly one, $ = end of the string 
#see last example in ?pivot_longer and Regular Expressions cheat sheet for help with syntax

write_csv(x = wclonggrowthrates, file = "processed_data/wc_data_long.csv")
write_csv(x = wcgrowthrates, file = "processed_data/wc_data.csv")

##Combined growth data ---------

ratiogrowthonly <- ratiodatabysite %>%
  mutate(growth = avggrowthweight,
         expt = "seagrass") %>%
  select(np, n, p, ntrt, nstand, n.s, p.s, np.s, nfac, growth, expt) %>%
  mutate(growth.s = stand(growth))
#ntrt, p, np are dbl; nstand, p.s, np.s are dbl standardized values; n is factor of ntrt; n.s is factor of nstand; nfac is factor of n values as chrs (a,b,c)


wcgrowthonly <- wcgrowthrates %>%
  select(bottle, day1to3growth, n, p, np, ntrt, nfac) %>%
  group_by(np,n,p,ntrt,nfac) %>%
  summarise(growth = mean(day1to3growth, na.rm = T)) %>%
  ungroup() %>%
  mutate(nstand = stand(ntrt),
         p.s = stand(p),
         np.s = stand(np)) %>%
  mutate (n.s = as.factor(round(nstand, digits=3))) %>%
  mutate(growth.s = stand(growth),
         expt = "phyto")
#ntrt, p, np are dbl; nstand, p.s, np.s are dbl standardized values; n is factor of ntrt; n.s is factor of nstand; nfac is factor of n values as chrs (a,b,c)


growthdatabytrt <- rbind(ratiogrowthonly,wcgrowthonly)
write_csv(x = growthdatabytrt, file = "processed_data/combined_growth.csv")





##Comparisons of seagrass data with controls ---------

#data by site with control, for comparisons
ratiodatabysite2 <- ratiodatawtreatments %>%
  mutate(N = replace_na(N, 0),
         P = replace_na(P, 0),
         NPratio = replace_na(NPratio, 0),
         nfac = replace_na(nfac, "d")) %>%
  group_by(Site) %>%
  summarise(avggrowtharea = mean(growthratearea), # mm2/day/shoot
            avggrowthweight = mean(growthrateweight), # g/day/shoot
            avgepweightperarea = mean(epweightperarea), # g/mm2
            avgbites = mean(bites), #avg bites/shoot by site
            avgbitespersa = mean(biteperarea), #avg bites/mm2 by site
            avgnotop = mean(notopratio), #average percent of blades missing top per shoot (aka ratio of top missing to full blades per shoot) per site
            avgbladearea = mean(totshootarea), #average blade area per shoot, by site
            avgheight = mean(shootheight), #average height of tallest blade in each shoot, by site
            ntrt = mean(N), #unique does the same thing b/c they are numbers, but if they were chrs, use unique
            nfac = as.factor(unique(nfac)), #makes the N treament categories a factor
            p = mean(P),
            np = mean(NPratio),
            .groups = "drop") %>% #explicitly tells summarize to drop groups at the end
  mutate(nstand = stand(ntrt),
         p.s = stand(p)) %>%
  mutate (n = as.factor(ntrt),
          n.s = as.factor(round(nstand, digits=3)),
          np.s = stand(np)) %>%
  left_join(shootcount) %>% #adds shoot count data as new columns, joins by Site #per m2
  left_join(nutdata) %>% #adds nutrient data (%N, %C, %P, N and C isotopes, and ratios)
  left_join(re)
#the last mutate function reduces the number of decimals to whatever "digits" equals and then converts the column to a factor so you can analyze the N treatment as a categorical variable

ratiodatabysite2$meanP[2] <- NA #contaminated sample
ratiodatabysite2$CP[2] <- NA #contaminated sample
ratiodatabysite2$NP[2] <- NA #contaminated sample

write_csv(x = ratiodatabysite2, file = "processed_data/ratio_data_wctrl.csv")


