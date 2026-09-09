setwd("~/Desktop/R programming/assignment1")
install.packages("corrplot")
install.packages("readr")
install.packages("dplyr")
library(readr)
library(ggplot2)
library(dplyr)

#DATAINLÄSNING---------------------------------------------------------------------------

#Läs in data set, för att undvika att nollan försvinner i election results FIPS
#använder vi colClasses
elect_result <- read.csv("general_result.csv", 
                         colClasses = c("FIPS" = "character", 
                         "combined_fips" = "character"))
#Läs in övriga data sets
facts <- read.csv("county_facts.csv")
facts_dictionary <- read.csv("county_facts_dictionary.csv")

#Crime data är en tabell och måste läsas in med read.table
crime_data <- read.table("crime_data.tsv", sep = "\t", header = TRUE)

#Undersök vilka rader som är 999 och 777
rader_999 <- crime_data[crime_data$FIPS_CT == "999", ]
rader_777 <- crime_data[crime_data$FIPS_CT == "777", ]

#Ta bort kolumner i crime_data där cty är 999 och 777
crime_data <- crime_data[crime_data$FIPS_CT != "999", ]
crime_data <- crime_data[crime_data$FIPS_CT != "777", ]

#Kolla kolumner som är 00000 i facts
rader_00000 <- facts[facts$fips == 00000, ]

#Ta bort kolumner i facts fips som är 00000
facts <- facts[facts$fips != 00000, ]

#Vi ser att alla tre dataset har olika sätt att skriva sin FIPS kod
#Crime_data har en fip för state och en för city, facts har en fyra siffrig fip
#elect_result har en fem siffrig, vi vill standardisera detta så vi kan mergea
#alla tre dataset by FIPS

#Facts df har endast en 4-5 siffrig FIPS kod, fyll ut med
#0:or för att matcha med de andra data sets och garantera 5 siffrig FIPS

facts$fips <- formatC(as.numeric(facts$fips), width = 5, format = "d", flag = "0")

#Ta bort summary rows för varje state 
facts <- facts %>%
  mutate(state_abbreviation = na_if(state_abbreviation, "")) #Vi gör de tomma till NA istället för ""

#Kolla vilka rader som har NA i state abbreviation
rader_na_state <- facts %>%
  filter(is.na(state_abbreviation))

#Ta bort dem rader där state abbreviation är NA
facts <- facts %>%
  filter(!is.na(state_abbreviation))

#Vi gör om crime datas två fip till en 5 siffrig så vi kan sammanfoga med de andra 

#Tvinga state till två tecken, fyll ut med 0:or åt vänster

crime_data$FIPS_ST <- formatC(as.numeric(crime_data$FIPS_ST), width = 2, format = "d", flag = "0")

#Samma sak - tvinga till tre tecken, fyll ut med 0:or åt vänster om behövs

crime_data$FIPS_CTY <- formatC(as.numeric(crime_data$FIPS_CT), width = 3, format = "d", flag = "0")

#Skapa en ny kolumn i crime med fullständig FIPS kod
crime_data$FIPS <- paste0(crime_data$FIPS_ST, crime_data$FIPS_CT)

#Byt namn så fips i facts också är med stora bokstäver, 
#så alla har FIPS med stora bokstäver
facts <- facts %>%
  rename(FIPS = fips)

#Sammanfoga alla tre dataset med deras 5 siffriga fip kod, vi kör left join med elect result
merged_data <- left_join(elect_result, crime_data, by = "FIPS")
merged_data <- left_join(merged_data, facts, by = "FIPS")

#Vi kollar efter duplikat av FIPS koder
sum(duplicated(merged_data$FIPS)) #Inga duplikat av FIPS koder!

#Kolla efter missing values i merged data
missing_data <- merged_data %>%
  filter(if_any(everything(), is.na))

#Vid kontroll av koden ser vi att alla dessa tillhör Alaska med exakt duplikat
#, vi tar därför bort Alaska 
merged_data <- merged_data[merged_data$state_abbr != "AK", ]

#Kolla igen efter missing values
#Kolla efter missing values i merged data
missing_data2 <- merged_data %>%
  filter(if_any(everything(), is.na)) 

#Den blev 0, vi fortsätter

#Vi kollar om vi har fler exakta duplikat i koden
copies <- duplicated(merged_data[, c("per_gop_2016", "votes_dem_2016", "total_votes_2016", "diff_2016")])

#Se hur många det är
sum(copies)

#Det va noll, vi kan fortsätta

#Kolla vilka FIPS som fanns i elect result men inte crime
missing_in_crime <- elect_result %>%
  anti_join(crime_data, by = "FIPS")

#Kolla vilka FIPS som fanns i elect result men inte facts
missing_in_facts <- elect_result %>%
  anti_join(facts, by = "FIPS")

#Kolla alla unika
all_missing_rows <- bind_rows(missing_in_crime, missing_in_facts) %>%
  distinct()

#Vi ser också att dessa är exakta duplikat som alla tillhör Alaska


#Ta bort onödiga kolumner
merged_data <- subset(merged_data, select = -c(X, combined_fips, FIPS_ST, FIPS_CTY, 
                                               STUDYNO, EDITION, PART, IDNO, 
                                               county_fips, state_fips))

#QUALITY CONTROL - Vi kollar varje dataset efter NA värden
#Kolla crime_data

colSums(is.na(crime_data))

#Kolla facts
colSums(is.na(facts))

#Kolla elect_result
colSums(is.na(elect_result))

#Vi hittar några NA värden men inga av dessa är av magnitud att skada analysen

#Vi ändrar till bättre namn i vår sammanfogade
merged_data <- merged_data %>%
  rename(
    pop_est_2014 = PST045214,
    pop_est_base_2014 = PST040210,
    pop_per_change_2010_2014 = PST120214,
    pop_2010 = POP010210,
    pct_under_5_2014 = AGE135214,
    pct_under_18_2014 = AGE295214,
    pct_over_65_2014 = AGE775214,
    pct_female_2014 = SEX255214,
    pct_white_alone_2014 = RHI125214,
    pct_black_alone_2014 = RHI225214,
    pct_american_indian_alaskan_native_2014 = RHI325214,
    pct_asian_alone_2014 = RHI425214,
    pct_native_hawaiian_pacific_islander_2014 = RHI525214,
    pct_races_two_or_more_2014 = RHI625214,
    pct_hispanic_latino_2014 = RHI725214,
    pct_white_not_hispanic_2014 = RHI825214,
    pct_in_same_house_0913 = POP715213,
    pct_foreign_born_0913 = POP645213,
    pct_language_other_than_english_0913 = POP815213,
    pct_high_school_degree_0913 = EDU635213,
    pct_bachelor_degree_or_higher_0913 = EDU685213,
    number_veterans_0913 = VET605213,
    mean_traveltime_0913 = LFE305213,
    housing_units_2014 = HSG010214,
    homeownership_rate_0913 = HSG445213,
    pct_multiunit_structures_0913 = HSG096213,
    median_value_owner_occupied_units_0913 = HSG495213,
    households_0913 = HSD410213,
    persons_per_household_0913 = HSD310213,
    per_capita_income_0913 = INC910213,
    median_household_income_0913 = INC110213,
    pct_persons_below_poverty_level_0913 = PVY020213,
    private_non_farm_establsh_2013 = BZA010213,
    private_non_farm_employment_2013 = BZA110213,
    private_non_farm_pct_change = BZA115213,
    nonempl_establish_2013 = NES010213,
    total_firms_2007 = SBO001207,
    pct_black_owned_firms_2007 = SBO315207,
    pct_amer_indian_alask_nativ_firms_2007 = SBO115207,
    pct_asian_owned_firms_2007 = SBO215207,
    pct_hawaiian_pac_islander_firms_2007 = SBO515207,
    pct_hispanic_owned_firms_2007 = SBO415207,
    pct_women_owned_firms_2007 = SBO015207,
    manufactur_shipment_2007 = MAN450207,
    merchant_wholesales_2007 = WTN220207,
    retail_sales_2007 = RTN130207,
    retail_saler_per_cap_2007 = RTN131207,
    accomodation_food_serv_sales_2007 = AFN120207,
    bulding_permits_2014 = BPS030214,
    land_area_sqmile_2010 = LND110210,
    pop_sqrmile_2010 = POP060210)

#-------------------------------------------------------------------------------

#VARIABELSKAPANDE-----------------------------------------------------------------

#Vi måste ha brotten i något annat än antal för att vara jämförbara
crime_list <- c("AG_ARRST", "AG_OFF", "VIOL", "PROPERTY", 
                "MURDER", "RAPE","ROBBERY", "AGASSLT", "BURGLRY", 
                "LARCENY", "MVTHEFT", "ARSON")

#Vi gör en loop som delar antalet brott per county på populationen 2014 i countyt, och tar gånger
#100 000 för att få andel per 100 000 invånare, byter också namn till AG_arrst_rate osv... 
for(x in crime_list){
  merged_data[[paste0(x, "_rate")]] <- (merged_data[[x]] / merged_data$pop_est_2014) * 100000
}

#Invertera variabeln per point diff så den blir större vid Trump vinst istället
#för tvärtom

merged_data$per_point_diff_2016 <- merged_data$per_point_diff_2016 * -1
merged_data$per_point_diff_2012 <- merged_data$per_point_diff_2012 * -1

#Vi tar per_gop_2016 * 100 för att få värden direkt i procent
merged_data$per_gop_2016 <- merged_data$per_gop_2016 * 100


#Vi kan skapa en variabel för säker Trump vinst, Säker Clinton och tie
merged_data$margin <- cut(merged_data$per_point_diff_2016, 
                          breaks = c(-1.2, -0.1, 0.1, 1.2), #1.2 ifall R avrundar skumt vid exakt 1
                          labels = c("Strong Clinton", "Tie", "Strong Trump"))


#Dela in percentage white i 4 nästan lika stora grupper (kvantiler)
limits_race <- quantile(merged_data$pct_white_not_hispanic_2014, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

merged_data$race_homogeneity <- cut(merged_data$pct_white_not_hispanic_2014, 
                                    breaks = limits_race,
                                    include.lowest = TRUE,
                                    labels = c("Most Diverse", 
                                               "Moderately Diverse", 
                                               "Majority White", 
                                               "Near all White"))

#Kolla längd per grupp
table(merged_data$race_homogeneity)

#Dela in multi unit housing i 5 nästan lika stora grupper (kvantiler)
limits_housing <- quantile(merged_data$pct_multiunit_structures_0913, 
                            probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)

merged_data$housing_groups <- cut(merged_data$pct_multiunit_structures_0913, 
                                   breaks = limits_housing,
                                   include.lowest = TRUE,
                                   labels = c("Very Low Multi-unit Housing", 
                                              "Low Multi-unit Housing", 
                                              "Moderate Multi-unit Housing", 
                                              "High Multi-unit Housing", 
                                              "Very High Multi-unit Housing"))
#Kolla längd per grupp
table(merged_data$housing_groups)

#Vi delar in utbildning i 4 grupper för utbildning baserat på fördelningen
merged_data$edu_groups <- cut(merged_data$pct_bachelor_degree_or_higher_0913, 
                               breaks = c(0, 15, 25, 40, 100), 
                               labels = c("Low Education", 
                                          "Moderate Education", 
                                          "High Education", 
                                          "Very High Education"), 
                               include.lowest = TRUE)

#Gör en variabel för att dela in rån i två  grupper
merged_data$robbery_groups <- ifelse(merged_data$ROBBERY_rate > median(merged_data$ROBBERY_rate, na.rm = TRUE),
                                     "Higher Robbery Rate (Upper limit)", "Lower Robbery Rate (Lower limit)")
    

#----------------------------------------------------------------------------------------------------------

#VARIABELSÖK-----------------------------------------------------------------------------------------------

#Vi väljer ut endast de kolumner med siffer värden
merged_data_numbers <- merged_data %>%
  select(where(is.numeric))

#Räkna korrelation mellan alla numeriska värden och andel Trump röster
cor_matrix <- cor(merged_data_numbers, merged_data_numbers$per_gop_2016, use = "pairwise.complete.obs")
print(cor_matrix)

#Sortera från högst till lägst korrelation, inkludera variabelnamn
cor_sorted <- sort(cor_matrix[,1], decreasing = TRUE)
#Sortera från lägst till högst korrelation, inkludera variabelnamn
cor_sorted2 <- sort(cor_matrix[,1], decreasing = FALSE)

#-----------------------------------------------------------------------------------------------------------

#UNCONDITIONAL SUMAMRIES-----------------------------------------------------------------------------------

#1. Trumpstöd i USA 2016

summary(merged_data$per_gop_2016)

#2. Andel med kandidatexamen eller högre
summary(merged_data$pct_bachelor_degree_or_higher_0913)

#3. Andel vita ensamma
summary(merged_data$pct_white_not_hispanic_2014)

#4. Housing units in multi-unit structures
summary(merged_data$pct_multiunit_structures_0913)

#5. Crime data - andel rån per 100 000 invånare 
summary(merged_data$ROBBERY_rate)

#----------------------------------------------------------------------------------------------------------

#GROUPED SUMMARIES-----------------------------------------------------------------------------------------

#Hur såg röstandel ut på Trump låg vs högutbildning
aggregate(per_gop_2016 ~ edu_groups, data = merged_data, FUN = mean, na.rm = TRUE)
#Hur många hamnade i varje grupp med denna indelning?
aggregate(per_gop_2016 ~ edu_groups, data = merged_data, FUN = length)

#Hur såg röstandel ut på Trump urban vs rural
aggregate(per_gop_2016 ~ housing_groups , data = merged_data, FUN = mean, na.rm = TRUE)
aggregate(per_gop_2016 ~ housing_groups , data = merged_data, FUN = length)

#Hur såg röstandel ut på Trump baserat på andel vita
aggregate(per_gop_2016 ~ race_homogeneity , data = merged_data, FUN = mean, na.rm = TRUE)
aggregate(per_gop_2016 ~ race_homogeneity , data = merged_data, FUN = length)

#Röstandel på Trump baserad på andel multi-unit structures
aggregate(per_gop_2016 ~ housing_groups , data = merged_data, FUN = mean, na.rm = TRUE)

#Hur såg röstandel ut på Trump baserat på rån per 100 000 medborgare
aggregate(per_gop_2016 ~ robbery_groups , data = merged_data, FUN = mean, na.rm = TRUE)
median(merged_data$ROBBERY_rate, na.rm = TRUE)

#----------------------------------------------------------------------------------------------------------

#KANDIDATUTBILDNING ELLER HÖGRE OCH TRUMPRÖSTER--------------------------------------------------------------------------

#Plot 1 - Scatterplot mellan kandidatutbildning eller högre och andel röster för GOP 2016
ggplot(merged_data, aes(x = pct_bachelor_degree_or_higher_0913, y = per_gop_2016)) +
  geom_point() + #Scatterplot
  geom_smooth(method = "lm", se = FALSE, color = "blue") + #Lägger till regressionslinje
  theme_minimal() +
  labs(title = "",
       x = "Bachelor's degree or higher, percent of persons age 25+, 2009-2013 (%)",
       y = "Percentage of votes for GOP 2016 (%)") +
  scale_x_continuous(limits = c(0, 100)) + #Sätter gränser för x-axeln
  scale_y_continuous(limits = c(0, 100)) #Sätter gränser för y axeln

#Plot 2 - Facet med 4 histogram + densitet, andel Trump röster per utbildningsgrupp
ggplot(merged_data, aes(x = per_gop_2016, fill = edu_groups)) + #Histogram
  geom_histogram(aes(y = ..density..), color = "black") + #Vi får gggplot att använda densitet då grupperna är olika stora
  geom_density(alpha = 0.4) + #Styr genomskinligheten o fixar densiteten
  facet_wrap(~edu_groups) + #Gör det till en facet så vi får 4 grupper
  theme_minimal() + #Minimalistiskt tema
  labs(title = "",
       x = "Percentage of GOP votes 2016 (%)",
       y = "Density",
       fill = "Education Groups") +
  scale_fill_manual(values = c("Low Education" = "lightblue", 
                               "Moderate Education" = "lightskyblue", 
                               "High Education" = "cornflowerblue",
                               "Very High Education" = "steelblue")) #Väljer färg per grupp

#Plot 3 - Scatterplot med tredje aestetic, färg för margin
#Samband mellan andel vita, kandidatexamen och vinstmarginal

ggplot(merged_data, aes(x = pct_bachelor_degree_or_higher_0913, y = pct_white_not_hispanic_2014, color = margin)) +
  geom_point() +
  scale_color_manual(values = c("Strong Trump" = "red", "Tie" = "yellow", "Strong Clinton" = "blue")) + #Färger för prickar för margin
  labs(title = "",
       x = "Bachelor's degree or higher, percent of persons age 25+, 2009-2013 (%)",
       y = "White alone, not Hispanic or Latino, percent, 2014 (%)",
       color = "Margin") + 
  scale_x_continuous(limits = c(0, 100)) + 
  scale_y_continuous(limits = c(0, 100))

#----------------------------------------------------------------------------------------------------------------------
#HUDFÄRG OCH TRUMPRÖSTER-----------------------------------------------------------------------------------------------

#Plot 4 - facet histogram av röstfördelning per andel vita grupper

ggplot(merged_data, aes(x = per_gop_2016, fill = race_homogeneity)) +
  geom_histogram(color = "black") + #Svarta kanter på staplarna
  facet_wrap(~race_homogeneity) + #Vi delar in de i de fyra grupperna
  theme_minimal() + #Minimalistiskt tema
  labs(title = "",
       x = "Percentage of GOP votes 2016 (%)",
       y = "Number of counties",
       fill = "Race Homogeneity") +
  scale_fill_manual(values = c("Most Diverse" = "steelblue", 
                               "Moderately Diverse" = "lightblue", 
                               "Majority White" = "salmon",
                               "Near all White" = "firebrick")) +
  scale_y_continuous(limits = c(0, 150)) #Vi sätter gränser för y axeln så inget klipps av


#Plot 5 - jittered boxplot - andel röster per andel vita grupper
ggplot(merged_data, aes(x = race_homogeneity, y = per_gop_2016, fill = race_homogeneity)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) + #Styr transparensen, hindrar outliers att ritas ut 2 gånger
  geom_jitter(width = 0.2, alpha = 0.5) + #Gör prickarna utspridda
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "GOP Vote Percentage (%)") +
  scale_fill_manual(values = c("Most Diverse" = "steelblue", 
                               "Moderately Diverse" = "lightblue", 
                               "Majority White" = "salmon",
                               "Near all White" = "firebrick")) +
  theme(legend.position = "none") + #Vi ser redan grupperna på x axeln, behöver de inte på sidan med
  scale_y_continuous(limits = c(0, 100))

#Plot 6 - Heatmap mellan andel vita och andel trump röster
ggplot(merged_data, aes(x = pct_white_not_hispanic_2014, y = per_gop_2016)) +
  geom_bin2d(bins = 30) + #styr upplösningen med bins + att de blir en heatmap
  geom_smooth(method = "lm", se = FALSE, color = "red") + #Drar en regressionslinje
  scale_fill_gradient(low = "lightblue", high = "darkblue") + #Färggradient för heatmap
  theme_minimal() +
  labs(title = "",
       x = "White alone, not Hispanic or Latino, percent, 2014 (%)",
       y = "Percantage of GOP votes (%)",
       fill = "Number of counties")
#---------------------------------------------------------------------------------------

#MULTI UNIT HOUSING OCH TRUMPRÖSTER--------------------------------------------------------------------------------
#Plot 7 - Jittered boxplot mellan housing groups vs trump röster
ggplot(merged_data, aes(x = housing_groups, y = per_gop_2016, fill = housing_groups)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "GOP Vote Percentage (%)",
       fill = "Housing Groups") +
  scale_fill_manual(values = c("Very Low Multi-unit Housing" = "lightblue", 
                               "Low Multi-unit Housing" = "lightskyblue", 
                               "Moderate Multi-unit Housing" = "cornflowerblue",
                               "High Multi-unit Housing" = "steelblue",
                               "Very High Multi-unit Housing" = "navy")) +
  scale_y_continuous(limits = c(0, 100))

#Plot 8 - Vi gör en bubble plot för multi unit housings vs GOP röster vs population
#För att undersöka hur vi ska sätta bubbelstorlekar undersök pop_est_2014 variabeln

min(merged_data$pop_est_2014)
quantile(merged_data$pop_est_2014, probs = c(0.25, 0.5, 0.75))
max(merged_data$pop_est_2014)

#Vi sätter gränser utifrån detta
breaks_pop <- c(10000, 25000, 75000, 500000, 1000000)

#Plot 9 - Scatterplot med multi unit housings, andel trump röster och population
ggplot(merged_data, aes(x = pct_multiunit_structures_0913, y = per_gop_2016, 
                        size = pop_est_2014)) + #Size styr bubblorna
  geom_point(alpha = 0.2, color = "blue") +
  scale_size_continuous(name = "Population Estimate 2014", breaks = breaks_pop, 
                        labels = format(breaks_pop, scientific = FALSE), 
                        range = c(1,12)) + #Använd våra skapade gränser på bubblor
  labs(title = "",
       x = "Housing units in multi-unit structures, percent, 2009-2013 (%)",
       y = "Percentage of GOP votes 2016 (%)",
       size = "Population") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100))

#---------------------------------------------------------------------------------------

#ANDEL RÅN OCH TRUMPRÖSTER------------------------------------------------------------------------------------

#PLOT 10 STACKED BARPLOT - andel trump röster per rångrupp
ggplot(merged_data, aes(x = robbery_groups, fill = margin)) + #Sortera på vinstmarginal
  geom_bar(position = "fill", color = "black") +
  scale_fill_manual(values = c("Strong Clinton" = "blue", 
                               "Tie" = "grey", 
                               "Strong Trump" = "red")) +
  scale_y_continuous(labels = scales :: percent) + #Visa y axeln i procent
  labs(title = "",
       x = "Robbery Groups",
       y = "Percentage (%)",
       fill = "Margin")

#Plot 11 - Bubble plot mellan rån, trump röster och population + housing groups
ggplot(merged_data, aes(x = ROBBERY_rate, y = per_gop_2016, 
                        size = pop_est_2014, color = housing_groups)) + #Housing groups får färger, population får storlek
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("Very Low Multi-unit Housing" = "lightgray", 
                                "Low Multi-unit housing" = "lightskyblue",
                                "Moderate Multi-unit Housing" = "skyblue4",
                                "High Multi-unit Housing" = "royalblue4",
                                "Very High Multi-unit Housing" = "midnightblue"),
                     name = "Housing Groups") +
  scale_size_continuous(name = "Population Estimate 2014", breaks = breaks_pop, 
                        labels = format(breaks_pop, scientific = FALSE), 
                        range = c(1,12)) + #Använd våra gränser för population
  labs(title = "",
       x = "Robberies per 100 000 habitants",
       y = "Percentage of GOP votes 2016 (%)",
       size = "Population") +
  scale_y_continuous(limits = c(0, 100))

#_--------------------------------------------------------------------------------------








