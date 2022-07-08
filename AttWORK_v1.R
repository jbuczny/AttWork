#title: "Attachment Styles and Workaholism"
#author: (anonymized for peer review)
#output:
#html_document
#theme: cosmo
#highlight: pygments

##This code was created in RStudio version RStudio 2022.02.3 build 492
##"Prairie Trillium" Release
##R version 4.2.0

##If you want run any of the additional code chunks, uncomment it

##install.packages("foreign")
##install.packages("data.table")
##install.packages("car")
##install.packages("dplyr")
##install.packages("tidyverse")
##install.packages("psych")
##install.packages("apaTables")
##install.packages("lavaan")
##install.packages("semTools")

library(foreign)
library(data.table)
library(car)
library(dplyr)
library(tidyverse)
library(psych)
library(apaTables)
library(lavaan)
library(semTools)

##This analysis utilizes PROCESS by Hayes (2022), namely the process.R script
##For more details see Hayes, A. F. (2022). Introduction to mediation, moderation,
##and conditional process analysis: A regression-based approach (3rd ed.).
##New York: Guilford Press.
##Download the script:
##http://processmacro.org/download.html  

##Locate process.R in the folder, open it, mark all text, and run it.
##On slower machines, it will take a few minutes to load PROCESS.
##If PROCESS is loaded successfully, you should see information in Console.

##Opening the data file

getwd()

##Set working directory, adjust it to your folder directory, place and load 
##file from your local folder

##setwd("...")

AttWork_data <- read.spss("AttWORK_v1.sav"
                          , to.data.frame = TRUE)

list.files() ##Ensure that the correct file is in the folder
##is on the list of file. If not, go to Session > Set Working Directory
## > To Source File Location

##file.choose() ##Use quickly open the file

##View data if necessary
##View(attwork_data)

head(AttWork_data)

##Sample size

nrow(AttWork_data)

##Demographics

summary(AttWork_data$Age)
describe(AttWork_data$Age)

summary(AttWork_data$Gender)
setDT(AttWork_data)[, 100 * .N / nrow(AttWork_data)
                    , by = Gender]

summary(AttWork_data$Education)
setDT(AttWork_data)[, 100 * .N / nrow(AttWork_data)
                    , by = Education]

summary(AttWork_data$Years_experience)
describe(AttWork_data$Years_experience)

summary(AttWork_data$Work_hours)
describe(AttWork_data$Work_hours)

summary(AttWork_data$Type_employment)
setDT(AttWork_data)[, 100 * .N / nrow(AttWork_data)
                    , by = Type_employment]

##Data quality, the percentage that did not pass attention checks, and duration

table(AttWork_data$attention_1_AT3)
setDT(AttWork_data)[, 100 * .N / nrow(AttWork_data)
                    , by = attention_1_AT3] ##Requested to select 2 or 4

table(AttWork_data$attention_2_SES7)
setDT(AttWork_data)[, 100 * .N / nrow(AttWork_data)
                    , by = attention_2_SES7] ##Requested to select 4

summary(AttWork_data$Duration_in_seconds)
describe(AttWork_data$Duration_in_seconds)

##Recoding for reliability and total scores computations

##AAS main recoding

AttWork_data$AT1R <- car::recode(AttWork_data$AT1
                                 , "1=5;2=4;4=2;5=1")
AttWork_data$AT5R <- car::recode(AttWork_data$AT5
                                 , "1=5;2=4;4=2;5=1")
AttWork_data$AT6R <- car::recode(AttWork_data$AT6
                                 , "1=5;2=4;4=2;5=1")
AttWork_data$AT12R <- car::recode(AttWork_data$AT12
                                  , "1=5;2=4;4=2;5=1")
AttWork_data$AT3R <- car::recode(AttWork_data$AT13
                                 , "1=5;2=4;4=2;5=1")
AttWork_data$AT14R <- car::recode(AttWork_data$AT14
                                  , "1=5;2=4;4=2;5=1")
##AAS additional recoding

AttWork_data$AT8R <- car::recode(AttWork_data$AT8
                                 , "1=5;2=4;4=2;5=1")
AttWork_data$AT13R <- car::recode(AttWork_data$AT13
                                  , "1=5;2=4;4=2;5=1")
AttWork_data$AT17R <- car::recode(AttWork_data$AT17
                                  , "1=5;2=4;4=2;5=1")
AttWork_data$AT2R <- car::recode(AttWork_data$AT2
                                 , "1=5;2=4;4=2;5=1")
AttWork_data$AT7R <- car::recode(AttWork_data$AT7
                                 , "1=5;2=4;4=2;5=1")
AttWork_data$AT16R <- car::recode(AttWork_data$AT16
                                  , "1=5;2=4;4=2;5=1")
AttWork_data$AT18R <- car::recode(AttWork_data$AT18
                                  , "1=5;2=4;4=2;5=1")

##SES recoding

AttWork_data$SES.1 <- car::recode(AttWork_data$SES.1
                                  , "1=4;2=3;3=2;4=1")
AttWork_data$SES.2 <- car::recode(AttWork_data$SES.2
                                  , "1=4;2=3;3=2;4=1")
AttWork_data$SES.4 <- car::recode(AttWork_data$SES.4
                                  , "1=4;2=3;3=2;4=1")
AttWork_data$SES.6 <- car::recode(AttWork_data$SES.6
                                  , "1=4;2=3;3=2;4=1")
AttWork_data$SES.7 <- car::recode(AttWork_data$SES.7
                                  , "1=4;2=3;3=2;4=1")

##Calculating the main total scores

AttWork_data$Anxious <- ((AttWork_data$AT3 + AttWork_data$AT4
                          + AttWork_data$AT9 + AttWork_data$AT10
                          + AttWork_data$AT11 + AttWork_data$AT15) / 6)

AttWork_data$Avoidant <- ((AttWork_data$AT1R + AttWork_data$AT2
                           + AttWork_data$AT5R + AttWork_data$AT6R
                           + AttWork_data$AT7 + AttWork_data$AT8
                           + AttWork_data$AT12R + AttWork_data$AT13R
                           + AttWork_data$AT14R + AttWork_data$AT16
                           + AttWork_data$AT17 + AttWork_data$AT18) / 12)

AttWork_data$WCS.neu_per <- ((AttWork_data$WCS.2 + AttWork_data$WCS.4
                              + AttWork_data$WCS.8 + AttWork_data$WCS.11
                              + AttWork_data$WCS.18 + AttWork_data$WCS.22
                              + AttWork_data$WCS.27) / 7)

AttWork_data$WCS.obs_des <- ((AttWork_data$WCS.1 + AttWork_data$WCS.3
                              + AttWork_data$WCS.6 + AttWork_data$WCS.13
                              + AttWork_data$WCS.15 + AttWork_data$WCS.21
                              + AttWork_data$WCS.28) / 7)

AttWork_data$WCS.ant_sel <- ((AttWork_data$WCS.7 + AttWork_data$WCS.10
                              + AttWork_data$WCS.14 + AttWork_data$WCS.16
                              + AttWork_data$WCS.23 + AttWork_data$WCS.25
                              + AttWork_data$WCS.26) / 7)

AttWork_data$WCS.ant_red <- ((AttWork_data$WCS.5 + AttWork_data$WCS.9
                              + AttWork_data$WCS.12 + AttWork_data$WCS.17
                              + AttWork_data$WCS.19 + AttWork_data$WCS.20
                              + AttWork_data$WCS.24) / 7)

AttWork_data$SES.assess <- ((AttWork_data$SES.3 + AttWork_data$SES.4
                             + AttWork_data$SES.5 + AttWork_data$SES.7
                             + AttWork_data$SES.9) / 5)

AttWork_data$SES.accept <- ((AttWork_data$SES.1 + AttWork_data$SES.2
                             + AttWork_data$SES.6 + AttWork_data$SES.8
                             + AttWork_data$SES.10) / 5)

##Calculating additional total scores

Close <- ((AttWork_data$AT1 + AttWork_data$AT6 + AttWork_data$AT8R
           + AttWork_data$AT12 + AttWork_data$AT13R + AttWork_data$AT17R) / 6)

Depend <- ((AttWork_data$AT2R + AttWork_data$AT5 + AttWork_data$AT7R
            + AttWork_data$AT14 + AttWork_data$AT16R + AttWork_data$AT18R) / 6)

Close_dep <-((Close + Depend) / 2)

AttWork_data$WCS.workaholism <- ((AttWork_data$WCS.2 + AttWork_data$WCS.4
                                  + AttWork_data$WCS.8 + AttWork_data$WCS.11
                                  + AttWork_data$WCS.18 + AttWork_data$WCS.22
                                  + AttWork_data$WCS.27 + AttWork_data$WCS.1
                                  + AttWork_data$WCS.3 + AttWork_data$WCS.6
                                  + AttWork_data$WCS.13 + AttWork_data$WCS.15
                                  + AttWork_data$WCS.21 + AttWork_data$WCS.28
                                  + AttWork_data$WCS.7 + AttWork_data$WCS.10
                                  + AttWork_data$WCS.14 + AttWork_data$WCS.16
                                  + AttWork_data$WCS.23 + AttWork_data$WCS.25
                                  + AttWork_data$WCS.26 + AttWork_data$WCS.5
                                  + AttWork_data$WCS.9 + AttWork_data$WCS.12
                                  + AttWork_data$WCS.17 + AttWork_data$WCS.19
                                  + AttWork_data$WCS.20 + AttWork_data$WCS.24)
                                  / 28)

AttWork_data$SES.general <- ((AttWork_data$SES.3 + AttWork_data$SES.4
                              + AttWork_data$SES.5 + AttWork_data$SES.7
                              + AttWork_data$SES.9 + AttWork_data$SES.1
                              + AttWork_data$SES.2 + AttWork_data$SES.6
                              + AttWork_data$SES.8 + AttWork_data$SES.10) / 10)

##Calculating four styles
##1 = Secure
##2 = Preoccupied
##3 = Dismissful
##4 = Fearful

AttWork_data$Style <- ifelse(AttWork_data$Close_dep > 3
                             & AttWork_data$Anxious <= 3
                             , 1
                             , ifelse(AttWork_data$Close_dep > 3
                                      & AttWork_data$Anxious > 3
                                      , 2
                                      , ifelse(AttWork_data$Close_dep <= 3
                                               & AttWork_data$Anxious <= 3
                                               , 3
                                               , ifelse(AttWork_data$Close_dep <= 3
                                                        & AttWork_data$Anxious > 3
                                                        , 4
                                                        , 999))))

##Calculating binary attachment styles
##0 = Secure
##1 = Insecure

AttWork_data$Insecure_style <- ifelse(AttWork_data$Close_dep > 3
                                      & AttWork_data$Anxious <= 3
                                      , 0
                                      , 1)

##Saving the data set for future possible use

write.csv(AttWork_data
          , file = "AttWork_data_v2.csv"
          , row.names = TRUE
          , col.names = FALSE)

write.foreign(AttWork_data
              , datafile = "AttWork_data_v2.sav"
              , codefile = "AttWork_data_v2.sps"
              , package="SPSS") #The script might not work in SPSS correctly

##Creating subsets for reliability analysis

AAS_ANX_relia <- AttWork_data %>% select("AT3"
                                         , "AT4"
                                         , "AT9"
                                         , "AT10"
                                         , "AT11"
                                         , "AT15")

AAS_AVO_relia <- AttWork_data %>% select("AT1R"
                                         , "AT2"
                                         , "AT5R"
                                         , "AT6R"
                                         , "AT7"
                                         , "AT8"
                                         , "AT12R"
                                         , "AT13R"
                                         , "AT14R"
                                         , "AT16"
                                         , "AT17"
                                         , "AT18")

WCS_NP_relia <- AttWork_data %>% select("WCS.2"
                                        , "WCS.4"
                                        , "WCS.8"
                                        , "WCS.11"
                                        , "WCS.18"
                                        , "WCS.22"
                                        , "WCS.27")

WCS_OC_relia <- AttWork_data %>% select("WCS.1"
                                        , "WCS.3"
                                        , "WCS.6"
                                        , "WCS.13"
                                        , "WCS.15"
                                        , "WCS.21"
                                        , "WCS.28")

WCS_SW_relia <- AttWork_data %>% select("WCS.7"
                                        , "WCS.10"
                                        , "WCS.14"
                                        , "WCS.16"
                                        , "WCS.23"
                                        , "WCS.25"
                                        , "WCS.26")

WCS_R_relia <- AttWork_data %>% select("WCS.5"
                                       , "WCS.9"
                                       , "WCS.12"
                                       , "WCS.17"
                                       , "WCS.19"
                                       , "WCS.20"
                                       , "WCS.24")

SES_assess_relia <- AttWork_data %>% select("SES.3"
                                            , "SES.4"
                                            , "SES.5"
                                            , "SES.7"
                                            , "SES.9")

SES_accept_relia <- AttWork_data %>% select("SES.1"
                                            , "SES.2"
                                            , "SES.6"
                                            , "SES.8"
                                            , "SES.10")

AAS_close_relia <- AttWork_data %>% select("AT1"
                                           , "AT6"
                                           , "AT8R"
                                           , "AT12"
                                           , "AT13R"
                                           , "AT17R")

AAS_depend_relia <- AttWork_data %>% select("AT2R"
                                            , "AT5"
                                            , "AT7R"
                                            , "AT14"
                                            , "AT16R"
                                            , "AT18R")

WCS_work_relia <- AttWork_data %>% select("WCS.5"
                                          , "WCS.9"
                                          , "WCS.12"
                                          , "WCS.17"
                                          , "WCS.19"
                                          , "WCS.20"
                                          , "WCS.24"
                                          , "WCS.1"
                                          , "WCS.3"
                                          , "WCS.6"
                                          , "WCS.13"
                                          , "WCS.15"
                                          , "WCS.21"
                                          , "WCS.28"
                                          , "WCS.7"
                                          , "WCS.10"
                                          , "WCS.14"
                                          , "WCS.16"
                                          , "WCS.23"
                                          , "WCS.25"
                                          , "WCS.26"
                                          , "WCS.5"
                                          , "WCS.9"
                                          , "WCS.12"
                                          , "WCS.17"
                                          , "WCS.19"
                                          , "WCS.20"
                                          , "WCS.24")

SES_general_relia <- AttWork_data %>% select("SES.3"
                                             , "SES.4"
                                             , "SES.5"
                                             , "SES.7"
                                             , "SES.9"
                                             , "SES.1"
                                             , "SES.2"
                                             , "SES.6"
                                             , "SES.8"
                                             , "SES.10")

##Saving the subsets as data frames

tibble(AAS_ANX_relia)
tibble(AAS_AVO_relia)
tibble(WCS_NP_relia)
tibble(WCS_OC_relia)
tibble(WCS_SW_relia)
tibble(WCS_R_relia)
tibble(SES_assess_relia)
tibble(SES_accept_relia)
tibble(AAS_close_relia)
tibble(AAS_depend_relia)
tibble(WCS_work_relia)
tibble(SES_general_relia)

##Calculating reliabilities

psych::reliability(AAS_ANX_relia)
psych::reliability(AAS_AVO_relia)
psych::reliability(WCS_NP_relia)
psych::reliability(WCS_OC_relia)
psych::reliability(WCS_SW_relia)
psych::reliability(WCS_R_relia)
psych::reliability(SES_assess_relia)
psych::reliability(SES_accept_relia)
psych::reliability(AAS_close_relia)
psych::reliability(AAS_depend_relia)
psych::reliability(WCS_work_relia)
psych::reliability(SES_general_relia)

##Calculating zero-order correlations

AttWork_cor <- AttWork_data %>% select("Anxious"
                                       , "Avoidant"
                                       , "WCS.neu_per"
                                       , "WCS.obs_des"
                                       , "WCS.ant_sel"
                                       , "WCS.ant_red"
                                       , "SES.assess"
                                       , "SES.accept"
                                       , "Age"
                                       , "Work_hours")

apa.cor.table(AttWork_cor, filename="AttWork_corTable1.doc")

##Opening the processed data set to work with PROCESS

AttWork_dataP <- read.csv(file = "AttWork_data_v2.csv"
                          , header = TRUE
                          , sep = ","
                          , dec = ".")

##Common Method Bias

AAS_ANX_relia <- AttWork_data %>% select("AT3"
                                         , "AT4"
                                         , "AT9"
                                         , "AT10"
                                         , "AT11"
                                         , "AT15")

AAS_AVO_relia <- AttWork_data %>% select("AT1R"
                                         , "AT2"
                                         , "AT5R"
                                         , "AT6R"
                                         , "AT7"
                                         , "AT8"
                                         , "AT12R"
                                         , "AT13R"
                                         , "AT14R"
                                         , "AT16"
                                         , "AT17"
                                         , "AT18")

WCS_NP_relia <- AttWork_data %>% select("WCS.2"
                                        , "WCS.4"
                                        , "WCS.8"
                                        , "WCS.11"
                                        , "WCS.18"
                                        , "WCS.22"
                                        , "WCS.27")

WCS_OC_relia <- AttWork_data %>% select("WCS.1"
                                        , "WCS.3"
                                        , "WCS.6"
                                        , "WCS.13"
                                        , "WCS.15"
                                        , "WCS.21"
                                        , "WCS.28")

modelCMB <- 'CMB =~ AT3 + AT4 + AT9 + AT10 + AT11 + AT15
                    + AT1R + AT2 + AT5R + AT6R + AT7 + AT8
                    + AT12R + AT13R + AT14R + AT16 + AT17 + AT18
                    + WCS.2 + WCS.4 + WCS.8 + WCS.11 + WCS.18 + WCS.22 + WCS.27
                    + WCS.1 + WCS.3 + WCS.6 + WCS.13 + WCS.15 + WCS.21'

##Creating bootstrap data

fitmodelCMB = sem(model = modelCMB
                  , data = AttWork_dataP
                  , se = 'bootstrap'
                  , bootstrap = 5000
                  , missing = 'pairwise') ##Uncomment if needed

##Saving bootstrapped data, the fit label stored as fitmodelCMB

save(fitmodelCMB
     , file = "fitmodelCMB.RData") ##Uncomment if needed

##Loading bootstrapped data distributed along the script

load("fitmodelCMB.RData")

summary(fitmodelCMB
        , fit.measures = TRUE
        , estimates = TRUE
        , ci = TRUE
        , standardized = TRUE
        , rsquare = TRUE)

##Printing variable labels to use them in PROCESS

head(AttWork_dataP)

##General information regarding the tested models in PROCESS:
##X1 = Anxious, anxious attachment style
##X2 = Avoidant, avoidant attachment style
##Y = obsessive-compulsive desire (WCS-OC), WCS.obs_des 
##M = WCS.neu_per (WCS-NP), WCS.neu_per
##COV = SES.assess (self-assessment), SES.accept (self-acceptance)
##Alternative X = binary attachment style (0 = secure, 1 = insecure)

##Settings:
##PROCESS model: 4 (simple mediation)
##Standarization: yes
##Bootstrap: 5000 samples
##Robust confidence intervals: yes, normality is not assumed
##Random seed: yes, = 654321, set for replication

##The main analysis
##On slower machines, it will take a few minutes to finish it

##Note: Linearity assumption is not tested

##Hypothesized model

process(data = AttWork_dataP
        , model = 4
        , y = "WCS.obs_des"
        , x = "Anxious"
        , m = "WCS.neu_per"
        , cov = "Avoidant"
        , total = 1
        , stand = 1
        , boot = 5000
        , modelbt = 1
        , seed = 654321)

##Excluding avoidant as no total effect with WCS-OC to test single mediation

process(data = AttWork_dataP
        , model = 4
        , y = "WCS.obs_des"
        , x = "Anxious"
        , m = "WCS.neu_per"
        , total = 1
        , stand = 1
        , boot = 5000
        , modelbt = 1
        , seed = 654321)

##Calculating structural model to separate and compare the mediation effects

modelHyp <- 'WCS.obs_des ~ b1 * WCS.neu_per + c1 * Anxious + c2 * Avoidant
             WCS.neu_per ~ a1 * Anxious + a2 * Avoidant
             ind1: = a1 * b1
             ind2: = a2 * b1
             contrast: = ind1 - ind2
             total: = a1 * b1 + a2 * b1 + c1'

##Creating bootstrap data

##fitmodelHyp = sem(model = modelHyp
                  ##, data = AttWork_dataP
                  ##, se = 'bootstrap'
                  ##, bootstrap = 5000
                  ##, missing = 'pairwise') ##Uncomment if needed

##Saving bootstrapped data, the fit label stored as fitmodelHyp

save(fitmodelHyp
     , file = "fitmodelHyp.RData") ##Uncomment if needed

##Loading bootstrapped data distributed along the script

load("fitmodelHyp.RData")

summary(fitmodelHyp
        , fit.measures = FALSE
        , estimates = TRUE
        , ci = TRUE
        , standardized = TRUE
        , rsquare = TRUE)

##Testing self-esteem as a covariate

process(data = AttWork_dataP
        , model = 4
        , y = "WCS.obs_des"
        , x = "Anxious"
        , m = "WCS.neu_per"
        , cov = c("Avoidant", "SES.assess", "SES.accept")
        , total = 1
        , stand = 1
        , boot = 5000
        , modelbt = 1
        , seed = 654321)

##An alternative model WCS-OC as M and WCS-NP as Y

process(data = AttWork_dataP
        , model = 4
        , y = "WCS.neu_per"
        , x = "Anxious"
        , m = "WCS.obs_des"
        , cov = "Avoidant"
        , total = 1
        , stand = 1
        , boot = 5000
        , modelbt = 1
        , seed = 654321)

process(data = AttWork_dataP
        , model = 4
        , y = "WCS.neu_per"
        , x = "Anxious"
        , m = "WCS.obs_des"
        , cov = c("Avoidant", "SES.assess", "SES.accept")
        , total = 1
        , stand = 1
        , boot = 5000
        , modelbt = 1
        , seed = 654321)

##An alternative model WCS-OC as M and WCS-NP as Y excluding avoidant

process(data = AttWork_dataP
        , model = 4
        , y = "WCS.neu_per"
        , x = "Anxious"
        , m = "WCS.obs_des"
        , total = 1
        , stand = 1
        , boot = 5000
        , modelbt = 1
        , seed = 654321)

process(data = AttWork_dataP
        , model = 4
        , y = "WCS.neu_per"
        , x = "Anxious"
        , m = "WCS.obs_des"
        , cov = c("SES.assess", "SES.accept")
        , total = 1
        , stand = 1
        , boot = 5000
        , modelbt = 1
        , seed = 654321)

##An alternative model with binary attachment style

AttWork_data$Insecure_style <- as.numeric(AttWork_data$Insecure_style)
## 0 = secure style, 1 = insecure style

process(data = AttWork_dataP
        , model = 4
        , y = "WCS.obs_des"
        , x = "Insecure_style"
        , m = "WCS.neu_per"
        , total = 1
        , stand = 1
        , boot = 5000
        , modelbt = 1
        , seed = 654321)

process(data = AttWork_dataP
        , model = 4
        , y = "WCS.obs_des"
        , x = "Insecure_style"
        , m = "WCS.neu_per"
        , cov = c("SES.assess", "SES.accept")
        , total = 1
        , stand = 1
        , boot = 5000
        , modelbt = 1
        , seed = 654321)

process(data = AttWork_dataP
        , model = 4
        , y = "WCS.neu_per"
        , x = "Insecure_style"
        , m = "WCS.obs_des"
        , total = 1
        , stand = 1
        , boot = 5000
        , modelbt = 1
        , seed = 654321)

process(data = AttWork_dataP
        , model = 4
        , y = "WCS.neu_per"
        , x = "Insecure_style"
        , m = "WCS.obs_des"
        , cov = c("SES.assess", "SES.accept")
        , total = 1
        , stand = 1
        , boot = 5000
        , modelbt = 1
        , seed = 654321)

##Multilevel analysis
##The following packages must be installed

##Install the following packages devtools, jmvcore, jmvtools, gamlj
##install.packages("jmvcore")
##install.packages('jmvtools', repos='https://repo.jamovi.org')
###install.packages("devtools")
##devtools::install_github("gamlj/gamlj")

##Loading necessary packages

library(jmvtools)
library(gamlj)

##Country as a cluster variable

##Null models

gamljMixed(formula = WCS.obs_des  ~ 1  + (1 | Country)
           , data = AttWork_dataP)
gamljMixed(formula = WCS.neu_per  ~ 1  + (1 | Country)
           , data = AttWork_dataP)

##Attachment styles added as fixed effects

gamljMixed(formula = WCS.obs_des ~ 1 + Anxious + Avoidant + (1 | Country)
           , data = AttWork_dataP
           , scaling = list(list(var = "Anxious"
                                 , var = "Avoidant"
                                 , type = "standardized")))

gamljMixed(formula = WCS.neu_per ~ 1 + Anxious + Avoidant + (1 | Country)
           , data = AttWork_dataP
           , scaling = list(list(var = "Anxious"
                                 , var = "Avoidant"
                                 , type = "standardized")))

##Attachment styles vary within countries

gamljMixed(formula = WCS.obs_des ~ 1 + Anxious + Avoidant
           + (1 + Anxious + Avoidant | Country)
           , data = AttWork_dataP
           , scaling = list(list(var = "Anxious"
                                 , var = "Avoidant"
                                 , type = "standardized")))

gamljMixed(formula = WCS.neu_per ~ 1 + Anxious + Avoidant
           + (1 + Anxious + Avoidant | Country)
           , data = AttWork_dataP
           , scaling = list(list(var = "Anxious"
                                 , var = "Avoidant"
                                 , type = "standardized")))

##Employment type as a cluster

##Null models

gamljMixed(formula = WCS.obs_des  ~ 1  + (1 | Type_employment)
           , data = AttWork_dataP)
gamljMixed(formula = WCS.neu_per  ~ 1  + (1 | Type_employment)
           , data = AttWork_dataP)

##Attachment styles added as fixed effects

gamljMixed(formula = WCS.obs_des ~ 1 + Anxious + Avoidant + (1 | Type_employment)
           , data = AttWork_dataP
           , scaling = list(list(var = "Anxious"
                                 , var = "Avoidant"
                                 , type = "standardized")))

gamljMixed(formula = WCS.neu_per ~ 1 + Anxious + Avoidant + (1 | Type_employment)
           , data = AttWork_dataP
           , scaling = list(list(var = "Anxious"
                                 , var = "Avoidant"
                                 , type = "standardized")))

##Attachment styles vary within countries

gamljMixed(formula = WCS.obs_des ~ 1 + Anxious + Avoidant
           + (1 + Anxious + Avoidant | Type_employment)
           , data = AttWork_dataP
           , scaling = list(list(var = "Anxious"
                                 , var = "Avoidant"
                                 , type = "standardized")))

gamljMixed(formula = WCS.neu_per ~ 1 + Anxious + Avoidant
           + (1 + Anxious + Avoidant | Type_employment)
           , data = AttWork_dataP
           , scaling = list(list(var = "Anxious"
                                 , var = "Avoidant"
                                 , type = "standardized")))