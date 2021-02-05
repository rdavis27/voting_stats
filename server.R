library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)

library(urbnmapr)
library(ggrepel)
library(maps)

input_dir <- "input/"
data_dir  <- "data/"

shinyServer(
    function(session,input, output) {
        options(width = 140, readr.show_progress = FALSE)
        options(max.print=999999)

        createCA <- function(){
            xx <- read_excel(paste0(input_dir,"CA20_csv-candidates.xlsx"), sheet = "csv-candidates")
            pp <- xx[xx$'Contest Name' == "President",]
            pp$'Party Name'[is.na(pp$'Party Name')] <- pp$'Candidate Name'[is.na(pp$'Party Name')]
            pp <- pp[,c("County Name","Party Name","Vote Total")]
            names(pp)[1] <- "COUNTY"
            pp <- pp %>% spread('Party Name','Vote Total')
            namespp <- names(pp)
            namespp[namespp == 'Democratic'] <- "DEM"
            namespp[namespp == 'Republican'] <- "REP"
            namespp <- paste0('"',namespp,'"')
            write(paste(namespp, collapse = " "), paste0(data_dir,"CA_President_2020.csv"))
            names(pp)[names(pp) == 'Democratic'] <- "BIDEN"
            names(pp)[names(pp) == 'Republican'] <- "TRUMP"
            write_delim(pp, paste0(data_dir,"CA_President_2020.csv"), append = TRUE, col_names = TRUE)

            names(xx) <- c("ELECTION_DATE","ELECTION_NAME","COUNTY_ID","County Name","CONTEST_ID","Contest Name","CANDIDATE_ID","Candidate Name","INCUMBENT_FLAG","WRITE_IN_FLAG","PARTY_ID","Party Name","Vote Total")
            pp <- xx[grepl("^United States Representative District ",xx$`Contest Name`),]
            pp$'Party Name'[is.na(pp$'Party Name')] <- pp$`Candidate Name`[is.na(pp$'Party Name')]
            pp <- pp[,c("County Name","Contest Name","Party Name","Vote Total")]
            names(pp)[1] <- "COUNTY"
            names(pp)[2] <- "DISTRICT"
            pp$DISTRICT <- substring(pp$DISTRICT, 39)
            pp <- pp %>%
                group_by(COUNTY, DISTRICT, `Party Name`) %>%
                summarise(`Vote Total` = sum(`Vote Total`))
            pp <- pp %>% spread(`Party Name`,`Vote Total`)
            namespp <- names(pp)
            namespp[namespp == 'Democratic'] <- "DEM"
            namespp[namespp == 'Republican'] <- "REP"
            namespp <- paste0('"',namespp,'"')
            write(paste(namespp, collapse = " "), paste0(data_dir,"CA_House_2020.csv"))
            write_delim(pp, paste0(data_dir,"CA_House_2020.csv"), append = TRUE, col_names = TRUE)

            xx <- read_excel(paste0(input_dir,"CA18_csv-candidates.xlsx"), sheet = "Sheet1")
            names(xx) <- c("ELECTION_DATE","ELECTION_NAME","COUNTY_ID","County Name","CONTEST_ID","Contest Name","CANDIDATE_ID","Candidate Name","INCUMBENT_FLAG","WRITE_IN_FLAG","PARTY_ID","Party Name","Vote Total")
            pp <- xx[grepl("^United States Representative District ",xx$`Contest Name`),]
            pp$'Party Name'[is.na(pp$'Party Name')] <- pp$`Candidate Name`[is.na(pp$'Party Name')]
            pp <- pp[,c("County Name","Contest Name","Party Name","Vote Total")]
            names(pp)[1] <- "COUNTY"
            names(pp)[2] <- "DISTRICT"
            pp$DISTRICT <- substring(pp$DISTRICT, 39)
            pp <- pp %>%
                group_by(COUNTY, DISTRICT, `Party Name`) %>%
                summarise(`Vote Total` = sum(`Vote Total`))
            pp <- pp %>% spread(`Party Name`,`Vote Total`)
            namespp <- names(pp)
            namespp[namespp == 'Democratic'] <- "DEM"
            namespp[namespp == 'Republican'] <- "REP"
            namespp <- paste0('"',namespp,'"')
            write(paste(namespp, collapse = " "), paste0(data_dir,"CA_House_2018.csv"))
            write_delim(pp, paste0(data_dir,"CA_House_2018.csv"), append = TRUE, col_names = TRUE)

            filenamex <- paste0(input_dir,"CA_Registered00.csv")
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            xx0 <- read_delim(filenamex, ' ', skip = 1)
            xx <- xx0[,c(1,4:12)]
            xxparty <- xxparty[c(1,4:12)]
            if (input$noparty == "Split by Ratio"){
                TotalReg <- xx$Democratic + xx$Republican
                xx$Democratic <- xx$Democratic + xx$NoParty * (xx$Democratic / TotalReg)
                xx$Republican <- xx$Republican + xx$NoParty * (xx$Republican / TotalReg)
                xx$NoParty <- 0
            }
            else if (input$noparty == "Count as Dem"){
                xx$Democratic <- xx$Democratic + xx$NoParty
                xx$NoParty <- 0
            }
            else if (input$noparty == "Count as Rep"){
                xx$Republican <- xx$Republican + xx$NoParty
                xx$NoParty <- 0
            }
            else{
                xx$Democratic <- xx$Democratic + (xx$NoParty / 2)
                xx$Republican <- xx$Republican + (xx$NoParty / 2)
                xx$NoParty <- 0
            }
            xxparty <- paste0('"',xxparty,'"')
            write(paste(xxparty, collapse = " "), paste0(data_dir,"CA_Registered_2020.csv"))
            write_delim(xx, paste0(data_dir,"CA_Registered_2020.csv"), append = TRUE, col_names = TRUE)
        }        
        createFL <- function(){
            xx <- read_delim(paste0(input_dir,"FL_All20_tab.csv"), '\t')
            pp <- xx[xx$RaceCode == "PRE",]
            pp$PartyCode[pp$PartyCode == "WRI"] <- pp$CanNameLast[pp$PartyCode == "WRI"]
            pp <- pp[,c("CountyName","PartyCode","CanVotes")]
            names(pp)[1] <- "COUNTY"
            pp <- pp %>% spread(PartyCode,CanVotes)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"FL_President_2020.csv"))
            write_delim(pp, paste0(data_dir,"FL_President_2020.csv"), append = TRUE, col_names = TRUE)

            pp <- xx[xx$RaceCode == "USR",]
            pp$PartyCode[pp$PartyCode == "WRI"] <- pp$CanNameLast[pp$PartyCode == "WRI"]
            pp <- pp[,c("CountyName","Juris1num","PartyCode","CanVotes")] # add Juris1num
            names(pp)[1] <- "COUNTY"
            names(pp)[2] <- "DISTRICT"
            pp <- pp %>% spread(PartyCode,CanVotes)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"FL_House_2020.csv"))
            write_delim(pp, paste0(data_dir,"FL_House_2020.csv"), append = TRUE, col_names = TRUE)

            xx <- read_delim(paste0(input_dir,"FL18_11062018Election.csv"), '\t')
            xx2 <<- xx
            pp <- xx[xx$RaceCode == "USR",]
            pp$PartyCode[pp$PartyCode == "WRI"] <- pp$CanNameLast[pp$PartyCode == "WRI"]
            pp$PartyCode[pp$PartyCode == "NPA"] <- pp$CanNameLast[pp$PartyCode == "NPA"] #additional duplicates
            pp <- pp[,c("CountyName","Juris1num","PartyCode","CanVotes")] # add Juris1num
            names(pp)[1] <- "COUNTY"
            names(pp)[2] <- "DISTRICT"
            pp <- pp %>% spread(PartyCode,CanVotes)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"FL_House_2018.csv"))
            write_delim(pp, paste0(data_dir,"FL_House_2018.csv"), append = TRUE, col_names = TRUE)
        }
        createGA <- function(){
            xx0 <- read_excel(paste0(input_dir,"GA20_detail.xlsx"), sheet = "1", skip = 2)
            xx <- xx0[,c(1,6,11,16)]
            names(xx) <- c("COUNTY","Trump","Biden","Jorgensen")
            xxparty <- c("COUNTY","REP","DEM","LIB")
            write(paste(xxparty, collapse = " "), paste0(data_dir,"GA_President_2020.csv"))
            write_delim(xx, paste0(data_dir,"GA_President_2020.csv"), append = TRUE, col_names = TRUE)

            xx0 <- read_excel(paste0(input_dir,"GA20_detail.xlsx"), sheet = "2", skip = 2)
            xx <- xx0[,c(1,6,11,16)]
            names(xx) <- c("COUNTY","Perdue","Ossoff","Hazel")
            xxparty <- c("COUNTY","REP","DEM","LIB")
            write(paste(xxparty, collapse = " "), paste0(data_dir,"GA_Senate_2020.csv"))
            write_delim(xx, paste0(data_dir,"GA_Senate_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createIA <- function(){
            filenamex <- paste0(input_dir,"IA_President00.csv")
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            xx0 <- read_delim(filenamex, ' ', skip = 1)
            xx <- xx0[,1:11]
            xxparty <- xxparty[1:11]
            xxed <- xx[xx$TYPE == "ElectionDay",-2]
            xxab <- xx[xx$TYPE == "Absentee",-2]
            xxto <- xx[xx$TYPE == "Total",-2]
            xxparty <- xxparty[-2]
            xxto$COUNTY <- xxed$COUNTY
            write(paste(xxparty, collapse = " "), paste0(data_dir,"IA_President_2020.csv"))
            write_delim(xxto, paste0(data_dir,"IA_President_2020.csv"), append = TRUE, col_names = TRUE)

            filenamex <- paste0(input_dir,"IA_Senate00.csv")
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            xx0 <- read_delim(filenamex, ' ', skip = 1)
            xx <- xx0[,1:7]
            xxparty <- xxparty[1:7]
            xxed <- xx[xx$TYPE == "ElectionDay",-2]
            xxab <- xx[xx$TYPE == "Absentee",-2]
            xxto <- xx[xx$TYPE == "Total",-2]
            xxparty <- xxparty[-2]
            xxto$COUNTY <- xxed$COUNTY
            write(paste(xxparty, collapse = " "), paste0(data_dir,"IA_Senate_2020.csv"))
            write_delim(xxto, paste0(data_dir,"IA_Senate_2020.csv"), append = TRUE, col_names = TRUE)

            filenamey <- paste0(data_dir,"IA_House_2020.csv")
            for (i in 1:4){
                filenamex <- paste0(input_dir,"IA_House",i,"_00.csv")
                xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
                xx0 <- read_delim(filenamex, ' ', skip = 1)
                xx <- xx0[,1:5]
                xxparty <- xxparty[1:5]
                xxed <- xx[xx$TYPE == "ElectionDay",-2]
                xxab <- xx[xx$TYPE == "Absentee",-2]
                xxto <- xx[xx$TYPE == "Total",-2]
                xxparty <- xxparty[-2]
                xxto$COUNTY <- xxed$COUNTY
                xxto$DISTRICT <- i
                xxout <- xxto[,c(1, NCOL(xxto), 2:(NCOL(xxto)-1))]
                xxparty$X6 <- "DISTRICT"
                xxparty <- xxparty[c(1,NCOL(xxparty),2:(NCOL(xxparty)-1))]
                if (i == 1){
                    write(paste(xxparty, collapse = " "), filenamey)
                    write_delim(xxout, filenamey, append = TRUE, col_names = TRUE)
                }
                else{
                    write_delim(xxout, filenamey, append = TRUE, col_names = FALSE)
                }
            }
            
            filenamex <- paste0(input_dir,"IA_Registered00.csv")
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            xx0 <- read_delim(filenamex, ' ', skip = 1)
            xx <- xx0[,1:5]
            xxparty <- xxparty[1:5]
            if (input$noparty == "Split by Ratio"){
                TotalReg <- xx$DemActive + xx$RepActive
                xx$DemActive <- xx$DemActive + xx$NoPartyAct * (xx$DemActive / TotalReg)
                xx$RepActive <- xx$RepActive + xx$NoPartyAct * (xx$RepActive / TotalReg)
                xx$NoPartyAct <- 0
            }
            else if (input$noparty == "Count as Dem"){
                xx$DemActive <- xx$DemActive + xx$NoPartyAct
                xx$NoPartyAct <- 0
            }
            else if (input$noparty == "Count as Rep"){
                xx$RepActive <- xx$RepActive + xx$NoPartyAct
                xx$NoPartyAct <- 0
            }
            else{
                xx$DemActive <- xx$DemActive + (xx$NoPartyAct / 2)
                xx$RepActive <- xx$RepActive + (xx$NoPartyAct / 2)
                xx$NoPartyAct <- 0
            }
            write(paste(xxparty, collapse = " "), paste0(data_dir,"IA_Registered_2020.csv"))
            write_delim(xx, paste0(data_dir,"IA_Registered_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createME <- function(){
            xx <- read_excel(paste0(input_dir,"ME20_presandvisecnty1120.xlsx"), sheet = "Statewide", skip = 2)
            names(xx) <- c("CTY","COUNTY","BIDEN","De La Fuente","Hawkins","Jorgensen","TRUMP","Others","Blank","TBC")
            xx <- xx[grepl(" Total", xx$COUNTY) | grepl(" UOCAVA", xx$COUNTY),]
            xx <- xx[,2:(NCOL(xx)-2)] # delete Blank as well
            xx$COUNTY <- gsub(" Totals", "", xx$COUNTY)
            xx$COUNTY <- gsub(" Total", "", xx$COUNTY)
            xx$COUNTY <- gsub("STATE UOCAVA", "UOCACA", xx$COUNTY)
            xx <- xx[xx$COUNTY != "Statewide",]
            namesxx <- names(xx)
            namesxx[2:6] <- c("DEM","ALL","GRN","LIB","REP")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_President_2020.csv"))
            write_delim(xx, paste0(data_dir,"ME_President_2020.csv"), append = TRUE, col_names = TRUE)

            xx <- read_excel(paste0(input_dir,"ME20_ussenator1120.xlsx"), sheet = "US Senator", skip = 2)
            names(xx) <- c("CTY","COUNTY","COLLINS","GIDEON","Linn","Savage","Others","Blank","TBC")
            xx <- xx[grepl(" Total", xx$COUNTY) | grepl(" UOCAVA", xx$COUNTY),]
            xx <- xx[,2:(NCOL(xx)-2)] # delete Blank as well
            xx$COUNTY <- gsub(" Totals", "", xx$COUNTY)
            xx$COUNTY <- gsub(" Total", "", xx$COUNTY)
            xx$COUNTY <- gsub("STATE UOCAVA", "UOCACA", xx$COUNTY)
            xx <- xx[xx$COUNTY != "State",]
            namesxx <- names(xx)
            namesxx[2:5] <- c("REP","DEM","IND1","IND2")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_Senate_2020.csv"))
            write_delim(xx, paste0(data_dir,"ME_Senate_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createMI <- function(){
            xx0 <- read_excel(paste0(input_dir,"MI20_2020GEN_MI_CENR_BY_COUNTY.xlsx"), sheet = "2020GEN_MI_CENR_BY_COUNTY")
            xx <- xx0[xx0$OfficeDescription == "President of the United States 4 Year Term (1) Position",]
            xx <- xx[,c("CountyName","CandidateLastName","CandidateVotes")]
            names(xx) <- c("COUNTY", "Candidate", "Votes")
            xx <- xx %>% spread("Candidate", "Votes")
            xx <- xx[!is.na(row.names),]
            xx <- xx[!is.na(xx$COUNTY), names(xx) != "<NA>"]
            xxparty <- names(xx)
            xxparty[xxparty == 'Biden'] <- "DEM"
            xxparty[xxparty == 'Trump'] <- "REP"
            xxparty <- paste0('"',xxparty,'"')
            write(paste(xxparty, collapse = " "), paste0(data_dir,"MI_President_2020.csv"))
            write_delim(xx, paste0(data_dir,"MI_President_2020.csv"), append = TRUE, col_names = TRUE)

            xx <- xx0[xx0$OfficeDescription == "United States Senator 6 Year Term (1) Position",]
            xx <- xx[,c("CountyName","CandidateLastName","CandidateVotes")]
            names(xx) <- c("COUNTY", "Candidate", "Votes")
            xx <- xx %>% spread("Candidate", "Votes")
            xx <- xx[!is.na(row.names),]
            xx <- xx[!is.na(xx$COUNTY), names(xx) != "<NA>"]
            xxparty <- names(xx)
            xxparty[xxparty == 'Peters'] <- "DEM"
            xxparty[xxparty == 'James'] <- "REP"
            xxparty <- paste0('"',xxparty,'"')
            write(paste(xxparty, collapse = " "), paste0(data_dir,"MI_Senate_2020.csv"))
            write_delim(xx, paste0(data_dir,"MI_Senate_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createNC <- function(){
            xx <- read_delim(paste0(input_dir,"NC20_results_pct_20201103.csv"), '\t')
            pp <- xx[xx$`Contest Name` == "US PRESIDENT",]
            pp$`Choice Party`[is.na(pp$`Choice Party`)] <- "NA"
            pp <- pp[,c("County","Choice Party","Total Votes")]
            names(pp)[1] <- "COUNTY"
            pp <- pp %>%
                group_by(COUNTY,`Choice Party`) %>%
                summarise(`Total Votes` = sum(`Total Votes`, na.rm = TRUE))
            pp <- pp %>% spread(`Choice Party`,`Total Votes`)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"NC_President_2020.csv"))
            write_delim(pp, paste0(data_dir,"NC_President_2020.csv"), append = TRUE, col_names = TRUE)

            pp <- xx[xx$`Contest Name` == "US SENATE",]
            pp$`Choice Party`[is.na(pp$`Choice Party`)] <- "NA"
            pp <- pp[,c("County","Choice Party","Total Votes")]
            names(pp)[1] <- "COUNTY"
            pp <- pp %>%
                group_by(COUNTY,`Choice Party`) %>%
                summarise(`Total Votes` = sum(`Total Votes`, na.rm = TRUE))
            pp <- pp %>% spread(`Choice Party`,`Total Votes`)
            write(paste(names(pp), collapse = " "), paste0(data_dir,"NC_Senate_2020.csv"))
            write_delim(pp, paste0(data_dir,"NC_Senate_2020.csv"), append = TRUE, col_names = TRUE)

            pp <- xx[grepl("^US HOUSE OF REPRESENTATIVES DISTRICT ",xx$`Contest Name`),]
            pp <- pp[,c("County","Contest Name","Choice Party","Total Votes")]
            names(pp) <- c("COUNTY","DISTRICT","PARTY","VOTES")
            pp$VOTES <- as.numeric(gsub(",","",pp$VOTES))
            pp$DISTRICT <- substring(pp$DISTRICT, 37)
            pp <- pp %>%
                group_by(COUNTY, DISTRICT, PARTY) %>%
                summarise(VOTES = sum(VOTES))
            pp <- pp %>% spread(PARTY, VOTES)
            namespp <- names(pp)
            write(paste(namespp, collapse = " "), paste0(data_dir,"NC_House_2020.csv"))
            write_delim(pp, paste0(data_dir,"NC_House_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createPA <- function(){
            xx0 <- read_csv(paste0(input_dir,"PA20_Official_1152021032923PM.CSV"))
            xx <- xx0[xx0$`Office Name` == "President of the United States",]
            xx$Pty_Candidate <- paste(xx$`Party Name`, word(xx$`Candidate Name`,1))
            gsub(",","",xx$Pty_Candidate)
            xx <- xx[,c("County Name","Pty_Candidate","Votes")]
            names(xx) <- c("COUNTY", "Candidate", "Votes")
            xx <- xx %>% spread("Candidate", "Votes")
            xx <- xx[!is.na(row.names),]
            xx <- xx[!is.na(xx$COUNTY), names(xx) != "<NA>"]
            xxparty <- word(names(xx), 1)
            xxparty[xxparty == "Democratic"] <- "DEM"
            xxparty[xxparty == "Republican"] <- "REP"
            names(xx)[xxparty == "DEM"] <- "BIDEN"
            names(xx)[xxparty == "REP"] <- "TRUMP"
            write(paste(xxparty, collapse = " "), paste0(data_dir,"PA_President_2020.csv"))
            write_delim(xx, paste0(data_dir,"PA_President_2020.csv"), append = TRUE, col_names = TRUE)

            rr0 <- read_excel(paste0(input_dir,"PA201116_currentvotestats.xls"), sheet = "Reg Voter", skip = 1)
            xx <- rr0[,c(1,3,5,7,9)]
            names(xx) <- c("COUNTY","Democratic","Republican","NoParty","Other")
            xxparty <- c("COUNTY","DEM","REP","NOP","OTH")
            xx <- xx[xx$COUNTY != "Totals:",]
            if (input$noparty == "Split by Ratio"){
                TotalReg <- xx$Democratic + xx$Republican
                xx$Democratic <- xx$Democratic + xx$NoParty * (xx$Democratic / TotalReg)
                xx$Republican <- xx$Republican + xx$NoParty * (xx$Republican / TotalReg)
                xx$NoParty <- 0
            }
            else if (input$noparty == "Count as Dem"){
                xx$Democratic <- xx$Democratic + xx$NoParty
                xx$NoParty <- 0
            }
            else if (input$noparty == "Count as Rep"){
                xx$Republican <- xx$Republican + xx$NoParty
                xx$NoParty <- 0
            }
            else{
                xx$Democratic <- xx$Democratic + (xx$NoParty / 2)
                xx$Republican <- xx$Republican + (xx$NoParty / 2)
                xx$NoParty <- 0
            }
            write(paste(xxparty, collapse = " "), paste0(data_dir,"PA_Registered_2020.csv"))
            write_delim(xx, paste0(data_dir,"PA_Registered_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createSC <- function(){
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "2", skip = 2)
            xx <- xx[,c(1,9,16,23,30,37)]
            names(xx) <- c("COUNTY","Hawkins","TRUMP","De La Fuente","Jorgensen","BIDEN")
            xx <- xx[xx$COUNTY != "Total:",]
            namesxx <- c("COUNTY","GRN","REP","ALN","LIB","DEM")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"SC_President_2020.csv"))
            write_delim(xx, paste0(data_dir,"SC_President_2020.csv"), append = TRUE, col_names = TRUE)

            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "3", skip = 2)
            xx <- xx[,c(1,9,16,23,30)]
            names(xx) <- c("COUNTY","Bledsoe","GRAHAM","HARRISON","Write-In")
            xx <- xx[xx$COUNTY != "Total:",]
            namesxx <- c("COUNTY","CON","REP","DEM","WRI")
            write(paste(namesxx, collapse = " "), paste0(data_dir,"SC_Senate_2020.csv"))
            write_delim(xx, paste0(data_dir,"SC_Senate_2020.csv"), append = TRUE, col_names = TRUE)

            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "4", skip = 2)
            xx <- xx[,c(1,2,9,16,22,23)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 1
            xx$CON <- 0
            yy <- xx
            
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "5", skip = 2)
            xx <- xx[,c(1,2,16,23,9,30)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 2
            yy <- rbind(yy, xx)
            
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "6", skip = 2)
            xx <- xx[,c(1,2,9,16,22,23)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 3
            xx$CON <- 0
            yy <- rbind(yy, xx)
            
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "7", skip = 2)
            xx <- xx[,c(1,2,16,23,9,30)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 4
            yy <- rbind(yy, xx)
            
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "8", skip = 2)
            xx <- xx[,c(1,2,9,16,22,23)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 5
            xx$CON <- 0
            yy <- rbind(yy, xx)
            
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "9", skip = 2)
            xx <- xx[,c(1,2,16,23,9,30)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 6
            yy <- rbind(yy, xx)
            
            xx <- read_excel(paste0(input_dir,"SC20_detail.xlsx"), sheet = "10", skip = 2)
            xx <- xx[,c(1,2,9,16,22,23)]
            names(xx) <- c("COUNTY","DISTRICT","REP","DEM","CON","WRI")
            xx <- xx[xx$COUNTY != "Total:",]
            xx$DISTRICT <- 7
            xx$CON <- 0
            yy <- rbind(yy, xx)
            write(paste(names(yy), collapse = " "), paste0(data_dir,"SC_House_2020.csv"))
            write_delim(yy, paste0(data_dir,"SC_House_2020.csv"), append = TRUE, col_names = TRUE)
        }
        createTX <- function(){
            xx <- read_excel(paste0(input_dir,"TX20_CountybyCountyCanvassReport.xlsx"), sheet = "Voter Turnout Report")
            #"ELECTION DATE-NAME,OFFICE NAME,CANDIDATE NAME,COUNTY NAME,TOTAL VOTES PER OFFICE PER COUNTY"
            pp <- xx[xx$`OFFICE NAME` == "PRESIDENT/VICE-PRESIDENT",]
            pp$Party <- "OTH"
            pp$Party[pp$`CANDIDATE NAME` == "JOSEPH R. BIDEN/KAMALA D. HARRIS"] <- "DEM"
            pp$Party[pp$`CANDIDATE NAME` == "DONALD J. TRUMP/MICHAEL R. PENCE"] <- "REP"
            pp <- pp[,c("COUNTY NAME","Party","TOTAL VOTES PER OFFICE PER COUNTY")]
            names(pp) <- c("COUNTY", "Party", "Votes")
            names(pp)[1] <- "COUNTY"
            pp <- pp %>%
                group_by(COUNTY, Party) %>%
                summarise(Votes = sum(as.numeric(gsub(",","",Votes)), na.rm = TRUE))
            pp <- pp %>% spread(Party, Votes)
            namespp <- names(pp)
            pp <- pp %>% rename("Biden" = "DEM", "Trump" = "REP")
            write(paste(namespp, collapse = " "), paste0(data_dir,"TX_President_2020.csv"))
            write_delim(pp, paste0(data_dir,"TX_President_2020.csv"), append = TRUE, col_names = TRUE)

            pp <- xx[xx$`OFFICE NAME` == "U. S.  SENATOR",]
            pp$Party <- "OTH"
            pp$Party[pp$`CANDIDATE NAME` == "MARY \"MJ\" HEGAR"] <- "DEM"
            pp$Party[pp$`CANDIDATE NAME` == "JOHN CORNYN"] <- "REP"
            pp <- pp[,c("COUNTY NAME","Party","TOTAL VOTES PER OFFICE PER COUNTY")]
            names(pp) <- c("COUNTY", "Party", "Votes")
            names(pp)[1] <- "COUNTY"
            pp <- pp %>%
                group_by(COUNTY, Party) %>%
                summarise(Votes = sum(as.numeric(gsub(",","",Votes)), na.rm = TRUE))
            pp <- pp %>% spread(Party, Votes)
            namespp <- names(pp)
            pp <- pp %>% rename("Hegar" = "DEM", "Cornyn" = "REP")
            write(paste(namespp, collapse = " "), paste0(data_dir,"TX_Senate_2020.csv"))
            write_delim(pp, paste0(data_dir,"TX_Senate_2020.csv"), append = TRUE, col_names = TRUE)

            zz <- read_excel(paste0(input_dir,"TX20_OfficialCanvassReport.xlsx"), sheet = "Official Canvass Report")
            zz <- zz[,c("OFFICE NAME","CANDIDATE NAME","PARTY"),]
            zz$`CANDIDATE NAME` <- gsub(" \\(I\\)","",zz$`CANDIDATE NAME`)
            pp <- xx[grepl("^U. S. REPRESENTATIVE DISTRICT ",xx$`OFFICE NAME`),]
            pp <- merge(pp, zz, by=c("OFFICE NAME","CANDIDATE NAME"))
            pp <- pp[,c("COUNTY NAME","OFFICE NAME","PARTY","TOTAL VOTES PER OFFICE PER COUNTY")]
            names(pp) <- c("COUNTY","DISTRICT","PARTY","VOTES")
            pp$VOTES <- as.numeric(gsub(",","",pp$VOTES))
            pp$DISTRICT <- substring(pp$DISTRICT, 30)
            pp <- pp %>%
                group_by(COUNTY, DISTRICT, PARTY) %>%
                summarise(VOTES = sum(VOTES))
            pp <- pp %>% spread(PARTY, VOTES)
            namespp <- names(pp)
            write(paste(namespp, collapse = " "), paste0(data_dir,"TX_House_2020.csv"))
            write_delim(pp, paste0(data_dir,"TX_House_2020.csv"), append = TRUE, col_names = TRUE)

            filenamex <- paste0(input_dir,"TX_Senate_2018_00.csv")
            xxparty <- read_delim(filenamex, '\t', col_names = FALSE, n_max = 1)
            xx <- read_delim(filenamex, '\t', skip = 1)
            #"County,REP,DEM,LIB,Votes,Voters,TurnOut"
            xx <- xx[,1:4]
            xxparty <- xxparty[1:4]
            write(paste(xxparty, collapse = " "), paste0(data_dir,"TX_Senate_2018.csv"))
            write_delim(xx, paste0(data_dir,"TX_Senate_2018.csv"), append = TRUE, col_names = TRUE)

            filenamex <- paste0(input_dir,"TX_President_2016_00.csv")
            xxparty <- read_delim(filenamex, '\t', col_names = FALSE, n_max = 1)
            xx <- read_delim(filenamex, '\t', skip = 1)
            #"County,REP,DEM,LIB,...,Votes,Voters,TurnOut"
            xx <- xx[,1:(NCOL(xx)-3)]
            xxparty <- xxparty[1:(NCOL(xxparty)-3)]
            write(paste(xxparty, collapse = " "), paste0(data_dir,"TX_President_2016.csv"))
            write_delim(xx, paste0(data_dir,"TX_President_2016.csv"), append = TRUE, col_names = TRUE)

            filenamex <- paste0(input_dir,"TX20_Registered00.csv")
            xxparty <- read_delim(filenamex, '\t', col_names = FALSE, n_max = 1)
            xx0 <- read_delim(filenamex, '\t', skip = 1)
            xx <- xx0[,c(1,4:5)]
            xx <- xx[xx$COUNTY != "Statewide Total",]
            xxparty <- xxparty[c(1,4:5)]
            write(paste(xxparty, collapse = " "), paste0(data_dir,"TX_Registered_2020.csv"))
            write_delim(xx, paste0(data_dir,"TX_Registered_2020.csv"), append = TRUE, col_names = TRUE)
        }
        getlabels <- function(){
            if (input$measure == "Percent change"){
                tshift <- "% Change in"
            }
            else if (input$measure == "Percent ratio"){
                tshift <- "% Ratio of"
            }
            else{
                tshift <- "Shift in"
            }
            if (input$units == "Percent"){
                tunits <- "Vote Share"
            }
            else{
                tunits <- "Votes"
            }
            if (input$party == "Margin"){
                tnote <- "(positive direction is more Democratic)"
            }
            else{
                tnote <- ""
            }
            if (input$district == ""){
                tstate2 <- input$state2
            }
            else{
                tstate2 <- paste0(input$state2,"-",input$district)
            }
            racex <- paste0(input$racex,"_",input$yearx)
            racey <- paste0(input$racey,"_",input$yeary)
            if (input$racey == "Registered"){
                title <- paste(tshift, input$party, "Voters for",
                               racex, "to", input$party, racey,
                               "in", tstate2,
                               "counties", tnote)
                ylabel <- paste(tshift, input$party, "Voters to", input$party,
                                racey)
            }
            else if (input$fcounty != ""){
                title <- paste(tshift, input$party, tunits, "from",
                               racex, "to", racey,
                               "Race in", tstate2,input$fcounty,
                               "County", tnote)
                ylabel <- paste(tshift, input$party, tunits, "for", racey)
            }
            else{
                title <- paste(tshift, input$party, tunits, "from",
                               racex, "to", racey,
                               "Race in", tstate2,
                               "counties", tnote)
                ylabel <- paste(tshift, input$party, tunits, "for", racey)
            }
            xlabel <- paste0(input$party," ",tunits," for ", racex,
                             "\nSources: see http://econdataus.com/voting_stats.htm")
            title2 <- title
            counties_in <- "counties"
            if (input$mapvar == "MARGIN1"){
                title2 <- paste("Margin", tunits, "in", racex, "Race in", tstate2,
                                counties_in, tnote)
            }
            else if (input$mapvar == "MARGIN2"){
                title2 <- paste("Margin", tunits, "in", racey, "Race in", tstate2,
                                counties_in, tnote)
            }
            labels <- c(title, xlabel, ylabel, title2)
            return(labels)
        }
        output$myUsage <- renderUI({
            includeHTML("voting_stats.htm")
        })
        output$myPlot <- renderPlot({
            xx <- getdata()
            if (input$units == "Count"){
                xx <- xx[-NROW(xx),]
            }
            if (names(xx)[2] == "DISTRICT")
            {
                names(xx)[1:10] <- c("COUNTY","DISTRICT","DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
            }
            else{
                names(xx)[1:9] <- c("COUNTY","DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
            }
            if (input$party == "Democrat"){
                preparty <- "DEM"
                party1 <- "DEM1"
            }
            else if (input$party == "Republican"){
                preparty <- "REP"
                party1 <- "REP1"
            }
            else if (input$party == "Total"){
                preparty <- "TOT"
                party1 <- "TOTAL1"
            }
            else{
                preparty <- "MAR"
                party1 <- "MARGIN1"
            }
            party_sh <- paste0(preparty,"_SH")
            party1n <- "TOT1_N"
            xx$Party <- ""
            if (input$xlimit != ""){
                vlimit <- as.numeric(unlist(strsplit(input$xlimit, ",")))
                vparty <- unlist(strsplit(input$xparty, ","))
                xx$Party <- vparty[length(vparty)]
                xx$Party[xx[["MARGIN1"]] < vlimit[1]] <- vparty[1]
                for (i in 1:length(vlimit)){
                    xx$Party[xx[["MARGIN1"]] >= vlimit[i] & xx[["MARGIN1"]] < vlimit[i+1]] <- vparty[i+1]
                }
            }
            if (input$vlimit != ""){
                vlimit <- as.numeric(unlist(strsplit(input$vlimit, ","))) * 1000
                vdesc <- unlist(strsplit(input$vdesc, ","))
                xx$Votes <- vdesc[length(vdesc)]
                xx$Votes[xx[[party1n]] < vlimit[1]] <- vdesc[1]
                for (i in 1:length(vlimit)){
                    xx$Votes[xx[[party1n]] >= vlimit[i] & xx[[party1n]] < vlimit[i+1]] <- vdesc[i+1]
                }
            }
            isParty <- NULL
            for (i in 1:length(vparty)){
                isParty <- c(isParty, any(xx$Party == vparty[i]))
            }
            isVotes <- NULL
            for (i in 1:length(vdesc)){
                isVotes <- c(isVotes, any(xx$Votes == vdesc[i]))
            }
            gg <- ggplot(xx, aes_string(x=party1, y=party_sh))
            gg <- gg + geom_point(data=xx, size=3, alpha=0.7,
                                  aes_string(color="Party",shape="Votes"))
            if (input$party == "Margin"){
                gg <- gg + geom_abline(intercept=0, slope=-1, color="gray", linetype="dashed")
            }
            if (input$party == "Margin" | input$units == "Count"){
                gg <- gg + geom_vline(xintercept=0, color="gray")
            }
            if (input$measure != "Percent ratio"){
                gg <- gg + geom_hline(yintercept=0, color="gray")
            }
            vcolor <- unlist(strsplit(input$xcolor, ","))
            vcolor <- vcolor[isParty]
            if (length(vcolor) > 1){
                gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
                gg <- gg + scale_color_manual(values = vcolor) # Line Graph
            }
            vshape <- as.numeric(unlist(strsplit(input$vshape, ",")))
            vshape <- vshape[isVotes]
            if (length(vshape) > 1){
                gg <- gg + scale_shape_manual(values = vshape) # Line Graph
            }
            if (input$measure == "Percent change"){
                tshift <- "% Change in"
            }
            else if (input$measure == "Percent ratio"){
                tshift <- "% Ratio of"
            }
            else{
                tshift <- "Shift in"
            }
            if (input$units == "Percent"){
                tunits <- "Vote Share"
            }
            else{
                tunits <- "Votes"
            }
            if (input$party == "Margin"){
                tnote <- "(positive direction is more Democratic)"
            }
            else{
                tnote <- ""
            }
            if (input$district == ""){
                tstate2 <- input$state2
            }
            else{
                tstate2 <- paste0(input$state2,"-",input$district)
            }
            racex <- paste0(input$racex,"_",input$yearx)
            racey <- paste0(input$racey,"_",input$yeary)
            if (input$racey == "Registered"){
                gg <- gg + ggtitle(paste(tshift, input$party, "Voters for",
                                         racex, "to", input$party, racey,
                                         "in", tstate2,
                                         "counties", tnote))
                gg <- gg + ylab(paste(tshift, input$party, "Voters to", input$party,
                                      racey))
            }
            else if (input$fcounty != ""){
                gg <- gg + ggtitle(paste(tshift, input$party, tunits, "from",
                                         racex, "to", racey,
                                         "Race in", tstate2,input$fcounty,
                                         "County", tnote))
                gg <- gg + ylab(paste(tshift, input$party, tunits, "for", racey))
            }
            else{
                gg <- gg + ggtitle(paste(tshift, input$party, tunits, "from",
                                         racex, "to", racey,
                                         "Race in", tstate2,
                                         "counties", tnote))
                gg <- gg + ylab(paste(tshift, input$party, tunits, "for", racey))
            }
            gg <- gg + xlab(paste0(input$party," ",tunits," for ", racex,
                                   "\nSources: see http://econdataus.com/voting_stats.htm"))
            if (input$fcounty != "" & names(xx)[2] == "DISTRICT"){
                xx$LABEL <- xx$DISTRICT
            }
            else{
                xx$LABEL <- xx$COUNTY
            }
            if (input$showrow){
                xx$LABEL <- paste0(xx$LABEL,"-",row.names(xx))
            }
            xx$POS   <- 2 # RESET TO 2
            spos1 <- unlist(strsplit(input$pos1, ","))
            xx$POS[xx$LABEL %in% spos1] <- 1
            spos3 <- unlist(strsplit(input$pos3, ","))
            xx$POS[xx$LABEL %in% spos3] <- 3
            xx$VJUST <- 0.5
            xx$VJUST[xx$POS == 1] <- -1
            xx$VJUST[xx$POS == 3] <- 2
            xx$PREPEND <- ""
            xx$PREPEND[xx$POS == 2] <- "  "
            xx$LABEL <- paste0(xx$PREPEND,xx$LABEL)
            if (input$fpop != ""){
                kpop <- 1000 * as.numeric(input$fpop)
                if (kpop > 0){
                    xx$LABEL[xx$TOT1_N < kpop] <- ""
                }
            }
            if (input$party == "Democrat"){
                gg <- gg + annotate("text", x = xx$DEM1, y =xx$DEM_SH, label = xx$LABEL,
                                    color="red", hjust = 0, vjust = xx$VJUST)
            }
            else if (input$party == "Republican"){
                gg <- gg + annotate("text", x = xx$REP1, y =xx$REP_SH, label = xx$LABEL,
                                    color="red", hjust = 0, vjust = xx$VJUST)
            }
            else if (input$party == "Total"){
                gg <- gg + annotate("text", x = xx$TOTAL1, y =xx$TOT_SH, label = xx$LABEL,
                                    color="red", hjust = 0, vjust = xx$VJUST)
            }
            else{
                gg <- gg + annotate("text", x = xx$MARGIN1, y =xx$MAR_SH, label = xx$LABEL,
                                    color="red", hjust = 0, vjust = xx$VJUST)
            }
            xx <- NULL
            yy <- NULL
            if(input$xscale != ""){
                sxx <- unlist(strsplit(input$xscale, ","))
                xx <- as.numeric(sxx)
                if (length(sxx) == 3){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[3]))
                }
                else if (length(sxx) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[4]))
                }
            }
            if(input$yscale != ""){
                syy <- unlist(strsplit(input$yscale, ","))
                yy <- as.numeric(syy)
                if (length(syy) == 3){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[3]))
                }
                else if (length(syy) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[4]))
                }
            }
            if (length(xx) >= 2){
                if (length(yy) >= 2){
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]), ylim = c(yy[1], yy[2]))
                }
                else{
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]))
                }
            }
            else if (length(yy) >= 2){
                gg <- gg + coord_cartesian(ylim = c(yy[1], yy[2]))
            }
            return(gg)
        }, height = 600, width = 1000)
        output$myText <- renderPrint({
            dd <- getdata()
            firstn <- 2
            if (names(dd)[2] == "DISTRICT"){
                firstn <- 3
            }
            dd <- dd[,1:(NCOL(dd)-4)]
            dp <- 2
            for (i in firstn:NCOL(dd)){
                dd[,i] <- format(round(dd[,i], dp), big.mark=",", scientific=FALSE)
            }
            if (input$measure == "Percent change"){
                tshift <- "Percent change in"
            }
            else if (input$measure == "Percent ratio"){
                tshift <- "Percent ratio of"
            }
            else{
                tshift <- "Shift in"
            }
            if (input$units == "Percent"){
                tunits <- "Vote Share"
            }
            else{
                tunits <- "Votes"
            }
            if (input$party == "Margin"){
                tnote <- "(positive direction is more Democratic)"
            }
            else{
                tnote <- ""
            }
            if (input$district == ""){
                tstate2 <- input$state2
            }
            else{
                tstate2 <- paste0(input$state2,"-",input$district)
            }
            racex <- paste0(input$racex,"_",input$yearx)
            racey <- paste0(input$racey,"_",input$yeary)
            if (input$racey == "Registered"){
                title <- paste0(tshift," Voters for ",
                                racex," to ",racey,
                                " Voters in ",tstate2," counties (",
                                input$units,")")
            }
            else{
                title <- paste0(tshift," ",tunits," from ",
                                racex," to ",racey,
                                " Race in ",tstate2," counties (",
                                input$units,")")
            }
            cat(paste0(title,"\n\n"))
            print(dd)
        })
        output$myVoteData <- renderPrint({
            states <- c("AZ","CA","FL","GA","IA","KY","ME","MI","MT","NC",
                        "PA","SC","TX")
            races <- c("President", "Senate", "House", "Registered")
            years <- seq(input$year_first, input$year_last, by = input$year_step)
            files <- data.frame(matrix(ncol = length(states)+2, nrow = 0))
            colnames(files) <- c("Year", "Race", states)
            for (year in years){
                for (race in races){
                    newrow <- c(year, race)
                    tot <- 0
                    for (state in states){
                        filename <- paste0(data_dir,state,"_",race,"_",year,".csv")
                        cnt <- 0
                        if (file.exists(filename)){
                            cnt <- length(readLines(filename)) - 2
                            tot <- tot + 1
                        }
                        newrow <- c(newrow, cnt)
                    }
                    if (tot > 0){
                        files[nrow(files)+1,] = newrow
                    }
                }
            }
            races <- c("Map_Parms", "Plot_Parms")
            for (race in races){
                newrow <- c(0, race)
                tot <- 0
                for (state in states){
                    filename <- paste0(data_dir,state,"_",race,".csv")
                    cnt <- 0
                    if (file.exists(filename)){
                        cnt <- length(readLines(filename)) - 1
                        tot <- tot + 1
                    }
                    newrow <- c(newrow, cnt)
                }
                if (tot > 0){
                    files[nrow(files)+1,] = newrow
                }
            }
            print(files)
        })
        output$myggMap <- renderPlot({
            dd <- getdata() # COUNTY,DEM1,REP1,MARGIN1,TOTAL1,DEM2,REP2,MARGIN2,TOTAL2,
                            # DEM_SH,REP_SH,MAR_SH,TOT_SH,DEM1_N,REP1_N,MAR1_N,TOT1_N
            dd <- dd[dd$COUNTY != "TOTAL",]

            dd$county_name <- paste(str_to_upper(dd$COUNTY), "COUNTY")
            counties$county_name <- str_to_upper(counties$county_name)
            scounties <- counties %>%
                filter(state_abbv == input$state2)
            if (input$state2 != ""){
                stcounty <- paste0(input$state2,"|",dd$county_name)
                stcounty_chng <- read_csv("stcounty_chng.csv")
                for (i in 1:NROW(stcounty_chng)){
                    dd$county_name[stcounty == stcounty_chng$old[i]] <- stcounty_chng$new[i]
                }
            }
            election_data <- left_join(dd, scounties, by = "county_name")
            counties1_not2 <- dd$county_name[!dd$county_name %in% scounties$county_name]
            counties2_not1 <- scounties$county_name[!scounties$county_name %in% dd$county_name]
            if (length(counties1_not2) > 0){
                print("counties1_not2:")
                print(counties1_not2)
            }
            if (length(counties2_not1) > 0){
                print("counties2_not1:")
                print(counties2_not1)
            }
            mapcolors <- unlist(strsplit(input$mapcolors, ","))
            fipref <- "state.txt" # read from local memory
            ff <<- read.csv(fipref, sep = "|")
            st_cities <- read.csv("us_cities.csv")
            if (input$state2 != ""){
                istate <- ff$STATE[ff$STUSAB == input$state2]
                sstate <- sprintf("%02d", istate)
                st_cities<-subset(st_cities,country.etc==input$state2)
                stdata <- urbnmapr::countydata[startsWith(urbnmapr::countydata$county_fips, sstate),]
            }
            else{
                stdata <- urbnmapr::countydata
            }
            labels <- getlabels()
            xlabel <- paste0("longitude\nSources: see http://econdataus.com/voting_stats.htm")
            mapvar <- input$mapvar
            if (mapvar == "DEM1"){mapvar <- names(dd)[2]}
            else if (mapvar == "REP1"){mapvar <- names(dd)[3]}
            else if (mapvar == "DEM2"){mapvar <- names(dd)[6]}
            else if (mapvar == "REP2"){mapvar <- names(dd)[7]}
            
            gg <- election_data %>%
                ggplot(aes_string("long", "lat", group = "group", fill = mapvar)) +
                geom_polygon(color = NA) +
                coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
                labs(fill = "Shift in Margin") +
                ggtitle(labels[4]) +
                xlab(xlabel) +
                ylab("latitude")
            maplimits <- input$maplimits
            if (input$maplimitset == "Auto set to min,max"){
                minlimit <- floor(min(election_data[[mapvar]]))
                maxlimit <- ceiling(max(election_data[[mapvar]]))
                maplimits <- c(minlimit, maxlimit)
                updateTextInput(session, "maplimits", value = maplimits)
            }
            else if (input$maplimitset == "Auto set balanced"){
                minlimit <- floor(min(election_data[[mapvar]]))
                maxlimit <- ceiling(max(election_data[[mapvar]]))
                maplimits <- max(abs(minlimit), abs(maxlimit))
                updateTextInput(session, "maplimits", value = maplimits)
            }
            if (maplimits == ""){
                gg <- gg + scale_fill_gradientn(
                    colors = mapcolors,
                    na.value = "white",
                    guide = guide_colorbar(title.position = "top"))
            }
            else{
                maplimits <- unlist(strsplit(input$maplimits, ","))
                if (length(maplimits) == 1){
                    maplimits <- c(paste0("-",maplimits[1]), maplimits[1])
                }
                gg <- gg + scale_fill_gradientn(
                    colors = mapcolors,
                    na.value = "white",
                    limits = as.numeric(maplimits),
                    guide = guide_colorbar(title.position = "top"))
            }
            skipcity <- unlist(strsplit(input$skipcity, ","))
            showcity <- unlist(strsplit(input$showcity, ","))
            if (input$state2 != ""){
                for (i in 1:dim(st_cities)[1]){
                    city <- substr(st_cities$name[i], 1, nchar(as.character(st_cities$name[i]))-3)
                    if (city %in% skipcity) next
                    if (city %in% showcity | st_cities$pop[i] > input$minpop){
                        gg <- gg + annotate(geom = "point", x = st_cities$long[i], y = st_cities$lat[i])
                        gg <- gg + annotate(geom = "text", x = st_cities$long[i] + input$longoff,
                                            y = st_cities$lat[i], label = city)
                    }
                }
            }
            print(gg)
        }, height = 900, width = 1200)
        states <- c("Alabama","Alaska","Arizona","Arkansas","California",
                    "Colorado","Connecticut","Delaware","DC","Florida",
                    "Georgia","Hawaii","Idaho","Illinois","Indiana",
                    "Iowa","Kansas","Kentucky","Louisiana","Maine",
                    "Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
                    "Missouri","Montana","Nebraska","Nevada","New Hampshire",
                    "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                    "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
                    "South Carolina","South Dakota","Tennessee","Texas","Utah",
                    "Vermont","Virginia","Washington","West Virginia","Wisconsin",
                    "Wyoming","D.C.")
        stabbr <- c("AL","AK","AZ","AR","CA",
                    "CO","CT","DE","DC","FL",
                    "GA","HI","ID","IL","IN",
                    "IA","KS","KY","LA","ME",
                    "MD","MA","MI","MN","MS",
                    "MO","MT","NE","NV","NH",
                    "NJ","NM","NY","NC","ND",
                    "OH","OK","OR","PA","RI",
                    "SC","SD","TN","TX","UT",
                    "VT","VA","WA","WV","WI","WY","DC")
        getStateAbbr <- function(str){
            for (i in 1:length(states)){
                pattern <- paste0("^",states[i])
                if (any(grep(pattern, str))){
                    str <- gsub(pattern, stabbr[i], str)
                    break
                }
            }
            return(str)
        }
        #getdata <- eventReactive(input$submit, {
        #############################################################
        # getdata
        #############################################################
        getdata <- reactive({
            if (input$createfiles){
                if (input$state2 == "CA"){
                    createCA()
                }
                else if (input$state2 == "FL"){
                    createFL()
                }
                else if (input$state2 == "GA"){
                    createGA()
                }
                else if (input$state2 == "IA"){
                    createIA()
                }
                else if (input$state2 == "ME"){
                    createME()
                }
                else if (input$state2 == "MI"){
                    createMI()
                }
                else if (input$state2 == "NC"){
                    createNC()
                }
                else if (input$state2 == "PA"){
                    createPA()
                }
                else if (input$state2 == "SC"){
                    createSC()
                }
                else if (input$state2 == "TX"){
                    createTX()
                }
            }
            filenamex <- paste0(data_dir,input$state2,"_",input$racex,"_",input$yearx,".csv")
            filenamey <- paste0(data_dir,input$state2,"_",input$racey,"_",input$yeary,".csv")
            xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
            yyparty <- read_delim(filenamey, ' ', col_names = FALSE, n_max = 1)
            xids <- 1
            yids <- 1
            if (grepl("^House",input$racex)){
                xids <- 2
            }
            if (grepl("^House",input$racey)){
                yids <- 2
            }
            
            xx0 <- read_delim(filenamex, ' ', skip = 1)
            # Remove columns where __party starts with X_, rows where COUNTY starts with X_
            xx0 <- xx0[,!grepl("^X_",xxparty)]
            xxparty <- xxparty[,!grepl("^X_",xxparty)]
            xx0 <- xx0[ !grepl("^X_",xx0$COUNTY),]
            xx0$COUNTY <- str_to_title(xx0$COUNTY) # COUNTY,all_parties
            
            yy0 <- read_delim(filenamey, ' ', skip = 1)
            # Remove columns where __party starts with X_, rows where COUNTY starts with X_
            yy0 <- yy0[,!grepl("^X_",yyparty)]
            yyparty <- yyparty[,!grepl("^X_",yyparty)]
            yy0 <- yy0[ !grepl("^X_",yy0$COUNTY),]
            yy0$COUNTY <- str_to_title(yy0$COUNTY) # COUNTY,all_parties
            
            xx0$MARGIN1 <- 0
            xx0$TOTAL1 <- rowSums(xx0[,(xids+1):(NCOL(xx0)-1)], na.rm = TRUE) # excludes MARGIN1
            idem <- which(xxparty == "DEM")
            irep <- which(xxparty == "REP")
            xx <- xx0[,c(1:xids,idem,irep,NCOL(xx0)-1,NCOL(xx0))] # COUNTY,[DISTRICT],DEM,REP,MARGIN1,TOTAL1
            
            yy0$MARGIN2 <- 0
            yy0$TOTAL2 <- rowSums(yy0[,(yids+1):(NCOL(yy0)-1)], na.rm = TRUE) # excludes MARGIN2
            idem <- which(yyparty == "DEM")
            irep <- which(yyparty == "REP")
            yy <- yy0[,c(1:yids,idem,irep,NCOL(yy0)-1,NCOL(yy0))] # COUNTY,[DISTRICT],DEM,REP,MARGIN2,TOTAL2
            
            if (xids == 2 & yids == 1){ # aggregate if only this is House race
                xids <- 1
                xx <- xx[,-2]
                xxparty <- xxparty[-2]
                names(xx) <- c("COUNTY","DEM1","REP1","MARGIN1","TOTAL1")
                if (input$distype == "1 & 2-party"){ # remove NA county/districts
                    xx <- xx[!is.na(xx$DEM1) & !is.na(xx$REP1),]
                    narm <- TRUE
                }
                else if (input$distype == "2-party"){ # remove entire NA counties
                    narm <- FALSE
                }
                else{ # All (and previously Non-zero)
                    xx$DEM1[!is.na(xx$DEM1)] <- 0
                    xx$REP1[!is.na(xx$REP1)] <- 0
                    narm <- TRUE
                }
                xx <- xx %>%
                    group_by(COUNTY) %>%
                    summarise(DEM1 = sum(DEM1, na.rm = narm), REP1 = sum(REP1, na.rm = narm),
                              MARGIN1 = sum(MARGIN1, na.rm = narm), TOTAL1 = sum(TOTAL1, na.rm = narm))
                if (input$distype != "All"){
                    xx <- xx[xx$DEM1 != 0 & xx$REP1 != 0,]
                }
                xx <- as.data.frame(xx)
            }
            if (yids == 2 & xids == 1){ # aggregate if only this is House race
                yids <- 1
                yy <- yy[,-2]
                yyparty <- yyparty[-2]
                names(yy) <- c("COUNTY","DEM2","REP2","MARGIN2","TOTAL2")
                if (input$distype == "1 & 2-party"){
                    yy <- yy[!is.na(yy$DEM2) & !is.na(yy$REP2),]
                    narm <- TRUE
                }
                else if (input$distype == "2-party"){
                    narm <- FALSE
                }
                else{ # All (and previously Non-zero)
                    yy$DEM2[!is.na(yy$DEM2)] <- 0
                    yy$REP2[!is.na(yy$REP2)] <- 0
                    narm <- TRUE
                }
                yy <- yy %>%
                    group_by(COUNTY) %>%
                    summarise(DEM2 = sum(DEM2, na.rm = narm), REP2 = sum(REP2, na.rm = narm),
                              MARGIN2 = sum(MARGIN2, na.rm = narm), TOTAL2 = sum(TOTAL2, na.rm = narm))
                if (input$distype != "All"){
                    yy <- yy[yy$DEM2 != 0 & yy$REP2 != 0,]
                }
                yy <- as.data.frame(yy)
            }
            if (xids == 2 & yids == 2){
                dd <- as.data.frame(merge(xx, yy, by = c("COUNTY","DISTRICT"), all = TRUE))
                ddnames <- names(dd)
                names(dd) <- c("COUNTY","DISTRICT","DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
                ddtot <- data.frame("TOTAL","",sum(dd$DEM1,na.rm=TRUE),sum(dd$REP1,na.rm=TRUE),
                                    sum(dd$MARGIN1,na.rm=TRUE),sum(dd$TOTAL1,na.rm=TRUE),
                                    sum(dd$DEM2,na.rm=TRUE),sum(dd$REP2,na.rm=TRUE),
                                    sum(dd$MARGIN2,na.rm=TRUE),sum(dd$TOTAL2,na.rm=TRUE))
            }
            else{
                dd <- as.data.frame(merge(xx, yy, by = "COUNTY"))
                ddnames <- names(dd)
                names(dd) <- c("COUNTY","DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
                ddtot <- data.frame("TOTAL",sum(dd$DEM1,na.rm=TRUE),sum(dd$REP1,na.rm=TRUE),
                                    sum(dd$MARGIN1,na.rm=TRUE),sum(dd$TOTAL1,na.rm=TRUE),
                                    sum(dd$DEM2,na.rm=TRUE),sum(dd$REP2,na.rm=TRUE),
                                    sum(dd$MARGIN2,na.rm=TRUE),sum(dd$TOTAL2,na.rm=TRUE))
            }
            names(ddtot) <- names(dd)
            dd <- rbind(dd, ddtot)
            if (input$fcounty != ""){
                dd <- dd[dd$COUNTY == input$fcounty,]
            }
            else if (input$district != "" & xids == 2){
                dd <- dd[dd$DISTRICT == input$district,]
            }
            if (xids == 2){
                #"COUNTY","DISTRICT","DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2"
                if (input$distype == "2-party"){
                    dd <- dd[!is.na(dd$DEM1) & !is.na(dd$REP1) & !is.na(dd$DEM2) & !is.na(dd$REP2),]
                }
                if (!input$showdist){
                    xids <- 1
                    dd <- dd[,-2]
                    ddnames <- ddnames[-2]
                    dd <- dd %>%
                        group_by(COUNTY) %>%
                        summarise(DEM1 = sum(DEM1, na.rm = TRUE), REP1 = sum(REP1, na.rm = TRUE),
                                  MARGIN1 = sum(MARGIN1, na.rm = TRUE), TOTAL1 = sum(TOTAL1, na.rm = TRUE),
                                  DEM2 = sum(DEM2, na.rm = TRUE), REP2 = sum(REP2, na.rm = TRUE),
                                  MARGIN2 = sum(MARGIN2, na.rm = TRUE), TOTAL2 = sum(TOTAL2, na.rm = TRUE))
                    if (input$distype != "All"){
                        dd <- dd[dd$DEM1 != 0 & dd$REP1 != 0 & dd$DEM2 != 0 & dd$REP2 != 0,]
                    }
                    dd <- as.data.frame(dd)
                }
            }
            DEM1_N <- dd$DEM1
            REP1_N <- dd$REP1
            MAR1_N <- dd$MARGIN1
            TOT1_N <- dd$TOTAL1
            dd$MARGIN1 <- dd$DEM1 - dd$REP1
            dd$MARGIN2 <- dd$DEM2 - dd$REP2
            if (input$dronly){
                dd$TOTAL1 <- dd$DEM1 + dd$REP1
                dd$TOTAL2 <- dd$DEM2 + dd$REP2
            }
            if (input$racey == "Registered"){
                if (input$measure == "Percent change"){
                    dd$DEM_SH <- 100 * (dd$DEM2 - dd$DEM1) / dd$DEM1
                    dd$REP_SH <- 100 * (dd$REP2 - dd$REP1) / dd$REP1
                    dd$MAR_SH <- 100 * (dd$MARGIN2 - dd$MARGIN1) / dd$MARGIN1
                    dd$TOT_SH <- 100 * (dd$TOTAL2 - dd$TOTAL1) / dd$TOTAL1
                }
                else if (input$measure == "Percent ratio"){
                    dd$DEM_SH <- 100 * dd$DEM1 / dd$DEM2
                    dd$REP_SH <- 100 * dd$REP1 / dd$REP2
                    dd$MAR_SH <- 100 * dd$MARGIN1 / dd$MARGIN2
                    dd$TOT_SH <- 100 * dd$TOTAL1 / dd$TOTAL2
                }
                else{
                    dd$DEM_SH <- dd$DEM2 - dd$DEM1
                    dd$REP_SH <- dd$REP2 - dd$REP1
                    dd$MAR_SH <- dd$MARGIN2 - dd$MARGIN1
                    dd$TOT_SH <- dd$TOTAL2 - dd$TOTAL1
                }
            }
            if (input$units == "Percent"){
                dd$DEM1 <- 100 * dd$DEM1 / dd$TOTAL1
                dd$REP1 <- 100 * dd$REP1 / dd$TOTAL1
                dd$MARGIN1 <- 100 * dd$MARGIN1 / dd$TOTAL1
                dd$DEM2 <- 100 * dd$DEM2 / dd$TOTAL2
                dd$REP2 <- 100 * dd$REP2 / dd$TOTAL2
                dd$MARGIN2 <- 100 * dd$MARGIN2 / dd$TOTAL2
                dd$TOTAL1 <- dd$DEM1 + dd$REP1
                dd$TOTAL2 <- dd$DEM2 + dd$REP2
            }
            if (input$racey != "Registered"){
                if (input$measure == "Percent change"){
                    dd$DEM_SH <- 100 * (dd$DEM2 - dd$DEM1) / dd$DEM1
                    dd$REP_SH <- 100 * (dd$REP2 - dd$REP1) / dd$REP1
                    dd$MAR_SH <- 100 * (dd$MARGIN2 - dd$MARGIN1) / dd$MARGIN1
                    dd$TOT_SH <- 100 * (dd$TOTAL2 - dd$TOTAL1) / dd$TOTAL1
                }
                else if (input$measure == "Percent ratio"){
                    dd$DEM_SH <- 100 * dd$DEM1 / dd$DEM2
                    dd$REP_SH <- 100 * dd$REP1 / dd$REP2
                    dd$MAR_SH <- 100 * dd$MARGIN1 / dd$MARGIN2
                    dd$TOT_SH <- 100 * dd$TOTAL1 / dd$TOTAL2
                }
                else{
                    dd$DEM_SH <- dd$DEM2 - dd$DEM1
                    dd$REP_SH <- dd$REP2 - dd$REP1
                    dd$MAR_SH <- dd$MARGIN2 - dd$MARGIN1
                    dd$TOT_SH <- dd$TOTAL2 - dd$TOTAL1
                }
            }
            names(dd)[1:9] <- ddnames
            dd$DEM1_N <- DEM1_N
            dd$REP1_N <- REP1_N
            dd$MAR1_N <- MAR1_N
            dd$TOT1_N <- TOT1_N
            dd
        })
        observeEvent(input$mapsave,{
            eventid <- "Map"
            parmid <- c("minpop", "longoff", "skipcity",
                        "showcity", "maplimitset", "maplimits",
                        "mapyear","mapvar","mapcolors")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                newversion <- max(parms$version) + 1
            }
            else{
                parms <- data.frame(version=integer(),
                                    label=character(),
                                    value=character(),
                                    stringsAsFactors = FALSE)
                newversion <- 1
            }
            nr <- NROW(parms)
            version <- rep(newversion, length(parmid))
            label <- parmid
            value <- NULL
            for (i in 1:length(parmid)){
                value <- c(value, input[[parmid[i]]])
            }
            aparms <- data.frame(version, label, value)
            parms <- rbind(parms, aparms)
            write_csv(parms, filename)
        })
        tabPanel("Plot",
                 sidebarPanel(
                     width = 2,
                     checkboxInput("showrow","Show row",value = FALSE),
                     textInput("pos1", "Position above", value = ""),
                     textInput("pos3", "Position below", value = ""),
                     textInput("xscale", "X From,To,Step,Tick", value = ""),
                     textInput("yscale", "Y From,To,Step,Tick", value = ""),
                     textInput("xlimit","Limit",value = "-9,-3,3,9"),
                     textInput("xcolor","Color",value = "red3,orange,green3,violet,blue3"),
                     textInput("xparty","Party",value = "1_Solid R,2_Leans R,3_Toss-Up,4_Leans D,5_Solid D"),
                     selectInput("noparty", "No-party",
                                 choices = c("Count as Dem","Split 50/50","Split by Ratio","Count as Rep"),
                                 selected = "Split by Ratio",
                                 multiple = FALSE),
                     textInput("vlimit","Vote Limit (1000s)",value = "1,10,100,1000"),
                     textInput("vshape","Vote Shape",value = "1,10,16,17,15"),
                     textInput("vdesc","Vote Desc",value = "< 1k,>=    1k,>=   10k,>=  100k,>= 1000k")
                 ),
        )
        observe({
            eventid <- "Plot"
            loadid <- "plotload"
            parmid <- c("showrow", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "noparty",
                        "vlimit", "vshape", "vdesc")
            parmup <- c("checkbox", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "select",
                        "vlimit", "vshape", "vdesc")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                loadversion <- input[[loadid]]
                pp <- parms[parms$version == loadversion,]
                for (i in 1:length(parmid)){
                    if (parmup[i] == "numeric"){
                        updateNumericInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "select"){
                        updateSelectInput(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "checkbox"){
                        updateCheckboxInput(session, parmid[i], value = as.logical(pp$value[pp$label == parmid[i]]))
                    }
                    else if (parmup[i] == "radio"){
                        updateRadioButtons(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else{
                        updateTextInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                }
            }
        })
        observeEvent(input$plotsave,{
            eventid <- "Plot"
            parmid <- c("showrow", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "noparty",
                        "vlimit", "vshape", "vdesc")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                newversion <- max(parms$version) + 1
            }
            else{
                parms <- data.frame(version=integer(),
                                    label=character(),
                                    value=character(),
                                    stringsAsFactors = FALSE)
                newversion <- 1
            }
            nr <- NROW(parms)
            version <- rep(newversion, length(parmid))
            label <- parmid
            value <- NULL
            for (i in 1:length(parmid)){
                value <- c(value, input[[parmid[i]]])
            }
            aparms <- data.frame(version, label, value)
            parms <- rbind(parms, aparms)
            write_csv(parms, filename)
        })
        observe({
            eventid <- "Map"
            loadid <- "mapload"
            parmid <- c("minpop", "longoff", "skipcity",
                        "showcity", "maplimitset", "maplimits",
                        "mapyear","mapvar","mapcolors")
            parmup <- c("numeric", "numeric", "skipcity",
                        "showcity", "select", "maplimits",
                        "numeric","select","mapcolors")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                loadversion <- input[[loadid]]
                pp <- parms[parms$version == loadversion,]
                for (i in 1:length(parmid)){
                    if (parmup[i] == "numeric"){
                        updateNumericInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "select"){
                        updateSelectInput(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "checkbox"){
                        updateCheckboxInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "radio"){
                        updateRadioButtons(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else{
                        updateTextInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                }
            }
        })
        observe({
            cat(file=stderr(), paste0("v3: ",input$state2," ",input$tabs,"\n"))
        })
    }
)
