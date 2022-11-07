library(httr); library(stringr); library(jsonlite); library(lubridate); library(rscopus); library(dplyr); library(rlist); library(tidyverse); library(stringdist)


load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\WOSdf.Rdata")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\mathscidata8.Rdata")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\pubsJournaldfAll.Rdata")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\mini_author_dfAll.RData")
pubsJournaldf <- as_tibble(pubsJournaldf)
mathscidata <- mathscidata%>%separate(PAGES, sep = "--", c("First_page", "Last_page"), remove = F)
WOSdf <- WOSdf%>%separate(pages, sep = "-", c("First_page", "Last_page"), remove = F)
pubsJournaldf <- pubsJournaldf%>%separate(`prism:pageRange`, sep = "-", c("First_page", "Last_page"), remove = F)
names(WOSdf) <- paste0("W.", names(WOSdf)) 
names(mathscidata) <- paste0("M.", names(mathscidata))
names(pubsJournaldf) <- paste0("S.", names(pubsJournaldf))

pubsJournaldf <- pubsJournaldf %>% rename(s.volume=`S.prism:volume`, s.number=`S.prism:issueIdentifier`, s.FIRST_PAGE=S.First_page)
pubsJournaldf <- pubsJournaldf %>% rename_with(toupper)
#mathscidata <- mathscidata %>% rename(volume=M.VOLUME, number = M.NUMBER, first_page=M.First_page)
mathscidata <- mathscidata %>% rename_with(toupper)
#WOSdf <- WOSdf %>% rename(volume=W.volume, number=W.number, first_page=W.First_page)
WOSdf <- WOSdf %>% rename_with(toupper)

WOSdf$W.FIRST_PAGE <- str_replace(WOSdf$W.FIRST_PAGE, pattern = "\\+" , replacement = "") #\&
WOSdf$W.LAST_PAGE <- str_replace(WOSdf$W.LAST_PAGE, "\\\\&", NA_character_)#





mathscidata$M.LAST_PAGE <- str_replace(string = mathscidata$M.LAST_PAGE, pattern = "\\D+.*$", replacement = "")
#c <- str_replace(string = "460 (1971)", pattern = "\\D+.*$", replacement = "")
WOSdf[c("W.VOLUME", "W.NUMBER", "W.FIRST_PAGE", "W.LAST_PAGE", "W.YEAR")] <- sapply(WOSdf[c("W.VOLUME", "W.NUMBER", "W.FIRST_PAGE", "W.LAST_PAGE", "W.YEAR")],  as.numeric)
mathscidata[c("M.VOLUME", "M.NUMBER", "M.FIRST_PAGE", "M.LAST_PAGE")] <- sapply(mathscidata[c("M.VOLUME", "M.NUMBER", "M.FIRST_PAGE", "M.LAST_PAGE")],  as.numeric)
pubsJournaldf[c("S.VOLUME", "S.NUMBER", "S.FIRST_PAGE", "S.LAST_PAGE")] <- sapply(pubsJournaldf[c("S.VOLUME", "S.NUMBER", "S.FIRST_PAGE", "S.LAST_PAGE")],  as.numeric)

pubsJournaldf <- pubsJournaldf %>% mutate(S.YEAR=year(ymd(`S.PRISM:COVERDATE`))) %>% relocate(S.YEAR, .before = `S.PRISM:COVERDATE`)
pubsJournaldf <- pubsJournaldf %>% rename(S.TITLE = `S.DC:TITLE`)

#mathscidata$M.VOLUME <- as.character(mathscidata$M.VOLUME)
#mathscidata$M.NUMBER <- as.character(mathscidata$M.NUMBER)

pubsJournaldf$S.TITLE <- trimws(pubsJournaldf$S.TITLE)
pubsJournaldf$S.TITLE <- str_replace_all(pubsJournaldf$S.TITLE, pattern = "[^a-zA-Z0-9\\s]", replacement = "")
pubsJournaldf$S.TITLE <- tolower(pubsJournaldf$S.TITLE)

mini_author_df[6:7] <- sapply(mini_author_df[6:7], trimws)
mini_author_df[6:7] <- sapply(mini_author_df[6:7], function(t) str_replace_all(t, pattern = "[^a-zA-Z0-9\\s]", replacement = ""))
mini_author_df[6:7] <- sapply(mini_author_df[6:7], tolower)
mini_author_df$name <- paste(mini_author_df[[7]], mini_author_df[[6]], sep = " ")
# mathscidata[3:7] <- sapply(mathscidata[3:7], trimws)
# mathscidata[3:7] <- sapply(mathscidata[3:7], function(t) str_replace_all(t, pattern = "[^a-zA-Z0-9\\s]", replacement = ""))
# mathscidata[3:7] <- sapply(mathscidata[3:7], tolower)

mathscidata$M.TITLE <- str_replace_all(mathscidata$M.TITLE, "\\n\\s*", " ")
mathscidata$M.TITLE <- trimws(mathscidata$M.TITLE)
mathscidata$M.TITLE <- str_replace_all(mathscidata$M.TITLE, pattern = "[^a-zA-Z0-9\\s]", replacement = "")
mathscidata$M.TITLE <- tolower(mathscidata$M.TITLE)
mathscidata[3:14] <- sapply(mathscidata[3:14], trimws)
mathscidata[3:14] <- sapply(mathscidata[3:14], function(t) str_replace_all(t, pattern = "[^a-zA-Z0-9\\s]", replacement = ""))
mathscidata[3:14] <- sapply(mathscidata[3:14], tolower)


WOSdf$W.TITLE <- str_replace_all(WOSdf$W.TITLE, "\\n\\s*", " ")
WOSdf$W.TITLE <- trimws(WOSdf$W.TITLE)
WOSdf$W.TITLE <- str_replace_all(WOSdf$W.TITLE, pattern = "[^a-zA-Z0-9\\s]", replacement = "")
WOSdf$W.TITLE <- tolower(WOSdf$W.TITLE)
WOSdf[2:13] <- sapply(WOSdf[2:13], trimws)
WOSdf[2:13] <- sapply(WOSdf[2:13], function(t) str_replace_all(t, pattern = "[^a-zA-Z0-9\\s]", replacement = ""))
WOSdf[2:13] <- sapply(WOSdf[2:13], tolower)


author_number_M <- NULL
author_number_W <- NULL
for (i in 1:nrow(mathscidata)) {
      m <- sum(!is.na(c(mathscidata[i, paste0("M.AUTHOR",seq(1,11))])))
      #w <- sum(!is.na(c(data_join_try_d[i, paste0("W.AUTHOR",seq(1,11))]))) 
      author_number_M <- append(author_number_M, m)
      #author_number_W <- append(author_number_W, w)
}

for (i in 1:nrow(WOSdf)) {
      #m <- sum(!is.na(c(mathscidata[i, paste0("M.AUTHOR",seq(1,11))])))
      w <- sum(!is.na(c(WOSdf[i, paste0("W.AUTHOR",seq(1,11))]))) 
      #author_number_M <- append(author_number_M, m)
      author_number_W <- append(author_number_W, w)
}

mathscidata$M.AUTHORS.COUNT <- author_number_M
WOSdf$W.AUTHORS.COUNT <- author_number_W
pubsJournaldf <- rename(pubsJournaldf, S.AUTHORS.COUNT="S.AUTHOR-COUNT")
pubsJournaldf$S.AUTHORS.COUNT <- as.integer(pubsJournaldf$S.AUTHORS.COUNT$`@total`)


mathscidata <- mathscidata %>% filter(!is.na(M.MRNUMBER))
pubsJournaldf <- pubsJournaldf %>% filter(!is.na(S.EID))
WOSdf <- WOSdf %>% filter(!is.na(`W.UNIQUE-ID`))

# save(WOSdf, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\WOSdfcomp.Rdata")
# save(mathscidata, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\mathscidatacomp.Rdata")
# save(pubsJournaldf, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\pubsJournaldfAllcomp.Rdata")
# save(mini_author_df, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\mini_author_dfAllcomp.RData")
# 
# 
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\WOSdfcomp.Rdata")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\mathscidatacomp.Rdata")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\pubsJournaldfAllcomp.Rdata")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\mini_author_dfAllcomp.RData")


#w <- WOSdf %>% select(W.TITLE, W.FIRST_PAGE, W.LAST_PAGE, W.VOLUME, W.NUMBER, W.YEAR)
#m <- mathscidata %>% select(M.TITLE, M.FIRST_PAGE, M.LAST_PAGE, M.VOLUME, M.NUMBER, M.YEAR)
#s <- pubsJournaldf %>% select(S.TITLE, S.FIRST_PAGE, S.LAST_PAGE, S.VOLUME, S.NUMBER, S.YEAR)
### finding the oldest pub year of WOSdf
min_year <- WOSdf%>%select(W.YEAR)%>% unlist()%>%as.numeric()%>%min() 
### oles oi vaseis apo to 1970, gia na einai koines me WOS
#pubsJournaldf <- pubsJournaldf %>% filter(`S.PRISM:COVERDATE`>=min_year)
#mathscidata <- mathscidata%>%filter(M.YEAR>=min_year)

###  number of dataframes pubs ###
nrow(pubsJournaldf); nrow(mathscidata); nrow(WOSdf)


sapply(c("S.TITLE" ,"S.PRISM:PAGERANGE", "S.VOLUME", "S.YEAR", "S.NUMBER", "S.PRISM:ISSN", "S.FIRST_PAGE", "S.LAST_PAGE"), function(t) sum(is.na(pubsJournaldf[,t])))
sapply(c("W.TITLE" ,"W.PAGES", "W.VOLUME", "W.YEAR", "W.NUMBER", "W.ISSN", "W.FIRST_PAGE", "W.LAST_PAGE"), function(t) sum(is.na(WOSdf[,t])))
sapply(c("M.TITLE" ,"M.PAGES", "M.VOLUME", "M.YEAR", "M.NUMBER", "M.ISSN", "M.FIRST_PAGE", "M.LAST_PAGE"), function(t) sum(is.na(mathscidata[,t])))


################################ pame pali ###############################################

#### 3 pedia outter join --- volume, number, first page ####

### elegxos kai stis 3 vaseis gia diploeggrafes tous kai removing###



#sDupls <- pubsJournaldf[which(duplicated(pubsJournaldf[,c("S.VOLUME", "S.NUMBER", "S.FIRST_PAGE")])), c("S.VOLUME", "S.NUMBER", "S.FIRST_PAGE")]%>%drop_na()%>%distinct() ### vriskw ta duplicated, kai apo auta exw ena dataframe me mono ta 3 pedia suzeukshw, opote afairw ta na, kai krataw ta unique 48 duplicated  sundiasmoi sto scopus
sDupls <- pubsJournaldf %>% group_by(S.VOLUME, S.NUMBER, S.FIRST_PAGE) %>% drop_na(S.VOLUME, S.NUMBER, S.FIRST_PAGE) %>% 
      filter(n()>1)#%>% distinct(S.VOLUME, S.NUMBER, S.FIRST_PAGE)  ## xwris to distinct tha eixa ola ta duplicated, twta mono ta monadika apo auta


## gia thn wra tha ta afairesoume ola ta duplicated apo to scopus, exw afairesei ola ta non_distinct

pubsJournaldfU <- pubsJournaldf %>% anti_join(sDupls) ## an ta afairousa eksarxhs ta s2dupls tou 2ou vhmatos gia dokimh ths orthothtas tou sum(step2SMW,SM,SW,S) meta to 2o vhma


wDupls <- WOSdf %>% group_by(W.VOLUME, W.NUMBER, W.FIRST_PAGE) %>% drop_na(W.VOLUME, W.NUMBER, W.FIRST_PAGE) %>% 
      filter(n()>1)

WOSdfU <-  WOSdf %>% anti_join(wDupls)


#mathscidata[which(duplicated(mathscidata[,c("M.VOLUME", "M.NUMBER", "M.FIRST_PAGE")])), c("M.VOLUME", "M.NUMBER", "M.FIRST_PAGE")]%>%drop_na()%>%distinct() ## 

mDupls <- mathscidata %>% group_by(M.VOLUME, M.NUMBER, M.FIRST_PAGE) %>% drop_na(M.VOLUME, M.NUMBER, M.FIRST_PAGE) %>% 
      filter(n()>1)

mathscidataU <- mathscidata %>% anti_join(mDupls)



##### xwris duplicated se kathe vash, kanoume full outter join me ta tria pedia #####

vif_oj1 <- pubsJournaldfU %>% full_join(mathscidataU, by = c("S.VOLUME"="M.VOLUME", "S.NUMBER"="M.NUMBER", "S.FIRST_PAGE"="M.FIRST_PAGE"), na_matches = "never", keep=T)%>%
      full_join(., WOSdfU, by =  c("S.VOLUME"="W.VOLUME", "S.NUMBER"="W.NUMBER", "S.FIRST_PAGE"="W.FIRST_PAGE"), na_matches = "never", keep=T) #%>% 
#full_join(., WOSdfU, by = c("M.VOLUME"="W.VOLUME", "M.NUMBER"="W.NUMBER", "M.FIRST_PAGE"="W.FIRST_PAGE"), na_matches = "never", keep=T)
#sum(duplicated(vif_oj$S.EID))
#purrr::reduce(list(pubsJournaldfU,mathscidataU,WOSdfU), dplyr::left_join, by = c('Flag')


vif_oj1_check <- vif_oj1[,c("S.VOLUME","S.NUMBER", "S.FIRST_PAGE", "S.LAST_PAGE","S.EID", "M.VOLUME","M.NUMBER", "M.FIRST_PAGE", "M.LAST_PAGE", "M.MRNUMBER",
                            "W.VOLUME","W.NUMBER", "W.FIRST_PAGE", "W.LAST_PAGE", "W.UNIQUE-ID")] ### pinakas gia eukolo check tou outter join
#which(is.na(vif_oj_check[,"S.EID"]) & is.na(vif_oj_check[,"M.MRNUMBER"]) & is.na(vif_oj_check[,"W.UNIQUE-ID"])) 


#f3VIFjoinSMW <- f3VIFjoinSMW[!(f3VIFjoinSMW$S.EID %in% unique(f3VIFjoinSMW$S.EID[duplicated(f3VIFjoinSMW$S.EID)])), ]
### tha mporousame na to kanoume eksarxhs sto vif_oj,  ###
sapply(list(pubsJournaldf$S.EID, mathscidata$M.MRNUMBER, WOSdf$`W.UNIQUE-ID`), function(t) sum(duplicated(t))) ### katarxas elegxoume an uparxoun duplicated sta arxika dataframes, ara o logos uparkseis toys sto vif einai logo koinwn 3 pediwn


#### twra tha doume posa ginane join kai stis 3, posa se 2 kai posa mona tous gia 3 fields ###

f3VIFjoinSMW <- vif_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))
f3VIFjoinSM <- vif_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER),all_vars(!is.na(.)))%>%filter(is.na(`W.UNIQUE-ID`))
f3VIFjoinSW <- vif_oj1 %>% filter_at(vars(S.EID, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(M.MRNUMBER))
f3VIFjoinMW <- vif_oj1 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(S.EID))
f3VIFjoinS <- vif_oj1 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))%>%filter(!is.na(S.EID))
f3VIFjoinW <- vif_oj1 %>% filter_at(vars(M.MRNUMBER, S.EID),all_vars(is.na(.)))%>%filter(!is.na(`W.UNIQUE-ID`))
f3VIFjoinM <- vif_oj1 %>% filter_at(vars(`W.UNIQUE-ID`, S.EID),all_vars(is.na(.)))%>%filter(!is.na(M.MRNUMBER))
f3VIFjoin0 <- vif_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))

nrow(f3VIFjoinSMW)+nrow(f3VIFjoinSM)+nrow(f3VIFjoinSW)+nrow(f3VIFjoinMW)+nrow(f3VIFjoinS)+nrow(f3VIFjoinW)+nrow(f3VIFjoinM)+nrow(f3VIFjoin0) ### epiveveaiswsh oti apoteloun oti diamerizoun th vif


vif_oj2 <- pubsJournaldfU %>% full_join(mathscidataU, by = c("S.VOLUME"="M.VOLUME", "S.NUMBER"="M.NUMBER", "S.FIRST_PAGE"="M.FIRST_PAGE"), na_matches = "never", keep=T)%>%
      full_join(., WOSdfU, by =  c("M.VOLUME"="W.VOLUME", "M.NUMBER"="W.NUMBER", "M.FIRST_PAGE"="W.FIRST_PAGE"), na_matches = "never", keep=T)

f3VIFjoinSMWw <- vif_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))
f3VIFjoinSMm <- vif_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER),all_vars(!is.na(.)))%>%filter(is.na(`W.UNIQUE-ID`))
f3VIFjoinSWw <- vif_oj2%>% filter_at(vars(S.EID, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(M.MRNUMBER))
f3VIFjoinMWw <- vif_oj2 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(S.EID))
f3VIFjoinSs <- vif_oj2 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))%>%filter(!is.na(S.EID))
f3VIFjoinWw <- vif_oj2 %>% filter_at(vars(M.MRNUMBER, S.EID),all_vars(is.na(.)))%>%filter(!is.na(`W.UNIQUE-ID`))
f3VIFjoinMm <- vif_oj2 %>% filter_at(vars(`W.UNIQUE-ID`, S.EID),all_vars(is.na(.)))%>%filter(!is.na(M.MRNUMBER))
f3VIFjoin00 <- vif_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))
nrow(intersect(f3VIFjoinSs, f3VIFjoinS)) ## idia me to prwto vif, dld sto 1o ena peros xrhsimpopoihthike gia to sm
nrow(intersect(f3VIFjoinMm, f3VIFjoinM)) ## idia me to deutero vif

#f3VIFjoinWww <- f3VIFjoinW %>% filter(f3VIFjoinW %in% f3VIFjoinMW$`W.UNIQUE-ID`) & !(`W.UNIQUE-ID` %in% f3VIFjoinMW$`W.UNIQUE-ID`))
c <- f3VIFjoinW %>% filter(!(`W.UNIQUE-ID` %in% f3VIFjoinMWw$`W.UNIQUE-ID`))
c2 <- f3VIFjoinWw %>% filter(!(`W.UNIQUE-ID` %in% f3VIFjoinSW$`W.UNIQUE-ID`))

f3VIFjoinMW <- f3VIFjoinMWw
f3VIFjoinM <- f3VIFjoinMm
f3VIFjoinW <- c

### ara to oliko dataframe einai rbind oloi oi sundiasmoi, kai sta mona tha pairnw ta mikrotera gia s kai m, enw gia w ayta pou emeinan sto ena kai parallhla den uparxoun sto sunduasmo tou allou (sw h mw) ### 
vif_oj <- f3VIFjoinSMW %>% bind_rows(f3VIFjoinSM, f3VIFjoinSW, f3VIFjoinMW, f3VIFjoinS, f3VIFjoinM, f3VIFjoinW)

sum(sapply(list(f3VIFjoinSMW, f3VIFjoinSM, f3VIFjoinSW, f3VIFjoinS), nrow))
sum(sapply(list(f3VIFjoinSMW, f3VIFjoinSW, f3VIFjoinMW, f3VIFjoinW), nrow))
sum(sapply(list(f3VIFjoinSMW, f3VIFjoinSM, f3VIFjoinMW, f3VIFjoinM), nrow))

step1SMW <- f3VIFjoinSMW
step1SM <- f3VIFjoinSM; step1SW <- f3VIFjoinSW; step1MW <- f3VIFjoinMW
step1S <- f3VIFjoinS; step1M <- f3VIFjoinM; step1W <- f3VIFjoinW

sum(sapply(list(step1SMW, step1SM, step1SW, step1S, sDupls), nrow))
sum(sapply(list(step1SMW, step1SM, step1MW, step1M, mDupls), nrow))
sum(sapply(list(step1SMW, step1SW, step1MW, step1W, wDupls), nrow))




### afairpume apo tis vaseis tis dhmosieuseis pou perilamvanonti sto full join f3VIFjoinsmw ###

#scopusStep2 <- pubsJournaldf %>% anti_join(step1SMW, by = "S.EID")
scopusStep2 <- pubsJournaldf%>%filter(!(S.EID %in% step1SMW$S.EID))
mathstep2 <- mathscidata%>%filter(!(M.MRNUMBER %in% step1SMW$M.MRNUMBER))
wosstep2 <- WOSdf%>%filter(!(`W.UNIQUE-ID` %in% step1SMW$`W.UNIQUE-ID`))

### epishs afairw apo autes ta duplicated gia to neo krithrio pou einai ta pedia -- volume issue kai last page ---###

s2Dupls <- scopusStep2 %>% group_by(S.VOLUME, S.NUMBER, S.LAST_PAGE) %>% drop_na(S.VOLUME, S.NUMBER, S.LAST_PAGE) %>%
      filter(n()>1)
scopusStep2U <- scopusStep2 %>% anti_join(s2Dupls) #%>% anti_join(sDupls)

m2Dupls <- mathstep2 %>% group_by(M.VOLUME, M.NUMBER, M.LAST_PAGE) %>% drop_na(M.VOLUME, M.NUMBER, M.LAST_PAGE) %>%
      filter(n()>1)
mathstep2U <- mathstep2 %>% anti_join(m2Dupls) #%>% anti_join(mDupls)

w2Dupls <- wosstep2 %>% group_by(W.VOLUME, W.NUMBER, W.LAST_PAGE) %>% drop_na(W.VOLUME, W.NUMBER, W.LAST_PAGE) %>%
      filter(n()>1)
wosstep2U <- wosstep2 %>% anti_join(w2Dupls) #%>% anti_join(wDupls)
#### join ta upoloipa apo to f3VIFjoinSMW me krithrio ta pedia volume issue kai last page ###


vil_oj1 <- scopusStep2U %>% full_join(mathstep2U, by = c("S.VOLUME"="M.VOLUME", "S.NUMBER"="M.NUMBER", "S.LAST_PAGE"="M.LAST_PAGE"), na_matches = "never", keep=T)%>%
      full_join(wosstep2U, by =  c("S.VOLUME"="W.VOLUME", "S.NUMBER"="W.NUMBER", "S.LAST_PAGE"="W.LAST_PAGE"), na_matches = "never", keep=T)


vil_oj1_check <- vil_oj1[,c("S.VOLUME","S.NUMBER", "S.FIRST_PAGE", "S.LAST_PAGE","S.EID", "M.VOLUME","M.NUMBER", "M.FIRST_PAGE", "M.LAST_PAGE", "M.MRNUMBER",
                            "W.VOLUME","W.NUMBER", "W.FIRST_PAGE", "W.LAST_PAGE", "W.UNIQUE-ID")]
#which(is.na(vf_oj_check[,"S.EID"]) & is.na(vf_oj_check[,"M.MRNUMBER"]) & is.na(vf_oj_check[,"W.UNIQUE-ID"]))


#### twra tha doume posa ginane join kai stis 3, posa se 2 kai posa mona tous ###



f3VILjoinSMW <- vil_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))
f3VILjoinSM <- vil_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER),all_vars(!is.na(.)))%>%filter(is.na(`W.UNIQUE-ID`))
f3VILjoinSW <- vil_oj1 %>% filter_at(vars(S.EID, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(M.MRNUMBER))
f3VILjoinMW <- vil_oj1 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(S.EID))
f3VILjoinS <- vil_oj1 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))%>%filter(!is.na(S.EID))
f3VILjoinW <- vil_oj1 %>% filter_at(vars(M.MRNUMBER, S.EID),all_vars(is.na(.)))%>%filter(!is.na(`W.UNIQUE-ID`))
f3VILjoinM <- vil_oj1 %>% filter_at(vars(`W.UNIQUE-ID`, S.EID),all_vars(is.na(.)))%>%filter(!is.na(M.MRNUMBER))
f3VILjoin0 <- vil_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))

nrow(f3VILjoinSMW)+nrow(f3VILjoinSM)+nrow(f3VILjoinSW)+nrow(f3VILjoinMW)+nrow(f3VILjoinS)+nrow(f3VILjoinW)+nrow(f3VILjoinM)+nrow(f3VILjoin0)


vil_oj2 <- scopusStep2U %>% full_join(mathstep2U, by = c("S.VOLUME"="M.VOLUME", "S.NUMBER"="M.NUMBER", "S.LAST_PAGE"="M.LAST_PAGE"), na_matches = "never", keep=T)%>%
      full_join(wosstep2U, by =  c("M.VOLUME"="W.VOLUME", "M.NUMBER"="W.NUMBER", "M.LAST_PAGE"="W.LAST_PAGE"), na_matches = "never", keep=T)


f3VILjoinSMWw <- vil_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))
f3VILjoinSMm <- vil_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER),all_vars(!is.na(.)))%>%filter(is.na(`W.UNIQUE-ID`))
f3VILjoinSWw <- vil_oj2 %>% filter_at(vars(S.EID, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(M.MRNUMBER))
f3VILjoinMWw <- vil_oj2 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(S.EID))
f3VILjoinSs <- vil_oj2 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))%>%filter(!is.na(S.EID))
f3VILjoinWw <- vil_oj2 %>% filter_at(vars(M.MRNUMBER, S.EID),all_vars(is.na(.)))%>%filter(!is.na(`W.UNIQUE-ID`))
f3VILjoinMm <- vil_oj2 %>% filter_at(vars(`W.UNIQUE-ID`, S.EID),all_vars(is.na(.)))%>%filter(!is.na(M.MRNUMBER))
f3VILjoin00 <- vil_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))
c <- f3VILjoinW %>% filter(!(`W.UNIQUE-ID` %in% f3VILjoinMWw$`W.UNIQUE-ID`)) #& !(`W.UNIQUE-ID` %in% f3joinMW$`W.UNIQUE-ID`))
c2 <- f3VILjoinWw %>% filter(!(`W.UNIQUE-ID` %in% f3VILjoinSW$`W.UNIQUE-ID`))

f3VILjoinMW <- f3VILjoinMWw
f3VILjoinM <- f3VILjoinMm
f3VILjoinW <- c

### ara to oliko dataframe einai rbind oloi oi sundiasmoi, kai sta mona tha pairnw ta mikrotera gia s kai m, enw gia w ayta pou emeinan sto ena kai parallhla den uparxoun sto sunduasmo tou allou (sw h mw) ### 
vil_oj <- f3VILjoinSMW %>% bind_rows(f3VILjoinSM, f3VILjoinSW, f3VILjoinMW, f3VILjoinS, f3VILjoinM, f3VILjoinW)
vil_oj_check <- vil_oj[,c("S.VOLUME","S.NUMBER", "S.FIRST_PAGE", "S.LAST_PAGE","S.EID", "M.VOLUME","M.NUMBER", "M.FIRST_PAGE", "M.LAST_PAGE", "M.MRNUMBER",
                          "W.VOLUME","W.NUMBER", "W.FIRST_PAGE", "W.LAST_PAGE", "W.UNIQUE-ID")]

step2SMW <- bind_rows(step1SMW, f3VILjoinSMW)
#step2SM <- bind_rows(step1SM, f3VILjoinSM) %>% distinct(S.EID, M.MRNUMBER, .keep_all = T) %>% anti_join(step2SMW, by = "S.EID") %>% anti_join(step2SMW, by = "M.MRNUMBER")
step2SM <- f3VILjoinSM %>% anti_join(step1SW, by = "S.EID") %>% anti_join(step1MW, by = "M.MRNUMBER") %>% anti_join(step1SM, by = "S.EID") %>% anti_join(step1SM, by = "M.MRNUMBER") %>% bind_rows(step1SM) %>% anti_join(step2SMW, by = "S.EID") %>% anti_join(step2SMW, by = "M.MRNUMBER")
### lathois to join metaksy tou sm tou 1ou step kai tou f3VILjoinSM sto 2o krithrio, kathws kai sm apo step1 pou phge se smw sto 20, opote mporei kapoio s na enwthike me allo m sto smw kai antisrofa ###
# s1 <- bind_rows(step1SM, f3VILjoinSM) %>% distinct(S.EID,.keep_all = T)
# s2 <- bind_rows(step1SM, f3VILjoinSM) %>% distinct(M.MRNUMBER,.keep_all = T)
# s12 <- bind_rows(step1SM, f3VILjoinSM) %>% distinct(S.EID, M.MRNUMBER, .keep_all = T)
# step2falseSM1 <- s1 %>% anti_join(s12, by = "S.EID") ### etsi pairnoume tis periptwseis opou thn idia dhmosieush tou mathsci thn exoume kanei join se diaforetikes tou scopus
# step2falseSM2 <- s2 %>% anti_join(s12, by = "M.MRNUMBER")
# 
# st <- f3VILjoinSM %>% anti_join(step1SM, by = "S.EID") %>% anti_join(step1SM, by = "M.MRNUMBER") %>% bind_rows(step1SM)
# st1 <- st %>% anti_join(step2SMW, by = "S.EID")
# st2 <- st %>% anti_join(step2SMW, by = "M.MRNUMBER")
# st12 <- st %>% anti_join(step2SMW, by = c("S.EID", "M.MRNUMBER"))
# step2falseSMtoSMW1 <- st1 %>% anti_join(st12, by = "S.EID")
# step2falseSMtoSMW1 <- st1 %>% anti_join(st12, by = "M.MRNUMBER")

####################################
step2SW <- f3VILjoinSW %>% anti_join(step1SM, by = "S.EID") %>% anti_join(step1MW, by = "W.UNIQUE-ID") %>% anti_join(step1SW, by = "S.EID") %>% anti_join(step1SW, by = "W.UNIQUE-ID") %>% bind_rows(step1SW) %>% anti_join(step2SMW, by = "S.EID") %>% anti_join(step2SMW, by = "W.UNIQUE-ID")
##### lathoi sto join tou SW ######

#sl1 <- f3VILjoinSW %>% filter(S.EID==intersect(f3VILjoinSW$S.EID, step1SM$S.EID))
sl2 <- f3VILjoinSW %>% filter(`W.UNIQUE-ID`== intersect(f3VILjoinSW$`W.UNIQUE-ID`, step1MW$`W.UNIQUE-ID`))
false1 <- sl2


s1 <- f3VILjoinSW %>% anti_join(step1SW, by = "S.EID")
s2 <- f3VILjoinSW %>% anti_join(step1SW, by = "W.UNIQUE-ID")
s12 <- f3VILjoinSW %>% anti_join(step1SW, by = c("W.UNIQUE-ID", "S.EID"))
step2falseSW1 <- s12 %>% anti_join(s1, by = "S.EID") ## auth h dhmosieush tou scopus vrisketai sta enapomeinanta tou f3VILjoinSW afairwntas to step1sw me diplo krithrio alla oxi sta enapomeinanta tou me to S ws krithrio, ara auto to S tou f3viljoinsw perilamvanetai sto step1sw alla me diaforetiko w join
step2falseSW2 <- s12 %>% anti_join(s2, by = "W.UNIQUE-ID")

st <- step1SW 
st1 <- st %>% anti_join(step2SMW, by = "S.EID") ### afairw apo to vhma 1 MW auta toy step 2 SMW pou exoun idio w, alla mesa mporei na uparxoun dhmpsieuseis pou den ginane idio join me to M sto step2 SMW kai to step1 MW
st2 <- st %>% anti_join(step2SMW, by = "W.UNIQUE-ID")
st12 <- st %>% anti_join(step2SMW, by = c("S.EID", "W.UNIQUE-ID")) ## afairw apo to vhma 1 MW auta pou einai akrivws idia join tou step 2 SMW
step2falseMWtoSMW1 <- st12 %>% anti_join(st1, by = "S.EID") ### vriskei to s.eid pou, sto prwto step SW to ekane join se allo tou W kai sto 2o step SMW se allo tou W
step2falseMWtoSMW2 <- st12 %>% anti_join(st2, by = "W.UNIQUE-ID")
#############################

step2S <- bind_rows(step1S, f3VILjoinS) %>% distinct(S.EID, .keep_all = T) %>% anti_join(step2SMW, by = "S.EID") %>% anti_join(step2SM, by = "S.EID") %>% anti_join(step2SW, by = "S.EID") %>% anti_join(step1SM, by = "S.EID") %>% anti_join(step1SW, by = "S.EID")


step2MW <-f3VILjoinMW %>% anti_join(step1SW, by = "W.UNIQUE-ID") %>% anti_join(step1SM, by = "M.MRNUMBER") %>% anti_join(step1MW, by = "M.MRNUMBER") %>% anti_join(step1MW, by = "W.UNIQUE-ID") %>% bind_rows(step1MW) %>% anti_join(step2SMW, by = "M.MRNUMBER") %>% anti_join(step2SMW, by = "W.UNIQUE-ID") ### anti-join kai me tis 2 sthles tou step2MW giati mporei to join sto step1MW na einai diaforetiko apo auto pou ennwnei metaksu MW sto step2SMW
### lathois to join metaksy tou mw tou 1ou step kai tou f3VILjoinSW sto 2o krithrio, kathws kai sm apo step1 pou phge se smw sto 2o, opote mporei kapoio m na enwthike me allo w sto smw kai antisrofa
s1 <- f3VILjoinMW %>% anti_join(step1MW, by = "M.MRNUMBER")
s2 <- f3VILjoinMW %>% anti_join(step1MW, by = "W.UNIQUE-ID")
s12 <- f3VILjoinMW %>% anti_join(step1MW, by = c("W.UNIQUE-ID", "M.MRNUMBER"))
step2falseSW1 <- s12 %>% anti_join(s1, by = "M.MRNUMBER")
step2falseSW2 <- s12 %>% anti_join(s2, by = "W.UNIQUE-ID")

st <- step1MW 
st1 <- st %>% anti_join(step2SMW, by = "W.UNIQUE-ID") ### afairw apo to vhma 1 MW auta toy step 2 SMW pou exoun idio w, alla mesa mporei na uparxoun dhmpsieuseis pou den ginane idio join me to M sto step2 SMW kai to step1 MW
st2 <- st %>% anti_join(step2SMW, by = "M.MRNUMBER")
st12 <- st %>% anti_join(step2SMW, by = c("W.UNIQUE-ID", "M.MRNUMBER")) ## afairw apo to vhma 1 MW auta pou einai akrivws idia join tou step 2 SMW
step2falseMWtoSMW1 <- st12 %>% anti_join(st1, by = "W.UNIQUE-ID") ### vriskei ta wunique pou, sto prwto step MW to ekane join se allo tou math kai sto 2o step SMW se allo tou math
step2falseMWtoSMW2 <- st12 %>% anti_join(st2, by = "M.MRNUMBER")

false2 <- step2falseMWtoSMW1

################################################ 


step2M <- bind_rows(step1M, f3VILjoinM) %>% distinct(M.MRNUMBER, .keep_all = T) %>% anti_join(step2SMW, by = "M.MRNUMBER") %>% anti_join(step2SM, by = "M.MRNUMBER") %>% anti_join(step2MW, by = "M.MRNUMBER") %>% anti_join(step1SM, by = "M.MRNUMBER") %>% anti_join(step1MW, by = "M.MRNUMBER")
step2W <- bind_rows(step1W, f3VILjoinW) %>% distinct(`W.UNIQUE-ID`, .keep_all = T) %>% anti_join(step2SMW, by = "W.UNIQUE-ID") %>% anti_join(step2SW, by = "W.UNIQUE-ID") %>% anti_join(step2MW, by = "W.UNIQUE-ID") %>% anti_join(step1SW, by = "W.UNIQUE-ID") %>% anti_join(step1MW, by = "W.UNIQUE-ID")


sum(sapply(list(step2SMW, step2SM, step2SW, step2S, intersect(sDupls, s2Dupls)), nrow)) ### pou einai idies grammes me to na afairousa eksarxhs sto step1 ta s2dupls mazi me ta sdupls

sum(sapply(list(step2SMW, step2SM, step2MW, step2M, intersect(mDupls, m2Dupls)), nrow))
sum(sapply(list(step2SMW, step2MW, step2SW, step2W, intersect(wDupls, w2Dupls)), nrow))
s <- bind_rows(step2SMW, step2SM, step2SW, step2S, intersect(sDupls, s2Dupls))
m <- bind_rows(step2SMW, step2SM, step2MW, step2M, intersect(mDupls, m2Dupls))
w <- bind_rows(step2SMW, step2MW, step2SW, step2W, intersect(wDupls, w2Dupls))
sum(duplicated(w$`W.UNIQUE-ID`))
sum(duplicated(s$S.EID))
sum(duplicated(m$M.MRNUMBER))

meneiektos <- pubsJournaldf %>% anti_join(s, by = "S.EID")

## "2-s2.0-0001699051" %in% f3VILjoinSW$S.EID, anhkei kai sta sdupls kai s4dupls
meneiektos$S.EID %in% sDupls$S.EID
f3VILjoinSW %>% filter(S.EID == "2-s2.0-0001699051") %>% select(`W.UNIQUE-ID`) ## auto to S egine join me W sto f3VILjoinSW
"WOS:A1981MJ88100002" %in% step1MW$`W.UNIQUE-ID` # omws auto to w pou egine join uparxei kai sto step1MW, opote to w to exoume hdh sto step1MW, enw to sw sto f3VILjoinSW telika diagrafetai me apotelesma to s na xathei
 

tiexasa <- setdiff(mathscidata$M.MRNUMBER, m$M.MRNUMBER)
#d <- w %>% filter(duplicated(w$`W.UNIQUE-ID`))
#sum(duplicated(bind_rows(step2SMW, step2MW)$`W.UNIQUE-ID`))
#sum(duplicated(step2SMW$`W.UNIQUE-ID`))
##### check pubs that were included in f3VILjoinSMW, from the f3VIFjoins except f3VIFjoinSMW, kathws oti egine inner_join sthn prohgoumenh fash, to exoume afairesei apo authn #####

sum(step2M$M.MRNUMBER %in% mDupls$M.MRNUMBER)


### afairpume apo tis vaseis tis dhmosieuseis pou perilamvanonti sto full join twn 2 prohgoumenwn vhmatwn ###


scopusStep3 <- pubsJournaldf%>%filter(!(S.EID %in% step2SMW$S.EID))
mathstep3 <- mathscidata%>%filter(!(M.MRNUMBER %in% step2SMW$M.MRNUMBER))
wosstep3 <- WOSdf%>%filter(!(`W.UNIQUE-ID` %in% step2SMW$`W.UNIQUE-ID`))


### epishs afairw apo autes ta duplicated gia to neo krithrio pou einai ta pedia -- volume first page kai last page ---###

s3Dupls <- scopusStep3 %>% group_by(S.VOLUME, S.FIRST_PAGE, S.LAST_PAGE) %>% drop_na(S.VOLUME, S.FIRST_PAGE, S.LAST_PAGE) %>%
      filter(n()>1)
scopusStep3U <- scopusStep3 %>% anti_join(s3Dupls)

m3Dupls <- mathstep3 %>% group_by(M.VOLUME, M.FIRST_PAGE, M.LAST_PAGE) %>% drop_na(M.VOLUME, M.FIRST_PAGE, M.LAST_PAGE) %>%
      filter(n()>1)
mathstep3U <- mathstep3 %>% anti_join(m3Dupls)

w3Dupls <- wosstep3 %>% group_by(W.VOLUME, W.FIRST_PAGE, W.LAST_PAGE) %>% drop_na(W.VOLUME, W.FIRST_PAGE, W.LAST_PAGE) %>%
      filter(n()>1)
wosstep3U <- wosstep3 %>% anti_join(w3Dupls)
#### join ta upoloipa apo to step2SMW me krithrio ta pedia volume first_page kai last page ###


vfl_oj1 <- scopusStep3U %>% full_join(mathstep3U, by = c("S.VOLUME"="M.VOLUME", "S.FIRST_PAGE"="M.FIRST_PAGE", "S.LAST_PAGE"="M.LAST_PAGE"), na_matches = "never", keep=T)%>%
      full_join(wosstep3U, by =  c("S.VOLUME"="W.VOLUME", "S.FIRST_PAGE"="W.FIRST_PAGE", "S.LAST_PAGE"="W.LAST_PAGE"), na_matches = "never", keep=T)


vfl_oj1_check <- vfl_oj1[,c("S.VOLUME","S.NUMBER", "S.FIRST_PAGE", "S.LAST_PAGE","S.EID", "M.VOLUME","M.NUMBER", "M.FIRST_PAGE", "M.LAST_PAGE", "M.MRNUMBER",
                            "W.VOLUME","W.NUMBER", "W.FIRST_PAGE", "W.LAST_PAGE", "W.UNIQUE-ID")]
#which(is.na(vf_oj_check[,"S.EID"]) & is.na(vf_oj_check[,"M.MRNUMBER"]) & is.na(vf_oj_check[,"W.UNIQUE-ID"]))


#### twra tha doume posa ginane join kai stis 3, posa se 2 kai posa mona tous gia 2 fields ###



f3VFLjoinSMW <- vfl_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))
f3VFLjoinSM <- vfl_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER),all_vars(!is.na(.)))%>%filter(is.na(`W.UNIQUE-ID`))
f3VFLjoinSW <- vfl_oj1 %>% filter_at(vars(S.EID, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(M.MRNUMBER))
f3VFLjoinMW <- vfl_oj1 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(S.EID))
f3VFLjoinS <- vfl_oj1 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))%>%filter(!is.na(S.EID))
f3VFLjoinW <- vfl_oj1 %>% filter_at(vars(M.MRNUMBER, S.EID),all_vars(is.na(.)))%>%filter(!is.na(`W.UNIQUE-ID`))
f3VFLjoinM <- vfl_oj1 %>% filter_at(vars(`W.UNIQUE-ID`, S.EID),all_vars(is.na(.)))%>%filter(!is.na(M.MRNUMBER))
f3VFLjoin0 <- vfl_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))

nrow(f3VFLjoinSMW)+nrow(f3VFLjoinSM)+nrow(f3VFLjoinSW)+nrow(f3VFLjoinMW)+nrow(f3VFLjoinS)+nrow(f3VFLjoinW)+nrow(f3VFLjoinM)+nrow(f3VFLjoin0)


vfl_oj2 <- scopusStep3U %>% full_join(mathstep3U, by = c("S.VOLUME"="M.VOLUME", "S.FIRST_PAGE"="M.FIRST_PAGE", "S.LAST_PAGE"="M.LAST_PAGE"), na_matches = "never", keep=T)%>%
      full_join(wosstep3U, by =  c("M.VOLUME"="W.VOLUME", "M.FIRST_PAGE"="W.FIRST_PAGE", "M.LAST_PAGE"="W.LAST_PAGE"), na_matches = "never", keep=T)


f3VFLjoinSMWw <- vfl_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))
f3VFLjoinSMm <- vfl_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER),all_vars(!is.na(.)))%>%filter(is.na(`W.UNIQUE-ID`))
f3VFLjoinSWw <- vfl_oj2 %>% filter_at(vars(S.EID, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(M.MRNUMBER))
f3VFLjoinMWw <- vfl_oj2 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(S.EID))
f3VFLjoinSs <- vfl_oj2 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))%>%filter(!is.na(S.EID))
f3VFLjoinWw <- vfl_oj2 %>% filter_at(vars(M.MRNUMBER, S.EID),all_vars(is.na(.)))%>%filter(!is.na(`W.UNIQUE-ID`))
f3VFLjoinMm <- vfl_oj2 %>% filter_at(vars(`W.UNIQUE-ID`, S.EID),all_vars(is.na(.)))%>%filter(!is.na(M.MRNUMBER))
f3VFLjoin00 <- vfl_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))
c <- f3VFLjoinW %>% filter(!(`W.UNIQUE-ID` %in% f3VFLjoinMWw$`W.UNIQUE-ID`)) #& !(`W.UNIQUE-ID` %in% f3joinMW$`W.UNIQUE-ID`))
c2 <- f3VFLjoinWw %>% filter(!(`W.UNIQUE-ID` %in% f3VFLjoinSW$`W.UNIQUE-ID`))

f3VFLjoinMW <- f3VFLjoinMWw
f3VFLjoinM <- f3VFLjoinMm
f3VFLjoinW <- c


### ara to oliko dataframe einai rbind oloi oi sundiasmoi, kai sta mona tha pairnw ta mikrotera gia s kai m, enw gia w ayta pou emeinan sto ena kai parallhla den uparxoun sto sunduasmo tou allou (sw h mw) ### 
vfl_oj <- f3VFLjoinSMW %>% bind_rows(f3VFLjoinSM, f3VFLjoinSW, f3VFLjoinMW, f3VFLjoinS, f3VFLjoinM, f3VFLjoinW)
vfl_oj_check <- vfl_oj[,c("S.VOLUME","S.NUMBER", "S.FIRST_PAGE", "S.LAST_PAGE","S.EID", "M.VOLUME","M.NUMBER", "M.FIRST_PAGE", "M.LAST_PAGE", "M.MRNUMBER",
                          "W.VOLUME","W.NUMBER", "W.FIRST_PAGE", "W.LAST_PAGE", "W.UNIQUE-ID")]

step3SMW <- bind_rows(step2SMW, f3VFLjoinSMW)
#step2SM <- bind_rows(step1SM, f3VILjoinSM) %>% distinct(S.EID, M.MRNUMBER, .keep_all = T) %>% anti_join(step2SMW, by = "S.EID") %>% anti_join(step2SMW, by = "M.MRNUMBER")
step3SM <- f3VFLjoinSM %>% anti_join(step2SW, by = "S.EID") %>% anti_join(step2MW, by = "M.MRNUMBER") %>% anti_join(step2SM, by = "S.EID") %>% anti_join(step2SM, by = "M.MRNUMBER") %>% bind_rows(step2SM) %>% anti_join(step3SMW, by = "S.EID") %>% anti_join(step3SMW, by = "M.MRNUMBER")
## lathois to join metaksy tou sm tou 2ou step kai tou f3VFLjoinSM sto 3o krithrio, kathws kai sm apo step2 pou phge se smw sto 3o, opote mporei kapoio s na enwthike me allo m sto smw kai antisrofa
s1 <- bind_rows(step2SM, f3VFLjoinSM) %>% distinct(S.EID,.keep_all = T)
s2 <- bind_rows(step2SM, f3VFLjoinSM) %>% distinct(M.MRNUMBER,.keep_all = T)
s12 <- bind_rows(step2SM, f3VFLjoinSM) %>% distinct(S.EID, M.MRNUMBER, .keep_all = T)
step3falseSM1 <- s1 %>% anti_join(s12, by = "S.EID") ### etsi pairnoume tis periptwseis opou thn idia dhmosieush tou mathsci thn exoume kanei join se diaforetikes tou scopus
step3falseSM2 <- s2 %>% anti_join(s12, by = "M.MRNUMBER")

st <- step2SM
st1 <- st %>% anti_join(step3SMW, by = "S.EID")
st2 <- st %>% anti_join(step3SMW, by = "M.MRNUMBER")
st12 <- st %>% anti_join(step3SMW, by = c("S.EID", "M.MRNUMBER"))
step3falseSMtoSMW1 <- st1 %>% anti_join(st12, by = "S.EID")
step3falseSMtoSMW2 <- st1 %>% anti_join(st12, by = "M.MRNUMBER")

####################################
step3SW <- f3VFLjoinSW %>% anti_join(step2SM, by = "S.EID") %>% anti_join(step2MW, by = "W.UNIQUE-ID") %>% anti_join(step2SW, by = "S.EID") %>% anti_join(step2SW, by = "W.UNIQUE-ID") %>% bind_rows(step2SW) %>% anti_join(step3SMW, by = "S.EID") %>% anti_join(step3SMW, by = "W.UNIQUE-ID")

step3S <- bind_rows(step2S, f3VFLjoinS) %>% distinct(S.EID, .keep_all = T) %>% anti_join(step3SMW, by = "S.EID") %>% anti_join(step3SM, by = "S.EID") %>% anti_join(step3SW, by = "S.EID") %>% anti_join(step2SM, by = "S.EID") %>% anti_join(step2SW, by = "S.EID")


step3MW <-f3VFLjoinMW %>% anti_join(step2SW, by = "W.UNIQUE-ID") %>% anti_join(step2SM, by = "M.MRNUMBER") %>% anti_join(step2MW, by = "M.MRNUMBER") %>% anti_join(step2MW, by = "W.UNIQUE-ID") %>% bind_rows(step2MW) %>% anti_join(step3SMW, by = "M.MRNUMBER") %>% anti_join(step3SMW, by = "W.UNIQUE-ID") ### anti-join kai me tis 2 sthles tou step2MW giati mporei to join sto step1MW na einai diaforetiko apo auto pou ennwnei metaksu MW sto step2SMW
### lathois to join metaksy tou mw tou 2ou step kai tou f3VFLjoinSW sto 3o krithrio, kathws kai sm apo step2 pou phge se smw sto 3o, opote mporei kapoio m na enwthike me allo w sto smw kai antisrofa
s1 <- bind_rows(step2MW, f3VFLjoinMW) %>% distinct(`W.UNIQUE-ID`,.keep_all = T)
s2 <- bind_rows(step2MW, f3VFLjoinMW) %>% distinct(M.MRNUMBER,.keep_all = T)
s12 <- bind_rows(step2MW, f3VFLjoinMW) %>% distinct(`W.UNIQUE-ID`, M.MRNUMBER, .keep_all = T)
step3falseMW1 <- s1 %>% anti_join(s12, by = "W.UNIQUE-ID")
step3falseMW2 <- s2 %>% anti_join(s12, by = "M.MRNUMBER")

st <- step2MW 
st1 <- st %>% anti_join(step3SMW, by = "W.UNIQUE-ID") ### afairw apo to vhma 1 MW auta toy step 2 SMW pou exoun idio w, alla mesa mporei na uparxoun dhmpsieuseis pou den ginane idio join me to M sto step2 SMW kai to step1 MW
st2 <- st %>% anti_join(step3SMW, by = "M.MRNUMBER")
st12 <- st %>% anti_join(step3SMW, by = c("W.UNIQUE-ID", "M.MRNUMBER")) ## afairw apo to vhma 1 MW auta pou einai akrivws idia join tou step 2 SMW
step3falseMWtoSMW1 <- st12 %>% anti_join(st1, by = "W.UNIQUE-ID") ### vriskei ta wunique pou, sto prwto step MW to ekane join se allo tou math kai sto 2o step SMW se allo tou math
step3falseMWtoSMW2 <- st12 %>% anti_join(st2, by = "M.MRNUMBER")
################################################ 


step3M <- bind_rows(step2M, f3VFLjoinM) %>% distinct(M.MRNUMBER, .keep_all = T) %>% anti_join(step3SMW, by = "M.MRNUMBER") %>% anti_join(step3SM, by = "M.MRNUMBER") %>% anti_join(step3MW, by = "M.MRNUMBER") %>% anti_join(step2SM, by = "M.MRNUMBER") %>% anti_join(step2MW, by = "M.MRNUMBER")
step3W <- bind_rows(step2W, f3VFLjoinW) %>% distinct(`W.UNIQUE-ID`, .keep_all = T) %>% anti_join(step3SMW, by = "W.UNIQUE-ID") %>% anti_join(step3SW, by = "W.UNIQUE-ID") %>% anti_join(step3MW, by = "W.UNIQUE-ID") %>% anti_join(step2SW, by = "W.UNIQUE-ID") %>% anti_join(step2MW, by = "W.UNIQUE-ID")

sum(sapply(list(step3SMW, step3SM, step3SW, step3S, intersect(intersect(sDupls, s2Dupls), s3Dupls)), nrow)) ### pou einai idies grammes me to na afairousa eksarxhs sto step1 ta s2dupls mazi me ta sdupls

#step2SM2 <- bind_rows(step1SM, f3VILjoinSM) %>% distinct(S.EID, M.MRNUMBER, .keep_all = T) #%>% filter(n()==1)
#v <- nrow(pubsJournaldf)-nrow(union(s2Dupls,sDupls)) 
#v2 <- intersect(s2Dupls$S.EID,sDupls$S.EID)
sum(sapply(list(step3SMW, step3SM, step3MW, step3M, intersect(intersect(mDupls, m2Dupls), m3Dupls)), nrow))
sum(sapply(list(step3SMW, step3MW, step3SW, step3W, intersect(intersect(wDupls, w2Dupls), w3Dupls)), nrow))
s <- bind_rows(step3SMW, step3SM, step3SW, step3S, intersect(intersect(sDupls, s2Dupls), s3Dupls))
m <- bind_rows(step3SMW, step3SM, step3MW, step3M, intersect(intersect(mDupls, m2Dupls), m3Dupls))
w <- bind_rows(step3SMW, step3MW, step3SW, step3W, intersect(intersect(wDupls, w2Dupls), w3Dupls))
sum(duplicated(w$`W.UNIQUE-ID`))
sum(duplicated(s$S.EID))
sum(duplicated(m$M.MRNUMBER))

#tiexasa <- setdiff(mathscidataU$M.MRNUMBER, m$M.MRNUMBER)







### afairpume apo tis vaseis tis dhmosieuseis pou perilamvanonti sto full join step3SMW ###


scopusStep4 <- pubsJournaldf%>%filter(!(S.EID %in% step3SMW$S.EID))
mathstep4 <- mathscidata%>%filter(!(M.MRNUMBER %in% step3SMW$M.MRNUMBER))
wosstep4 <- WOSdf%>%filter(!(`W.UNIQUE-ID` %in% step3SMW$`W.UNIQUE-ID`))


### epishs afairw apo autes ta duplicated gia to neo krithrio pou einai ta pedia -- volume, first_page ---###

s4Dupls <- scopusStep4 %>% group_by(S.VOLUME, S.FIRST_PAGE) %>% drop_na(S.VOLUME, S.FIRST_PAGE) %>%
      filter(n()>1)
scopusStep4U <- scopusStep4 %>% anti_join(s4Dupls)

m4Dupls <- mathstep4 %>% group_by(M.VOLUME, M.FIRST_PAGE) %>% drop_na(M.VOLUME, M.FIRST_PAGE) %>%
      filter(n()>1)
mathstep4U <- mathstep4 %>% anti_join(m4Dupls)

w4Dupls <- wosstep4 %>% group_by(W.VOLUME, W.FIRST_PAGE) %>% drop_na(W.VOLUME, W.FIRST_PAGE) %>%
      filter(n()>1)
wosstep4U <- wosstep4 %>% anti_join(w4Dupls)
#### join ta upoloipa apo to step3SMW me krithrio ta pedia volume kai first page ###


vf_oj1 <- scopusStep4U %>% full_join(mathstep4U, by = c("S.VOLUME"="M.VOLUME", "S.FIRST_PAGE"="M.FIRST_PAGE"), na_matches = "never", keep=T)%>%
      full_join(wosstep4U, by =  c("S.VOLUME"="W.VOLUME", "S.FIRST_PAGE"="W.FIRST_PAGE"), na_matches = "never", keep=T)


vf_oj1_check <- vf_oj1[,c("S.VOLUME","S.NUMBER", "S.FIRST_PAGE", "S.LAST_PAGE", "S.EID", "M.VOLUME","M.NUMBER", "M.FIRST_PAGE", "M.LAST_PAGE", "M.MRNUMBER", "W.VOLUME","W.NUMBER", "W.FIRST_PAGE", "W.LAST_PAGE", "W.UNIQUE-ID")]
#which(is.na(vf_oj_check[,"S.EID"]) & is.na(vf_oj_check[,"M.MRNUMBER"]) & is.na(vf_oj_check[,"W.UNIQUE-ID"]))


#### twra tha doume posa ginane join kai stis 3, posa se 2 kai posa mona tous gia ta 2 fields ###



f2VFjoinSMW <- vf_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))
f2VFjoinSM <- vf_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER),all_vars(!is.na(.)))%>%filter(is.na(`W.UNIQUE-ID`))
f2VFjoinSW <- vf_oj1 %>% filter_at(vars(S.EID, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(M.MRNUMBER))
f2VFjoinMW <- vf_oj1 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(S.EID))
f2VFjoinS <- vf_oj1 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))%>%filter(!is.na(S.EID))
f2VFjoinW <- vf_oj1 %>% filter_at(vars(M.MRNUMBER, S.EID),all_vars(is.na(.)))%>%filter(!is.na(`W.UNIQUE-ID`))
f2VFjoinM <- vf_oj1 %>% filter_at(vars(`W.UNIQUE-ID`, S.EID),all_vars(is.na(.)))%>%filter(!is.na(M.MRNUMBER))
f2VFjoin0 <- vf_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))

nrow(f2VFjoinSMW)+nrow(f2VFjoinSM)+nrow(f2VFjoinSW)+nrow(f2VFjoinMW)+nrow(f2VFjoinS)+nrow(f2VFjoinW)+nrow(f2VFjoinM)+nrow(f2VFjoin0)


vf_oj2 <- scopusStep4U %>% full_join(mathstep4U, by = c("S.VOLUME"="M.VOLUME", "S.FIRST_PAGE"="M.FIRST_PAGE"), na_matches = "never", keep=T)%>%
      full_join(wosstep4U, by =  c("M.VOLUME"="W.VOLUME", "M.FIRST_PAGE"="W.FIRST_PAGE"), na_matches = "never", keep=T)


f2VFjoinSMWw <- vf_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))
f2VFjoinSMm <- vf_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER),all_vars(!is.na(.)))%>%filter(is.na(`W.UNIQUE-ID`))
f2VFjoinSWw <- vf_oj2 %>% filter_at(vars(S.EID, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(M.MRNUMBER))
f2VFjoinMWw <- vf_oj2 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(S.EID))
f2VFjoinSs <- vf_oj2 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))%>%filter(!is.na(S.EID))
f2VFjoinWw <- vf_oj2 %>% filter_at(vars(M.MRNUMBER, S.EID),all_vars(is.na(.)))%>%filter(!is.na(`W.UNIQUE-ID`))
f2VFjoinMm <- vf_oj2 %>% filter_at(vars(`W.UNIQUE-ID`, S.EID),all_vars(is.na(.)))%>%filter(!is.na(M.MRNUMBER))
f2VFjoin00 <- vf_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))
c <- f2VFjoinW %>% filter(!(`W.UNIQUE-ID` %in% f2VFjoinMWw$`W.UNIQUE-ID`)) #& !(`W.UNIQUE-ID` %in% f3joinMW$`W.UNIQUE-ID`))
c2 <- f2VFjoinWw %>% filter(!(`W.UNIQUE-ID` %in% f2VFjoinSW$`W.UNIQUE-ID`))

f2VFjoinMW <- f2VFjoinMWw
f2VFjoinM <- f2VFjoinMm
f2VFjoinW <- c

### ara to oliko dataframe einai rbind oloi oi sundiasmoi, kai sta mona tha pairnw ta mikrotera gia s kai m, enw gia w ayta pou emeinan sto ena kai parallhla den uparxoun sto sunduasmo tou allou (sw h mw) ### 
vf_oj <- f2VFjoinSMW %>% bind_rows(f2VFjoinSM, f2VFjoinSW, f2VFjoinMW, f2VFjoinS, f2VFjoinM, f2VFjoinW)
vf_oj_check <- vf_oj[,c("S.VOLUME","S.NUMBER", "S.FIRST_PAGE", "S.EID", "M.VOLUME","M.NUMBER", "M.FIRST_PAGE", "M.MRNUMBER", "W.VOLUME","W.NUMBER", "W.FIRST_PAGE", "W.UNIQUE-ID")]
step4SMW <- bind_rows(step3SMW, f2VFjoinSMW)
step4SM <- f2VFjoinSM %>% anti_join(step3SW, by = "S.EID") %>% anti_join(step3MW, by = "M.MRNUMBER") %>% anti_join(step3SM, by = "S.EID") %>% anti_join(step3SM, by = "M.MRNUMBER") %>% bind_rows(step3SM) %>% anti_join(step4SMW, by = "S.EID") %>% anti_join(step4SMW, by = "M.MRNUMBER")
### lathois to join metaksy tou sm tou 3ou step kai tou f2VFjoinSM sto 4o krithrio, kathws kai sm apo step3 pou phge se smw sto 40, opote mporei kapoio s na enwthike me allo m sto smw kai antisrofa
# s1 <- bind_rows(step3SM, f2VFjoinSM) %>% distinct(S.EID,.keep_all = T)
# s2 <- bind_rows(step3SM, f2VFjoinSM) %>% distinct(M.MRNUMBER,.keep_all = T)
# s12 <- bind_rows(step3SM, f2VFjoinSM) %>% distinct(S.EID, M.MRNUMBER, .keep_all = T)
# step4falseSM1 <- s1 %>% anti_join(s12, by = "S.EID") ### etsi pairnoume tis periptwseis opou thn idia dhmosieush tou mathsci thn exoume kanei join se diaforetikes tou scopus
# step4falseSM2 <- s2 %>% anti_join(s12, by = "M.MRNUMBER")
# 
# st <- step3SM
# st1 <- st %>% anti_join(step4SMW, by = "S.EID")
# st2 <- st %>% anti_join(step4SMW, by = "M.MRNUMBER")
# st12 <- st %>% anti_join(step4SMW, by = c("S.EID", "M.MRNUMBER"))
# step4falseSMtoSMW1 <- st1 %>% anti_join(st12, by = "S.EID")
# step4falseSMtoSMW2 <- st1 %>% anti_join(st12, by = "M.MRNUMBER")

####################################
step4SW <- f2VFjoinSW %>% anti_join(step3SM, by = "S.EID") %>% anti_join(step3MW, by = "W.UNIQUE-ID") %>% anti_join(step3SW, by = "S.EID") %>% anti_join(step3SW, by = "W.UNIQUE-ID") %>% bind_rows(step3SW) %>% anti_join(step4SMW, by = "S.EID") %>% anti_join(step4SMW, by = "W.UNIQUE-ID")

step4S <- bind_rows(step3S, f2VFjoinS) %>% distinct(S.EID, .keep_all = T) %>% anti_join(step4SMW, by = "S.EID") %>% anti_join(step4SM, by = "S.EID") %>% anti_join(step4SW, by = "S.EID") %>% anti_join(step3SM, by = "S.EID") %>% anti_join(step3SW, by = "S.EID")


step4MW <-f2VFjoinMW %>% anti_join(step3SM, by = "M.MRNUMBER") %>% anti_join(step3SW, by = "W.UNIQUE-ID") %>% anti_join(step3MW, by = "M.MRNUMBER") %>% anti_join(step3MW, by = "W.UNIQUE-ID") %>% bind_rows(step3MW) %>% anti_join(step4SMW, by = "M.MRNUMBER") %>% anti_join(step4SMW, by = "W.UNIQUE-ID") ### anti-join kai me tis 2 sthles tou step2MW giati mporei to join sto step1MW na einai diaforetiko apo auto pou ennwnei metaksu MW sto step2SMW
### lathois to join metaksy tou mw tou 3ou step kai tou f2VFjoinSW sto 4o krithrio, kathws kai sm apo step3 pou phge se smw sto 4o, opote mporei kapoio m na enwthike me allo w sto smw kai antisrofa
s1 <- bind_rows(step3MW, f2VFjoinMW) %>% distinct(`W.UNIQUE-ID`,.keep_all = T)
s2 <- bind_rows(step3MW, f2VFjoinMW) %>% distinct(M.MRNUMBER,.keep_all = T)
s12 <- bind_rows(step3MW, f2VFjoinMW) %>% distinct(`W.UNIQUE-ID`, M.MRNUMBER, .keep_all = T)
step4falseMW1 <- s1 %>% anti_join(s12, by = "W.UNIQUE-ID")
step4falseMW2 <- s2 %>% anti_join(s12, by = "M.MRNUMBER")

st <- step3MW 
st1 <- st %>% anti_join(step4SMW, by = "W.UNIQUE-ID") ### afairw apo to vhma 1 MW auta toy step 2 SMW pou exoun idio w, alla mesa mporei na uparxoun dhmpsieuseis pou den ginane idio join me to M sto step2 SMW kai to step1 MW
st2 <- st %>% anti_join(step4SMW, by = "M.MRNUMBER")
st12 <- st %>% anti_join(step4SMW, by = c("W.UNIQUE-ID", "M.MRNUMBER")) ## afairw apo to vhma 1 MW auta pou einai akrivws idia join tou step 2 SMW
step4falseMWtoSMW1 <- st12 %>% anti_join(st1, by = "W.UNIQUE-ID") ### vriskei ta wunique pou, sto prwto step MW to ekane join se allo tou math kai sto 2o step SMW se allo tou math
step4falseMWtoSMW2 <- st12 %>% anti_join(st2, by = "M.MRNUMBER")
################################################ 


step4M <- bind_rows(step3M, f2VFjoinM) %>% distinct(M.MRNUMBER, .keep_all = T) %>% anti_join(step4SMW, by = "M.MRNUMBER") %>% anti_join(step4SM, by = "M.MRNUMBER") %>% anti_join(step4MW, by = "M.MRNUMBER") %>% anti_join(step3SM, by = "M.MRNUMBER") %>% anti_join(step3MW, by = "M.MRNUMBER")
step4W <- bind_rows(step3W, f2VFjoinW) %>% distinct(`W.UNIQUE-ID`, .keep_all = T) %>% anti_join(step4SMW, by = "W.UNIQUE-ID") %>% anti_join(step4SW, by = "W.UNIQUE-ID") %>% anti_join(step4MW, by = "W.UNIQUE-ID") %>% anti_join(step3SW, by = "W.UNIQUE-ID") %>% anti_join(step3MW, by = "W.UNIQUE-ID")

sum(sapply(list(step4SMW, step4SM, step4SW, step4S, intersect(intersect(intersect(sDupls, s2Dupls), s3Dupls), s4Dupls)), nrow)) ### pou einai idies grammes me to na afairousa eksarxhs sto step1 ta s2dupls mazi me ta sdupls

#step2SM2 <- bind_rows(step1SM, f3VILjoinSM) %>% distinct(S.EID, M.MRNUMBER, .keep_all = T) #%>% filter(n()==1)
#v <- nrow(pubsJournaldf)-nrow(union(s2Dupls,sDupls)) 
#v2 <- intersect(s2Dupls$S.EID,sDupls$S.EID)
sum(sapply(list(step4SMW, step4SM, step4MW, step4M, intersect(intersect(intersect(mDupls, m2Dupls), m3Dupls), m4Dupls)), nrow))
sum(sapply(list(step4SMW, step4MW, step4SW, step4W, intersect(intersect(intersect(wDupls, w2Dupls), w3Dupls), w4Dupls)), nrow))
s <- bind_rows(step4SMW, step4SM, step4SW, step4S, intersect(intersect(intersect(sDupls, s2Dupls), s3Dupls), s4Dupls))
m <- bind_rows(step4SMW, step4SM, step4MW, step4M, intersect(intersect(intersect(mDupls, m2Dupls), m3Dupls), m4Dupls))
w <- bind_rows(step4SMW, step4MW, step4SW, step4W, intersect(intersect(intersect(wDupls, w2Dupls), w3Dupls), w4Dupls))
sum(duplicated(w$`W.UNIQUE-ID`))
sum(duplicated(s$S.EID))
sum(duplicated(m$M.MRNUMBER))

### afairpume apo tis vaseis tis dhmosieuseis pou perilamvanonti sto full join step4SMW ###


scopusStep5 <- pubsJournaldf%>%filter(!(S.EID %in% step4SMW$S.EID))
mathstep5 <- mathscidata%>%filter(!(M.MRNUMBER %in% step4SMW$M.MRNUMBER))
wosstep5 <- WOSdf%>%filter(!(`W.UNIQUE-ID` %in% step4SMW$`W.UNIQUE-ID`))




### epishs afairw apo autes ta duplicated gia to neo krithrio pou einai ta pedia -- volume, last_page ---###

s5Dupls <- scopusStep5 %>% group_by(S.VOLUME, S.LAST_PAGE) %>% drop_na(S.VOLUME, S.LAST_PAGE) %>%
      filter(n()>1)
scopusStep5U <- scopusStep5 %>% anti_join(s5Dupls)

m5Dupls <- mathstep5 %>% group_by(M.VOLUME, M.LAST_PAGE) %>% drop_na(M.VOLUME, M.LAST_PAGE) %>%
      filter(n()>1)
mathstep5U <- mathstep5 %>% anti_join(m5Dupls)

w5Dupls <- wosstep5 %>% group_by(W.VOLUME, W.LAST_PAGE) %>% drop_na(W.VOLUME, W.LAST_PAGE) %>%
      filter(n()>1)
wosstep5U <- wosstep5 %>% anti_join(w5Dupls)
#### join ta upoloipa apo to step3SMW me krithrio ta pedia volume kai LAST page ###


vl_oj1 <- scopusStep5U %>% full_join(mathstep5U, by = c("S.VOLUME"="M.VOLUME", "S.LAST_PAGE"="M.LAST_PAGE"), na_matches = "never", keep=T)%>%
      full_join(wosstep5U, by =  c("S.VOLUME"="W.VOLUME", "S.LAST_PAGE"="W.LAST_PAGE"), na_matches = "never", keep=T)


vl_oj1_check <- vl_oj1[,c("S.VOLUME","S.NUMBER", "S.FIRST_PAGE", "S.LAST_PAGE", "S.EID", "M.VOLUME","M.NUMBER", "M.FIRST_PAGE", "M.LAST_PAGE", "M.MRNUMBER", "W.VOLUME","W.NUMBER", "W.FIRST_PAGE", "W.LAST_PAGE", "W.UNIQUE-ID")]
#which(is.na(vl_oj_check[,"S.EID"]) & is.na(vl_oj_check[,"M.MRNUMBER"]) & is.na(vl_oj_check[,"W.UNIQUE-ID"]))


#### twra tha doume posa ginane join kai stis 3, posa se 2 kai posa mona tous gia ta 2 fields ###



f2VLjoinSMW <- vl_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))
f2VLjoinSM <- vl_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER),all_vars(!is.na(.)))%>%filter(is.na(`W.UNIQUE-ID`))
f2VLjoinSW <- vl_oj1 %>% filter_at(vars(S.EID, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(M.MRNUMBER))
f2VLjoinMW <- vl_oj1 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(S.EID))
f2VLjoinS <- vl_oj1 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))%>%filter(!is.na(S.EID))
f2VLjoinW <- vl_oj1 %>% filter_at(vars(M.MRNUMBER, S.EID),all_vars(is.na(.)))%>%filter(!is.na(`W.UNIQUE-ID`))
f2VLjoinM <- vl_oj1 %>% filter_at(vars(`W.UNIQUE-ID`, S.EID),all_vars(is.na(.)))%>%filter(!is.na(M.MRNUMBER))
f2VLjoin0 <- vl_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))

nrow(f2VLjoinSMW)+nrow(f2VLjoinSM)+nrow(f2VLjoinSW)+nrow(f2VLjoinMW)+nrow(f2VLjoinS)+nrow(f2VLjoinW)+nrow(f2VLjoinM)+nrow(f2VLjoin0)


vl_oj2 <- scopusStep5U %>% full_join(mathstep5U, by = c("S.VOLUME"="M.VOLUME", "S.LAST_PAGE"="M.LAST_PAGE"), na_matches = "never", keep=T)%>%
      full_join(wosstep5U, by =  c("M.VOLUME"="W.VOLUME", "M.LAST_PAGE"="W.LAST_PAGE"), na_matches = "never", keep=T)


f2VLjoinSMWw <- vl_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))
f2VLjoinSMm <- vl_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER),all_vars(!is.na(.)))%>%filter(is.na(`W.UNIQUE-ID`))
f2VLjoinSWw <- vl_oj2 %>% filter_at(vars(S.EID, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(M.MRNUMBER))
f2VLjoinMWw <- vl_oj2 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(S.EID))
f2VLjoinSs <- vl_oj2 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))%>%filter(!is.na(S.EID))
f2VLjoinWw <- vl_oj2 %>% filter_at(vars(M.MRNUMBER, S.EID),all_vars(is.na(.)))%>%filter(!is.na(`W.UNIQUE-ID`))
f2VLjoinMm <- vl_oj2 %>% filter_at(vars(`W.UNIQUE-ID`, S.EID),all_vars(is.na(.)))%>%filter(!is.na(M.MRNUMBER))
f2VLjoin00 <- vl_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))
c <- f2VLjoinW %>% filter(!(`W.UNIQUE-ID` %in% f2VLjoinMWw$`W.UNIQUE-ID`)) #& !(`W.UNIQUE-ID` %in% f3joinMW$`W.UNIQUE-ID`))
c2 <- f2VLjoinWw %>% filter(!(`W.UNIQUE-ID` %in% f2VLjoinSW$`W.UNIQUE-ID`))

f2VLjoinMW <- f2VLjoinMWw
f2VLjoinM <- f2VLjoinMm
f2VLjoinW <- c

### ara to oliko dataframe einai rbind oloi oi sundiasmoi, kai sta mona tha pairnw ta mikrotera gia s kai m, enw gia w ayta pou emeinan sto ena kai parallhla den uparxoun sto sunduasmo tou allou (sw h mw) ### 
vl_oj <- f2VLjoinSMW %>% bind_rows(f2VLjoinSM, f2VLjoinSW, f2VLjoinMW, f2VLjoinS, f2VLjoinM, f2VLjoinW)
vl_oj_check <- vl_oj[,c("S.VOLUME","S.NUMBER", "S.LAST_PAGE", "S.EID", "M.VOLUME","M.NUMBER", "M.LAST_PAGE", "M.MRNUMBER", "W.VOLUME","W.NUMBER", "W.LAST_PAGE", "W.UNIQUE-ID")]
step5SMW <- bind_rows(step4SMW, f2VLjoinSMW)
step5SM <- f2VLjoinSM %>% anti_join(step4SW, by = "S.EID") %>% anti_join(step4MW, by = "M.MRNUMBER") %>% anti_join(step4SM, by = "S.EID") %>% anti_join(step4SM, by = "M.MRNUMBER") %>% bind_rows(step4SM) %>% anti_join(step5SMW, by = "S.EID") %>% anti_join(step5SMW, by = "M.MRNUMBER")
### lathois to join metaksy tou sm tou 4ou step kai tou f2VLjoinSM sto 5o krithrio, kathws kai sm apo step4 pou phge se smw sto 50, opote mporei kapoio s na enwthike me allo m sto smw kai antisrofa
# s1 <- bind_rows(step3SM, f2VLjoinSM) %>% distinct(S.EID,.keep_all = T)
# s2 <- bind_rows(step3SM, f2VLjoinSM) %>% distinct(M.MRNUMBER,.keep_all = T)
# s12 <- bind_rows(step3SM, f2VLjoinSM) %>% distinct(S.EID, M.MRNUMBER, .keep_all = T)
# step5falseSM1 <- s1 %>% anti_join(s12, by = "S.EID") ### etsi pairnoume tis periptwseis opou thn idia dhmosieush tou mathsci thn exoume kanei join se diaforetikes tou scopus
# step5falseSM2 <- s2 %>% anti_join(s12, by = "M.MRNUMBER")
# 
# st <- step3SM
# st1 <- st %>% anti_join(step5SMW, by = "S.EID")
# st2 <- st %>% anti_join(step5SMW, by = "M.MRNUMBER")
# st12 <- st %>% anti_join(step5SMW, by = c("S.EID", "M.MRNUMBER"))
# step5falseSMtoSMW1 <- st1 %>% anti_join(st12, by = "S.EID")
# step5falseSMtoSMW2 <- st1 %>% anti_join(st12, by = "M.MRNUMBER")

####################################
step5SW <- f2VLjoinSW %>% anti_join(step4SM, by = "S.EID") %>% anti_join(step4MW, by = "W.UNIQUE-ID") %>% anti_join(step4SW, by = "S.EID") %>% anti_join(step4SW, by = "W.UNIQUE-ID") %>% bind_rows(step4SW) %>% anti_join(step5SMW, by = "S.EID") %>% anti_join(step5SMW, by = "W.UNIQUE-ID")

step5S <- bind_rows(step4S, f2VLjoinS) %>% distinct(S.EID, .keep_all = T) %>% anti_join(step5SMW, by = "S.EID") %>% anti_join(step5SM, by = "S.EID") %>% anti_join(step5SW, by = "S.EID") %>% anti_join(step4SM, by = "S.EID") %>% anti_join(step4SW, by = "S.EID")


step5MW <-f2VLjoinMW %>% anti_join(step4SM, by = "M.MRNUMBER") %>% anti_join(step4SW, by = "W.UNIQUE-ID") %>% anti_join(step4MW, by = "M.MRNUMBER") %>% anti_join(step4MW, by = "W.UNIQUE-ID") %>% bind_rows(step4MW) %>% anti_join(step5SMW, by = "M.MRNUMBER") %>% anti_join(step5SMW, by = "W.UNIQUE-ID") ### anti-join kai me tis 2 sthles tou step2MW giati mporei to join sto step1MW na einai diaforetiko apo auto pou ennwnei metaksu MW sto step2SMW
### lathois to join metaksy tou mw tou 4ou step kai tou f2VLjoinSW sto 5o krithrio, kathws kai sm apo step4 pou phge se smw sto 5o, opote mporei kapoio m na enwthike me allo w sto smw kai antisrofa
s1 <- bind_rows(step4MW, f2VLjoinMW) %>% distinct(`W.UNIQUE-ID`,.keep_all = T)
s2 <- bind_rows(step4MW, f2VLjoinMW) %>% distinct(M.MRNUMBER,.keep_all = T)
s12 <- bind_rows(step4MW, f2VLjoinMW) %>% distinct(`W.UNIQUE-ID`, M.MRNUMBER, .keep_all = T)
step5falseMW1 <- s1 %>% anti_join(s12, by = "W.UNIQUE-ID")
step5falseMW2 <- s2 %>% anti_join(s12, by = "M.MRNUMBER")

st <- step4MW 
st1 <- st %>% anti_join(step5SMW, by = "W.UNIQUE-ID") ### afairw apo to vhma 1 MW auta toy step 2 SMW pou exoun idio w, alla mesa mporei na uparxoun dhmpsieuseis pou den ginane idio join me to M sto step2 SMW kai to step1 MW
st2 <- st %>% anti_join(step5SMW, by = "M.MRNUMBER")
st12 <- st %>% anti_join(step5SMW, by = c("W.UNIQUE-ID", "M.MRNUMBER")) ## afairw apo to vhma 1 MW auta pou einai akrivws idia join tou step 2 SMW
step5falseMWtoSMW1 <- st12 %>% anti_join(st1, by = "W.UNIQUE-ID") ### vriskei ta wunique pou, sto prwto step MW to ekane join se allo tou math kai sto 2o step SMW se allo tou math
step5falseMWtoSMW2 <- st12 %>% anti_join(st2, by = "M.MRNUMBER")
################################################ 


step5M <- bind_rows(step4M, f2VLjoinM) %>% distinct(M.MRNUMBER, .keep_all = T) %>% anti_join(step5SMW, by = "M.MRNUMBER") %>% anti_join(step5SM, by = "M.MRNUMBER") %>% anti_join(step5MW, by = "M.MRNUMBER") %>% anti_join(step4SM, by = "M.MRNUMBER") %>% anti_join(step4MW, by = "M.MRNUMBER")
step5W <- bind_rows(step4W, f2VLjoinW) %>% distinct(`W.UNIQUE-ID`, .keep_all = T) %>% anti_join(step5SMW, by = "W.UNIQUE-ID") %>% anti_join(step5SW, by = "W.UNIQUE-ID") %>% anti_join(step5MW, by = "W.UNIQUE-ID") %>% anti_join(step4SW, by = "W.UNIQUE-ID") %>% anti_join(step4MW, by = "W.UNIQUE-ID")

duplsSInt <- intersect(intersect(intersect(intersect(sDupls, s2Dupls), s3Dupls), s4Dupls), s5Dupls)
sum(sapply(list(step5SMW, step5SM, step5SW, step5S, duplsSInt), nrow)) ### pou einai idies grammes me to na afairousa eksarxhs sto step1 ta s2dupls mazi me ta sdupls

#step2SM2 <- bind_rows(step1SM, f3VILjoinSM) %>% distinct(S.EID, M.MRNUMBER, .keep_all = T) #%>% filter(n()==1)
#v <- nrow(pubsJournaldf)-nrow(union(s2Dupls,sDupls)) 
#v2 <- intersect(s2Dupls$S.EID,sDupls$S.EID)
duplsMInt <- intersect(intersect(intersect(intersect(mDupls, m2Dupls), m3Dupls), m4Dupls), m5Dupls)
sum(sapply(list(step5SMW, step5SM, step5MW, step5M, duplsMInt), nrow))
duplsWInt <- intersect(intersect(intersect(intersect(wDupls, w2Dupls), w3Dupls), w4Dupls), w5Dupls)
sum(sapply(list(step5SMW, step5MW, step5SW, step5W, duplsWInt), nrow))
s <- bind_rows(step5SMW, step5SM, step5SW, step5S, duplsSInt)
m <- bind_rows(step5SMW, step5SM, step5MW, step5M, duplsMInt)
w <- bind_rows(step5SMW, step5MW, step5SW, step5W, duplsWInt)
sum(duplicated(w$`W.UNIQUE-ID`))
sum(duplicated(s$S.EID))
sum(duplicated(m$M.MRNUMBER))




### afairpume apo tis vaseis tis dhmosieuseis pou perilamvanonti sto full join step4SMW ###


scopusStep6 <- pubsJournaldf%>%filter(!(S.EID %in% step5SMW$S.EID))
mathstep6 <- mathscidata%>%filter(!(M.MRNUMBER %in% step5SMW$M.MRNUMBER))
wosstep6 <- WOSdf%>%filter(!(`W.UNIQUE-ID` %in% step5SMW$`W.UNIQUE-ID`))




### epishs afairw apo autes ta duplicated gia to neo krithrio pou einai ta pedia -- volume, last_page ---###

s6Dupls <- scopusStep6 %>% group_by(S.FIRST_PAGE, S.LAST_PAGE) %>% drop_na(S.FIRST_PAGE, S.LAST_PAGE) %>%
      filter(n()>1)
scopusStep6U <- scopusStep6 %>% anti_join(s6Dupls)

m6Dupls <- mathstep6 %>% group_by(M.FIRST_PAGE, M.LAST_PAGE) %>% drop_na(M.FIRST_PAGE, M.LAST_PAGE) %>%
      filter(n()>1)
mathstep6U <- mathstep6 %>% anti_join(m6Dupls)

w6Dupls <- wosstep6 %>% group_by(W.FIRST_PAGE, W.LAST_PAGE) %>% drop_na(W.FIRST_PAGE, W.LAST_PAGE) %>%
      filter(n()>1)
wosstep6U <- wosstep6 %>% anti_join(w6Dupls)
#### join ta upoloipa apo to step3SMW me krithrio ta pedia FIRST_PAGE kai LAST page ###


fl_oj1 <- scopusStep6U %>% full_join(mathstep6U, by = c("S.FIRST_PAGE"="M.FIRST_PAGE", "S.LAST_PAGE"="M.LAST_PAGE"), na_matches = "never", keep=T)%>%
      full_join(wosstep6U, by =  c("S.FIRST_PAGE"="W.FIRST_PAGE", "S.LAST_PAGE"="W.LAST_PAGE"), na_matches = "never", keep=T)


fl_oj1_check <- fl_oj1[,c("S.VOLUME","S.NUMBER", "S.FIRST_PAGE", "S.LAST_PAGE", "S.EID", "M.VOLUME","M.NUMBER", "M.FIRST_PAGE", "M.LAST_PAGE", "M.MRNUMBER", "W.VOLUME","W.NUMBER", "W.FIRST_PAGE", "W.LAST_PAGE", "W.UNIQUE-ID")]
#which(is.na(fl_oj_check[,"S.EID"]) & is.na(fl_oj_check[,"M.MRNUMBER"]) & is.na(fl_oj_check[,"W.UNIQUE-ID"]))


#### twra tha doume posa ginane join kai stis 3, posa se 2 kai posa mona tous gia ta 2 fields ###



f2FLjoinSMW <- fl_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))
f2FLjoinSM <- fl_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER),all_vars(!is.na(.)))%>%filter(is.na(`W.UNIQUE-ID`))
f2FLjoinSW <- fl_oj1 %>% filter_at(vars(S.EID, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(M.MRNUMBER))
f2FLjoinMW <- fl_oj1 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(S.EID))
f2FLjoinS <- fl_oj1 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))%>%filter(!is.na(S.EID))
f2FLjoinW <- fl_oj1 %>% filter_at(vars(M.MRNUMBER, S.EID),all_vars(is.na(.)))%>%filter(!is.na(`W.UNIQUE-ID`))
f2FLjoinM <- fl_oj1 %>% filter_at(vars(`W.UNIQUE-ID`, S.EID),all_vars(is.na(.)))%>%filter(!is.na(M.MRNUMBER))
f2FLjoin0 <- fl_oj1 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))

nrow(f2FLjoinSMW)+nrow(f2FLjoinSM)+nrow(f2FLjoinSW)+nrow(f2FLjoinMW)+nrow(f2FLjoinS)+nrow(f2FLjoinW)+nrow(f2FLjoinM)+nrow(f2FLjoin0)


fl_oj2 <- scopusStep6U %>% full_join(mathstep6U, by = c("S.FIRST_PAGE"="M.FIRST_PAGE", "S.LAST_PAGE"="M.LAST_PAGE"), na_matches = "never", keep=T)%>%
      full_join(wosstep6U, by =  c("M.FIRST_PAGE"="W.FIRST_PAGE", "M.LAST_PAGE"="W.LAST_PAGE"), na_matches = "never", keep=T)


f2FLjoinSMWw <- fl_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))
f2FLjoinSMm <- fl_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER),all_vars(!is.na(.)))%>%filter(is.na(`W.UNIQUE-ID`))
f2FLjoinSWw <- fl_oj2 %>% filter_at(vars(S.EID, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(M.MRNUMBER))
f2FLjoinMWw <- fl_oj2 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(!is.na(.)))%>%filter(is.na(S.EID))
f2FLjoinSs <- fl_oj2 %>% filter_at(vars(M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))%>%filter(!is.na(S.EID))
f2FLjoinWw <- fl_oj2 %>% filter_at(vars(M.MRNUMBER, S.EID),all_vars(is.na(.)))%>%filter(!is.na(`W.UNIQUE-ID`))
f2FLjoinMm <- fl_oj2 %>% filter_at(vars(`W.UNIQUE-ID`, S.EID),all_vars(is.na(.)))%>%filter(!is.na(M.MRNUMBER))
f2FLjoin00 <- fl_oj2 %>% filter_at(vars(S.EID, M.MRNUMBER, `W.UNIQUE-ID`),all_vars(is.na(.)))
c <- f2FLjoinW %>% filter(!(`W.UNIQUE-ID` %in% f2FLjoinMWw$`W.UNIQUE-ID`)) #& !(`W.UNIQUE-ID` %in% f3joinMW$`W.UNIQUE-ID`))
c2 <- f2FLjoinWw %>% filter(!(`W.UNIQUE-ID` %in% f2FLjoinSW$`W.UNIQUE-ID`))

f2FLjoinMW <- f2FLjoinMWw
f2FLjoinM <- f2FLjoinMm
f2FLjoinW <- c

### ara to oliko dataframe einai rbind oloi oi sundiasmoi, kai sta mona tha pairnw ta mikrotera gia s kai m, enw gia w ayta pou emeinan sto ena kai parallhla den uparxoun sto sunduasmo tou allou (sw h mw) ### 
fl_oj <- f2FLjoinSMW %>% bind_rows(f2FLjoinSM, f2FLjoinSW, f2FLjoinMW, f2FLjoinS, f2FLjoinM, f2FLjoinW)
fl_oj_check <- fl_oj[,c("S.VOLUME","S.NUMBER", "S.LAST_PAGE", "S.EID", "M.VOLUME","M.NUMBER", "M.LAST_PAGE", "M.MRNUMBER", "W.VOLUME","W.NUMBER", "W.LAST_PAGE", "W.UNIQUE-ID")]
step6SMW <- bind_rows(step5SMW, f2FLjoinSMW)
step6SM <- f2FLjoinSM %>% anti_join(step5SW, by = "S.EID") %>% anti_join(step5MW, by = "M.MRNUMBER") %>% anti_join(step5SM, by = "S.EID") %>% anti_join(step5SM, by = "M.MRNUMBER") %>% bind_rows(step5SM) %>% anti_join(step6SMW, by = "S.EID") %>% anti_join(step6SMW, by = "M.MRNUMBER")
### lathois to join metaksy tou sm tou 5ou step kai tou f2FLjoinSM sto 6o krithrio, kathws kai sm apo step5 pou phge se smw sto 60, opote mporei kapoio s na enwthike me allo m sto smw kai antisrofa
# s1 <- bind_rows(step3SM, f2FLjoinSM) %>% distinct(S.EID,.keep_all = T)
# s2 <- bind_rows(step3SM, f2FLjoinSM) %>% distinct(M.MRNUMBER,.keep_all = T)
# s12 <- bind_rows(step3SM, f2FLjoinSM) %>% distinct(S.EID, M.MRNUMBER, .keep_all = T)
# step6falseSM1 <- s1 %>% anti_join(s12, by = "S.EID") ### etsi pairnoume tis periptwseis opou thn idia dhmosieush tou mathsci thn exoume kanei join se diaforetikes tou scopus
# step6falseSM2 <- s2 %>% anti_join(s12, by = "M.MRNUMBER")
# 
# st <- step3SM
# st1 <- st %>% anti_join(step6SMW, by = "S.EID")
# st2 <- st %>% anti_join(step6SMW, by = "M.MRNUMBER")
# st12 <- st %>% anti_join(step6SMW, by = c("S.EID", "M.MRNUMBER"))
# step6falseSMtoSMW1 <- st1 %>% anti_join(st12, by = "S.EID")
# step6falseSMtoSMW2 <- st1 %>% anti_join(st12, by = "M.MRNUMBER")

####################################
step6SW <- f2FLjoinSW %>% anti_join(step5SM, by = "S.EID") %>% anti_join(step5MW, by = "W.UNIQUE-ID") %>% anti_join(step5SW, by = "S.EID") %>% anti_join(step5SW, by = "W.UNIQUE-ID") %>% bind_rows(step5SW) %>% anti_join(step6SMW, by = "S.EID") %>% anti_join(step6SMW, by = "W.UNIQUE-ID")

step6S <- bind_rows(step5S, f2FLjoinS) %>% distinct(S.EID, .keep_all = T) %>% anti_join(step6SMW, by = "S.EID") %>% anti_join(step6SM, by = "S.EID") %>% anti_join(step6SW, by = "S.EID") %>% anti_join(step5SM, by = "S.EID") %>% anti_join(step5SW, by = "S.EID")


step6MW <-f2FLjoinMW %>% anti_join(step5SM, by = "M.MRNUMBER") %>% anti_join(step5SW, by = "W.UNIQUE-ID") %>% anti_join(step5MW, by = "M.MRNUMBER") %>% anti_join(step5MW, by = "W.UNIQUE-ID") %>% bind_rows(step5MW) %>% anti_join(step6SMW, by = "M.MRNUMBER") %>% anti_join(step6SMW, by = "W.UNIQUE-ID") ### anti-join kai me tis 2 sthles tou step2MW giati mporei to join sto step1MW na einai diaforetiko apo auto pou ennwnei metaksu MW sto step2SMW
### lathois to join metaksy tou mw tou 5ou step kai tou f2FLjoinSW sto 6o krithrio, kathws kai sm apo step5 pou phge se smw sto 6o, opote mporei kapoio m na enwthike me allo w sto smw kai antisrofa
s1 <- bind_rows(step5MW, f2FLjoinMW) %>% distinct(`W.UNIQUE-ID`,.keep_all = T)
s2 <- bind_rows(step5MW, f2FLjoinMW) %>% distinct(M.MRNUMBER,.keep_all = T)
s12 <- bind_rows(step5MW, f2FLjoinMW) %>% distinct(`W.UNIQUE-ID`, M.MRNUMBER, .keep_all = T)
step6falseMW1 <- s1 %>% anti_join(s12, by = "W.UNIQUE-ID")
step6falseMW2 <- s2 %>% anti_join(s12, by = "M.MRNUMBER")

st <- step5MW 
st1 <- st %>% anti_join(step6SMW, by = "W.UNIQUE-ID") ### afairw apo to vhma 1 MW auta toy step 2 SMW pou exoun idio w, alla mesa mporei na uparxoun dhmpsieuseis pou den ginane idio join me to M sto step2 SMW kai to step1 MW
st2 <- st %>% anti_join(step6SMW, by = "M.MRNUMBER")
st12 <- st %>% anti_join(step6SMW, by = c("W.UNIQUE-ID", "M.MRNUMBER")) ## afairw apo to vhma 1 MW auta pou einai akrivws idia join tou step 2 SMW
step6falseMWtoSMW1 <- st12 %>% anti_join(st1, by = "W.UNIQUE-ID") ### vriskei ta wunique pou, sto prwto step MW to ekane join se allo tou math kai sto 2o step SMW se allo tou math
step6falseMWtoSMW2 <- st12 %>% anti_join(st2, by = "M.MRNUMBER")
################################################ 


step6M <- bind_rows(step5M, f2FLjoinM) %>% distinct(M.MRNUMBER, .keep_all = T) %>% anti_join(step6SMW, by = "M.MRNUMBER") %>% anti_join(step6SM, by = "M.MRNUMBER") %>% anti_join(step6MW, by = "M.MRNUMBER") %>% anti_join(step5SM, by = "M.MRNUMBER") %>% anti_join(step5MW, by = "M.MRNUMBER")
step6W <- bind_rows(step5W, f2FLjoinW) %>% distinct(`W.UNIQUE-ID`, .keep_all = T) %>% anti_join(step6SMW, by = "W.UNIQUE-ID") %>% anti_join(step6SW, by = "W.UNIQUE-ID") %>% anti_join(step6MW, by = "W.UNIQUE-ID") %>% anti_join(step5SW, by = "W.UNIQUE-ID") %>% anti_join(step5MW, by = "W.UNIQUE-ID")

duplsSInt <- intersect(duplsSInt, s6Dupls)
sum(sapply(list(step6SMW, step6SM, step6SW, step6S, duplsSInt), nrow)) ### pou einai idies grammes me to na afairousa eksarxhs sto step1 ta s2dupls mazi me ta sdupls

#step2SM2 <- bind_rows(step1SM, f3VILjoinSM) %>% distinct(S.EID, M.MRNUMBER, .keep_all = T) #%>% filter(n()==1)
#v <- nrow(pubsJournaldf)-nrow(union(s2Dupls,sDupls)) 
#v2 <- intersect(s2Dupls$S.EID,sDupls$S.EID)
duplsMInt <- intersect(duplsMInt, m6Dupls)
sum(sapply(list(step6SMW, step6SM, step6MW, step6M, duplsMInt), nrow))
duplsWInt <- intersect(duplsWInt, w6Dupls)
sum(sapply(list(step6SMW, step6MW, step6SW, step6W, duplsWInt), nrow))
s <- bind_rows(step6SMW, step6SM, step6SW, step6S, duplsSInt)
m <- bind_rows(step6SMW, step6SM, step6MW, step6M, duplsMInt)
w <- bind_rows(step6SMW, step6MW, step6SW, step6W, duplsWInt)
sum(duplicated(w$`W.UNIQUE-ID`))
sum(duplicated(s$S.EID))
sum(duplicated(m$M.MRNUMBER))


### afairpume apo tis vaseis tis dhmosieuseis pou perilamvanonti sto full join step6SMW ###


scopusStep7 <- pubsJournaldf%>%filter(!(S.EID %in% step6SMW$S.EID))
mathstep7 <- mathscidata%>%filter(!(M.MRNUMBER %in% step6SMW$M.MRNUMBER))
wosstep7 <- WOSdf%>%filter(!(`W.UNIQUE-ID` %in% step6SMW$`W.UNIQUE-ID`))





step6SMWmini <- step6SMW %>% select(S.TITLE, S.FIRST_PAGE, S.LAST_PAGE, S.VOLUME, S.NUMBER, S.YEAR, S.AUTHORS.COUNT, S.EID,
                                    M.TITLE, M.FIRST_PAGE, M.LAST_PAGE, M.VOLUME, M.NUMBER, M.YEAR, M.AUTHORS.COUNT, M.MRNUMBER,
                                    W.TITLE, W.FIRST_PAGE, W.LAST_PAGE, W.VOLUME, W.NUMBER, W.YEAR, W.AUTHORS.COUNT, `W.UNIQUE-ID`)
step6SMmini <- step6SM %>% select(S.TITLE, S.FIRST_PAGE, S.LAST_PAGE, S.VOLUME, S.NUMBER, S.YEAR, S.AUTHORS.COUNT, S.EID,
                                  M.TITLE, M.FIRST_PAGE, M.LAST_PAGE, M.VOLUME, M.NUMBER, M.YEAR, M.AUTHORS.COUNT, M.MRNUMBER,
                                  W.TITLE, W.FIRST_PAGE, W.LAST_PAGE, W.VOLUME, W.NUMBER, W.YEAR, W.AUTHORS.COUNT, `W.UNIQUE-ID`)
step6SWmini <- step6SW %>% select(S.TITLE, S.FIRST_PAGE, S.LAST_PAGE, S.VOLUME, S.NUMBER, S.YEAR, S.AUTHORS.COUNT, S.EID,
                                  M.TITLE, M.FIRST_PAGE, M.LAST_PAGE, M.VOLUME, M.NUMBER, M.YEAR, M.AUTHORS.COUNT, M.MRNUMBER,
                                  W.TITLE, W.FIRST_PAGE, W.LAST_PAGE, W.VOLUME, W.NUMBER, W.YEAR, W.AUTHORS.COUNT, `W.UNIQUE-ID`)
step6MWmini <- step6MW %>% select(S.TITLE, S.FIRST_PAGE, S.LAST_PAGE, S.VOLUME, S.NUMBER, S.YEAR, S.AUTHORS.COUNT, S.EID,
                              M.TITLE, M.FIRST_PAGE, M.LAST_PAGE, M.VOLUME, M.NUMBER, M.YEAR, M.AUTHORS.COUNT, M.MRNUMBER,
                              W.TITLE, W.FIRST_PAGE, W.LAST_PAGE, W.VOLUME, W.NUMBER, W.YEAR, W.AUTHORS.COUNT, `W.UNIQUE-ID`)

### author titles sugkrish, meta tha diwksw ta special , ta kanw mikra kai diwxnw kena mprosta pisw, tha epileksw to edit distance, kai tha
### vale ena thrashold kai an einai katw apo auto den milame gia ton idio author
### edit distance (levenstein)

### kai year (date month year), kai oti leipei apo volume


field_comparison_SMW <- function(df, colname){

      case_when((df[paste0("S.", colname)] == df[paste0("M.", colname)]) & (df[paste0("S.", colname)] == df[paste0("M.", colname)]) ~ "SAME",
                                                                 (df[paste0("S.", colname)] == df[paste0("M.", colname)]) & (df[paste0("S.", colname)] != df[paste0("W.", colname)]) ~ "WOS",
                                                                 (df[paste0("S.", colname)] == df[paste0("W.", colname)]) & (df[paste0("S.", colname)] != df[paste0("M.", colname)]) ~ "MATH",
                                                                 (df[paste0("M.", colname)] == df[paste0("W.", colname)]) & (df[paste0("S.", colname)] != df[paste0("M.", colname)]) ~ "SCOPUS",
                                                                is.na(df[paste0("M.", colname)]) & is.na(df[paste0("W.", colname)]) & is.na(df[paste0("S.", colname)]) ~ "NA",
                                                                is.na(df[paste0("M.", colname)]) & is.na(df[paste0("W.", colname)]) & !is.na(df[paste0("S.", colname)]) ~ "SCOPUS_ONLY",
                                                                is.na(df[paste0("M.", colname)]) & !is.na(df[paste0("W.", colname)]) & is.na(df[paste0("S.", colname)]) ~ "WOS_ONLY",
                                                                !is.na(df[paste0("M.", colname)]) & is.na(df[paste0("W.", colname)]) & is.na(df[paste0("S.", colname)]) ~ "MATH_ONLY",
                                                                (!is.na(df[paste0("M.", colname)]) & !is.na(df[paste0("W.", colname)]) & is.na(df[paste0("S.", colname)])) & (df[paste0("M.", colname)] == df[paste0("W.", colname)]) ~ "SCOPUS_NA & SAME",
                                                                (!is.na(df[paste0("M.", colname)]) & !is.na(df[paste0("W.", colname)]) & is.na(df[paste0("S.", colname)])) & (df[paste0("M.", colname)] != df[paste0("W.", colname)]) ~ "SCOPUS_NA & DIFF",
                                                                (!is.na(df[paste0("M.", colname)]) & is.na(df[paste0("W.", colname)]) & !is.na(df[paste0("S.", colname)])) & (df[paste0("S.", colname)] == df[paste0("M.", colname)]) ~ "WOS_NA & SAME",
                                                                (!is.na(df[paste0("M.", colname)]) & is.na(df[paste0("W.", colname)]) & !is.na(df[paste0("S.", colname)])) & (df[paste0("S.", colname)] != df[paste0("M.", colname)]) ~ "WOS_NA & DIFF",                                                
                                                                (is.na(df[paste0("M.", colname)]) & !is.na(df[paste0("W.", colname)]) & !is.na(df[paste0("S.", colname)])) & (df[paste0("S.", colname)] == df[paste0("W.", colname)]) ~ "MATH_NA & SAME",
                                                                (is.na(df[paste0("M.", colname)]) & !is.na(df[paste0("W.", colname)]) & !is.na(df[paste0("S.", colname)])) & (df[paste0("S.", colname)] != df[paste0("W.", colname)]) ~ "MATH_NA & DIFF",
                                                                 TRUE ~ "DIFF")
}



step6SMWmini$FIRST_PAGE <- field_comparison_SMW(step6SMWmini, "FIRST_PAGE")
step6SMWmini$LAST_PAGE <- field_comparison_SMW(step6SMWmini, "LAST_PAGE")
step6SMWmini$NUMBER <- field_comparison_SMW(step6SMWmini, "NUMBER")
step6SMWmini$YEAR <- field_comparison_SMW(step6SMWmini, "YEAR")
step6SMWmini$VOLUME <- field_comparison_SMW(step6SMWmini, "VOLUME")

sum(is.na(step6SMWmini$M.LAST_PAGE))

#step6SMWmini %>% filter(is.na(M.LAST_PAGE) & !(LAST_PAGE=="NA"))


check <- step6SMWmini %>%  filter(VOLUME=="MATH_NA & SAME") %>% select(S.VOLUME, M.VOLUME, W.VOLUME)
#check <- step6SMWmini %>%  filter(LAST_PAGE=="SCOPUS_ONLY") %>% select(S.LAST_PAGE, M.LAST_PAGE, W.LAST_PAGE)


field_comparison_SM <- function(df, colname){
      case_when(df[paste0("S.", colname)] == df[paste0("M.", colname)] ~ "SAME",
                is.na(df[paste0("S.", colname)]) & is.na(df[paste0("M.", colname)]) ~ "NA",
                is.na(df[paste0("S.", colname)]) & !is.na(df[paste0("M.", colname)]) ~ "SCOPUS_NA",
                !is.na(df[paste0("S.", colname)]) & is.na(df[paste0("M.", colname)]) ~ "MATH_NA",
                TRUE ~ "DIFF")
}

step6SMmini$FIRST_PAGE <- field_comparison_SM(step6SMmini, "FIRST_PAGE")
step6SMmini$LAST_PAGE <- field_comparison_SM(step6SMmini, "LAST_PAGE")
step6SMmini$NUMBER <- field_comparison_SM(step6SMmini, "NUMBER")
step6SMmini$YEAR <- field_comparison_SM(step6SMmini, "YEAR")
step6SMmini$VOLUME <- field_comparison_SM(step6SMmini, "VOLUME")

check <- step6SMmini %>%  filter(LEV=="DIFF") %>% select(S.LAST_PAGE, M.LAST_PAGE)

field_comparison_SW <- function(df, colname){
      case_when(df[paste0("S.", colname)] == df[paste0("W.", colname)] ~ "SAME",
                is.na(df[paste0("S.", colname)]) & is.na(df[paste0("W.", colname)]) ~ "NA",
                is.na(df[paste0("S.", colname)]) & !is.na(df[paste0("W.", colname)]) ~ "SCOPUS_NA",
                !is.na(df[paste0("S.", colname)]) & is.na(df[paste0("W.", colname)]) ~ "WOS_NA",
                TRUE ~ "DIFF")
}

step6SWmini$FIRST_PAGE <- field_comparison_SW(step6SWmini, "FIRST_PAGE")
step6SWmini$LAST_PAGE <- field_comparison_SW(step6SWmini, "LAST_PAGE")
step6SWmini$NUMBER <- field_comparison_SW(step6SWmini, "NUMBER")
step6SWmini$YEAR <- field_comparison_SW(step6SWmini, "YEAR")
step6SWmini$VOLUME <- field_comparison_SW(step6SWmini, "VOLUME")

#check <- step6SWmini %>%  filter(LEV=="DIFF") %>% select(S.LAST_PAGE, M.LAST_PAGE)

field_comparison_MW <- function(df, colname){
      case_when(df[paste0("M.", colname)] == df[paste0("W.", colname)] ~ "SAME",
                is.na(df[paste0("M.", colname)]) & is.na(df[paste0("W.", colname)]) ~ "NA",
                is.na(df[paste0("M.", colname)]) & !is.na(df[paste0("W.", colname)]) ~ "MATH_NA",
                !is.na(df[paste0("M.", colname)]) & is.na(df[paste0("W.", colname)]) ~ "WOS_NA",
                TRUE ~ "DIFF")
}

step6MWmini$FIRST_PAGE <- field_comparison_MW(step6MWmini, "FIRST_PAGE")
step6MWmini$LAST_PAGE <- field_comparison_MW(step6MWmini, "LAST_PAGE")
step6MWmini$NUMBER <- field_comparison_MW(step6MWmini, "NUMBER")
step6MWmini$YEAR <- field_comparison_MW(step6MWmini, "YEAR")
step6MWmini$VOLUME <- field_comparison_MW(step6MWmini, "VOLUME")

#check <- step6MWmini %>%  filter(LEV=="DIFF") %>% select(S.LAST_PAGE, M.LAST_PAGE)




# col_comparison <- function (df, colname) {
#       #myenc <- enquo(colname)
#       #colname <- "FIRST_PAGE"
#       #colname <- enquo(colname)
#       #get(check, colname)
#       #w <- within(check, print(colname))
#       #pars <- as.list(match.call()[-1])
#       df %>% mutate((!! sym(paste0("S.",colname)))=max(!! sym(paste0("S.",colname))))
# }
# 
# # case_when(({{paste0("S.", colname)}} == {{paste0("M.", colname)}}) & ({{paste0("S.", colname)}} == {{paste0("W.", colname)}}) ~ "SAME",
# #           TRUE ~ "DIFF"))
# col_comparison(check, "LAST_PAGE")
# 
# check %>% mutate(v=max(S.LAST_PAGE))
library(combinat)

fields5Same <- step6SMWmini[step6SMWmini[ ,"FIRST_PAGE"]=="SAME" & step6SMWmini[ ,"LAST_PAGE"]=="SAME" 
             & step6SMWmini[ ,"NUMBER"]=="SAME" & step6SMWmini[ ,"YEAR"]=="SAME" 
             & step6SMWmini[ ,"VOLUME"]=="SAME", ]


# fields5Same$FIRST_PAGE <- field_comparison_SMW(fields5Same, "FIRST_PAGE")
# fields5Same$LAST_PAGE <- field_comparison_SMW(fields5Same, "LAST_PAGE")
# fields5Same$NUMBER <- field_comparison_SMW(fields5Same, "NUMBER")
# fields5Same$YEAR <- field_comparison_SMW(fields5Same, "YEAR")
# fields5Same$VOLUME <- field_comparison_SMW(fields5Same, "VOLUME")
# 
# 
# sum(is.na(fields5Same$M.LAST_PAGE))

#step6SMWmini %>% filter(is.na(M.LAST_PAGE) & !(LAST_PAGE=="NA"))


check <- fields5Same %>%  filter(FIRST_PAGE=="SAME") %>% select(S.VOLUME, M.VOLUME, W.VOLUME)


fields <- combn(c("FIRST_PAGE", "LAST_PAGE", "NUMBER", "YEAR", "VOLUME"), 4)


fields4Same <- function(data ,field_vector) {
      data[data[, field_vector[1]]=="SAME" & data[, field_vector[2]]=="SAME" 
      & data[, field_vector[3]]=="SAME" & data[, field_vector[4]]=="SAME", ]
}

remaining <- step6SMWmini %>%  anti_join(fields5Same)
fields4SameFLNY <- fields4Same(remaining, fields[,1])
check <- fields4SameFLNY %>%  filter(VOLUME=="SCOPUS") %>% select(S.VOLUME, M.VOLUME, W.VOLUME)

remaining <- remaining %>% anti_join(fields4SameFLNY)
fields4SameFLNV <- fields4Same(remaining, fields[,2])
check <- fields4SameFLNV %>%  filter(YEAR=="SCOPUS") %>% select(S.YEAR, M.YEAR, W.YEAR)

remaining <- remaining %>% anti_join(fields4SameFLNV)
fields4SameFLYV <- fields4Same(remaining, fields[,3])
check <- fields4SameFLYV %>%  filter(NUMBER=="MATH_NA & SAME") %>% select(S.NUMBER, M.NUMBER, W.NUMBER)

remaining <- remaining %>% anti_join(fields4SameFLYV)
fields4SameFNYV <- fields4Same(remaining, fields[,4])
check <- fields4SameFNYV %>%  filter(LAST_PAGE=="MATH_NA & DIFF") %>% select(S.LAST_PAGE, M.LAST_PAGE, W.LAST_PAGE)

remaining <- remaining %>% anti_join(fields4SameFNYV)
fields4SameLNYV <- fields4Same(remaining, fields[,5])
check <- fields4SameLNYV %>%  filter(FIRST_PAGE=="SCOPUS") %>% select(S.FIRST_PAGE, M.FIRST_PAGE, W.FIRST_PAGE)

fields <- combn(c("FIRST_PAGE", "LAST_PAGE", "NUMBER", "YEAR", "VOLUME"), 3)

fields3Same <- function(data ,field_vector) {
      data[data[, field_vector[1]]=="SAME" & data[, field_vector[2]]=="SAME" 
           & data[, field_vector[3]]=="SAME", ]
}

remaining <- remaining %>% anti_join(fields4SameLNYV)
fields3SameFLN <- fields3Same(remaining, fields[,1])

remaining <- remaining %>% anti_join(fields3SameFLN)
fields3SameFLY <- fields3Same(remaining, fields[,2])
check <- fields3SameFLY %>%  filter(NUMBER=="SCOPUS") %>% select(S.NUMBER, M.NUMBER, W.NUMBER)
check <- fields3SameFLY %>%  filter(VOLUME=="SCOPUS") %>% select(S.VOLUME, M.VOLUME, W.VOLUME)

remaining <- remaining %>% anti_join(fields3SameFLY)
fields3SameFLV <- fields3Same(remaining, fields[,3])

remaining <- remaining %>% anti_join(fields3SameFLV)
fields3SameFNY <- fields3Same(remaining, fields[,4])

remaining <- remaining %>% anti_join(fields3SameFNY)
fields3SameFNV <- fields3Same(remaining, fields[,5])

remaining <- remaining %>% anti_join(fields3SameFNV)
fields3SameFYV <- fields3Same(remaining, fields[,6])
check <- fields3SameFYV %>%  filter(NUMBER=="MATH_NA & SAME") %>% select(S.NUMBER, M.NUMBER, W.NUMBER)
check <- fields3SameFYV %>%  filter(LAST_PAGE=="DIFF") %>% select(S.LAST_PAGE, M.LAST_PAGE, W.LAST_PAGE)


remaining <- remaining %>% anti_join(fields3SameFYV)
fields3SameLNY <- fields3Same(remaining, fields[,7])

remaining <- remaining %>% anti_join(fields3SameLNY)
fields3SameLNV <- fields3Same(remaining, fields[,8])

remaining <- remaining %>% anti_join(fields3SameLNV)
fields3SameLYV <- fields3Same(remaining, fields[,9]) ################## edw teleiwnoun ta remaining gia to step6SMW
check <- fields3SameLYV %>%  filter(NUMBER=="WOS_ONLY") %>% select(S.NUMBER, M.NUMBER, W.NUMBER)
check <- fields3SameLYV %>%  filter(FIRST_PAGE=="MATH") %>% select(S.FIRST_PAGE, M.FIRST_PAGE, W.FIRST_PAGE)


remaining <- remaining %>% anti_join(fields3SameLYV)
fields3SameNYV <- fields3Same(remaining, fields[,10])

fields <- combn(c("FIRST_PAGE", "LAST_PAGE", "NUMBER", "YEAR", "VOLUME"), 2)

fields2Same <- function(data ,field_vector) {
      data[data[, field_vector[1]]=="SAME" & data[, field_vector[2]]=="SAME", ]
}

remaining <- remaining %>% anti_join(fields3SameNYV)
fields2SameFL <- fields2Same(remaining, fields[,1])

remaining <- remaining %>% anti_join(fields2SameFL)
fields2SameFN <- fields2Same(remaining, fields[,2])

remaining <- remaining %>% anti_join(fields2SameFN)
fields2SameFY <- fields2Same(remaining, fields[,3])

remaining <- remaining %>% anti_join(fields2SameFY)
fields2SameFV <- fields2Same(remaining, fields[,4])

remaining <- remaining %>% anti_join(fields2SameFV)
fields2SameLN <- fields2Same(remaining, fields[,5])

remaining <- remaining %>% anti_join(fields2SameLN)
fields2SameLY <- fields2Same(remaining, fields[,6])

remaining <- remaining %>% anti_join(fields2SameLY)
fields2SameLV <- fields2Same(remaining, fields[,7])

remaining <- remaining %>% anti_join(fields2SameLV)
fields2SameNY <- fields2Same(remaining, fields[,8])

remaining <- remaining %>% anti_join(fields2SameNY)
fields2SameNV <- fields2Same(remaining, fields[,9])

remaining <- remaining %>% anti_join(fields2SameNV)
fields2SameYV <- fields2Same(remaining, fields[,10])

remaining <- remaining %>% anti_join(fields2SameYV)

#################################################################### SM1970 ####################################################################

step6SM1970 <- step6SMmini %>% filter(S.YEAR < 1970) ### to idio vgainei kai an M.YEAR < 1970

fields5Same <- step6SM1970[step6SM1970[ ,"FIRST_PAGE"]=="SAME" & step6SM1970[ ,"LAST_PAGE"]=="SAME" 
                            & step6SM1970[ ,"NUMBER"]=="SAME" & step6SM1970[ ,"YEAR"]=="SAME" 
                            & step6SM1970[ ,"VOLUME"]=="SAME", ]

fields <- combn(c("FIRST_PAGE", "LAST_PAGE", "NUMBER", "YEAR", "VOLUME"), 4)


fields4Same <- function(data ,field_vector) {
      data[data[, field_vector[1]]=="SAME" & data[, field_vector[2]]=="SAME" 
           & data[, field_vector[3]]=="SAME" & data[, field_vector[4]]=="SAME", ]
}

remaining <- step6SM1970 %>%  anti_join(fields5Same)
fields4SameFLNY <- fields4Same(remaining, fields[,1])

remaining <- remaining %>% anti_join(fields4SameFLNY)
fields4SameFLNV <- fields4Same(remaining, fields[,2])

remaining <- remaining %>% anti_join(fields4SameFLNV)
fields4SameFLYV <- fields4Same(remaining, fields[,3])
check <- fields4SameFLYV %>%  filter(NUMBER=="MATH_NA") %>% select(S.NUMBER, M.NUMBER)

remaining <- remaining %>% anti_join(fields4SameFLYV)
fields4SameFNYV <- fields4Same(remaining, fields[,4])
check <- fields4SameFNYV %>%  filter(LAST_PAGE=="DIFF") %>% select(S.LAST_PAGE, M.LAST_PAGE)

remaining <- remaining %>% anti_join(fields4SameFNYV)
fields4SameLNYV <- fields4Same(remaining, fields[,5])
check <- fields4SameLNYV %>%  filter(FIRST_PAGE=="DIFF") %>% select(S.FIRST_PAGE, M.FIRST_PAGE)

fields <- combn(c("FIRST_PAGE", "LAST_PAGE", "NUMBER", "YEAR", "VOLUME"), 3)

fields3Same <- function(data ,field_vector) {
      data[data[, field_vector[1]]=="SAME" & data[, field_vector[2]]=="SAME" 
           & data[, field_vector[3]]=="SAME", ]
}

remaining <- remaining %>% anti_join(fields4SameLNYV)
fields3SameFLN <- fields3Same(remaining, fields[,1])

remaining <- remaining %>% anti_join(fields3SameFLN)
fields3SameFLY <- fields3Same(remaining, fields[,2])

remaining <- remaining %>% anti_join(fields3SameFLY)
fields3SameFLV <- fields3Same(remaining, fields[,3])

remaining <- remaining %>% anti_join(fields3SameFLV)
fields3SameFNY <- fields3Same(remaining, fields[,4])

remaining <- remaining %>% anti_join(fields3SameFNY)
fields3SameFNV <- fields3Same(remaining, fields[,5])

remaining <- remaining %>% anti_join(fields3SameFNV)
fields3SameFYV <- fields3Same(remaining, fields[,6])

remaining <- remaining %>% anti_join(fields3SameFYV)
fields3SameLNY <- fields3Same(remaining, fields[,7])

remaining <- remaining %>% anti_join(fields3SameLNY)
fields3SameLNV <- fields3Same(remaining, fields[,8])

remaining <- remaining %>% anti_join(fields3SameLNV)
fields3SameLYV <- fields3Same(remaining, fields[,9]) ################## edw teleiwnoun ta remaining gia to step6SM1970

remaining <- remaining %>% anti_join(fields3SameLYV)
fields3SameNYV <- fields3Same(remaining, fields[,10])

fields <- combn(c("FIRST_PAGE", "LAST_PAGE", "NUMBER", "YEAR", "VOLUME"), 2)

fields2Same <- function(data ,field_vector) {
      data[data[, field_vector[1]]=="SAME" & data[, field_vector[2]]=="SAME", ]
}

remaining <- remaining %>% anti_join(fields3SameNYV)
fields2SameFL <- fields2Same(remaining, fields[,1])

remaining <- remaining %>% anti_join(fields2SameFL)
fields2SameFN <- fields2Same(remaining, fields[,2])

remaining <- remaining %>% anti_join(fields2SameFN)
fields2SameFY <- fields2Same(remaining, fields[,3])

remaining <- remaining %>% anti_join(fields2SameFY)
fields2SameFV <- fields2Same(remaining, fields[,4])

remaining <- remaining %>% anti_join(fields2SameFV)
fields2SameLN <- fields2Same(remaining, fields[,5])

remaining <- remaining %>% anti_join(fields2SameLN)
fields2SameLY <- fields2Same(remaining, fields[,6])

remaining <- remaining %>% anti_join(fields2SameLY)
fields2SameLV <- fields2Same(remaining, fields[,7])

remaining <- remaining %>% anti_join(fields2SameLV)
fields2SameNY <- fields2Same(remaining, fields[,8])

remaining <- remaining %>% anti_join(fields2SameNY)
fields2SameNV <- fields2Same(remaining, fields[,9])

remaining <- remaining %>% anti_join(fields2SameNV)
fields2SameYV <- fields2Same(remaining, fields[,10])

remaining <- remaining %>% anti_join(fields2SameYV)

#####################################################################################################


############################################# SM >= 1970 #####################################################

step6SM19702 <- step6SMmini %>% filter(S.YEAR >= 1970) 

sum(is.na(step6SMmini$M.YEAR))

fields5Same <- step6SM19702[step6SM19702[ ,"FIRST_PAGE"]=="SAME" & step6SM19702[ ,"LAST_PAGE"]=="SAME" # 6
                           & step6SM19702[ ,"NUMBER"]=="SAME" & step6SM19702[ ,"YEAR"]=="SAME" 
                           & step6SM19702[ ,"VOLUME"]=="SAME", ]

fields <- combn(c("FIRST_PAGE", "LAST_PAGE", "NUMBER", "YEAR", "VOLUME"), 4)


fields4Same <- function(data ,field_vector) {
      data[data[, field_vector[1]]=="SAME" & data[, field_vector[2]]=="SAME" 
           & data[, field_vector[3]]=="SAME" & data[, field_vector[4]]=="SAME", ]
}

remaining <- step6SM19702 %>%  anti_join(fields5Same)
fields4SameFLNY <- fields4Same(remaining, fields[,1])

remaining <- remaining %>% anti_join(fields4SameFLNY)
fields4SameFLNV <- fields4Same(remaining, fields[,2])

remaining <- remaining %>% anti_join(fields4SameFLNV)
fields4SameFLYV <- fields4Same(remaining, fields[,3])
check <- fields4SameFLYV %>%  filter(NUMBER=="MATH_NA") %>% select(S.NUMBER, M.NUMBER) # 42 

remaining <- remaining %>% anti_join(fields4SameFLYV)
fields4SameFNYV <- fields4Same(remaining, fields[,4])
check <- fields4SameFNYV %>%  filter(LAST_PAGE=="DIFF") %>% select(S.LAST_PAGE, M.LAST_PAGE)

remaining <- remaining %>% anti_join(fields4SameFNYV)
fields4SameLNYV <- fields4Same(remaining, fields[,5])
check <- fields4SameLNYV %>%  filter(FIRST_PAGE=="DIFF") %>% select(S.FIRST_PAGE, M.FIRST_PAGE)

fields <- combn(c("FIRST_PAGE", "LAST_PAGE", "NUMBER", "YEAR", "VOLUME"), 3)

fields3Same <- function(data ,field_vector) {
      data[data[, field_vector[1]]=="SAME" & data[, field_vector[2]]=="SAME" 
           & data[, field_vector[3]]=="SAME", ]
}

remaining <- remaining %>% anti_join(fields4SameLNYV)
fields3SameFLN <- fields3Same(remaining, fields[,1])

remaining <- remaining %>% anti_join(fields3SameFLN)
fields3SameFLY <- fields3Same(remaining, fields[,2])

remaining <- remaining %>% anti_join(fields3SameFLY)
fields3SameFLV <- fields3Same(remaining, fields[,3])

remaining <- remaining %>% anti_join(fields3SameFLV)
fields3SameFNY <- fields3Same(remaining, fields[,4])

remaining <- remaining %>% anti_join(fields3SameFNY)
fields3SameFNV <- fields3Same(remaining, fields[,5])

remaining <- remaining %>% anti_join(fields3SameFNV)
fields3SameFYV <- fields3Same(remaining, fields[,6])
check <- fields3SameFYV %>% filter(FIRST_PAGE=="DIFF") %>% select(S.FIRST_PAGE, M.FIRST_PAGE) # 1

remaining <- remaining %>% anti_join(fields3SameFYV)
fields3SameLNY <- fields3Same(remaining, fields[,7])

remaining <- remaining %>% anti_join(fields3SameLNY)
fields3SameLNV <- fields3Same(remaining, fields[,8])

remaining <- remaining %>% anti_join(fields3SameLNV)
fields3SameLYV <- fields3Same(remaining, fields[,9])

################################ SW #####################################


fields5Same <- step6SWmini[step6SWmini[ ,"FIRST_PAGE"]=="SAME" & step6SWmini[ ,"LAST_PAGE"]=="SAME" # 12
                            & step6SWmini[ ,"NUMBER"]=="SAME" & step6SWmini[ ,"YEAR"]=="SAME" 
                            & step6SWmini[ ,"VOLUME"]=="SAME", ]

fields <- combn(c("FIRST_PAGE", "LAST_PAGE", "NUMBER", "YEAR", "VOLUME"), 4)


fields4Same <- function(data ,field_vector) {
      data[data[, field_vector[1]]=="SAME" & data[, field_vector[2]]=="SAME" 
           & data[, field_vector[3]]=="SAME" & data[, field_vector[4]]=="SAME", ]
}

remaining <- step6SWmini %>%  anti_join(fields5Same)
fields4SameFLNY <- fields4Same(remaining, fields[,1])

remaining <- remaining %>% anti_join(fields4SameFLNY)
fields4SameFLNV <- fields4Same(remaining, fields[,2])

remaining <- remaining %>% anti_join(fields4SameFLNV)
fields4SameFLYV <- fields4Same(remaining, fields[,3])
check <- fields4SameFLYV %>%  filter(NUMBER=="SCOPUS_NA") %>% select(S.NUMBER, M.NUMBER) # 16

remaining <- remaining %>% anti_join(fields4SameFLYV)
fields4SameFNYV <- fields4Same(remaining, fields[,4])
check <- fields4SameFNYV %>%  filter(LAST_PAGE=="NA") %>% select(S.LAST_PAGE, M.LAST_PAGE) # 4

remaining <- remaining %>% anti_join(fields4SameFNYV)
fields4SameLNYV <- fields4Same(remaining, fields[,5])
check <- fields4SameLNYV %>%  filter(FIRST_PAGE=="DIFF") %>% select(S.FIRST_PAGE, M.FIRST_PAGE)

fields <- combn(c("FIRST_PAGE", "LAST_PAGE", "NUMBER", "YEAR", "VOLUME"), 3)

fields3Same <- function(data ,field_vector) {
      data[data[, field_vector[1]]=="SAME" & data[, field_vector[2]]=="SAME" 
           & data[, field_vector[3]]=="SAME", ]
}

remaining <- remaining %>% anti_join(fields4SameLNYV)
fields3SameFLN <- fields3Same(remaining, fields[,1])

remaining <- remaining %>% anti_join(fields3SameFLN)
fields3SameFLY <- fields3Same(remaining, fields[,2])

remaining <- remaining %>% anti_join(fields3SameFLY)
fields3SameFLV <- fields3Same(remaining, fields[,3])

remaining <- remaining %>% anti_join(fields3SameFLV)
fields3SameFNY <- fields3Same(remaining, fields[,4])

remaining <- remaining %>% anti_join(fields3SameFNY)
fields3SameFNV <- fields3Same(remaining, fields[,5])

remaining <- remaining %>% anti_join(fields3SameFNV)
fields3SameFYV <- fields3Same(remaining, fields[,6])
check <- fields3SameFYV %>% filter(LAST_PAGE=="WOS_NA") %>% select(S.LAST_PAGE, M.LAST_PAGE) # 6

remaining <- remaining %>% anti_join(fields3SameFYV)
fields3SameLNY <- fields3Same(remaining, fields[,7])

remaining <- remaining %>% anti_join(fields3SameLNY)
fields3SameLNV <- fields3Same(remaining, fields[,8])

remaining <- remaining %>% anti_join(fields3SameLNV)
fields3SameLYV <- fields3Same(remaining, fields[,9])

################################# MW ####################################

fields5Same <- step6MWmini[step6MWmini[ ,"FIRST_PAGE"]=="SAME" & step6MWmini[ ,"LAST_PAGE"]=="SAME" # 95
                           & step6MWmini[ ,"NUMBER"]=="SAME" & step6MWmini[ ,"YEAR"]=="SAME" 
                           & step6MWmini[ ,"VOLUME"]=="SAME", ]

fields <- combn(c("FIRST_PAGE", "LAST_PAGE", "NUMBER", "YEAR", "VOLUME"), 4)


fields4Same <- function(data ,field_vector) {
      data[data[, field_vector[1]]=="SAME" & data[, field_vector[2]]=="SAME" 
           & data[, field_vector[3]]=="SAME" & data[, field_vector[4]]=="SAME", ]
}

remaining <- step6MWmini %>%  anti_join(fields5Same)
fields4SameFLNY <- fields4Same(remaining, fields[,1])

remaining <- remaining %>% anti_join(fields4SameFLNY)
fields4SameFLNV <- fields4Same(remaining, fields[,2])

remaining <- remaining %>% anti_join(fields4SameFLNV)
fields4SameFLYV <- fields4Same(remaining, fields[,3])
check <- fields4SameFLYV %>%  filter(NUMBER=="NA") %>% select(S.NUMBER, M.NUMBER) # 46

remaining <- remaining %>% anti_join(fields4SameFLYV)
fields4SameFNYV <- fields4Same(remaining, fields[,4])
check <- fields4SameFNYV %>%  filter(LAST_PAGE=="DIFF") %>% select(S.LAST_PAGE, M.LAST_PAGE) # 14

remaining <- remaining %>% anti_join(fields4SameFNYV)
fields4SameLNYV <- fields4Same(remaining, fields[,5])
check <- fields4SameLNYV %>%  filter(FIRST_PAGE=="DIFF") %>% select(S.FIRST_PAGE, M.FIRST_PAGE) # 2

fields <- combn(c("FIRST_PAGE", "LAST_PAGE", "NUMBER", "YEAR", "VOLUME"), 3)

fields3Same <- function(data ,field_vector) {
      data[data[, field_vector[1]]=="SAME" & data[, field_vector[2]]=="SAME" 
           & data[, field_vector[3]]=="SAME", ]
}

remaining <- remaining %>% anti_join(fields4SameLNYV)
fields3SameFLN <- fields3Same(remaining, fields[,1])

remaining <- remaining %>% anti_join(fields3SameFLN)
fields3SameFLY <- fields3Same(remaining, fields[,2])

remaining <- remaining %>% anti_join(fields3SameFLY)
fields3SameFLV <- fields3Same(remaining, fields[,3]) # 2

remaining <- remaining %>% anti_join(fields3SameFLV)
fields3SameFNY <- fields3Same(remaining, fields[,4]) 

remaining <- remaining %>% anti_join(fields3SameFNY)
fields3SameFNV <- fields3Same(remaining, fields[,5])

remaining <- remaining %>% anti_join(fields3SameFNV)
fields3SameFYV <- fields3Same(remaining, fields[,6])
check <- fields3SameFYV %>% filter(LAST_PAGE=="WOS_NA") %>% select(S.LAST_PAGE, M.LAST_PAGE) # 33
check <- fields3SameFYV %>% filter(NUMBER=="MATH_NA") %>% select(S.NUMBER, M.NUMBER)

remaining <- remaining %>% anti_join(fields3SameFYV)
fields3SameLNY <- fields3Same(remaining, fields[,7])

remaining <- remaining %>% anti_join(fields3SameLNY)
fields3SameLNV <- fields3Same(remaining, fields[,8])

remaining <- remaining %>% anti_join(fields3SameLNV)
fields3SameLYV <- fields3Same(remaining, fields[,9])

############################################################################

######### check for  NA the singles #########

sapply(c("S.TITLE", "S.VOLUME", "S.YEAR", "S.NUMBER", "S.FIRST_PAGE", "S.LAST_PAGE"), function(t) sum(is.na(step6S[,t])))
sapply(c("W.TITLE" , "W.VOLUME", "W.YEAR", "W.NUMBER", "W.FIRST_PAGE", "W.LAST_PAGE"), function(t) sum(is.na(step6W[,t])))
sapply(c("M.TITLE", "M.VOLUME", "M.YEAR", "M.NUMBER", "M.FIRST_PAGE", "M.LAST_PAGE"), function(t) sum(is.na(step6M[,t])))

##############################################



####################### title comparison ################################

#### title comparison for SMW using lv method ####      
threshold <- 0.5
step6SMWmini[,c("S.TITLE_LENGTH", "M.TITLE_LENGTH", "W.TITLE_LENGTH")] <- sapply(list(step6SMWmini$S.TITLE, step6SMWmini$M.TITLE, step6SMWmini$W.TITLE), str_length)
step6SMWmini$LEV_SM <- stringdist(step6SMWmini[["S.TITLE"]], step6SMWmini[["M.TITLE"]], method = "lv")/pmax(step6SMWmini[["S.TITLE_LENGTH"]], step6SMWmini[["M.TITLE_LENGTH"]]) < threshold
step6SMWmini$LEV_SW <- stringdist(step6SMWmini[["S.TITLE"]], step6SMWmini[["W.TITLE"]], method = "lv")/pmax(step6SMWmini[["S.TITLE_LENGTH"]], step6SMWmini[["W.TITLE_LENGTH"]]) < threshold
step6SMWmini$LEV_MW <- stringdist(step6SMWmini[["M.TITLE"]], step6SMWmini[["W.TITLE"]], method = "lv")/pmax(step6SMWmini[["M.TITLE_LENGTH"]], step6SMWmini[["W.TITLE_LENGTH"]]) < threshold
step6SMWmini$LEV <- case_when(step6SMWmini$LEV_SM  & step6SMWmini$LEV_MW ~ "SAME",
                              step6SMWmini$LEV_SM & !step6SMWmini$LEV_SW ~ "WOS",
                              step6SMWmini$LEV_SW & !step6SMWmini$LEV_SM ~ "MATH",
                              step6SMWmini$LEV_MW & !step6SMWmini$LEV_SM ~ "SCOPUS",
                              TRUE ~ "DIFF")
step6SMWmini <- step6SMWmini %>% select(-c(LEV_SM, LEV_SW, LEV_MW))

check <- step6SMWmini %>%  filter(LEV=="DIFF") %>% select(S.TITLE, M.TITLE, W.TITLE)


###### sto smw ana duo upologizw average lev ##########

step6SMWmini[,c("S.TITLE_LENGTH", "M.TITLE_LENGTH", "W.TITLE_LENGTH")] <- sapply(list(step6SMWmini$S.TITLE, step6SMWmini$M.TITLE, step6SMWmini$W.TITLE), str_length)
step6SMWmini$LEV_SM <- stringdist(step6SMWmini[["S.TITLE"]], step6SMWmini[["M.TITLE"]], method = "lv")/pmax(step6SMWmini[["S.TITLE_LENGTH"]], step6SMWmini[["M.TITLE_LENGTH"]]) 
step6SMWmini$LEV_SW <- stringdist(step6SMWmini[["S.TITLE"]], step6SMWmini[["W.TITLE"]], method = "lv")/pmax(step6SMWmini[["S.TITLE_LENGTH"]], step6SMWmini[["W.TITLE_LENGTH"]]) 
step6SMWmini$LEV_MW <- stringdist(step6SMWmini[["M.TITLE"]], step6SMWmini[["W.TITLE"]], method = "lv")/pmax(step6SMWmini[["M.TITLE_LENGTH"]], step6SMWmini[["W.TITLE_LENGTH"]]) 
step6SMWmini$LEV_AVE <- apply(step6SMWmini[,c("LEV_SM", "LEV_SW", "LEV_MW")], 1, function(t) sum(t)/3)
#step6SMWmini <-  step6SMWmini %>% mutate(LEV_AVE2 = rowSums(step6SMWmini[,c("LEV_SM", "LEV_SW", "LEV_MW")])/3)
#step6SMWmini <-  step6SMWmini %>% rowwise() %>% mutate(LEV_AVE3 = sum(c_across(c("LEV_SM", "LEV_SW", "LEV_MW")))/3)
step6SMWmini <- step6SMWmini %>% select(-c(LEV_SM, LEV_SW, LEV_MW, LEV_AVE2, LEV_AVE3))


#### title comparison for duos using lv method ####
step6SM1970[,c("S.TITLE_LENGTH", "M.TITLE_LENGTH")] <- sapply(list(step6SM1970$S.TITLE, step6SM1970$M.TITLE), str_length)
step6SM1970$LEV <- stringdist(step6SM1970$S.TITLE, step6SM1970$M.TITLE, method = "lv")/pmax(step6SM1970$S.TITLE_LENGTH, step6SM1970$M.TITLE_LENGTH) #### sta duos eixa opws kai sto smw pio panw, threshold!
# step6SM1970$LEV <- case_when(step6SM1970$LEV ~ "SAME",
#                              TRUE ~ "DIFF")


step6SM19702[,c("S.TITLE_LENGTH", "M.TITLE_LENGTH")] <- sapply(list(step6SM19702$S.TITLE, step6SM19702$M.TITLE), str_length)
step6SM19702$LEV <- stringdist(step6SM19702$S.TITLE, step6SM19702$M.TITLE, method = "lv")/pmax(step6SM19702$S.TITLE_LENGTH, step6SM19702$M.TITLE_LENGTH)

step6SWmini[,c("S.TITLE_LENGTH", "W.TITLE_LENGTH")] <- sapply(list(step6SWmini$S.TITLE, step6SWmini$W.TITLE), str_length)
step6SWmini$LEV <- stringdist(step6SWmini$S.TITLE, step6SWmini$W.TITLE, method = "lv")/pmax(step6SWmini$S.TITLE_LENGTH, step6SWmini$W.TITLE_LENGTH) 
# step6SWmini$LEV <- case_when(step6SWmini$LEV ~ "SAME",
#                              TRUE ~ "DIFF")

step6MWmini[,c("M.TITLE_LENGTH", "W.TITLE_LENGTH")] <- sapply(list(step6MWmini$M.TITLE, step6MWmini$W.TITLE), str_length)
step6MWmini$LEV <- stringdist(step6MWmini$M.TITLE, step6MWmini$W.TITLE, method = "lv")/pmax(step6MWmini$M.TITLE_LENGTH, step6MWmini$W.TITLE_LENGTH) 
# step6MWmini$LEV <- case_when(step6MWmini$LEV ~ "SAME",
#                              TRUE ~ "DIFF")


#### raw title comparison for SMW and SM ####

step6SMWmini$TITLE <- case_when((step6SMWmini$S.TITLE == step6SMWmini$M.TITLE) & (step6SMWmini$S.TITLE == step6SMWmini$W.TITLE) ~ "SAME",
                                (step6SMWmini$S.TITLE == step6SMWmini$M.TITLE) & (step6SMWmini$S.TITLE != step6SMWmini$W.TITLE) ~ "WOS",
                                (step6SMWmini$S.TITLE == step6SMWmini$W.TITLE) & (step6SMWmini$S.TITLE != step6SMWmini$M.TITLE)  ~ "MATH",
                                (step6SMWmini$M.TITLE == step6SMWmini$W.TITLE) & (step6SMWmini$M.TITLE != step6SMWmini$S.TITLE) ~ "SCOPUS" ,
                                TRUE ~ "DIFF")

check <- step6SMWmini %>% filter(TITLE == "SAME") %>% select(S.TITLE, M.TITLE, W.TITLE)

step6SMmini$TITLE <- case_when(step6SMmini$S.TITLE == step6SMmini$M.TITLE ~ "SAME",
                               TRUE ~ "DIFF")

check <- step6SMmini %>% filter(TITLE == "SAME") %>% select(S.TITLE, M.TITLE, W.TITLE)
############################## Authors Comparison #########################



nrow(filter(step6SMWmini, M.AUTHORS.COUNT>W.AUTHORS.COUNT & M.AUTHORS.COUNT>S.AUTHORS.COUNT))
nrow(filter(step6SMWmini, S.AUTHORS.COUNT>M.AUTHORS.COUNT & S.AUTHORS.COUNT>W.AUTHORS.COUNT))
nrow(filter(step6SMWmini, W.AUTHORS.COUNT>S.AUTHORS.COUNT & W.AUTHORS.COUNT>M.AUTHORS.COUNT))


paste0("M.AUTHOR", 1:mathscidata$M.AUTHORS.COUNT[1])

sum(pubsJournaldf$S.AUTHORS.COUNT==0)

v <- step6SMWmini %>% filter(S.AUTHORS.COUNT==0) %>% select(S.TITLE, M.TITLE,W.TITLE)

for (i in 3:3) { #nrow(step6SMWmini)
      
      if (step6SMWmini$S.AUTHORS.COUNT[i] != 0) {
            s <- step6SMWmini$S.EID[i]
            m <- step6SMWmini$M.MRNUMBER[i]
            w <- step6SMWmini$`W.UNIQUE-ID`[i]
            
            snames <- unname(unlist(mini_author_df %>% filter(pub_eid==s) %>% select(name))) #### epishs ginetai apla me to pull !
            mnames <- unname(unlist(mathscidata %>% filter(M.MRNUMBER==m) %>% select(paste0("M.AUTHOR", 1:mathscidata$M.AUTHORS.COUNT[i]))))
            mnames <- unname(unlist(WOSdf %>% filter(`W.UNIQUE-ID`==w) %>% select(paste0("W.AUTHOR", 1:WOSdf$W.AUTHORS.COUNT[i]))))
      }
      
}


############################ authors comparison ############################

sum(step6SMWmini$M.AUTHORS.COUNT==0)

check <- step6SMWmini %>% mutate(AUTHORS.COUNT=case_when((S.AUTHORS.COUNT==M.AUTHORS.COUNT) & (S.AUTHORS.COUNT==W.AUTHORS.COUNT) ~ "SAME",
                                                        (S.AUTHORS.COUNT > M.AUTHORS.COUNT) & (S.AUTHORS.COUNT > W.AUTHORS.COUNT) ~ "SCOPUS_MORE",
                                                        (M.AUTHORS.COUNT > S.AUTHORS.COUNT) & (M.AUTHORS.COUNT > W.AUTHORS.COUNT) ~ "MATH_MORE",
                                                        (W.AUTHORS.COUNT > S.AUTHORS.COUNT) & (W.AUTHORS.COUNT > M.AUTHORS.COUNT) ~ "WOS_MORE")) %>% 
      select(S.AUTHORS.COUNT, M.AUTHORS.COUNT, W.AUTHORS.COUNT, AUTHORS.COUNT)

check <- step6SMmini %>% mutate(AUTHORS.COUNT=case_when((S.AUTHORS.COUNT==M.AUTHORS.COUNT) ~ "SAME",
                                                         (S.AUTHORS.COUNT > M.AUTHORS.COUNT) ~ "SCOPUS_MORE",
                                                         (M.AUTHORS.COUNT > S.AUTHORS.COUNT) ~ "MATH_MORE")) %>% 
      select(S.AUTHORS.COUNT, M.AUTHORS.COUNT, AUTHORS.COUNT)



####################### some changes ##################


pubsJournaldfnew <- pubsJournaldf %>% select(-c(1,2, 3, 19, 24,30:39)) # to 3 einai to url
mini_author_df_new <- mini_author_df %>% select(-c(1:3))
mini_affil_df_new <- mini_affil_df %>% select(-c(1:2))

names(pubsJournaldf)[3]

ref_df_new <- ref_df %>% rename(pub_index = seq) %>% relocate(pub_index)
ref_author_df_new <- ref_author_df %>% select(-c(3, 6:8)) %>% rename(pub_index = `@seq`)

citedby_dfnew <- citedby_df %>% select(-c(1,28:30, 33, 35)) %>% rename(pub_index = seq) %>% relocate(pub_index)%>% select(-`prism:url`)
citedby_author_df_new <- citedby_author_df %>% select(-c(1:3, 9, 11, 12)) %>% rename( afid = `afid.$`) # to 3 einai to url
citedby_affil_df_new <-  citedby_affil_df %>% select(-c(1,2,7))

authorsPubs_df_new <- authorsPubs_df %>% select(-c(1:2, 14, 30:32, 35)) %>% rename(pub_index = seq) %>% relocate(pub_index)
authorpubs_author_df_new <- authorpubs_author_df %>% select(-c(1:3, 5, 10, 12)) %>% rename( afid = `afid.$`)
authorpubs_affil_df_new <- authorpubs_affil_df %>% select(-c(1,2,7))

names(pubsJournaldfnew)
names(mini_affil_df)
names(citedby_dfnew)
as_tibble(pubsJournaldfnew)
as_tibble(mini_author_df_new)
as_tibble(mini_affil_df_new)
as_tibble(ref_df_new)
as_tibble(ref_author_df_new)
as_tibble(citedby_dfnew)
as_tibble(citedby_author_df_new)
as_tibble(citedby_affil_df_new)
as_tibble(authorsPubs_df_new)
as_tibble(authorpubs_author_df_new)
as_tibble(authorpubs_affil_df_new)

mini_author_df$afid[1]

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
head(ToothGrowth)
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
      geom_violin()
p
# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
      geom_violin(trim=FALSE)
p
p + scale_x_discrete(limits=c("0.5", "2"))
p + stat_summary(fun.y=mean, geom="point", shape=23, size=2)
p + geom_boxplot(width=0.1)
p<-ggplot(ToothGrowth, aes(x=dose, y=len, color=dose)) +
      geom_violin(trim=FALSE)
p + geom_boxplot(width=0.1)+scale_color_brewer(palette="Dark2") #




# creating the modified dataframe
data_mod <- melt(data, id.vars='col1', 
                  measure.vars=c('col2', 'col3'))
data1 <- data.frame(LEV=step6SMWmini$LEV_AVE, Group="SMW")
data2 <- data.frame(LEV=step6SM1970$LEV, Group="SM<1970")
data3 <- data.frame(LEV=step6SM19702$LEV, Group="SM>1970")
data4 <- data.frame(LEV=step6MWmini$LEV, Group="MW")
data5 <- data.frame(LEV=step6SWmini$LEV, Group="SW")
mydata <- bind_rows(data1, data2, data3, data4, data5)
p<-ggplot(mydata, aes(x=id, y=value, fill=id)) +
      geom_violin(width = 1, draw_quantiles = T) 
p + scale_fill_brewer(palette="Blues")# + geom_boxplot(width=0.3) #+ theme_classic()

p<-ggplot(data1, aes(x=id, y=value, fill=id)) +
      geom_violin(trim=T) 
p + scale_fill_brewer(palette="Blues") + geom_boxplot(width=0.1, outlier.shape = NA) #+ theme_classic()


ggplot(data1, aes(x=value, fill=id)) +
      geom_histogram( color='#e9ecef', alpha=0.6, position='identity', bins = 100)

mydata <- data2
p<-ggplot(mydata, aes(x=Group, y=LEV, fill=Group)) +
      geom_boxplot(outlier.alpha = 0.03, varwidth = F, outlier.stroke = 0.5, outlier.colour = "orange") +
      labs(title="Modified Levenshtein Distance") 
p + scale_fill_brewer(palette="Blues") + theme_classic()



sum(almost.equal(step6SM1970$LEV, 0))/nrow(step6SM1970)
sum(almost.equal(step6SMWmini$LEV_AVE, 0))/nrow(step6SMWmini)
mean(step6SM1970$LEV)
summary(step6SM1970$LEV)
summary(step6SMWmini$LEV_AVE)
plot(step6SM1970$LEV)
plot(step6SMWmini$LEV_AVE)
