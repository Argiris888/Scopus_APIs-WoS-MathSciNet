library(httr); library(stringr); library(jsonlite); library(lubridate); library(rscopus); library(dplyr); library(rlist); library(tidyverse)

### setting the api key ###

api_key <- "8b7feaa12d6897d165960222eab621eb"
rscopus::set_api_key(api_key)
r <- rscopus::get_api_key( error = TRUE)
print(r, reveal = TRUE)




journalName <- "Transactions of the American Mathematical Society"
journalName <- str_replace_all(string = journalName, pattern = " ", replacement = "+")

### creating the journal's publications dataframe ###

start_time <- Sys.time()
pubsJournaldf2 <- NULL
count <- 25
cursor <- "*"
res = GET(paste0("https://api.elsevier.com/content/search/scopus?query=SRCTITLE(", journalName, ")+and+PUBYEAR+bef+2020&cursor=", cursor, "&view=COMPLETE&count=", count, "&apiKey=", api_key))
contentJournal <- fromJSON(rawToChar(res$content)) ### periexei link gia cited-by
pubsJournal25 <- contentJournal$`search-results`$entry
pubsJournaldf2 <- bind_rows(pubsJournaldf, pubsJournal25)
number_of_pubs <- as.numeric(contentJournal$`search-results`$`opensearch:totalResults`)
number_of_queries <- number_of_pubs%/%count

for (i in 1:number_of_queries) {    #(number_of_pubs%/%25)
      #browser()  
      print(i * count)
      url <- contentJournal$`search-results`$link$`@href`[3]
      res = GET(url)
      contentJournal <- fromJSON(rawToChar(res$content)) ### periexei link gia cited-by
      pubsJournal25 <- contentJournal$`search-results`$entry
      pubsJournaldf2 <- bind_rows(pubsJournaldf, pubsJournal25)
      
}

end_time <- Sys.time()
dur <- end_time-start_time

row.names(pubsJournaldf) <-1:nrow(pubsJournaldf)

#save(pubsJournaldf, file = "C:/Users/StoooKton/Desktop/Rscopus/pubsJournaldf.RData" )
load(file = "C:/Users/StoooKton/Desktop/Rscopus/pubsJournaldf.RData")




### checking the total cited by and max cited by ###
total_cited_by <- sum(as.numeric(pubsJournaldf$`citedby-count` [7001:nrow(pubsJournaldf)] ))
avg_cited_by <- total_cited_by/ nrow(pubsJournaldf)
which.max(pubsJournaldf$`citedby-count`)
imax <- which(as.numeric(pubsJournaldf$`citedby-count`) == max(as.numeric(pubsJournaldf$`citedby-count`)))
max <- pubsJournaldf[imax,]

max.queries <- nrow(pubsJournaldf) - 7001 -1 + ceiling(total_cited_by/25)

### check limit reset for scopus search ###

as_datetime(as.numeric(res$headers$`x-ratelimit-reset`), tz="Europe/Istanbul")


### check diff between cited by results by ref search and ictedby-count column ###

differ <- which(sapply(X = cited_by_info, function(t) t$total_results)!=as.numeric(pubsJournaldf$`citedby-count`))
i <- which(sapply(X = cited_by_info[differ], function(t) t$total_results)==0)
differ[i]








nafun <-  function(t) {
      
      if (is.na(t)) {
            t <- ""
      } else {t}
}


cited_by_info <- NULL
start_time <- Sys.time() 

for (i in 7001:nrow(pubsJournaldf[1:17704, ])) {

    #number <- 15720
    pub <- pubsJournaldf[i, ]
    title_of_pub <- nafun(pub$`dc:title`)
    creator_pub <- nafun(pub$`dc:creator`)
    sourceof_pub <- nafun(pub$`prism:publicationName`)
    page_pub <- pub$`prism:pageRange`
    page_pub <- nafun(str_remove_all(string = page_pub, pattern = "\\-.*"))
    date_pub <- ymd(pub$`prism:coverDate`)
    year_pub <- nafun(year(date_pub))
    
    
    query_ref <- str_replace_all(tolower(paste(title_of_pub, sourceof_pub, year_pub, creator_pub, page_pub, sep = " ")), pattern = "\\S*inf\\S*", replacement = "")
    query_ref <- str_replace_all(query_ref, pattern = "\\S*\u00A0\\S*", replacement = "")
    query_ref <- str_replace_all(query_ref, pattern = "\\S*[^A-Za-z0-9\\-'.\\s,]\\S*", replacement = "")
    query_ref <- str_replace_all(query_ref, pattern = "\\sand\\s", replacement = " 'and' ")
    query_ref <- str_replace_all(query_ref, pattern = "\\sor\\s", replacement = " 'or' ")
    query_ref <- paste0("REF(", query_ref, ")")
    
    ## get the cited by pubs ##
    
    res <- scopus_search(query = query_ref, view = "COMPLETE", api_key = api_key) 
    cited_by_info <- append(cited_by_info, list(res))
    print(i)
    # df = gen_entries_to_df(res$entries) ## periexei ta idia sto df, kai epipelon ksexwrizei affiliation kai author!!!
    # cited <- df$df
    # authorsdf <- df$author
    # names(authorsdf)
    # affildf <- df$affiliation 
    
    }
    
end_time <- Sys.time()
dur <- end_time - start_time 


### comparing the counts diff between the diff objects ###

### entopizeis ta lathh kai manual ta ftiaxneis meta, phgainontas sto corrections ###

sapply(X = cited_by_info[differ], function(t) t$total_results)
as.numeric(pubsJournaldf$`citedby-count`[differ])

### the number refers to specific value of number ###

as.numeric(pubsJournaldf$`citedby-count`[number])
sapply(X = cited_by_info, function(t) t$total_results)[number]

### for corrections ###

f <- "on the behaviour of harmonic functions at the boundary transactions of the american mathematical society 1950   47"
f <- paste0("REF(", f, ")")
res <- scopus_search(query = f, view = "COMPLETE", api_key = api_key)
res$total_results


cited_by_info[number] <- list(res)  ### correction of the citedby list 


#save(cited_by_info, file = "C:/Users/StoooKton/Desktop/Rscopus/cited_by_info_list.RData" )






############ references ###################
ref_info <- NULL
#ref_info <-ref_info[length(ref_info)] 

start_time <- Sys.time()
count <- 9909
for (i in pubsJournaldf$eid[9910:14000]) {
      #i <- pubsJournaldf$eid[[1]]
      count <- count + 1
      #print(pubsJournaldf$`prism:doi`, max = 200)
      
      res <-  abstract_retrieval(i, identifier = "eid",
                                 verbose = FALSE)
      
      pub_ref <- res$content$`abstracts-retrieval-response`$item$bibrecord$tail$bibliography
      ref_info <- append(ref_info, list(pub_ref))
      print(count)
}

end_time <- Sys.time()
dur <- end_time - start_time


#length(unique(pubsJournaldf$))
save(ref_info, file = "C:/Users/StoooKton/Desktop/Rscopus/ref_info_list.RData" )
load("C:/Users/StoooKton/Desktop/Rscopus/ref_info_list.RData")
#ref_info <- ref_info[1:13302]

### df tranformation ###

library(tidyverse)
# tibble_pubsJournaldf <- as_tibble(pubsJournaldf)
# mini_df <- tibble_pubsJournaldf %>% slice_head(n = 50)
# 
# 
# ref_tibble <- as.data.frame(ref_info)
# names(ref_info[[1]]$reference)

b <- sapply(ref_info, function(t) t$`@refcount`)

isnull <- NULL
for (i in b){
  isnull <- append(isnull, is.null(i[[1]]))
}

ref_null_pubs <- which(isnull==T )   ### apo to 13303 KAI META DEN EXEI REFERENCES !!!!
sum(isnull)




minipubsdf <- pubsJournaldf
minipubsdf$author[[2]]
mini_author_df <- NULL
eid_list <- NULL
index_list <- NULL
for (i in 1:nrow(minipubsdf)) {
  if (is.null(minipubsdf$author[[i]])==F) {
      
    mini_author_df <- bind_rows(mini_author_df, minipubsdf$author[[i]])
    
    for (j in 1:nrow(minipubsdf$author[[i]])) {
        eid_list <- append(eid_list, minipubsdf$eid[[i]])
        index_list <- append(index_list, i)
  }
  
  }
}

mini_author_df$pub_index <- index_list
mini_author_df$pub_eid <- eid_list

#save(mini_author_df, file = "C:/Users/StoooKton/Desktop/Rscopus/mini_author_df.RData")
load("C:/Users/StoooKton/Desktop/Rscopus/mini_author_df.RData")

length(unique(mini_author_df$pub_eid)) # 17650
length(unique(mini_affil_df$pub_doi)) # 17080

sum(sapply(pubsJournaldf$author ,function(t) is.null(t[[1]]))) ## osa kai an epsaxna me author$total--- 54
sum(sapply(pubsJournaldf$affiliation ,function(t) is.null(t[[1]]))) ## 521

length(unique(pubsJournaldf$`prism:doi`)) # 17598
sum(is.na(pubsJournaldf$`prism:doi`)) #############################tha prepei na xrhsimopoihsw to eid ###########################
which(is.na(pubsJournaldf$`prism:doi`))
length(unique(pubsJournaldf$eid))

mini_affil_df <- NULL
eid_list <- NULL
index_list <- NULL
for (i in 1:nrow(minipubsdf)) {
  if (is.null(minipubsdf$affiliation[[i]])==F) {
    
    mini_affil_df <- bind_rows(mini_affil_df, minipubsdf$affiliation[[i]])
    
    for (j in 1:nrow(minipubsdf$affiliation[[i]])) {
      eid_list <- append(eid_list, minipubsdf$eid[[i]])
      index_list <- append(index_list, i)
    }
    
  }
}

mini_affil_df$pub_index <- index_list
mini_affil_df$pub_eid <- eid_list

#save(mini_affil_df, file = "C:/Users/StoooKton/Desktop/Rscopus/mini_affil_df.RData")
load(file = "C:/Users/StoooKton/Desktop/Rscopus/mini_affil_df.RData")

### adding ref count to my main journal database ###
ref_list <- NULL
for (i in 1:length(ref_info)) {
  if (is.null(ref_info[[i]])==F) {
    ref_list <- append(ref_list, ref_info[[i]]$`@refcount`)
  } else {
      ref_list <- append(ref_list, NA)
    }
  
}

ref_list[(length(ref_list)+1):nrow(pubsJournaldf)] <- NA
pubsJournaldf$ref_count <- ref_list
which(is.na(pubsJournaldf$ref_count))





### references dataframe ###

        

#load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\ref_info_list.Rdata")

trial_list <- ref_info[1:200]


re <- trial_list[[1]]
#trial_list[[1]]$reference[[1]]$`ref-fulltext`

pub_doi_list <- NULL; SGR <- NULL; full_text_list <- NULL; title_list <- NULL; source_tittle_list <- NULL; volume_list <- NULL
issue_list <- NULL; page_range_list <- NULL; year_list <- NULL; ref_eid_list <- NULL; seq_list <- NULL; authors_list <- NULL


for (i in 1:length(ref_info)) {
  print(i)
  #seq_list <- NULL
  #ref_doi_list <- NULL
  if (is.null(ref_info[[i]])==FALSE) {
  
    if (is.list(ref_info[[i]]$reference[[1]])) {
    #is.list(ref_info[[10028]]$reference[[1]])
    
       for (j in 1:length(ref_info[[i]]$reference)) {
         
         pub_doi_list <- append(pub_doi_list, list(ref_info[[i]]$reference[[j]]$`ref-info`$`refd-itemidlist`$itemid))
         
         full_text_list <- append(full_text_list, list(ref_info[[i]]$reference[[j]]$`ref-fulltext`))
         title_list <- append(title_list, list(ref_info[[i]]$reference[[j]]$`ref-info`$`ref-title`$`ref-titletext`))
         source_tittle_list <- append(source_tittle_list, list(ref_info[[i]]$reference[[j]]$`ref-info`$`ref-sourcetitle`))
         volume_list <- append(volume_list, list(ref_info[[i]]$reference[[j]]$`ref-info`$`ref-volisspag`$voliss$`@volume`))
         issue_list <- append(issue_list, list(ref_info[[i]]$reference[[j]]$`ref-info`$`ref-volisspag`$voliss$`@issue`))
         page_range_list <- append(page_range_list, list(ref_info[[i]]$reference[[j]]$`ref-info`$`ref-volisspag`$pagerange))
         year_list <- append(year_list, list(ref_info[[i]]$reference[[j]]$`ref-info`$`ref-publicationyear`$`@first`))
         seq_list <- append(seq_list, list(j))
         ref_eid_list <- append(ref_eid_list, list(pubsJournaldf$eid[[i]]))
         authors_list <- append(authors_list, list(ref_info[[i]]$reference[[j]]$`ref-info`$`ref-authors`$author))
       }
    } else {
      pub_doi_list <- append(pub_doi_list, list(ref_info[[i]]$reference$`ref-info`$`refd-itemidlist`$itemid))
      
      full_text_list <- append(full_text_list, list(ref_info[[i]]$reference$`ref-fulltext`))
      title_list <- append(title_list, list(ref_info[[i]]$reference$`ref-info`$`ref-title`$`ref-titletext`))
      source_tittle_list <- append(source_tittle_list, list(ref_info[[i]]$reference$`ref-info`$`ref-sourcetitle`))
      volume_list <- append(volume_list, list(ref_info[[i]]$reference$`ref-info`$`ref-volisspag`$voliss$`@volume`))
      issue_list <- append(issue_list, list(ref_info[[i]]$reference$`ref-info`$`ref-volisspag`$voliss$`@issue`))
      page_range_list <- append(page_range_list, list(ref_info[[i]]$reference$`ref-info`$`ref-volisspag`$pagerange))
      year_list <- append(year_list, list(ref_info[[i]]$reference$`ref-info`$`ref-publicationyear`$`@first`))
      seq_list <- append(seq_list, list(1))
      ref_eid_list <- append(ref_eid_list, list(pubsJournaldf$eid[[i]]))
      authors_list <- append(authors_list, list(ref_info[[i]]$reference$`ref-info`$`ref-authors`$author))
      
       }
    }
}  

sum(sapply(ref_info, is.null))### ara se 4 pubs den exoume reference list


#save(pub_doi_list, full_text_list, title_list, source_tittle_list, volume_list, issue_list, page_range_list, year_list, seq_list, ref_eid_list, authors_list, page_range,pub_sgr, pub_doi, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\ref_lists.Rdata")
#load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\ref_lists.Rdata")



# c <- trial_list[[1]]$reference[[1]]$`ref-info`$`ref-authors`$author
# as.data.frame(do.call(rbind, c))
# listwithdfs <- lapply(trial_list[[3]]$reference[21], function(t) as.data.frame(do.call(rbind, t$`ref-info`$`ref-authors`$author)))
# listwithdfs <- lapply(authors_list[3], function(t) as.data.frame(do.call(rbind, t)))
# c <- authors_list[[1]]
# as.data.frame(do.call(rbind, authors_list[[1]]))
# sum(sapply(listwithdfs, nrow))
# as.data.frame(authors_list[1])


# tot <- as.data.frame(do.call(rbind, listwithdfs))
# tot$ref_doi <- pubsJournal_doi_vector
# length(unique(ref_author_df$ref_doi))

nullToNAfunction <- function(alist){
  #alist <- year_list
  vector <- NULL
  for (i in 1:length(alist)) {
    if (is.null(alist[[i]])==F) {
      vector <- append(vector, alist[[i]])
    } else {
      vector <- append(vector, NA)
    }
    
  }
  return (vector)
}


page_range <- NULL
for (i in 1:length(page_range_list)) {
  if (is.null(page_range_list[[i]])==F) {
    page_range <- append(page_range, paste(page_range_list[[i]]$`@first`, page_range_list[[i]]$`@last`, sep = "-"))
  } else {
    page_range <- append(page_range, NA)
  }
}  

#page_range_list[[111587]]

# nullToNAfunction(year_list)
# nullToNAfunction(seq_list)
# nullToNAfunction(ref_eid_list)
# nullToNAfunction(issue_list)
# nullToNAfunction(volume_list)
# nullToNAfunction(source_tittle_list)
# nullToNAfunction(title_list)
# nullToNAfunction(full_text_list)



# trial_list[[1]]$reference[[31]]$`ref-info`$`refd-itemidlist`$itemid
# pub_doi_list[[31]][[1]]

pub_doi <- NULL
pub_sgr <- NULL

for ( i in pub_doi_list) {
  
  #i <- pub_doi_list[[1]]
  if (is.list(i[[1]])) {   ## giati kapoia den periexounlistes
    
    doi_log <- TRUE
    sgr_log <- TRUE
    
    for (j in i) {
    
      if (j$`@idtype`== "DOI"){
      
        pub_doi <- append(pub_doi, j$`$`)
      
        doi_log <- FALSE
    
      } else if (j$`@idtype`== "SGR") {
        
        pub_sgr <- append(pub_sgr, j$`$`)
        
        sgr_log <- FALSE
      }
      
    } 
    
    if (doi_log) { #### dld an den vrei to doi tote vale na 
      
      pub_doi <- append(pub_doi, NA)
      
    } else if (sgr_log) {
      pub_sgr <- append(pub_sgr, NA)
    }
    
  } else {
    
    if (i$`@idtype`== "DOI"){
      
      pub_doi <- append(pub_doi, i$`$`)
      pub_sgr <- append(pub_sgr, NA)
      
    } else {
      
      pub_doi <- append(pub_doi, NA)
      pub_sgr <- append(pub_sgr, i$`$`)
      
    }
    
    
  }
  
}
ref_df <- data.frame(matrix(ncol=11,nrow=length(pub_sgr)))
colnames(ref_df) <- c("pub_doi", "SGR", "full_text", "title", "source_title", "volume", "issue", "page_range", "year", "ref_eid", "seq")
ref_df$pub_doi <- pub_doi; ref_df$issue <- nullToNAfunction(issue_list)
ref_df$SGR <- pub_sgr; ref_df$ref_eid <- nullToNAfunction(ref_eid_list)
ref_df$full_text <- nullToNAfunction(full_text_list) ; ref_df$seq <- nullToNAfunction(seq_list)
ref_df$title <- nullToNAfunction(title_list); ref_df$year <- nullToNAfunction(year_list)
ref_df$source_title <- nullToNAfunction(source_tittle_list); ref_df$page_range <- page_range
ref_df$volume <- nullToNAfunction(volume_list)

save(ref_df, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\ref_df.Rdata")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\ref_df.Rdata")

##### ref author df ############

ref_eid_vector <- NULL
SGR_vector <- NULL
ref_author_df <- NULL
for (i in 1:length(authors_list)) {
  #i <- 3
    
    for (j in 1:length(authors_list[[i]])) {
  
      if (is.null(authors_list[[i]][[j]])==FALSE){
      
         ref_author_df <- bind_rows(ref_author_df, authors_list[[i]][[j]])
         SGR_vector <- append(SGR_vector, ref_df$SGR[i])
         ref_eid_vector <- append(ref_eid_vector ,ref_df$ref_eid[i]) 
       } 
    }
  }


ref_author_df$SGR <- SGR_vector
ref_author_df$ref_eid <- ref_eid_vector

save(ref_author_df, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\ref_author_df.Rdata")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\ref_author_df.Rdata")


### cited-by dataframe ###

#trial3 <- cited_by_info

# cited_by_df <- NULL
# do.call(what = rbind, trial3[[1]]$entries[1])
# v <- sapply(trial3[[1]]$entries, bind_rows)
# bind_rows(cited_by_df, v[[1]])
# do.call(bind_rows, trial3[[1]]$entries)


load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\cited_by_info_list.RData")

citedby_df <- NULL
cit_eid <- NULL
count <- 0
for (i in cited_by_info) {
  count <- count+1
  if (i$total_results!=0) {
    entries <- gen_entries_to_df(i$entries)
    citedby_df <- bind_rows(citedby_df, entries$df)
    cit_eid <- append(cit_eid, rep(pubsJournaldf$eid[[count]], i$total_results))     #### gia kathe pub toujournal, des posa citedby exei kai ftiakse sthlh me to eid tou pub sto opoio dinoun citation
  }
}

seq_cited_by <- unlist(sapply(cited_by_info, function(t) {if (t$total_results!=0){seq(1:t$total_results)}}))

citedby_df$cit_eid <- cit_eid
citedby_df$seq <- seq_cited_by


#save(citedby_df ,file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\citedby_df.RData")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\citedby_df.RData")

# entries <- gen_entries_to_df(trial3[[102]]$entries)
# View(entries$df)

citedbyAA_df_tr <- function(alist, astring) {
  citedby_aa_df <- NULL
  cit_eid <- NULL
  pub_eid <- NULL
  count <- 0
  for (i in alist) {
    count <- count+1
    #i <- cited_by_info[[253]]
    if (i$total_results!=0) {
      entries <- gen_entries_to_df(i$entries) ### mporei px gia mia sugkekrimenh dhmosieushs ta alist na periexei 3 citedby pubs, kai to kathena na exei 5 suggrafeis, opote dhmioiurgoume proswrino df (entries$author) me tous 15 suggrafeis
      if (astring %in% names(entries)) {
      citedby_aa_df <- bind_rows(citedby_aa_df, entries[[astring]]) ### prosthetoume tous 15 autous suggrafeis sto geniko author df mas
      cit_eid <- append(cit_eid, rep(pubsJournaldf$eid[[count]], nrow(entries[[astring]]))) ### gia autous tous dekapente suggrafeis, kollame ena kkoino cit_eid 
      
      ## gia na vroume to eid tou citedby kai na to prosthesoume sto authordf, gia kathe ena apo tous suggrafeis entries$author psaxnoume sth sthlh entrynumber kai to vriskooume sston deikth tou entries$pubs$eid
      for (j in entries[[astring]]$entry_number){
        if (!is.null(entries$df$eid[j])) {
          pub_eid <- append(pub_eid, entries$df$eid[j])
        } else {pub_eid <- append(pub_eid, NA)} # to 102 px einai NULL to pub doi
        }
      }
    }
  }
  citedby_aa_df$cit_eid <- cit_eid
  citedby_aa_df$pub_eid <- pub_eid
  return(citedby_aa_df)
}

citedby_affil_df <- citedbyAA_df_tr(cited_by_info, "affiliation")
citedby_author_df <- citedbyAA_df_tr(cited_by_info, "author")

citedby_affil_df <- citedby_affil_df[!is.na(citedby_affil_df$afid), ] ## removing na
citedby_author_df <- citedby_author_df[!is.na(citedby_author_df$authid), ] ## removing na
#cited_by_info[[253]]

tr <- cited_by_info[17698]
#citedby_affil_df <- na.omit(citedby_affil_df)
row.names(citedby_affil_df) <- 1:nrow(citedby_affil_df)
#save(citedby_affil_df ,file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\citedby_affil_df.RData")
#citedby_author_df <- na.omit(citedby_author_df)
row.names(citedby_author_df) <- 1:nrow(citedby_author_df)
#save(citedby_author_df ,file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\citedby_author_df.RData")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\citedby_author_df.RData")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\citedby_affil_df.RData")








############# authors publications #######################
load(file = "C:/Users/StoooKton/Desktop/Rscopus/mini_author_df.RData")
unique_author_ids <- unique(mini_author_df$authid)

author_pubs <- NULL
count <- 14716
for (id in unique_author_ids[14716:length(unique_author_ids)]) {
  
  query_author <- paste0("AU-ID(", id, ")") 
  res <- scopus_search(query = query_author, view = "COMPLETE", api_key = api_key) 
  author_pubs <- append(author_pubs, list(res))
  print(count)
  count <- count+1
  
}

#save(author_pubs, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\author_pubs.RData")
load(file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\author_pubs.RData")


#mini_list <- author_pubs[1:500]
#rm(author_pubs)

authorsPubs_df <- NULL
author_eid <- NULL
count <- 0
for (i in author_pubs) {
  count <- count+1
  if (i$total_results!=0) {
    entries <- gen_entries_to_df(i$entries)
    authorsPubs_df <- bind_rows(authorsPubs_df, entries$df)
    author_eid <- append(author_eid, rep(unique_author_ids[count], i$total_results))     #### gia kathe pub toujournal, des posa citedby exei kai ftiakse sthlh me to eid tou pub sto opoio dinoun citation
  }
}

seq_authorPubs <- unlist(sapply(author_pubs, function(t) {if (t$total_results!=0){seq(1:t$total_results)}}))

authorsPubs_df$author_eid <- author_eid
authorsPubs_df$seq <- seq_authorPubs

#save(authorsPubs_df ,file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\authorsPubs_df.RData")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\authorsPubs_df.RData")



authorpubsAA_df_tr <- function(alist, astring) {
  authorpubs_aa_df <- NULL
  journal_author_eid <- NULL
  pub_eid <- NULL
  count <- 0
  for (i in alist) {
    count <- count+1
    #i <- cited_by_info[[253]]
    if (i$total_results!=0) {
      entries <- gen_entries_to_df(i$entries) ### mporei px gia mia sugkekrimenh dhmosieushs ta alist na periexei 3 citedby pubs, kai to kathena na exei 5 suggrafeis, opote dhmioiurgoume proswrino df (entries$author) me tous 15 suggrafeis
      if (astring %in% names(entries)) {
        authorpubs_aa_df <- bind_rows(authorpubs_aa_df, entries[[astring]]) ### prosthetoume tous 15 autous suggrafeis sto geniko author df mas
        journal_author_eid <- append(journal_author_eid, rep(unique_author_ids[count], nrow(entries[[astring]]))) ### gia autous tous dekapente suggrafeis, kollame ena kkoino cit_eid 
        
        ## gia na vroume to eid tou pub tou opoiou eiinai suggrafeis oi authors pou emfanizontai sto entires[[authors]] kai na to prosthesoume sto authordf, gia kathe ena apo tous suggrafeis entries$author psaxnoume sth sthlh entrynumber kai to vriskooume sston deikth tou entries$pubs$eid
        for (j in entries[[astring]]$entry_number){
          if (!is.null(entries$df$eid[j])) {
            pub_eid <- append(pub_eid, entries$df$eid[j])
          } else {pub_eid <- append(pub_eid, NA)} # to 102 px einai NULL to pub doi
        }
      }
    }
  }
  authorpubs_aa_df$journal_author_eid <- journal_author_eid
  authorpubs_aa_df$pub_eid <- pub_eid
  return(authorpubs_aa_df)
}

authorpubs_affil_df <- authorpubsAA_df_tr(author_pubs, "affiliation")
authorpubs_author_df <- authorpubsAA_df_tr(author_pubs, "author")



authorpubs_affil_df <- authorpubs_affil_df[!is.na(authorpubs_affil_df$afid), ] ## removing na
authorpubs_author_df <- authorpubs_author_df[!is.na(authorpubs_author_df$authid), ] ## removing na

#save(authorpubs_affil_df, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\authorpubs_affil_df.RData")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\authorpubs_affil_df.RData")
#save(authorpubs_author_df, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\authorpubs_author_df.RData")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\authorpubs_author_df.RData")





########## nested columns #########

trdata <- mini_author_df[35:55,]
r1 <- pubsJournaldf[12,]
raff1 <- r1$affiliation[[1]]
rauth1 <- r1$author[[1]]
r2 <- pubsJournaldf[21,]
raff2 <- r2$affiliation[[1]]
rauth2 <- r2$author[[1]]
#library(dplyr)

# v <- bind_rows(trdata$afid)
 trdata$afid <-  bind_rows(trdata$afid, .id = "id")             
# mini_author_df$afid <- bind_rows(mini_author_df$afid)
# spread(trdata$afid[1])
# unnest(trdata$afid[1])
# c <- bind_rows(trdata$afid, .id = "id")
# separate(trdata, trdata$afid)

v1 <-unname(unlist(sapply(unique(mini_affil_df$pub_eid), function(t) seq(1, sum(t==mini_affil_df$pub_eid)))))
mini_affil_df$`@seq` <- v1

citedby_affil_df <- citedby_affil_df[!is.na(citedby_affil_df$afid), ] ## removing na
citedby_author_df <- citedby_author_df[!is.na(citedby_author_df$authid), ] ## removing na

v2 <- unname(unlist(sapply(unique(citedby_affil_df$pub_eid), function(t) seq(1, sum(t==citedby_affil_df$pub_eid)))))
citedby_affil_df$`@seq` <- v2




rm(v2,v1,trdata)


c <- mini_author_df$afid[[21]]
v <- mini_author_df$afid[[16679]]


res <- scopus_search(query = paste0("SRCTITLE(", journalName, ") and PUBYEAR is 2019 "), view = "COMPLETE", api_key = api_key)
x <- gen_entries_to_df(res$entries)
x1 <- x$df
x2 <- x$affiliation
x3 <- x$author


#### corrections ###

trial <- head(pubsJournaldf, 25)
trial$afid <- sapply(trial$afid, function(t) t$`$`[[1]])
do.call(unnest, trial)
d <- unnest_wider(trial, afid, names_repair = "minimal")
unnest_wider(d, `$`, names_repair = "minimal")
v <- unnest(d, `$`, names_repair = "minimal")

## ok

### correction to mini_author_df$afid ##########

mini_author_df$afid <- nullToNAfunction(sapply(mini_author_df$afid, function(t) t$`$`[[1]]))
#mini_author_df$afid <- sapply(mini_author_df$afid, function(t) t[[1]])


x <- pubsJournaldf$freetoreadLabel
x <- unnest_wider(x, value)
x <- unnest_wider(x, `$`)
colnames(x) <- c("Access", "Bronze", "Green" )
pubsJournaldf <- add_column(pubsJournaldf, x, .after = "freetoreadLabel")
x <- pubsJournaldf$freetoread
x <- unnest_wider(x, value)
x <- unnest_wider(x, `$`)
colnames(x) <- c("ftr1", "ftr2", "ftr3", "ftr4", "ftr5")
pubsJournaldf <- add_column(pubsJournaldf, x, .after = "freetoreadLabel")

## okend


WOSdf$author <- sapply(WOSdf$author, function(t) unlist(strsplit(t, split = " and ")))
WOSdf <- unnest_wider(WOSdf, col = author)

# n <- unnest_wider(m, `$`)

names(WOSdf)[2:12] <- paste0("author", seq(1:11))

c <- WOSdf[!is.na(WOSdf$author11), ]

mathscidata$AUTHOR <- sapply(mathscidata$AUTHOR, function(t) unlist(strsplit(t, split = "/")))
mathscidata <- unnest_wider(mathscidata, col = AUTHOR)
names(mathscidata)[3:6] <- paste0("author", seq(1:4))

#save(WOSdf, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\WOSdf.RData")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\WOSdf.RData")
#save(mathscidata, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\mathscidata.RData")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\mathscidata.RData")

tr <- WOSdf[1:5,]
str(tr$`cited-references`)
refs <- unlist(str_split(tr$`cited-references`, pattern = "\n"))


ref_id_f <- function(df) {
  times <- df$`number-of-cited-references`
  unique_id <- df$`unique-id` #########################################
  ref_id <- NULL
  for (i in 1:length(unique_id)) {
    ref_id <- append(ref_id, rep(unique_id[i], times=times[i]))
  }
  return(ref_id)
}
ref_id <- ref_id_f(tr)


refs <- sapply(refs, function(t) str_split(t, pattern = ", "), USE.NAMES = F)

as.data.frame(refs, row.names = NULL, check.names = FALSE)

 b <- as.data.frame(do.call(rbind, refs))
        



df <- data.frame(matrix(ncol = 1, nrow = length(refs)))
colnames(df) <- "col1"
df$col1 <- refs

df <- unnest_wider(df, col1)
df$...7 <- NULL

#names(df) <- c("author","year","journal","4","5","6")

upper.tri()