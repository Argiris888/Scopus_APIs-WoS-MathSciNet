library(bib2df); library(dplyr); library(rvest); library(rlist); library(RefManageR)

yearsOfPublications <- function(link = "https://mathscinet.ams.org/mathscinet/search/publications.html?batch_title=Selected%20Matches%20for%3A%20&fmt=doc&jourGroupId=2125&searchin=&sort=newest&vfpref=html&r=") {
      page <- read_html(link)
      years <- page %>% html_nodes("dl:nth-child(6) .form-checkbox-title") %>% html_text() %>% as.numeric
      return (years)
}

#years <- yearsOfPublications(link)

##### in case we want all years ########

years <- seq(2021, 1900, by = -1)
urlsYearsBased <- sapply(years, function(t) paste0("https://mathscinet.ams.org/mathscinet/search/publications.html?agg_year_", t, "=", t, "&batch_title=Selected%20Matches%20for%3A%20&bdlall=&extend=1&fmt=bibtex&jourGroupId=2125&searchin=&sort=newest&vfpref=html&r="))

library(stringr)
numberOfPubsPerYear <- function(link) {
      page <- read_html(link)
      #page <- urlsYearsBased[5]
      numberOfPubs <- page %>% html_nodes(".matches") %>% html_text()                 ############# na tsekarw pipeline!!!!!!!!!!!!
      numberOfPubs <- as.numeric(str_extract_all(numberOfPubs, "[0-9]+")[[1]])
      return(numberOfPubs)
}


count <- 0
for (year in ###urlsYearsBased) {
     #year <- "https://mathscinet.ams.org/mathscinet/search/publications.html?agg_year_2018=2018&batch_title=Selected%20Matches%20for%3A%20&bdlall=&extend=1&fmt=bibtex&jourGroupId=2125&searchin=&sort=newest&vfpref=html&r="
     numberOfPubs <- numberOfPubsPerYear(year)
     #print(numberOfPubs)
     pagesNumber <- seq(1, numberOfPubs, by = 100)
     urlsOfYearPagesBib <- paste0(year, pagesNumber)
     my_bibtex <- ""
     for (page in urlsOfYearPagesBib) {
           #page <- "https://mathscinet.ams.org/mathscinet/search/publications.html?agg_year_2018=2018&batch_title=Selected%20Matches%20for%3A%20&bdlall=&extend=1&fmt=bibtex&jourGroupId=2125&searchin=&sort=newest&vfpref=html&r=301"
           pageHtml <- read_html(page)
           text_retriever <- pageHtml %>% html_nodes("pre") %>% html_text()
           text_retriever <- paste(text_retriever, collapse = "") # wste oles h dhmosieuseis ths selidas na einai ena eniao keimeno
           my_bibtex <- paste(my_bibtex, text_retriever)
           
     }
     count <- count + 1
     fileNumber <- years[count]
     cat(my_bibtex, file = paste0("bibSet", fileNumber, ".bib"))
     }

# bibFileReader <- function(bibFile) {
#       bib <- bib2df(file = bibFile)
#       bibDF <- select(bib, CATEGORY, BIBTEXKEY, AUTHOR, TITLE, JOURNAL, FJOURNAL, VOLUME, YEAR, NUMBER, PAGES, ISSN, MRCLASS, MRNUMBER, DOI, URL)
#       return(bibDF)
# }
#bibFileReader("bibSet1.bib")

# dfOfFiles <- function( years = years) {
#       fileNames <- paste0("bibSet", years, ".bib")
#       mydf <- data.frame()
#       for (f in fileNames) {
#             mydf <- rbind(mydf, bibFileReader(f))
#       }
#       return(mydf)
# }


yearsOfdata <- seq(2020, 1900, by=-1)

# v <- dfOfFiles(yearsOfdata)
# 
# trial <- bib2df(paste0("bibSet", 2020, ".bib"), separate_names = T)
# trial4 <- bibFileReader(paste0("bibSet", 2020, ".bib"))
# trial2 <- read.bib(paste0("bibSet", 2020, ".bib"))
# trial3 <- as.data.frame(ReadBib(paste0("bibSet", 2020, ".bib")), row.names = ignored)
# bib <- read.bib( package = "bibtex" )  
# as.data.frame(bib)

fileNames <- paste0("bibSet", yearsOfdata, ".bib") ### ta arxeia vriskontai ston folder Dokimi project!!! 
mydf <- data.frame()
for (f in fileNames) {
      bib <- as.data.frame(ReadBib(f))
      mydf <- bind_rows(mydf, bib)
}

names(mydf) <- toupper(names(mydf))
mydf$BIBTEXKEY <- rownames(mydf)
rownames(mydf) <- NULL
mydf <- mydf %>% relocate(BIBTEXKEY, .before = AUTHOR) %>% rename(CATEGORY=BIBTYPE)
mathscidata <- mydf

mathscidata$AUTHOR <- sapply(mathscidata$AUTHOR, function(t) unlist(strsplit(t, split = " and ")))

mathscidata <- unnest_wider(mathscidata, col = AUTHOR)
names(mathscidata)[3:13] <- paste0("author", seq(1:11))

save(mathscidata, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\mathscidata8.RData")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\mathscidata8.RData")

#bib <- read.csv("references.csv")
# Read from .bib file and separate authors' and editors' names:

## function for separating authors through the vectors
# pst <- function(x) {
#       paste(x, collapse = " / ")
# }
# 
# v$AUTHOR = sapply(X = v$AUTHOR, FUN =  pst)

# library(data.table)
# fwrite(v, file ="totaldf.csv", col.names = T)

# library("writexl")
# write_xlsx(v,"totaldf.xlsx")
# table(v$YEAR)
# 
# sum(table(v$YEAR))




#mydata <- read.csv(file = "C:/Users/StoooKton/Desktop/Dokimi_Project/totaldf.csv" )


dfOfWOSFiles <- function( sequenceOfints) {
      #sequenceOfints <- 1:3
      fileNames <- paste0("C:\\Users\\StoooKton\\Downloads\\savedrecs (", sequenceOfints, ").bib")
      mydf <- data.frame()
      for (f in fileNames) {
            #f2 <- fileNames[1]
            bib <- ReadBib(file = f)
            bib <- as.data.frame(bib)
            #bib <- select(bib, CATEGORY, BIBTEXKEY, AUTHOR, TITLE, JOURNAL, VOLUME, YEAR, NUMBER, PAGES, PUBLISHER, ADDRESS, TYPE, LANGUAGE, DOI, ISSN, 
            #RESEARCH.AREAS, WEB.OF.SCIENCE.CATEGORIES, 
            #CITED.REFERENCES, NUMBER.OF.CITED.REFERENCES, TIMES.CITED, USAGE.COUNT.LAST.180.DAYS, USAGE.COUNT.SINCE.2013,
            #JOURNAL.ISO, DOC.DELIVERY.NUMBER, WEB.OF.SCIENCE.INDEX, UNIQUE.ID, OA, DA,
            #MONTH, ABSTRACT,AFFILIATION, EISSN, KEYWORDS,KEYWORDS.PLUS,  AUTHOR.EMAIL, FUNDING.ACKNOWLEDGEMENT, FUNDING.TEXT)
            mydf <- bind_rows(mydf, bib)
      }
      return(mydf)
}
#bib <- bib2df(file = "C:\\Users\\StoooKton\\Downloads\\savedrecs (105).bib")
bib2 <- ReadBib(file = "C:\\Users\\StoooKton\\Downloads\\savedrecs.bib")
#bib3 <- read.bib(file = "C:\\Users\\StoooKton\\Downloads\\savedrecs (0).bib")
bib2df <- as.data.frame(bib2)



WOSdf <- dfOfWOSFiles(sequenceOfints = 0:26)
WOSdf <- select(WOSdf, CATEGORY, BIBTEXKEY, AUTHOR, TITLE, JOURNAL, VOLUME, YEAR, NUMBER, PAGES, PUBLISHER, ADDRESS, TYPE, LANGUAGE, DOI, ISSN, 
                RESEARCH.AREAS, WEB.OF.SCIENCE.CATEGORIES, 
                CITED.REFERENCES, NUMBER.OF.CITED.REFERENCES, TIMES.CITED, USAGE.COUNT.LAST.180.DAYS, USAGE.COUNT.SINCE.2013,
                JOURNAL.ISO, DOC.DELIVERY.NUMBER, WEB.OF.SCIENCE.INDEX, UNIQUE.ID, OA, DA,
                MONTH, ABSTRACT,AFFILIATION, EISSN, KEYWORDS,KEYWORDS.PLUS,  AUTHOR.EMAIL, FUNDING.ACKNOWLEDGEMENT, FUNDING.TEXT)
nrow(mathscidata[mathscidata$YEAR>1969,])
#nrow(pubsJournaldf[pubsJournaldf$`prism:coverDate`>1969,])
WOSdf <- bind_rows(bib2df, WOSdf)
WOSdf$author <- sapply(WOSdf$author, function(t) unlist(strsplit(t, split = " and ")))
WOSdf <- unnest_wider(WOSdf, col = author)

# n <- unnest_wider(m, `$`)

names(WOSdf)[2:12] <- paste0("author", seq(1:11))

c <- WOSdf[!is.na(WOSdf$author11), ]

#save(WOSdf, file = "C:\\Users\\StoooKton\\Desktop\\Rscopus\\WOSdf.RData")
load("C:\\Users\\StoooKton\\Desktop\\Rscopus\\WOSdf.RData")
