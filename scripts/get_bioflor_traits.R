
# Setup -------------------------------------------------------------------

library("here")
library("rvest")
library("dplyr")
library("tidyr")
library("taxize")
library("stringr")
library("rgbif")
library("beepr")


# Functions ---------------------------------------------------------------

# just to be save, run functions script
source(here('scripts', 'functions.R'))

# Compile list of plant species with their traits from BioFlor ------------


#initiate data frame
plant_traits <- setNames(data.frame(
  matrix(ncol = 19,
         nrow = 4000)),
  c("AccName", "GbifKey", "OrigName", "species", "bioflor_id",  "LifeForm",
    "LifeSpan", "FlStart", "FlEnd", "FlDur", "ReprType", "Dicliny", "Dichogamy",
    "SelfComp", "PollVec", "PollVecGrp", "BreedSys", "FlowClass", "Habitat"))

i <- 0

# for loop to scrape traits from each species page,
# yes a while loop might be better but i cant be 
# asked to change it since the indexing relies on it
for (i in seq(i,
              10000,
              # nrow(plant_traits),
              1)) {
  
  # for (i in grep("character.+", plant_traits$OrigName)-1) {
  
  p <- i+1
  
  # download bioflor page
  # if an error occurs, catch that and continue
  pagetext <- tryCatch(read_html(
    paste("https://www.ufz.de/biolflor/taxonomie/taxonomie.jsp?ID_Taxonomie=",
          i, sep = ""),
    options = c("NOERROR", "RECOVER")) %>%
      html_text(),
    error = function(e) {
      cat("Error while retreiving page, end probably reached")
      }
  )
  
  # if the error actually means the end is reached and we
  #   don't have a pagetext, stop
  if(is.null(pagetext)) {
  
    # wait half a minute and try again
    Sys.sleep(30)
    
    pagetext <- tryCatch(read_html(
      paste("https://www.ufz.de/biolflor/taxonomie/taxonomie.jsp?ID_Taxonomie=",
            i, sep = ""),
      options = c("NOERROR", "RECOVER")) %>%
        html_text(),
      error = function(e) {
        cat("Error while retreiving page, end probably reached")
        }
    )
    
    #if it's still null stop the loop
    if (is.null(pagetext)) { break }
    
    }
  
  #use right set of regex for the language,
  #check if the end of bioflor is reached
  if (str_detect(
    pagetext, "(?<=german name\\(s\\)\n\t\t\t\n\t\t\t\n\t\t\t\t).+")) {
    
    #extract scientific name
    OrigName <- pagetext %>%
      str_extract_all(
        "(?<=german name\\(s\\)\n\t\t\t\n\t\t\t\n\t\t\t\t).+"
      ) %>% 
      paste() %>%
      as.character() %>%
      gsub(x = ., pattern = "\\([^\\)]{10,}\\)$", replacement = "") %>%
      gsub(x = ., pattern = "excl\\..+$", replacement = "")
    
    #extract traits data
    traits_i <-
      get_traits(
        pagetext,
        names(plant_traits)[6:length(names(plant_traits))],
        c(
          "(?<=Life form\n\t\t)[:alpha:]+",
          "(?<=Life span\n\t\t)[:alpha:]+",
          "(?<=Begin of flowering \\(months\\)\n\t\t)[:digit:]+",
          "(?<=End of flowering \\(months\\)\n\t\t)[:digit:]+",
          "(?<=Duration of flowering \\(months\\)\n\t\t)[:digit:]+",
          "(?<=Type of reproduction\n\t\t)[:alpha:]+",
          "(?<=Dicliny\n\t\t)[:alpha:]+",
          "(?<=Dichogamy\n\t\t)[:alnum:]+",
          "(?<=Self[:punct:]sterility and self[:punct:]incompatibility\n\t\t)[:alpha:]+",
          "(?<=Pollen vector\n\t\t)[:alpha:]+",
          "(?<=Pollen vector group: )[:alpha:]+",
          "(?<=Breeding system\n\t\t)[:alpha:]+",
          "(?<=Flower class after MUELLER\n\t\t)[:alpha:]+",
          "(?<=Habitat\n\t\t)[:graph:]+"
        )
      )
    
  } else if (str_detect(
    pagetext, "(?<=Deutsche\\(r\\) Name\\(n\\)\n\t\t\t\n\t\t\t\n\t\t\t\t).+")) {
    
    #extract scientific name
    OrigName <- pagetext %>%
      str_extract_all(
        "(?<=Deutsche\\(r\\) Name\\(n\\)\n\t\t\t\n\t\t\t\n\t\t\t\t).+"
      ) %>% 
      paste() %>%
      as.character() %>%
      gsub(x = ., pattern = "\\([^\\)]{10,}\\)$", replacement = "") %>%
      gsub(x = ., pattern = "excl\\..+$", replacement = "")
    
    #extract traits data
    traits_i <-
      get_traits(
        pagetext,
        names(plant_traits)[6:length(names(plant_traits))],
        c(
          "(?<=Lebensform\n\t\t)[:alpha:]+",
          "(?<=Lebensdauer\n\t\t)[:alpha:]+",
          "(?<=Blühbeginn \\(Monate\\)\n\t\t)[:digit:]+",
          "(?<=Blühende \\(Monate\\)\n\t\t)[:digit:]+",
          "(?<=Blühdauer \\(Monate\\)\n\t\t)[:digit:]+",
          "(?<=Reproduktionstyp\n\t\t)[:alpha:]+",
          "(?<=Diklinie\n\t\t)[:alpha:]+",
          "(?<=Dichogamie\n\t\t)[:alnum:]+",
          "(?<=Selbst[:punct:]Sterilität und Selbst[:punct:]Inkompatibilität\n\t\t)[:alpha:]+",
          "(?<=Bestäubung\n\t\t)[:alpha:]+",
          "(?<=Gruppenzugehörigkeit: )[:alpha:]+",
          "(?<=Befruchtungssystem\n\t\t)[:alpha:]+",
          "(?<=Blumenklassen\n\t\t)[:alpha:]+",
          "(?<=Biotop\n\t\t)[:graph:]+"
        )
      )
    
  } 
  
  
  if (any(grep("agg\\.", OrigName))) { 
    
    # next if the taxon is a conglomerate
    plant_traits$OrigName[p] <- OrigName
    
    next
    
  } else if (any(grep("sect\\.", OrigName))) { 
    
    # next if the taxon is a section of something
    plant_traits$OrigName[p] <- OrigName
    
    next
    
  }
  
  #get gbif key and the most accepted name
  #pretty hacky, might clean up later 
  keyname <- get_gbifid_(OrigName, messages = FALSE)[[1]]
  
  if (nrow(keyname) == 0) {
    
    #no names found, return NAs
    keyname <- data.frame(AccName = NA,
                          GbifKey = NA,
                          stringsAsFactors = FALSE)
    
  } else {
    
    #get just the first row of the results
    data <- keyname[1, ]
    
    if (anyNA(data$rank) | data$rank != "species") {
      
      #if we have a NA or not a species, return NA
      keyname <- data.frame(
        AccName = NA,
        GbifKey = NA,
        stringsAsFactors = FALSE)
      
    } else if (data$status == "ACCEPTED") {
      
      #it's an accepted name! Let's write that down!
      keyname <- data.frame(
        AccName = as.character(data$scientificname),
        GbifKey = as.character(data$usagekey),
        stringsAsFactors = FALSE)
      
    } else if (any(data$status == "SYNONYM", data$status == "DOUBTFUL") &
               data$matchtype == "EXACT") {
      
      #if its an exact match, but the status is doubtful, get the usageKey 
      #and with that, determine the accepted name
      keyname <- data.frame(
        AccName = as.character(name_usage(
          key = data$specieskey)$data$scientificName),
        GbifKey = as.character(data$specieskey),
        stringsAsFactors = FALSE)
      
    } else {
      
      #nothing above worked? return NA
      keyname <- data.frame(
        AccName = NA,
        GbifKey = NA,
        stringsAsFactors = FALSE)
      
    }
  }
  
  #add original name
  keyname$OrigName <- OrigName
  
  #extract species name  
  keyname$species <- str_extract(keyname$AccName, "^\\w+\\s\\w+")
  
  #save bioflor id
  keyname$bioflor_id <- i

  row <- cbind(keyname, traits_i)

  plant_traits[p,] <- row[1,]
  
  cat(i, ": ", OrigName, ";   ", sep = "")

}

#remove rows without species name
plant_traits <- drop_na(plant_traits, species)

# make sure there is only one trait in the reproduction type col
# this is handling just one problem but meh
plant_traits$ReprType <- str_extract(plant_traits$ReprType, "[:alnum:]+")

#replace empty cells with NA
plant_traits <- na_if(plant_traits, "")

#remove duplicate species
plant_traits <- filter(plant_traits, !duplicated(species))

#save trait table
write.csv(plant_traits, here("static_data", "bioflor_traits.csv"),
          row.names=FALSE)


# Add degrees of pollinator dependence ------------------------------------

if (! exists(plant_traits)){
  
  #load plant trait table
  plant_traits <- read.csv(here("static_data", "bioflor_traits.csv"))
  
}

#define function for calculating pollinator dependence
poll_dep <- function(data,
                     ReprType = data$ReprType,
                     Dicliny = data$Dicliny,
                     Dichogamy = data$Dichogamy,
                     SelfComp = data$SelfComp,
                     PollVec = data$PollVec,
                     PollVecGrp = data$PollVecGrp,
                     BreedSys = data$BreedSys) {

  PollDep <- vector(mode = "character", length = nrow(data))

  for (i in seq_along(PollDep)) {

    #check if indicators of poll dependency are  not NA
    if (!anyNA(c(SelfComp[i], PollVec[i])) &
        isTRUE(SelfComp[i] == "SI" & PollVec[i] == "in")) {

      PollDep[i] <- "Yes"

    } else if (!anyNA(c(Dicliny[i], PollVec[i])) &
               isTRUE(Dicliny[i] == "do" & PollVec[i] == "in")) {

      PollDep[i] <- "Yes"

    } else if (!anyNA(c(Dichogamy[i], PollVec[i])) &
               isTRUE((Dichogamy[i] == "g3" & PollVec[i] == "in" |
                       Dichogamy[i] == "a3" & PollVec[i] == "in"))) {

      PollDep[i] <- "Yes"

    } else if (!anyNA(c(ReprType[i], BreedSys[i], PollVecGrp[i])) &
               isTRUE(ReprType[i] == "v" | BreedSys[i] == "I" |
                      PollVecGrp[i] == "ab" | PollVecGrp[i] == "se")) {

      PollDep[i] <- "No"

    } else if (!anyNA(c(ReprType[i], Dicliny[i], Dichogamy[i], SelfComp[i],
                        PollVec[i], PollVecGrp[i], BreedSys[i]))) {

      PollDep[i] <- "Intermediate"

    } else {

      PollDep[i] <- NA

    }

  }

  return(PollDep)

}

#apply function to data frame
plant_traits$PollDep <- poll_dep(plant_traits)


# Simplyfiy trait data ----------------------------------------------------

#simplify trait data: Only get upper Habitat levels
plant_traits$Habitat <- vapply(
  strsplit(as.character(plant_traits$Habitat), ", "),
  function(x) {
    
    y <- regmatches(x, regexpr("[[:alpha:]][[:digit:]]+", x))
    paste(unique(y), collapse = ", ")
    
  },
  character(1L)) %>%
  na_if("")

#save again
write.csv(plant_traits, here("static_data", "bioflor_traits.csv"),
          row.names=FALSE)
