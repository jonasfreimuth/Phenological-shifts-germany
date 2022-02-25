
# download previously compiled dataset from github, as this script tends to 
#   fail and needs to be restarted which can be quite tedious
# also the data at the link might slightly differ from the data gotten from 
#   running this script fresh due to changes in the gbif taxonomy or, less 
#   likely in BiolFlor
# download.file(paste0("https://raw.githubusercontent.com/jonasfreimuth/",
#                      "Phenological-shifts-germany/first_submission_analysis",
#                      "/static_data/bioflor_traits.csv"),
#               "static_data/bioflor_traits.csv")
# 
# warning("Downloaded old plant trait data from github instead of running script")

# Setup -------------------------------------------------------------------

library("rvest")
library("dplyr")
library("tidyr")
library("taxize")
library("stringr")
library("rgbif")
library("data.table")

# define vectors of regular expressions for german and english site version
eng_rx_vec <- c(
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

ger_rx_vec <- c(
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

# Functions ---------------------------------------------------------------

# just to be save, run functions script
source("scripts/functions.R")

# Check prerequisites -----------------------------------------------------

dir.check("download")

# Compile list of plant species with their traits from BioFlor ------------


max_rows <- 4000

#initiate data frame
plant_traits <- setNames(data.frame(
  matrix(ncol = 19,
         nrow = max_rows)),
  c("AccName", "GbifKey", "OrigName", "species", "bioflor_id",  "LifeForm",
    "LifeSpan", "FlStart", "FlEnd", "FlDur", "ReprType", "Dicliny", "Dichogamy",
    "SelfComp", "PollVec", "PollVecGrp", "BreedSys", "FlowClass", "Habitat"))

# for loop to scrape traits from each species page,
# yes a while loop might be better but i cant be
# asked to change it since the indexing relies on it
for (i in 0:max_rows) {

  # get index
  p <- i + 1

  # download bioflor page
  # if an error occurs, catch that and continue
  pagetext <- tryCatch(read_html(
    paste("https://www.ufz.de/biolflor/taxonomie/taxonomie.jsp?ID_Taxonomie=",
          i, sep = ""),
    options = c("NOERROR", "RECOVER")) %>%
      html_text(),
    error = function(e) {
      cat("Error while retreiving page, end probably reached\n")
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

    # if it's still null stop the loop
    if (is.null(pagetext)) { break }

  }

  # use right set of regex for the language,
  # check if the end of bioflor is reached
  if (str_detect(pagetext, "german name")) {

    # extract scientific name
    OrigName <- pagetext %>%
      str_extract_all(
        "(?<=german name\\(s\\)\\s{10}).+"
      ) %>%
      paste() %>%
      as.character() %>%
      gsub(x = ., pattern = "\\([^\\)]{10,}\\)$", replacement = "") %>%
      gsub(x = ., pattern = "excl\\..+$", replacement = "")

    # extract traits data
    traits_i <- get_traits(pagetext,
                           names(plant_traits)[6:length(names(plant_traits))],
                           eng_rx_vec)

  } else if (str_detect(pagetext, "Deutsche\\(r\\) Name\\(n\\)")) {

    #extract scientific name
    OrigName <- pagetext %>%
      str_extract_all(
        "(?<=Deutsche\\(r\\) Name\\(n\\)\\s{10}).+"
      ) %>%
      paste() %>%
      as.character() %>%
      gsub(x = ., pattern = "\\([^\\)]{10,}\\)$", replacement = "") %>%
      gsub(x = ., pattern = "excl\\..+$", replacement = "")

    #extract traits data
    traits_i <- get_traits(pagetext,
                           names(plant_traits)[6:length(names(plant_traits))],
                           ger_rx_vec)

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

  cat(i, ": ", OrigName, ";   ", "\n", sep = "")

}

# remove rows without species name
plant_traits <- drop_na(plant_traits, species)

# replace empty cells with NA
plant_traits <- na_if(plant_traits, "")

# save raw traits
# done to preserve info abt duplicate species
fwrite(plant_traits, "download/bioflor_traits_raw.csv")

plant_traits <- fread("download/bioflor_traits_raw.csv",
                      na.strings = c("", "NA"))

# remove duplicate species, keeping trait information
plant_traits <- plant_traits %>%
  select(- bioflor_id) %>%
  mutate(across(c(FlStart, FlEnd, FlDur),
                ~ as.numeric(.x))) %>%
  group_by(species, GbifKey) %>%
  summarise(across(c(LifeForm, LifeSpan, ReprType:Habitat),
                   ~ paste(.x, collapse = ", ") %>%
                     str_split(", ") %>%
                     lapply(unique) %>%
                     lapply(na_rm_str) %>%
                     lapply(sort) %>%
                     lapply(paste, collapse = ", ") %>%
                     unlist()),
            across(where(is.numeric),
                   ~ mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(across(everything(),
                ~ na_if(.x, ""),
                ~ na_if(.x, "NA"),
                ~ na_if(.x, "NaN")))

#save trait table
fwrite(plant_traits, "static_data/bioflor_traits.csv")


# Add degrees of pollinator dependence ------------------------------------

if (!exists("plant_traits")) {
  
  #load plant trait table
  plant_traits <- fread("static_data/bioflor_traits.csv",
                        na.strings = c("", "NA"))
  
}

plant_traits <- plant_traits %>% 
  
  # check if a record is definitely PollDep = No
  mutate(pd_no_1 = str_detect(ReprType, "^v$")) %>% 
  
  mutate(pd_no_2 = !str_detect(PollVecGrp, "zo")) %>%
  
  # save cases where NAs would arise in pd_no
  mutate(pd_no_na = if_any(matches("pd_no"), is.na)) %>% 
  
  # change NAs to FALSE --> case that one is true NA doesnt matter
  mutate(across(matches("pd_no_\\d"),
                ~ if_else(.x, TRUE, FALSE, missing = FALSE))) %>%
  
  # compute final vector for pd = No
  mutate(pd_no = if_any(matches("pd_no_\\d"))) %>% 
  
  
  # check if a record is definitely PollDep = Yes
  mutate(pd_yes_1 = str_detect(Dicliny, "^do$") &
           str_detect(ReprType, "^s$") &
           str_detect(PollVec,  "^in$")) %>% 
  
  mutate(pd_yes_2 = str_detect(Dichogamy, "^(a3|g3)$") &
           str_detect(ReprType, "^s$") &
           str_detect(PollVec,  "^in$")) %>% 
  
  mutate(pd_yes_3 = str_detect(SelfComp, "^SI$") &
           str_detect(ReprType,  "^s$") &
           str_detect(PollVec,   "^in$")) %>% 
  
  mutate(pd_yes_4 = str_detect(BreedSys, "^X$") &
           str_detect(ReprType,  "^s$") &
           str_detect(PollVec,   "^in$")) %>% 
  
  # save cases where NAs would arise in pd_yes
  mutate(pd_yes_na = if_any(matches("pd_yes"), is.na)) %>% 
  
  # change NAs to FALSE --> case that one is true NA doesnt matter
  mutate(across(matches("pd_yes_\\d"),
                ~ if_else(.x, TRUE, FALSE, missing = FALSE))) %>%
  
  # compute final vector for pd = Yes
  mutate(pd_yes = if_any(matches("pd_yes_\\d"))) %>% 
  
  
  # set cases not clearly pd = yes or pd = no with enough available info to 
  #  intermediate
  mutate(pd_int = !(pd_yes | pd_no) & !(pd_yes_na | pd_no_na)) %>% 
  
  
  # make final single PollDep col
  mutate(PollDep = case_when(pd_int ~ "Intermediate",
                             pd_no  ~ "No",
                             pd_yes ~ "Yes")) %>% 
  
  
  # drop helper cols
  select(-matches("^pd_"))


# Simply trait data ----------------------------------------------------

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
fwrite(plant_traits, "static_data/bioflor_traits.csv")
