library(dplyr)

# Read in Data 
SankeySample <- read.csv("IXRS_StudySubjectData.csv")
glimpse(SankeySample)

# Code up standard starts and stop values (that aren't cleaving to the cycle day count system)
GenericVisitLookupTable <- c("Screening" = 0, "Rescreening 1" = 0.1, "Enrollment" = 1, "Screen Failure" = 999, "Discontinuation from Study Tx" = 9999)

# Create "nodes" dataframe
SankeyVisitCodeMap <- 
    select(SankeySample, VISIT_NAME) %>%
    distinct() %>% 
    mutate(vst = (gsub("Cycle | Day 1","", VISIT_NAME)),
           vstID = GenericVisitLookupTable[vst],
           vstID = ifelse(is.na(vstID),vst,vstID),
           vstID = as.numeric(vstID)) %>% 
    arrange(vstID) %>% 
    select(VISIT_NAME, vstID)
glimpse(SankeyVisitCodeMap)

# Create SankeyTriple
SankeyTriple <- 
    left_join(SankeySample, SankeyVisitCodeMap) %>%
    arrange(SCREENING_NUMBER, vstID)%>% 
    mutate(source = vstID, 
           target = lead(vstID)) %>%
    filter(target != 0) %>% 
    select(source, target) %>%
    group_by(source,target) %>% 
    summarise(value = n()) %>%
    na.omit
glimpse(SankeyTriple)
