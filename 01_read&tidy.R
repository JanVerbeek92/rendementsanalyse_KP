# -----------------------------------------------------------------------------
#' 01. read & tidy data
# -----------------------------------------------------------------------------

    library(dplyr)
    library(readxl)

    dat_Weddeelnemers1e <- read_excel("~/OneDrive - KNVB/3. SAS data/P1/P1_STAGE_SL_WEDDEELNEMERS_1e.xlsx")
    dat_Weds1e <- read_excel("~/OneDrive - KNVB/3. SAS data/P1/P1_STAGE_SL_WEDS_1e.xlsx") 
    
    dat_Weddeelnemers1e <- read_excel("~/OneDrive - KNVB/3. SAS data/P1/P1_STAGE_SL_WEDDEELNEMERS_1e_2020.xlsx")
    dat_Weds1e <- read_excel("~/OneDrive - KNVB/3. SAS data/P1/P1_STAGE_SL_WEDS_1e_2020.xlsx") 

    source('R/functie_SpelersBVU21.R', echo=TRUE)    
    
    #' Het aantal ingezette zelfopgeleide jeugdspelers onder 21 jaar 
    #' in een reguliere competitiewedstrijd van het 1e elftal te analyseren
    # ----------------------------------------------------------------------
    
    # Peilseizoen 2020/'21
    dat_SpelersU21Seizoen <- functie_SpelersBVU21(opleidingsjaren = 6, peil_seizoen_id = 2020)
    
    # Peilseizoenen previous
    dat_Compensatie <- dat_SpelersU21Seizoen %>% mutate(
        TotGames = ifelse(
            Klasse_id == "01" & Seizoen_id != 2019,
            34,
            ifelse(
                Klasse_id == "02" & Seizoen_id != 2019,
                38,
                ifelse(
                    Klasse_id == "01" & Seizoen_id == 2019,
                    26,
                    ifelse(Klasse_id == "02" &
                               Seizoen_id == 2019, 29, NA)
                )
            )
        ),
        PercPlayed = round(MinsPlayed / (TotGames * 90), 2),
        Compensabel = ifelse(PercPlayed >= (35 / 100),
                             "Compensabel",
                             "Geen Compensatie")
    ) %>% ungroup %>% group_by(Persoon_id, Compensabel) %>%
        filter((Compensabel == "Geen Compensatie") |
                   (Compensabel == "Compensabel" &
                        row_number() < 3)) %>% select(-TotGames)
    
    openxlsx::write.xlsx(dat_Compensatie, "output/dat_Compensatie.xlsx")
    
    # Peilseizoenen 2020/'21
    # ----------------------------------------------------------------------
    
    dat_Weddeelnemers1e %>% distinct(Wedstrijd_id_int, Team_id) %>%
        group_by(Team_id) %>% summarise(TotGames = n())
    
    dat_Compensatie <- dat_SpelersU21Seizoen %>%
        # Aantal gespeelde wedstrijden per team
        left_join(
            dat_Weddeelnemers1e %>% distinct(Wedstrijd_id_int, Team_id) %>%
                group_by(Team_id) %>% summarise(TotGames = n())
        ) %>%
        mutate(
            PercPlayed = round(MinsPlayed / (TotGames * 90), 2),
            Compensabel = ifelse(PercPlayed >= (35 / 100),
                                 "Compensabel",
                                 "Geen Compensatie")
        ) %>% ungroup %>% group_by(Persoon_id, Compensabel) %>%
        filter((Compensabel == "Geen Compensatie") |
                   (Compensabel == "Compensabel" &
                        row_number() < 3)) %>% select(-TotGames)
    
    openxlsx::write.xlsx(dat_Compensatie, "output/dat_Compensatie_2020.xlsx")
    