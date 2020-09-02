# -----------------------------------------------------------------------------
#' 02. analyse
# -----------------------------------------------------------------------------

    library(readxl)
    library(dplyr)

    tbl_Compensatie <- read_excel("output/dat_Compensatie.xlsx")

    dat_Loopbaan <-
        read_excel("~/OneDrive - KNVB/3. SAS data/VM/VM_2020_Loopbaan.xlsx") 
    
    # --------------------------------------------------------------------------
    
    # Compensatie bedrag per seizoen (o.b.v totaal aantal compensabel)
    vec_Compensatie <-
        tbl_Compensatie %>% group_by(Seizoen, Klasse_id) %>% filter(Compensabel == "Compensabel") %>%
        summarise(n = n()) %>% ungroup %>%
        mutate(Compensatie_bedrag =
                   ifelse(Klasse_id == "01", round((620000 * (
                       2 / 3
                   )) / n, 0),
                   round((620000 * (
                       1 / 3
                   )) / n, 0)))
    
    tbl_Compensatie <- tbl_Compensatie %>% left_join(vec_Compensatie)

    # Uitbetaling per speler
    tbl_Uitbetaling <-
        bind_rows(
            # Bedragen per speler op basis van vorige clubs (3 seizoenen terug)
            tbl_Compensatie %>% mutate(
                Seizoen_2_id = Seizoen_id - 1,
                Seizoen_3_id = Seizoen_id - 2,
                Seizoen_4_id = Seizoen_id - 3
            ) %>% ungroup %>% select(
                Persoon_id,
                Klasse_id,
                Compensatie_seizoen,
                Compensatie_bedrag,
                Compensabel,
                Seizoen_id,
                Seizoen_2_id,
                Seizoen_3_id,
                Seizoen_4_id
            ) %>%
                tidyr::gather("PrevSeizoen", "Seizoen_id_Prev", c("Seizoen_2_id",
                                                                  "Seizoen_3_id",
                                                                  "Seizoen_4_id")) %>%
                left_join(
                    dat_Loopbaan %>% select(Team_id,
                                            Persoon_id, Seizoen_id),
                    by = c("Persoon_id", "Seizoen_id_Prev" = "Seizoen_id")
                ) %>% filter(Compensabel == "Compensabel"),
            # Bedragen per speler op basis van huidige 1e elftal waar 
            # minuten in BV zijn gemaakt
            tbl_Compensatie %>% select(
                Persoon_id,
                Klasse_id,
                Compensatie_seizoen,
                Compensatie_bedrag,
                Compensabel,
                Seizoen_id,
                Team_id
            )  %>% filter(Compensabel == "Compensabel")
        ) %>%
        arrange(Persoon_id, Compensatie_seizoen, desc(PrevSeizoen)) %>%
        mutate(Eerste_elftal = ifelse(is.na(Seizoen_id_Prev), "1e_elftal", NA),
               Soort_compensatie = ifelse(is.na(Seizoen_id_Prev), "Debuut","Opleiding"),
               Seizoen_id_Prev = ifelse(is.na(Seizoen_id_Prev), Seizoen_id, Seizoen_id_Prev)) %>% 
        group_by(Persoon_id, Seizoen_id_Prev, Seizoen_id) %>%
        mutate(n = n())  %>%
        mutate(Compensatie = ifelse(
            n == 1,
            round(Compensatie_bedrag * 0.25, 0),
            ifelse(n == 2, round((
                Compensatie_bedrag * 0.25
            ) * 0.5, 0), NA)
        ))  %>% ungroup  
                
    
    # Teams behorende bij BVOs
    vec_BVOids <- bind_rows(
        read_excel(
            "~/OneDrive - KNVB/3. SAS data/VM/VM_BVO_teams_2020.xlsx",
            sheet = "BVO_Jeugd_teams"
        ),
        read_excel(
            "~/OneDrive - KNVB/3. SAS data/VM/VM_BVO_teams_2020.xlsx",
            sheet = "BVO_Senioren_teams"
        ) %>% mutate(Team = as.character(Team))
    ) %>% distinct(BVO, BVO_id =  Vereniging_id, Team_id, Seizoen_id_Prev = Seizoen_id)  

    
    tbl_RendementClub <- tbl_Uitbetaling %>% left_join(vec_BVOids) %>%
        select(Persoon_id, BVO, Compensatie_seizoen, Seizoen_id, 
               Seizoen_id_Prev, Eerste_elftal, Soort_compensatie, Compensatie) 

    openxlsx::write.xlsx(tbl_RendementClub, "output/dat_RendementClub.xlsx")    
    