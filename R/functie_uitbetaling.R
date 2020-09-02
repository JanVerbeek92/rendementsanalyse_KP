
#' Een functie om hetaantal ingezette zelfopgeleide jeugdspelers onder 21 jaar 
#' in een reguliere competitiewedstrijd van het 1e elftal te analyseren
#' @param BVO naam van de club voor welke je de regio wilt weten
#' @param Straal straal van de regio in KM's
#' @return basisplot met daarin Nederland en de regio omtrent van 25 km

        functie_uitbetaling <-
                function(dat = tbl_compensatie,
                         cutOffPerc = 35.00,
                         pot_1 = 40000,
                         pot_2 = 620000) {
                        
                
                #' compensatie bedrag per
                # --------------------------------------------------------------  
                
                vec_BVOids <-
                        read_excel("~/Documents/3. Data/1. Voetbalmonitor/Data/20190820 - KNVB - Bronbestand BVO jeugdteams.xlsx",
                                   sheet = "BVO") %>% distinct(BVO,BVO_id,Team_id, Seizoen_id_Prev = Seizoen_id) %>%
                        mutate(Team_id = as.character(Team_id))
                
                dat  <- dat  %>% mutate(
                        PercPlayed =
                                ifelse(
                                        Klasse_id == "01",
                                        round(MinsPlayed / (34 * 90), 2),
                                        ifelse(Klasse_id == "02", round(MinsPlayed / (38 * 90), 2), NA)
                                ),
                        Compensabel = ifelse(PercPlayed >= (cutOffPerc/100), "Compensabel", "Geen Compensatie")
                ) %>% ungroup %>% group_by(Persoon_id, Compensabel) %>%
                        filter((Compensabel == "Geen Compensatie") |
                                       (Compensabel == "Compensabel" &
                                                row_number() < 3))
                        
                #' compensatie bedrag per
                # --------------------------------------------------------------
                vec_Compensatie <-
                        dat %>% group_by(Seizoen, Klasse_id) %>% filter(Compensabel == "Compensabel") %>%
                        summarise(n = n()) %>% ungroup %>% 
                        mutate(Compensatie_bedrag = 
                                       ifelse(Klasse_id == "01", round((pot_2 * (2 / 3)) / n, 0),
                                              round((pot_2 * (1 / 3)) / n, 0)))
                
                
                dat <- dat %>% left_join(vec_Compensatie)

                tbl_Uitbetaling <-
                        bind_rows(
                                dat %>% mutate(
                                        Seizoen_2_id = Seizoen_id - 1,
                                        Seizoen_3_id = Seizoen_id - 2,
                                        Seizoen_4_id = Seizoen_id - 3
                                ) %>% ungroup %>% select(
                                        Persoon_id,
                                        Volledige_naam,
                                        Klasse_id,
                                        Compensatie_bedrag,
                                        Compensabel,
                                        Seizoen_id,
                                        Seizoen_2_id,
                                        Seizoen_3_id,
                                        Seizoen_4_id
                                ) %>%
                                        tidyr::gather("PrevSeizoen", "Seizoen_id_Prev", c(7:9)) %>%
                                        left_join(
                                                dat_Loopbaan %>% select(Team_id,
                                                                        Persoon_id, Seizoen_id),
                                                by = c("Persoon_id", "Seizoen_id_Prev" = "Seizoen_id")
                                        ) %>% filter(Compensabel == "Compensabel") %>%
                                        left_join(vec_BVOids) %>% group_by(Persoon_id, Seizoen_id_Prev, Seizoen_id) %>%
                                        mutate(n = n()) %>%
                                mutate(Compensatie = ifelse(
                                        n == 1,
                                        round(Compensatie_bedrag * 0.25, 0),
                                        ifelse(n == 2, round((
                                                Compensatie_bedrag * 0.25
                                        ) * 0.5, 0), NA)
                                ))  %>% ungroup %>% select(Persoon_id, Volledige_naam, Seizoen_id, Compensatie, BVO, BVO_id),
                                dat %>% group_by(Persoon_id, Seizoen_id) %>%
                                        mutate(Transfer = ifelse(n() == 2, "Transfer", NA)) %>% 
                                        filter(Compensabel == "Compensabel") %>% ungroup %>%
                                        mutate(Compensatie = round(Compensatie_bedrag * 0.25, 0)) %>%
                                        left_join(
                                                read_excel(
                                                        "~/Documents/3. Data/1. Voetbalmonitor/Data/20190820 - KNVB - Bronbestand BVO jeugdteams.xlsx",
                                                        sheet = "BVO"
                                                ) %>% distinct(BVO, BVO_id,Team_id, Seizoen_id)
                                        ) %>% select(
                                                Persoon_id,
                                                Volledige_naam,
                                                Seizoen_id,
                                                Compensatie,
                                                BVO,
                                                BVO_id,
                                                Transfer
                                        )
                        ) %>% group_by(Seizoen_id, BVO, BVO_id) %>%
                        summarise(Rendement = sum(Compensatie)) %>%
                        filter(!(Seizoen_id %in% c(2016, 2019))) 
                
                tbl <- read_excel("~/OneDrive - KNVB/3. SAS data/EX/EX_K&P_BV_status.xlsx") %>% left_join(
                        read_excel(
                                "~/Documents/3. Data/1. Voetbalmonitor/Data/20190820 - KNVB - Bronbestand BVO jeugdteams.xlsx",
                                sheet = "BVO"
                        ) %>% filter(Seizoen_id >= 2017) %>%
                                distinct(BVO_id, BVO)
                )  %>%
                        distinct(BVO_id, BVO, Seizoen_id, Totaal, Opleiding_ID) %>%
                        left_join(tbl_Uitbetaling) %>%
                        mutate(
                                Basisbedrag = Opleiding_ID * pot_1,
                                Compensatie_tot = ifelse(is.na(Rendement), Basisbedrag, Basisbedrag + Rendement),
                                Verschil = Compensatie_tot - Totaal,
                                CutoffPerc = cutOffPerc,
                                BedragPot1 = pot_1,
                                BedragPot2 = pot_2
                        )
                
                return(tbl)
        }

                