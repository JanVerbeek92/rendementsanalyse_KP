
#' Een functie om hetaantal ingezette zelfopgeleide jeugdspelers onder 21 jaar 
#' in een reguliere competitiewedstrijd van het 1e elftal te analyseren
#' @param opleidingsjaren wat is het minimum aantal jaar dat spelers al in Nederland
#' moeten zijn opgeleid
#' @param peil_seizoen_id vanaf welk seizoen moeten de spelers worden geanalyseerd
#' @return data.frame met compensabele spelers

        functie_SpelersBVU21 <-
                function(opleidingsjaren = 6, peil_seizoen_id = 2016) {
                    
                dat_Loopbaan <-
                    read_excel("~/OneDrive - KNVB/3. SAS data/VM/VM_2020_Loopbaan.xlsx")    
                    
                #' Reguliere 1e elftallen
                # --------------------------------------------------------------    
                vec_TeamsBV <- dat_Weds1e %>% filter(Competitie_type == "Nederlandse reguliere competitie") %>%
                        distinct(Vereniging_thuis, Vereniging_id_thuis, Team_id_thuis, 
                                 Team_thuis, Klasse_id, Seizoen_id) %>%
                        rename(Vereniging = "Vereniging_thuis",
                               Vereniging_id = "Vereniging_id_thuis",
                               Team_id = "Team_id_thuis",
                               Team = "Team_thuis") %>%
                        mutate(Team = paste(Vereniging, gsub("2","Jong",Team)))
                
                
                #' Vind alleen de spelers die opgeleid zijn in NL
                # --------------------------------------------------------------
                
                vec_OpgeleidNL <-
                    dat_Loopbaan %>% 
                    group_by(Persoon_id, Volledige_naam) %>%
                    filter(Leeftijd < 21.00) %>%
                    summarise(n = n_distinct(Seizoen_id)) %>%
                    filter(n >= opleidingsjaren)
                        
                        
                dat_SpelersU21Seizoen <-
                        # Vind alle reguliere competitiewedstrijden vanaf seizoen 2007/'08 
                        dat_Weddeelnemers1e %>% left_join(
                                dat_Weds1e %>%
                                        filter(Competitie_type == "Nederlandse reguliere competitie") %>%
                                        select(Wedstrijd_id_int,
                                               Wedstrijddatum,
                                               Competitie_type)
                        ) %>%
                        # Join het goede team bij iedere ingezette spelers
                        left_join(vec_TeamsBV,
                                  by = c("Team_id", "Seizoen_id", "Klasse_id")) %>% 
                        filter(!grepl("Jong", Team) & Klasse_id %in% c("01", "02")) %>%
                        arrange(Persoon_id, Seizoen_id) %>%
                        distinct(Persoon_id,
                                 Seizoen_id,
                                 Team_id,
                                 .keep_all = T) %>%
                        select(
                                Persoon_id,
                                Geboortedatum,
                                Team_id,
                                Seizoen_id,
                                Seizoen,
                                Klasse_id,
                        )  %>%
                        left_join(
                                data.frame(
                                        Peildatum =
                                                seq(as.Date("1991/1/1"), as.Date("2020/1/1"), "years"),
                                        Seizoen_id = seq(1990, 2019)
                                ),
                                by = c("Seizoen_id")
                        ) %>%
                        mutate(
                                Leeftijd_peildatum = round(as.numeric(
                                        as.POSIXct(Peildatum) - Geboortedatum
                                ) / 365.25, 2)
                        ) %>%
                        filter(Seizoen_id >= peil_seizoen_id & Leeftijd_peildatum < 21.00) %>% 
                        filter(Persoon_id %in% vec_OpgeleidNL$Persoon_id) #%>%
                                #left_join(dat_Loopbaan %>% distinct(Persoon_id, Volledige_naam)) 
                
                        
                        vec_ids <- dat_SpelersU21Seizoen %>% distinct(Persoon_id, Seizoen_id) %>%
                                group_by(Persoon_id) %>% mutate(Compensatie_seizoen = 
                                                                        paste0("Seizoen_", row_number()))
                        
                        dat_SpelersU21Seizoen <- dat_SpelersU21Seizoen %>% left_join(vec_ids)
                        
                        # Bepaalde gespeelde minuten per speler, per seizoen,
                        # per team
                        tbl_spelersU21SeizoenMins <- 
                            do.call("rbind", as.list(by(dat_SpelersU21Seizoen,
                                        list(
                                            var1 = dat_SpelersU21Seizoen$Persoon_id,
                                            var2 = dat_SpelersU21Seizoen$Seizoen_id,
                                            var3 = dat_SpelersU21Seizoen$Team_id
                                        ), function(x) {
                                            y <- dat_Weddeelnemers1e %>% left_join(
                                                # Vind alle reguliere competitiewedstrijden vanaf seizoen 2007/'08 
                                                dat_Weds1e %>%
                                                    filter(Competitie_type == "Nederlandse reguliere competitie") %>%
                                                    select(Wedstrijd_id_int,
                                                           Wedstrijddatum,
                                                           Competitie_type),
                                                by = c("Wedstrijd_id_int")) %>%
                                                left_join(vec_TeamsBV,
                                                          by = c("Team_id", "Seizoen_id", "Klasse_id")) %>%
                                                # Filter wedstrijdminuten voor Jong teams
                                                filter(!grepl("Jong", Team) &
                                                           Klasse_id %in% c("01", "02")) %>%
                                                filter(Persoon_id %in% x$Persoon_id &
                                                           Seizoen_id == x$Seizoen_id &
                                                           Team_id == x$Team_id) %>%
                                                group_by(Persoon_id,
                                                         Team_id,
                                                         Team,
                                                         Seizoen_id)  %>%
                                                summarise(MinsPlayed = sum(Minuten_gespeeld))
                                        })))
                        
                        dat_SpelersU21Seizoen <- dat_SpelersU21Seizoen %>% 
                            left_join(tbl_spelersU21SeizoenMins)
                        
                        return(dat_SpelersU21Seizoen)
                }