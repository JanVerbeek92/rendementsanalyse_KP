
functie_doorrekening <-
        function(dat = tbl_compensatie,
                 cutOffPerc = 35.00,
                 pot_1 = 40000,
                 pot_2 = 620000) {
                
                
                #' compensatie bedrag per
                # --------------------------------------------------------------  
                
                vec_BVOids <-
                        read_excel("data/raw/20190820 - KNVB - Bronbestand BVO jeugdteams.xlsx",
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
                                        ))  %>% ungroup %>% select(
                                                Persoon_id,
                                                Volledige_naam,
                                                Seizoen_id,
                                                Compensatie,
                                                BVO,
                                                BVO_id,
                                                Klasse_id,
                                                Compensatie_bedrag,
                                                n
                                        ),
                                dat %>% group_by(Persoon_id, Seizoen_id) %>%
                                        mutate(Transfer = ifelse(n() == 2, "Transfer", NA)) %>%
                                        filter(Compensabel == "Compensabel") %>% ungroup %>%
                                        mutate(Compensatie = round(Compensatie_bedrag * 0.25, 0),
                                               n = 1) %>%
                                        left_join(
                                                read_excel(
                                                    "~/Documents/3. Data/1. Voetbalmonitor/Data/20190820 - KNVB - Bronbestand BVO jeugdteams.xlsx",
                                                        sheet = "BVO"
                                                ) %>% distinct(BVO, BVO_id, Team_id, Seizoen_id)
                                        ) %>% select(
                                                Persoon_id,
                                                Volledige_naam,
                                                Seizoen_id,
                                                Compensatie,
                                                BVO,
                                                BVO_id,
                                                Transfer,
                                                Klasse_id,
                                                Compensatie_bedrag,
                                                n
                                        )
                        ) %>% group_by(Seizoen_id, BVO, BVO_id, Klasse_id, Compensatie_bedrag, n) %>%
                        summarise(Rendement = sum(Compensatie),
                                  n_Compensabel = n())  %>%
                        filter(!(Seizoen_id %in% c(2019))) 
                
                test <- tbl_Uitbetaling %>% group_by(BVO, BVO_id, Klasse_id, n) %>%
                        summarise(ave_Compensabel = mean(n_Compensabel, na.rm = T)) %>%
                        left_join(vec_Compensatie %>%
                                          filter(Seizoen %in% c("2016/'17","2017/'18", "2018/'19")) %>%
                                          group_by(Klasse_id) %>% summarise(Compensatie_bedrag = mean(Compensatie_bedrag))) %>%
                        mutate(Compensatie_bedrag = ifelse(
                                n == 1,
                                round((Compensatie_bedrag * 0.25)*ave_Compensabel, 0),
                                ifelse(n == 2, round(((
                                        Compensatie_bedrag * 0.25
                                )*ave_Compensabel) * 0.5, 0), NA))) %>% group_by(BVO, BVO_id) %>%
                        summarise(Rendement = sum(Compensatie_bedrag))
                
                
                
                tbl <- read_excel("~/OneDrive - KNVB/3. SAS data/EX/EX_K&P_BV_status.xlsx") %>% group_by(BVO_id) %>%
                    summarise(Totaal = mean(Totaal, na.rm = T),
                              `K&P` = mean(`K&P`, na.rm = T),
                              BVO_bedrag = mean(BVO_bedrag, na.rm = T),
                              Opleiding_ID = mean(Opleiding_ID)) %>% left_join(
                                  read_excel(
                                      "~/Documents/3. Data/1. Voetbalmonitor/Data/20190820 - KNVB - Bronbestand BVO jeugdteams.xlsx",
                                      sheet = "BVO"
                                  ) %>% filter(Seizoen_id >= 2017) %>%
                                      distinct(BVO_id, BVO)
                              )  %>%
                    distinct(BVO_id, BVO, `K&P`, BVO_bedrag, Opleiding_ID) %>%
                    left_join(test) %>% group_by(BVO_id) %>% mutate(`K&P` = ifelse(`K&P` == "NaN", 0, `K&P`),
                                                                    Rendement = ifelse(is.na(Rendement), 0, Rendement)) %>% ungroup %>%
                    mutate(
                        Basisbedrag = Opleiding_ID * pot_1,
                        Verschil_KP = Rendement - `K&P`,
                        Verschil_Basis = Basisbedrag - BVO_bedrag
                    )
                
                return(tbl)
        }


tbl <- tbl %>% mutate(Verschil_tot = Verschil_KP + Verschil_Basis) %>% 
    mutate(color = ifelse(Verschil_tot < 0, "1", "0"))


theme <- ggplotTheme_KNVB(grid = " ", text.size = 14)
plot <- ggplot(tbl ) + geom_bar(aes(x = reorder(BVO, Verschil_tot), y = Verschil_tot,
                                    fill = color), stat = "identity", show_guide = F, width = 0.5) +
    coord_flip() +
    scale_y_continuous(labels = paste("\u20AC", seq(-150000,150000, by = 5000)),
                       breaks = seq(-150000,150000, by = 5000),
                       limits = c(-50000,50000), 
                       name = "VERSCHIL OUD - NIEUW") + theme +
    scale_x_discrete(NULL) +
    geom_text(data = tbl %>% filter(color == 0),
              aes(x = reorder(BVO, Verschil_tot), y = -7500,
                  label = BVO)) +
    geom_text(data = tbl %>% filter(color == 1),
              aes(x = reorder(BVO, Verschil_tot), y = 7500,
                  label = BVO)) +
    geom_text(data = tbl %>% filter(color == 0),
              aes(x = reorder(BVO, Verschil_tot), y = Verschil_tot,
                  label = paste0("\u20AC",formatC(Verschil_tot, format="f", big.mark=".", digits=0))), hjust = -0.5) +
    geom_text(data = tbl %>% filter(color == 1),
              aes(x = reorder(BVO, Verschil_tot), y = Verschil_tot,
                  label = paste0("\u20AC",formatC(Verschil_tot, format="f", big.mark=".", digits=0))), hjust = 1.5) +
    ggplot2::theme( axis.text.y = element_blank(),
                    axis.line.y = element_blank(),
                    axis.ticks.y = element_blank()) +
    scale_fill_manual(values = c('#008000', '#ff0000'))
    