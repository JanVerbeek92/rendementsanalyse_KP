# -----------------------------------------------------------------------------
#' 03. datavis
# -----------------------------------------------------------------------------

    library(readxl)
    library(dplyr)
    library(ggplot2)
    
    tbl_RendementClub <- read_excel("output/dat_RendementClub.xlsx") 
    
    # Split gezamenlijke jeugdopleidingen
    dubbele_BVO <- tbl_RendementClub %>% filter(grepl(",", BVO))
    s <- strsplit(dubbele_BVO$BVO, split = ", ")
    
    tbl_RendementClub <-
        bind_rows(
            # Aparte jeugdopleidingen
            tbl_RendementClub %>% filter(!grepl(",", BVO)),
            # Gezamenlijke jeugdopleidingen (gehalveerd)
            data.frame(
                Persoon_id = rep(dubbele_BVO$Persoon_id, sapply(s, length)),
                Volledige_naam = rep(dubbele_BVO$Volledige_naam, sapply(s, length)),
                BVO = unlist(s),
                Seizoen_id = rep(dubbele_BVO$Seizoen_id, sapply(s, length)),
                Eerste_elftal = rep(dubbele_BVO$Eerste_elftal, sapply(s, length)),
                Soort_compensatie = rep(dubbele_BVO$Soort_compensatie, sapply(s, length)),
                Compensatie = rep(dubbele_BVO$Compensatie, sapply(s, length))
            ) %>% mutate(Compensatie = Compensatie * 0.5)
        )
    
    
    # --------------------------------------------------------------------------
    
    tbl_RendementClub <- tbl_RendementClub %>% 
            group_by(BVO, Soort_compensatie) %>% 
            summarise(Compensatie = sum(Compensatie)) %>% ungroup %>% group_by(BVO) %>%
            mutate(Tot_compensatie = sum(Compensatie)) %>% filter(!is.na(BVO))    
    
    # Totaal per club 
    # --------------------------------------------------------------------------
    source('~/Documents/3. Data/1. Voetbalmonitor/R/datavis_knvb/ggplot_ThemeKNVB.R', echo=TRUE)
    
    theme <- ggplot_ThemeKNVB(text.size = 14, grid = "X")
    color <- ifelse(tbl_RendementClub %>% arrange(Tot_compensatie) %>% distinct(BVO) %>% .$BVO %in% c("FC Twente"),
                    "black",
                    "white")
    
    plot <- ggplot(tbl_RendementClub) + geom_bar(
        aes(
            x = reorder(BVO, Tot_compensatie),
            y = Compensatie,
            fill = Soort_compensatie
        ),
        position = "stack",
        stat = "identity",
        width = 0.5
    ) +
        coord_flip() + scale_y_continuous(
            expand = c(0, 0),
            labels = scales::comma,
            limits = c(0, 750000),
            name = paste0("Compensatie bedrag [in ", "\u20AC","]")
        ) +
        geom_text(
            data = tbl_RendementClub %>% distinct(BVO, Tot_compensatie),
            aes(
                x = reorder(BVO, Tot_compensatie),
                y = Tot_compensatie,
                label = paste("\u20AC", format(round(Tot_compensatie, 0), big.mark = "."))
            ),
            hjust = -0.5
        ) + theme +
        scale_fill_manual(values = c("#253780", "#00AADB"), name = "Soort compensatie") +
        labs(title = "COMPENSATIEBEDRAG OPGELEIDE SPELERS",
             subtitle = "SEIZOEN 2014/'15 TOT EN MET 2019/'20") + scale_x_discrete(name = NULL) +
        theme(axis.text.y = element_text(color = color))
    
    ggsave(
        filename = paste0("output/compensatie bedrag_FC Twente", ".png"),
        plot,
        width =  16,
        height = 9,
        dpi = 300,
        units = "in",
        device = 'png'
    )
    
    # Verschil met vorige K&P cyclus
    # --------------------------------------------------------------------------
    
    KP_bedrag_ave <- read_excel("~/OneDrive - KNVB/3. SAS data/EX/EX_K&P_BV_status.xlsx") %>%
         mutate(Tot_bedrag = Status_bedrag + BVO_bedrag) %>% group_by(BVO) %>% filter(Seizoen_id %in% c(2016:2018)) %>%
        summarise(Tot_bedrag = mean(Tot_bedrag, na.rm = T))
    
    
    tbl_RendementClub_ave <- tbl_RendementClub %>%
        group_by(BVO, Seizoen_id) %>%
        summarise(Compensatie = sum(Compensatie)) %>% mutate(Seizoen_id = factor(Seizoen_id)) %>%
        tidyr::complete(BVO, Seizoen_id, fill = list(Compensatie = 0)) %>% filter(Seizoen_id %in% c(2016:2018)) %>% 
        ungroup %>%
        group_by(BVO) %>% summarise(ave_Compensatie = mean(Compensatie)) %>%
        mutate(ave_Compensatie = ifelse(
            BVO %in% c("Telstar 1963", "TOP Oss",
                       "Heracles Almelo", "RKC Waalwijk"),
            ave_Compensatie ,
            ifelse(
                BVO %in% c("VVV-Venlo", "Helmond Sport"),
                ave_Compensatie  + 20000,
                ave_Compensatie  + 40000
            ))
        ) %>% filter(!is.na(BVO) &
                          !grepl("Achilles", BVO))  

    tbl_Diff <- tbl_RendementClub_ave %>%
        left_join(KP_bedrag_ave) %>% ungroup %>%
        mutate(diff_nieuw_oud = round(ave_Compensatie - Tot_bedrag,0),
               color = ifelse(diff_nieuw_oud < 1, "1", "0")) %>% ungroup
    
    color <- ifelse(tbl_Diff %>% arrange(diff_nieuw_oud) %>% distinct(BVO) %>% .$BVO %in% c("Willem II"),
                    "black",
                    "white")

    plot <- ggplot(tbl_Diff) + geom_bar(
        aes(
            x = reorder(BVO, diff_nieuw_oud),
            y = diff_nieuw_oud,
            fill = color
        ),
        stat = "identity",
        show_guide = F,
        width = 0.5
    ) +
        coord_flip() +
        scale_y_continuous(
            labels = paste("\u20AC", seq(-150000, 150000, by = 5000)),
            breaks = seq(-150000, 150000, by = 5000),
            limits = c(-50000, 50000),
            name = NULL
        ) + theme +
        scale_x_discrete(NULL) +
        geom_text(
            data = tbl_Diff %>% filter(color == 0),
            aes(
                x = reorder(BVO, diff_nieuw_oud),
                y = diff_nieuw_oud,
                label = paste0(
                    "\u20AC",
                    formatC(
                        diff_nieuw_oud,
                        format = "f",
                        big.mark = ".",
                        digits = 0
                    )
                )
            ),
            hjust = -0.5
        ) +
        geom_text(
            data = tbl_Diff %>% filter(color == 1),
            aes(
                x = reorder(BVO, diff_nieuw_oud),
                y = diff_nieuw_oud,
                label = paste0(
                    "\u20AC",
                    formatC(
                        diff_nieuw_oud,
                        format = "f",
                        big.mark = ".",
                        digits = 0
                    )
                )
            ),
            hjust = 1.5
        ) +
        ggplot2::theme(
            axis.text.y = element_text(color = color),
            #axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank()
        ) +
        scale_fill_manual(values = c('#008000', '#ff0000')) +
        labs(title = "VERSCHIL COMPENSATIE OUD VS. NIEUW K&P TRAJECT",
             subtitle = "SEIZOEN 2016/'17 TOT EN MET 2018/'19")   
    
    ggsave(
        filename = paste0("output/oudvsnieuw_Willem II", ".png"),
        plot,
        width =  16,
        height = 9,
        dpi = 300,
        units = "in",
        device = 'png'
    )
    