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
    
    source('~/Documents/3. Data/1. Voetbalmonitor/R/datavis_knvb/ggplot_ThemeKNVB.R', echo=TRUE)
    
    theme <- ggplot_ThemeKNVB(text.size = 14, grid = "X")
    
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
        theme(axis.text.y = element_text(color = "white"))
    
    ggsave(
        filename = paste0("output/compensatie bedrag", ".png"),
        plot,
        width =  16,
        height = 9,
        dpi = 300,
        units = "in",
        device = 'png'
    )
    