seq\_map\_data
================
Rebecca Batstone
2019-09-03

Load packages
-------------

``` r
# packages
library("tidyverse") ## includes ggplot2, dplyr, readr, stringr
library("cowplot") ## paneled graphs
library("RColorBrewer") ## to specify color brewers
library("magick") ## include images
```

Sequencing data
---------------

``` r
seq_data <- read_csv("./raw_data/seq_data.csv", 
    col_types = cols(plot = col_factor(levels = c("P1","P2","P3","P4"))))

plot_names <- list(
  'P1'="Plot 1",
  'P2'="Plot 2",
  'P3'="Plot 3",
  'P4'="Plot 4"
)

plot_labeller <- function(variable,value){
  return(plot_names[value])
}

seq_data.rc <- seq_data %>%
  mutate(category=recode(category, 
                         'bacillus'="Bacillus spp.",
                         'Ensifer'="Ensifer spp.",
                         'medicae'="E. medicae",
                         'meliloti'="E. meliloti"))

seq_data$category <- factor(seq_data$category, 
                            levels = c("Bacillus spp.","Ensifer spp.","E. medicae","E. meliloti"))

species_colours <- brewer.pal(4,"Paired")
names(species_colours) <- levels(seq_data$category)

colScale <- scale_fill_manual(name = "Species", values = species_colours, 
                               breaks=c("Bacillus spp.","Ensifer spp.","E. medicae","E. meliloti"),
                               labels=c(substitute(paste(italic("Bacillus")," spp.")),
                                        substitute(paste(italic("Ensifer")," spp.")),
                                        substitute(paste(italic("E. medicae"))),
                                        substitute(paste(italic("E. meliloti")))))

(plot_16S_seq <- ggplot(data=seq_data.rc, aes(x=plot, fill=category)) +
  geom_bar(stat="count") +
  theme_bw() + 
  xlab(NULL) + 
  ylab("Nodules (no.)") +
  colScale +
  scale_x_discrete(labels=plot_labeller) +
  theme(axis.text.x = element_text(size = 16, colour = c('darkred', 'darkblue','darkmagenta', 'darkgoldenrod')),
    axis.title.y = element_text(colour = "black", size = 18), 
        axis.text.y = element_text(size=16), 
        axis.title.x = element_text(colour = "black", size = 18), 
        legend.position=c(0.62,0.8),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size=8),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())) 
```

![](figure1_seq_map_files/figure-markdown_github/16S_seq-1.png)

``` r
plot_maps <- ggdraw() + draw_image("./updated_Fig1A.tiff", scale = 1)

fig <- plot_grid(plot_maps, plot_16S_seq,
          labels = "AUTO",
          ncol = 2,
          nrow = 1,
          align = "h")

save_plot("./figures/Fig1_maps_seq.pdf", fig,
          ncol = 2, # we're saving a grid plot of 4 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
          )
```
