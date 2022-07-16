library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

# set up indices and directory names
out <- readRDS('landis-output.rds')

# get economically-relevant harvested totals (Mg)
econ_status <-  read.csv('input/WI_economic_trees.csv')
colnames(econ_status) <- c('Species', 'Commercial')
econ_status[, 'Commercial'] <- ifelse(econ_status[, 'Commercial'] == 'X', TRUE, FALSE)
harvest <- lapply(out, function(l) l$Harvest)
harvest_tot <- ldply(harvest, .fun=colSums)[,-1]
colnames(harvest_tot) <- sapply(strsplit(colnames(harvest_tot), '_'), '[', 3)
econ_harvest <- harvest_tot[, colnames(harvest_tot) %in% econ_status[which(econ_status[, 'Commercial']), 'Species']]

# get stored carbon in soil and underground (MT C)
Cstore <- ldply(lapply(out, function(l) l$Soil_OrgMat), .fun='sum')

# get biomass of culturally relevant species
culturallyImportantSpp<-c("WhitePine", "SugarMaple","ServiceBerries", "BUtternut")
biomass <- lapply(out, function(l) l$AvgBiomass[l$AvgBiomass$Time == 100 & l$AvgBiomass$Species %in% culturallyImportantSpp, 3])
biomass <- ldply(biomass, .fun='sum')

# get diversity metric
diversity <- ldply(lapply(out, function(l) l$Diversity[l$Diversity$each_year == 100, 3]), .fun='identity')

harvest <- seq(0, 4)
wind <- seq(0, 3)
climate <- seq(0, 6)
gdd <- seq(0, 2)
scenarios <- expand.grid(harvest, wind, climate, gdd)
dat <- data.frame(scenarios, econ_harvest=rowSums(econ_harvest), Cstore=as.numeric(Cstore$sum), biomass=as.numeric(biomass$sum), diversity=diversity$identity)
colnames(dat) <- c('harvest', 'wind', 'climate', 'gdd', 'econ_harvest', 'Cstore', 'biomass', 'diversity')
dat$climate <- mapvalues(dat$climate, from=c("0", "1", "2", "3", "4", "5", "6"), to=c('Baseline', 'HAD', 'bcc', 'CCSM4', 'CSIRO', 'MRI', 'MIROC'))
dat$harvest <- mapvalues(dat$harvest, from=c("0", "1", "2", "3", "4"), to=c("BAU", "BAU Plus 20%", "BAU Minus 20%", "Even-Aged", "Uneven-Aged"))
dat <- dat %>% group_by(wind, gdd) %>% mutate(deltaC = Cstore - Cstore[climate == 'Baseline' & harvest == 'BAU'])
dat <-  dat %>% group_by(wind, gdd) %>% mutate(deltaH = econ_harvest - econ_harvest[climate == 'Baseline' & harvest == 'BAU'])
dat <- dat %>% group_by(wind, gdd) %>% mutate(deltaB = biomass - biomass[climate == 'Baseline' & harvest == 'BAU'])
dat <- dat %>% group_by(wind, gdd) %>% mutate(deltaD = diversity - diversity[climate == 'Baseline' & harvest == 'BAU'])
dat_diff <- dat %>% filter(climate != 'Baseline')
dat_diff <- as.data.frame(dat_diff)

dat_agg_list <- apply(dat_diff[,9:12], 2, function(col) {aggregate(col, by=list(dat_diff$harvest, dat_diff$climate), FUN=mean)})
dat_agg <- as.data.frame(ldply(dat_agg_list))
colnames(dat_agg) <- c('Metric', 'Harvest', 'Climate', 'Value')
dat_agg <- as.data.frame(pivot_wider(dat_agg, names_from=Metric, values_from=Value))

clim_labeller <- c(
  "Baseline" = "Baseline",
  "HAD" = "High\nWarming +\nDrier",
  "bcc" = "Moderate\nWarming +\nWetter",
  "CCSM4" = "Moderate\nWarming +\nDrier",
  "CSIRO" = "High\nWarming +\nWetter",
  "MRI" = "Less Warming",
  "MIROC" = "Moderate Warming")

colorder <- c('MRI', 'CCSM4', 'MIROC', 'bcc', 'HAD', 'CSIRO')
dat_agg$Climate <- factor(dat_agg$Climate, ordered=TRUE, levels=colorder)
dat_diff$climate <- factor(dat_diff$climate, ordered=TRUE, levels=colorder)

cols <- brewer.pal(7, 'Dark2')
col_labels <- c("MRI" = "Less Warming", 
                "CCSM4" = "Moderate Warming + Drier", 
                "MIROC" = "Moderate Warming", 
                "bcc" = "Moderate Warming + Wetter", 
                "HAD" = "High Warming + Drier", 
                "CSIRO" = "High Warming + Wetter")

style_landis_output <- function(p, colors, color_labels, base_size=10) {
  p <- p +
    theme_classic(base_size=base_size) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_vline(xintercept=0, linetype="dashed") +
    scale_color_manual('Climate Scenario', labels=color_labels, values = colors[c(1:5, 7)]) +
    scale_shape_discrete('Harvest Strategy') + 
    theme(legend.position="bottom", legend.box="vertical") + 
    guides(shape=guide_legend(nrow=3, byrow=TRUE), color=guide_legend(ncol=1, byrow=TRUE))

  p
}

get_legend <- function(p) {
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

## plot tradeoff plots with averaged metrics
# harvest vs stored carbon
p1 <- ggplot(dat_agg) + 
  geom_point(aes(y=deltaC, x=deltaH / 1000, color=Climate, shape=Harvest), size=4) +
  scale_x_continuous("Total Harvest 2000-2100 (Tt)") +
  scale_y_continuous("Stored Carbon in 2100 (Mt C)") +
  labs(tag="a)")
p1 <- style_landis_output(p1, colors=cols, color_labels=col_labels)

# harvest vs. biomass
p2 <- ggplot(dat_agg) + 
  geom_point(aes(y=deltaB, x=deltaH / 1000, color=Climate, shape=Harvest), size=4) + 
  scale_y_continuous(expression(paste('Culturally Significant Species Biomass ', (g/m^2)))) + 
  scale_x_continuous("Total Harvest 2000-2100 (Tt)") + 
  labs(tag="b)")
p2 <- style_landis_output(p2, colors=cols, color_labels=col_labels)

# harvest vs. diversity
p3 <- ggplot(dat_agg) + 
  geom_point(aes(y=deltaD, x=deltaH / 1000, color=Climate, shape=Harvest), size=4) + 
  scale_y_continuous('Shannon Diversity Index in 2100') + 
  scale_x_continuous("Total Harvest 2000-2100 (Tt)") +
  labs(tag="c)")
p3 <- style_landis_output(p3, colors=cols, color_labels=col_labels)

legend <- get_legend(p1)
p1 <- p1 + theme(legend.position="none")
p2 <- p2 + theme(legend.position="none")
p3 <- p3 + theme(legend.position="none")

pdf("figures/tradeoffs.pdf", height=8, width=8)
grid.arrange(p1, p2, p3, legend, nrow=2, top="Tradeoffs Between Metrics, Changes from Baseline Climate and Harvest")
dev.off()

png("figures/tradeoffs.png", height=8, width=8, units="in", res=300)
grid.arrange(p1, p2, p3, legend, nrow=2, top="Tradeoffs Between Metrics, Changes from Baseline Climate and Harvest")
dev.off()


# # plot tradeoff plots with non-averaged metrics
# pdf('harvest-carbon-all.pdf', height=6, width=6)
# ggplot(dat_diff) + 
#   geom_point(aes(x=deltaH / 1000, y=deltaC, color=climate, shape=harvest), size=2, alpha=0.5) + 
#   theme_classic(base_size=8) + 
#   scale_y_continuous('Change in Stored Carbon in 2100 From Baseline Climate (Mt C)') + 
#   scale_x_continuous('Change from Baseline, Total Harvest from 2000-2100 (Tt)') + 
#   scale_color_manual('Climate Scenario', labels=c("Less Warming", "Moderate Warming + Drier", "Moderate Warming", "Moderate Warming + Wetter", "High Warming + Drier", "High Warming + Wetter"),
#                      values = cols[c(1:5, 7)]) + 
#   scale_shape_discrete('Harvest Strategy') + theme(legend.position="bottom", legend.box="vertical") + 
#   guides(shape=guide_legend(nrow=2, byrow=TRUE), color=guide_legend(nrow=4, byrow=TRUE))
# dev.off()

# pdf('harvest-biodiversity-all.pdf', height=6, width=6)
# ggplot(dat_diff) + 
#   geom_point(aes(y=deltaD, x=deltaH / 1000, color=climate, shape=harvest), size=3) + 
#   theme_classic(base_size=8) + 
#   scale_y_continuous('Change in Shannon Diversity Index in 2100 from Baseline Climate') + 
#   scale_x_continuous('Change in Total Harvest from 2000-2100 From Baseline Climate (Tt)') + 
#   scale_color_manual('Climate Scenario', labels=c("Less Warming", "Moderate Warming + Drier", "Moderate Warming", "Moderate Warming + Wetter", "High Warming + Drier", "High Warming + Wetter"),
#                      values = cols[c(1:5, 7)]) +
#   scale_shape_discrete('Harvest Strategy') + theme(legend.position="bottom", legend.box="vertical") + 
#   guides(shape=guide_legend(nrow=2, byrow=TRUE), color=guide_legend(nrow=4, byrow=TRUE))
# dev.off()

# ## plot facets
# pdf('facet-carbon-harvest.pdf')
# ggplot() + geom_point(aes(y=deltaC, x=deltaH / 1000, color=Climate, shape=Harvest), data=dat_agg, size=3) + 
#     geom_point(data=dat_diff, aes(y=deltaC, x=deltaH / 1000, color=Climate, shape=Harvest), alpha=0.4) + 
#     facet_grid(Climate ~ Harvest, labeller=labeller(.rows = clim_labeller)) +
#     theme_bw(base_size=8) + 
#     scale_y_continuous('Change in Stored Carbon in 2100 From Baseline Climate (Mt C)') + 
#     scale_x_continuous('Change in Total Harvest from 2000-2100 From Baseline Climate (Tt)') + 
#     scale_color_manual('Climate Scenario', labels=c("Less Warming", "Moderate Warming + Drier", "Moderate Warming", "Moderate Warming + Wetter", "High Warming + Drier", "High Warming + Wetter"),
#                       values = cols[c(1:5, 7)], guide="none") +
#     scale_shape_discrete('Harvest Strategy', guide="none") + theme(legend.position="bottom", legend.box="vertical") 
# dev.off()

# pdf('facet-biodiversity-harvest.pdf')
# ggplot() + geom_point(aes(y=deltaD, x=deltaH / 1000, color=Climate, shape=Harvest), data=dat_agg, size=3) + 
#     geom_point(data=dat_diff, aes(y=deltaD, x=deltaH / 1000, color=Climate, shape=Harvest), alpha=0.4) + 
#     facet_grid(Climate ~ Harvest, labeller=labeller(.rows = clim_labeller)) +
#     theme_bw(base_size=8) + 
#     scale_y_continuous('Change in Shannon Diversity Index in 2100 from Baseline Climate') + 
#     scale_x_continuous('Change in Total Harvest from 2000-2100 From Baseline Climate (Tt)') + 
#     scale_color_manual('Climate Scenario', labels=c("Less Warming", "Moderate Warming + Drier", "Moderate Warming", "Moderate Warming + Wetter", "High Warming + Drier", "High Warming + Wetter"),
#                       values = cols[c(1:5, 7)], guide="none") +
#     scale_shape_discrete('Harvest Strategy', guide="none") + theme(legend.position="bottom", legend.box="vertical") 
# dev.off()