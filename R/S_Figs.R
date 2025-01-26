library(ggplot2)
library(cowplot)

###########################################################################
# Fig. 1 - Endemism levels ------------------------------------------------
head(sppRich)
sppRich$class <- factor(sppRich$class, levels=c("Amphibians", "Reptiles", "Birds", "Mammals"))
sppRich$year <- factor(sppRich$year)

sppRich$labNe <- c(NA,NA,"Ne=45", "Ne=124", NA,NA,"Ne=24", "Ne=129", NA,NA,"Ne=29", "Ne=45", NA,NA,"Ne=19", "Ne=42")
sppRich$labN <- c(NA,NA,"N=150", "N=292", NA,NA,"N=120", "N=419", NA,NA,"N=837", "N=982", NA,NA,"N=161", "N=354")
sppRich$lab <- round(sppRich$lab*100, digits=2)
sppRich$lab_perc <- sprintf("%0.2f%%", sppRich$lab)
lab_pos <- c(NA, NA, 0.38, 0.505, NA, NA, 0.28, 0.39, NA, NA, 0.1146, 0.1358, NA, NA, 0.198, 0.198)
sppRich$lab_pos <- lab_pos

colors.book <- c("#426635", "#67984C")

fig1 <- ggplot2::ggplot(data=sppRich, aes(x=year, y=richness, fill=end))+
  geom_bar(stat="identity", position=position_fill(reverse = TRUE), width = .7)+
  facet_grid(~class)+
  geom_text(aes(y=lab_pos, label=lab_perc), vjust=1.6, 
            color="white", size=3)+
  geom_text(aes(y=0.80, label=labNe), vjust=1.6, 
            color="white", size=3)+
  geom_text(aes(y=0.90, label=labN), vjust=1.6, 
            color="white", size=3)+
  labs(x= "Year", y= "Richness")+
  scale_fill_manual(values=colors.book)+
  scale_y_continuous(expand=c(0,0), breaks = c(0, 0.5, 1))+
  theme_classic()+
  theme(#panel.spacing = unit(-1, "lines"),
        aspect.ratio = 1.3/1,
        legend.position='none',
        strip.text.x = element_blank(),
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=10))

fig1

ggsave("Fig 1.png",
       device = png,
       plot = fig1,
       path = here::here("outputs", "figures"),
       width = 168,
       height = 80,
       units = "mm",
       dpi = 300,
)

###########################################################################
# Fig 2a ------------------------------------------------------------------

spyear <- as.data.frame(table(list$year))
spyear$Var1 <- as.numeric(as.character(spyear$Var1))

mean(spyear$Freq[spyear$Var1>=2000])

fig2 <- ggplot(data=spyear, aes(x=Var1, y=Freq))+
  geom_point(size=3)+
  labs(x="Year of description", y="Number of species")+
  geom_smooth(method="gam", color="black", se=FALSE)+
  theme_classic()+
  theme(legend.position='right',
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=10))
fig2

ggsave("Fig 2.png",
       device = png,
       plot = fig2,
       path = here::here("outputs", "figures"),
       width = 168,
       height = 90,
       units = "mm",
       dpi = 300,
)

###########################################################################
# Fig. 4 Color ------------------------------------------------------------

colors <- c("#000000","#d6231e", "#fd7e4b","#fae639","#d1d1c5","#cce041","#67c262")

# Fig. 4a - Species threat status -----------------------------------------

fig4a <- ggplot2::ggplot(data=iucn.class, aes(x=class, y=Freq, fill=category))+
  geom_bar(stat="identity", position=position_fill(reverse = TRUE), width = .8)+
  geom_text(aes(y=0.9, label=paste("N = ", labelN, sep="")), 
            vjust=1.6, color="black", size=3)+
  scale_fill_manual(values=colors)+
  labs(x= "Class", y= "Frequency", fill="Threat Category")+
  scale_y_continuous(expand=c(0,0), breaks = c(0,0.5,1))+
  theme_classic()+
  theme(aspect.ratio = 1/1.2,
        plot.background = element_rect(fill='transparent', color=NA),
        panel.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent', colour = "transparent"),
        legend.position='none',
        legend.direction = "vertical", 
        legend.box.margin = margin(t=0, r=0, b=0, l=0, unit="mm"),
        legend.box.spacing = unit(0, "mm"),
        legend.key.size = unit(10, "mm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin = unit(c(1.5,0,0,0), "mm"),
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=10))+
  guides(fill = guide_legend(title.position = "top", 
                             title.hjust = 0.5,
                             label.position = "right",
                             reverse=TRUE)) 

fig4a

legend <- cowplot::get_legend(fig4a)
plot(legend)

ggsave("Fig 4a.png",
       device = png,
       plot = fig4a,
       path = here::here("outputs", "figures"),
       width = 56,
       height = 78,
       units = "mm",
       dpi = 300,
)

# Fig. 4b - Range category - Threat ----------------------------------------
#Done

fig4b <- ggplot2::ggplot(data=range.iucn, aes(x=range.cat, y=Freq, fill=category))+
  geom_bar(stat="identity", position=position_fill(reverse = TRUE), width = .7)+
  geom_text(aes(y=0.9, label=paste("N = ", labelN, sep="")), 
            vjust=1.6, color="black", size=3)+
  scale_fill_manual(values=colors)+
  labs(x= "Range Size Category", y= "Frequency", fill="Threat Category")+
  scale_y_continuous(expand=c(0,0), breaks = c(0,0.5,1))+
  scale_x_discrete(expand=c(0.23,0))+
  theme_classic()+
  theme(aspect.ratio = 1/1.2,
        plot.background = element_rect(fill='transparent', color=NA),
        panel.background = element_rect(fill='transparent'),
        legend.position='none', legend.key.spacing = unit(3,"mm"),
        legend.box.spacing=unit(5, "mm"),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(1.5,0,0,0), "mm"),
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=10))

fig4b

ggsave("Fig 4b.png",
       device = png,
       plot = fig4b,
       path = here::here("outputs", "figures"),
       width = 56,
       height = 78,
       units = "mm",
       dpi = 300,
)

# Fig. 4c - Habitat loss versus threat status ------------------------------
#Done

fig4c <- ggplot2::ggplot(data=hab.cat, aes(x=loss, y=Freq, fill=category))+
  geom_bar(stat="identity", position=position_fill(reverse = TRUE), width = .8)+
  geom_text(aes(y=0.9, label=paste("N = ", labelN, sep="")), 
            vjust=1.6, color="black", size=3)+
  scale_fill_manual(values=colors)+
  labs(x= "Remaining Habitat", y= "Frequency")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_discrete(expand=c(0.15,0))+
  theme_classic()+
  theme(aspect.ratio = 1/1.2,
        plot.background = element_rect(fill='transparent', color=NA),
        panel.background = element_rect(fill='transparent'),
        legend.position='none',
        plot.margin = unit(c(0,0,0,0), "mm"),
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=10))

fig4c

ggsave("Fig 4c.png",
       device = png,
       plot = fig4c,
       path = here::here("outputs", "figures"),
       width = 56,
       height = 78,
       units = "mm",
       dpi = 300,
)

###########################################################################
###########################################################################

fig4d <- ggplot2::ggplot(data=gap.tab, aes(x=gap.cat, y=Freq, fill=IUCN))+
  geom_bar(stat="identity", position=position_fill(reverse = TRUE), width = .8)+
  geom_text(aes(y=0.9, label=paste("N = ", labelN, sep="")), 
            vjust=1.6, color="black", size=3)+
  scale_fill_manual(values=colors)+
  labs(x= "Protected Range", y= "Frequency")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_discrete(expand=c(0.11, 0))+
  theme_classic()+
  guides(fill = guide_legend(override.aes= list(alpha = 0, color = "white"))) +
  theme(aspect.ratio = 1/1.2,
        legend.position='none',
        plot.background = element_rect(fill='transparent', color=NA),
        panel.background = element_rect(fill='transparent'),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "mm"),
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=10))

fig4d

ggsave("Fig 4d.png",
       device = png,
       plot = fig3d,
       path = here::here("outputs", "figures"),
       width = 56,
       height = 78,
       units = "mm",
       dpi = 300,
)

###########################################################################
###########################################################################

fig4 <- plot_grid(fig4a, fig4b, fig4c, fig4d, 
                  nrow=2, ncol=2, align = 'hv',
                  labels = 'auto', label_size=12, 
                  vjust=5,hjust=-5)
fig4

fig4_full <- plot_grid(fig4, legend, nrow=1, rel_widths = c(5,1))

fig4_full

?plot_grid

ggsave("Fig 4.png",
       plot = fig4_full,
       path = here::here("outputs", "figures"),
       width = 250,
       height = 250,
       units = "mm",
       dpi = 300,
       bg="transparent"
)

###########################################################################
###########################################################################

# Fig 5a ------------------------------------------------------------------

list.noNE <- list[list$IUCN!="-",]
list.noNE$IUCN <- factor(list.noNE$IUCN, levels = c("EX","CR","EN","VU","DD","NT","LC"))
list.noNE$taxa <- factor(list.noNE$taxa, levels = c("Amphibians", "Reptiles", "Birds", "Mammals"))
colors <- c("#000000","#d6231e", "#fd7e4b","#fae639","#d1d1c5","#cce041","#67c262")

fig5a <- ggplot2::ggplot(data=list.noNE, aes(x=year, y=log(rangesize), color=IUCN, shape=taxa))+
  geom_point(size=3)+
  geom_smooth(method="lm", formula=y~x, se=FALSE,  aes(group = 1), color="black")+
  scale_color_manual(values=colors)+
  scale_shape_manual(values=c(16, 15, 17, 18))+
  scale_y_continuous(breaks = c(5, 10, 15))+
  labs(x= "Year of description", y= bquote("Log species range size " (km^2)), color="Threat Category", shape="Class")+
  theme_classic()+
  theme(legend.position='right',
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=10))+
  guides(shape='none', color = guide_legend(reverse=TRUE), fill = guide_legend(override.aes = list(shape=21,linetype=0,color="black")))
fig5a

# Fig. 5b - Range size versus habitat loss ---------------------------------

head(uso)
unique(uso$binomial) #338 spp

head(list)
list$binomial
taxa_uso <- list[,c(1,5, 18)] #taxa, binomial, IUCN

uso <- merge(uso, taxa_uso, by="binomial")
head(uso)
uso2020 <- uso[uso$lulcYear==2020,]
head(uso2020)

uso2020 <- uso2020[uso2020$IUCN!="-",]

uso2020$IUCN <- factor(uso2020$IUCN, levels = c("EX", "CR","EN","VU","DD","NT","LC"))
uso2020$taxa <- factor(uso2020$taxa, levels = c("Amphibians", "Reptiles", "Birds", "Mammals"))

colors <- c("#000000","#d6231e", "#fd7e4b","#fae639","#d1d1c5","#cce041","#67c262")

fig5b <- ggplot2::ggplot(data=uso2020, aes(x=log(range), y=(1-percNat), color=IUCN, shape=taxa))+
  geom_point(size=3)+
  scale_color_manual(values=colors)+
  scale_shape_manual(values=c(16, 15, 17, 18))+
  scale_y_continuous(limits = c(0,1), breaks=c(0, 0.5, 1))+
  geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = 9)+
  labs(x= bquote("Log species range size "(km^2)), y= bquote("Percentage of converted habitat "(km^2)), color="Threat Category", shape="Class")+
  theme_classic()+
  theme(legend.position='right',
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=10))+
  guides(color='none')
fig5b

fig_grid <- plot_grid(fig5a, fig5b, nrow=2, ncol=1, align="v", axis="l",
                      labels ="auto", label_size=10,
                      label_fontface= "bold")
fig_grid

ggsave("Fig 5.png",
       device = png,
       plot = fig_grid,
       path = here::here("outputs", "figures"),
       width = 168,
       height = 168,
       units = "mm",
       dpi = 300,
)

###########################################################################
# SUPPLEMENTARY MATERIAL --------------------------------------------------
###########################################################################

# Supp. 1 - SPPAs area accumulation through time --------------------------

head(ucs)

summary(ucs$STATUS_YR) # 1957-2018

ucs$decade <- NA
ucs$decade[ucs$STATUS_YR<2020] <- "2020"
ucs$decade[ucs$STATUS_YR<2010] <- "2010"
ucs$decade[ucs$STATUS_YR<2000] <- "2000"
ucs$decade[ucs$STATUS_YR<1990] <- "1990"
ucs$decade[ucs$STATUS_YR<1980] <- "1980"
ucs$decade[ucs$STATUS_YR<1970] <- "1970"
ucs$decade[ucs$STATUS_YR<1960] <- "1960"

ucs[ucs$decade=="1960",]
ucs$ORIG_NAME[ucs$STATUS_YR==2018]

table(ucs$decade)

ucs_dec <- as.data.frame(tapply(ucs$REP_AREA, ucs$decade, sum))
ucs_dec$decade <- rownames(ucs_dec)
names(ucs_dec)[1] <- "area"
ucs_dec$areasum <- cumsum(ucs_dec$area)/1000

sup1 <- ggplot2::ggplot(data=ucs_dec, aes(x=decade, y=areasum))+
  geom_line(stat="identity", group=1)+
  labs(x= "Decade" , y= bquote("Protected areas extent "(km^2~x~10^3)))+
  scale_y_continuous(limits=c(0,80), expand=c(0,0))+
  theme_classic()+
  theme(legend.position='none',
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=10))

sup1

ggsave("supp_fig_.png",
       device = png,
       plot = sup1,
       path = here::here("outputs", "figures", "supp"),
       width = 168,
       height = 78,
       units = "mm",
       dpi = 300,
)

# Supp. 2 - Range-size versus class ---------------------------------------

sup2 <- ggplot(data=list, aes(x=(rangesize/10^6)))+
  geom_histogram(bins=10)+
  facet_wrap(~taxa)+
  scale_y_continuous(expand=c(0,0), limits = c(0, 100))+
  theme_classic()+
  theme(legend.position='none',
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=8))+
  labs(x= bquote("Range size "(km^2~x~10^6)), y= "Frequency")

sup2

ggsave("supp_fig_2.png",
       device = png,
       plot = sup2,
       path = here::here("outputs", "figures", "supp"),
       width = 168,
       height = 168,
       units = "mm",
       dpi = 300,
)

# Supp. 3 - Range-size versus IUCN Categories ---------------------------
plots_iucn <- analyses_iucn[analyses_iucn$IUCN!="EX",]
colors <- c("#d6231e", "#fd7e4b","#fae639","#d1d1c5", "#cce041","#67c262")

sup3 <- ggplot2::ggplot(data=plots_iucn, aes(x=IUCN, y=log(rangesize),color=IUCN))+
  geom_violin()+
  geom_point(position = position_jitter(.2))+
  scale_color_manual(values=colors)+
  labs(x= "Threat Categories" , y= bquote("Log species range size "(km^2)), color="Threat Category")+
  facet_wrap(~taxa)+
  theme_classic()+
  theme(legend.position='right',
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=10))

sup3

ggsave("supp_fig_3.png",
       device = png,
       plot = sup3,
       path = here::here("outputs", "figures", "supp"),
       width = 168,
       height = 168,
       units = "mm",
       dpi = 300,
)

# Supp. 4 - Percentage of natural habitat versus IUCN Categories --------
plots_iucn <- analyses_iucn[analyses_iucn$IUCN!="EX",]
colors <- c("#d6231e", "#fd7e4b","#fae639","#d1d1c5", "#cce041","#67c262")

sup4 <- ggplot2::ggplot(data=plots_iucn, aes(x=IUCN, y=percNat,color=IUCN))+
  geom_violin()+
  geom_point(position = position_jitter(.2))+
  scale_color_manual(values=colors)+
  labs(x= "Threat Categories" , y= "Percentage of Remaining Habitat", color="Threat Category")+
  facet_wrap(~taxa)+
  theme_classic()+
  theme(legend.position='right',
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=10))

sup4

ggsave("supp_fig_4.png",
       device = png,
       plot = sup4,
       path = here::here("outputs", "figures", "supp"),
       width = 168,
       height = 168,
       units = "mm",
       dpi = 300,
)

# Supp. 5 - Percentage of protected range versus IUCN Categories --------
plots_iucn <- analyses_iucn[analyses_iucn$IUCN!="EX",]
colors <- c("#d6231e", "#fd7e4b","#fae639","#d1d1c5", "#cce041","#67c262")

sup5 <- ggplot2::ggplot(data=plots_iucn, aes(x=IUCN, y=prot_perc,color=IUCN))+
  geom_point(position=position_jitter(.4), size=2)+
  scale_color_manual(values=colors)+
  labs(x= "Threat Categories" , y= "Percentage of Protected Range", color="Threat Category")+
  facet_wrap(~taxa)+
  theme_classic()+
  theme(legend.position='right',
        axis.title = element_text(size=10, margin = margin(t=0, r=0, b=0, l=0, unit="mm")), 
        axis.text = element_text(size=10))

sup5

ggsave("supp_fig_5.png",
       device = png,
       plot = sup5,
       path = here::here("outputs", "figures", "supp"),
       width = 168,
       height = 168,
       units = "mm",
       dpi = 300,
)
