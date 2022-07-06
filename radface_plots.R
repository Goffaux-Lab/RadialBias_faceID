
#color palette
#https://davidmathlogic.com/colorblind/#%23332288-%23F9A63D-%2334A584

colorblind <- c("black", "#4a3466", "#00814f")
color.fov <- c("black", "grey40")
color.hor <- c("#4a3466", "#B983FF")
color.vert <- c("#00814f", "#00D885")

colorsslopes <- c("#00D885", "#B983FF")


# yellow/blue
colorblind <- c("black", "#B8B400", "#4F7FF5") #C0BC00
color.fov <- c("black", "grey60")
color.hor <- c("#B8B400", "grey60")
color.vert <- c("#4F7FF5", "grey60")

colorsslopes <- c("#00D885", "#B983FF")


# ggplot theme 
alxtheme <- theme_bw() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.background = element_rect(fill = 'white'),
    panel.border = element_blank(), #element_rect(size = 2, color = "grey"),
    axis.line = element_blank(), 
    strip.background = element_rect(fill = 'white', color = "white"),
    strip.placement = 'outside',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines"),
    axis.text = element_text(colour = "black"),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 17)) 







# UP-LEFT ------------------------------------------------------------------------------------------


averagesUL <- df %>%
  filter(subjGroup == "up-left") %>% 
  group_by(Subject, meridian, inversion, phase) %>%
  dplyr::summarise(
    accuracy = mean(accuracy) # first summarize across trial (1 value per subj*condition)
  ) %>% 
  Rmisc::summarySE( # then compute CI etc.
    measurevar = "accuracy",
    groupvars = c("meridian", "inversion", "phase"))
averagesUL$meridian <- as.factor(averagesUL$meridian)

#model predictions (fixed effect)
predictionsUL <-
  ggeffects::ggpredict(fullmodel_UL, terms = c("phase[1:7 by=0.1]", "inversion", "meridian")) %>%
  dplyr::rename(phase = x,
                meridian = facet,
                inversion = group)

levels(predictionsUL$meridian) <- c("Fovea", "Horizontal Meridian", "Vertical Meridian")


# ..........Fovea -----------------------------------------------------------------------------------------------------

plotFOV.UL <- averagesUL %>% 
  filter(meridian == "Fovea") %>% 
  ggplot(
    aes(
      x = phase,
      y = accuracy,
      color = inversion,
      fill = inversion,
      group = inversion)
  ) +
  geom_point (shape = 21, size = 2.5) +
  geom_errorbar(aes(ymin = accuracy - ci,
                    ymax = accuracy + ci),
                width = 0,
                size = 1) +
  labs (y = "Accuracy") +
  geom_line(
    data = predictionsUL %>% filter(meridian == "Fovea"),
    aes(
      x = phase,
      y = predicted,
      color = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = 2
  ) +
  geom_ribbon(
    data = predictionsUL %>% filter(meridian == "Fovea"),
    aes(
      x = phase,
      ymin = conf.low,
      ymax = conf.high,
      fill = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    alpha = 0.3
  ) +
  scale_color_manual (values = color.fov) +
  scale_fill_manual (values = color.fov) +
  scale_y_continuous(breaks = c(0.33, 0.4, 0.6, 0.8, 1), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(0, 90, 15), expand = c(0, 0)) +
  alxtheme + 
  coord_cartesian(ylim = c(0.25, 1.1), xlim = c(0.5, 7.5)) +
  geom_segment(x = 1, xend = 7, y = 0.25, yend = 0.25,
               size = 1, color = "black") +
  geom_segment(x = 0.5, xend = 0.5, y = 0.33, yend = 1,
               size = 1, color = "black") +
  # ggtitle("Fovea") +
  theme(plot.title = element_text(color = colorblind[1]))
plotFOV.UL

# ..........Horiz -----------------------------------------------------------------------------------------------------

plotHORIZ.UL <- averagesUL %>% 
  filter(meridian == "HorizontalMeridian") %>% 
  ggplot(
    aes(
      x = phase,
      y = accuracy,
      color = inversion,
      fill = inversion,
      group = inversion)
  ) +
  geom_point (shape = 21, size = 2.5) +
  geom_errorbar(aes(ymin = accuracy - ci,
                    ymax = accuracy + ci),
                width = 0,
                size = 1) +
  labs (y = "Accuracy") +
  geom_line(
    data = predictionsUL %>% filter(meridian == "Horizontal Meridian"),
    aes(
      x = phase,
      y = predicted,
      color = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = 2
  ) +
  geom_ribbon(
    data = predictionsUL %>% filter(meridian == "Horizontal Meridian"),
    aes(
      x = phase,
      ymin = conf.low,
      ymax = conf.high,
      fill = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    alpha = 0.3
  ) +
  scale_color_manual (values = color.hor) +
  scale_fill_manual (values = color.hor) +
  scale_y_continuous(breaks = c(0.33, 0.4, 0.6, 0.8, 1), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(0, 90, 15), expand = c(0, 0)) +
  alxtheme + 
  coord_cartesian(ylim = c(0.25, 1.1), xlim = c(0.5, 7.5)) +
  geom_segment(x = 1, xend = 7, y = 0.25, yend = 0.25,
               size = 1, color = "black") +
  #ggtitle("Horizontal Meridian") +
  theme(plot.title = element_text(color = colorblind[2]),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank() 
  )
plotHORIZ.UL

# ..........Vertical -----------------------------------------------------------------------------------------------------

plotVERT.UL <- averagesUL %>% 
  filter(meridian == "VerticalMeridian") %>% 
  ggplot(
    aes(
      x = phase,
      y = accuracy,
      color = inversion,
      fill = inversion,
      group = inversion)
  ) +
  geom_point (shape = 21, size = 2.5) +
  geom_errorbar(aes(ymin = accuracy - ci,
                    ymax = accuracy + ci),
                width = 0,
                size = 1) +
  labs (y = "Accuracy") +
  geom_line(
    data = predictionsUL %>% filter(meridian == "Vertical Meridian"),
    aes(
      x = phase,
      y = predicted,
      color = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = 2
  ) +
  geom_ribbon(
    data = predictionsUL %>% filter(meridian == "Vertical Meridian"),
    aes(
      x = phase,
      ymin = conf.low,
      ymax = conf.high,
      fill = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    alpha = 0.3
  ) +
  scale_color_manual (values = color.vert) +
  scale_fill_manual (values = color.vert) +
  scale_y_continuous(breaks = c(0.33, 0.4, 0.6, 0.8, 1), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(0, 90, 15), expand = c(0, 0)) +
  alxtheme + 
  coord_cartesian(ylim = c(0.25, 1.1), xlim = c(0.5, 7.5)) +
  geom_segment(x = 1, xend = 7, y = 0.25, yend = 0.25,
               size = 1, color = "black") +
  #ggtitle("Vertical Meridian") +
  theme(plot.title = element_text(color = colorblind[3]),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank() 
  )
plotVERT.UL

# ............Inversion effect ----------------------------------------------------------------------------------------

predIE.UL <- predictionsUL %>%
  group_by(meridian, inversion, phase) %>%
  dplyr::summarise(
    accuracy = mean(predicted) # first summarize across trial (1 value per subj*condition)
  ) %>% 
  dcast(meridian + phase ~ inversion, value.var = "accuracy") %>% 
  mutate(IE = upright - inverted) %>% 
  select(-upright,
         -inverted) %>% 
  Rmisc::summarySE( # then compute CI etc.
    measurevar = "IE",
    groupvars = c("meridian", "phase"))
predIE.UL$meridian <- as.factor(predIE.UL$meridian)

y.fov.UL <- predIE.UL %>% filter(phase == 7, meridian == "Fovea")
y.fov.UL <- y.fov.UL[1,4]
y.hor.UL <- predIE.UL %>% filter(phase == 7, meridian == "Horizontal Meridian")
y.hor.UL <- y.hor.UL[1,4]
y.vert.UL <- predIE.UL %>% filter(phase == 7, meridian == "Vertical Meridian")
y.vert.UL <- y.vert.UL[1,4]

plotIEs.UL <- ggplot(
  predIE.UL,
  aes(
    x = phase,
    y = IE,
    color = meridian, 
    fill = meridian
  )
) +
  stat_smooth(se = FALSE, geom = "area",
              method = 'loess', alpha = .5) +
  geom_smooth (size = 2, se = FALSE) +
  # geom_line() +
  labs (y = "Predicted inversion effect") +
  scale_color_manual (values = colorblind) +
  scale_fill_manual (values = colorblind) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(0, 90, 15), expand = c(0, 0)) +
  alxtheme + 
  coord_cartesian(ylim = c(0, 0.24), xlim = c(1, 15)) +
  geom_segment(x = 1, xend = 7, y = 0, yend = 0,
               size = 1, color = "black") +
  geom_segment(x = 1, xend = 1, y = 0, yend = 0.2,
               size = 1, color = "black") +
  theme(axis.ticks = element_blank()) +
  #ggtitle("Inversion effect size") +
  annotate(geom = "text", label = "  Fovea", x = 7.7, y = 0.20, hjust = "left", color = colorblind[1], size = 7) +
  annotate(geom = "text", label = "  Horizontal meridian", x = 7.7, y = 0.17, hjust = "left", color = colorblind[2], size = 7) +
  annotate(geom = "text", label = "  Vertical meridian", x = 7.7, y = 0.14, hjust = "left", color = colorblind[3], size = 7)

plotIEs.UL







# LOW-RIGHT ------------------------------------------------------------------------------------------


averagesLR <- df %>%
  filter(subjGroup == "low-right") %>% 
  group_by(Subject, meridian, inversion, phase) %>%
  dplyr::summarise(
    accuracy = mean(accuracy) # first summarize across trial (1 value per subj*condition)
  ) %>% 
  Rmisc::summarySE( # then compute CI etc.
    measurevar = "accuracy",
    groupvars = c("meridian", "inversion", "phase"))
averagesLR$meridian <- as.factor(averagesLR$meridian)

#model predictions (fixed effect)
predictionsLR <-
  ggeffects::ggpredict(fullmodel_LR, terms = c("phase[1:7 by=0.1]", "inversion", "meridian")) %>%
  dplyr::rename(phase = x,
                meridian = facet,
                inversion = group)

levels(predictionsLR$meridian) <- c("Fovea", "Horizontal Meridian", "Vertical Meridian")


# ..........Fovea -----------------------------------------------------------------------------------------------------

plotFOV.LR <- averagesLR %>% 
  filter(meridian == "Fovea") %>% 
  ggplot(
    aes(
      x = phase,
      y = accuracy,
      color = inversion,
      fill = inversion,
      group = inversion)
  ) +
  geom_point (shape = 21, size = 2.5) +
  geom_errorbar(aes(ymin = accuracy - ci,
                    ymax = accuracy + ci),
                width = 0,
                size = 1) +
  labs (y = "Accuracy") +
  geom_line(
    data = predictionsLR %>% filter(meridian == "Fovea"),
    aes(
      x = phase,
      y = predicted,
      color = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = 2
  ) +
  geom_ribbon(
    data = predictionsLR %>% filter(meridian == "Fovea"),
    aes(
      x = phase,
      ymin = conf.low,
      ymax = conf.high,
      fill = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    alpha = 0.3
  ) +
  scale_color_manual (values = color.fov) +
  scale_fill_manual (values = color.fov) +
  scale_y_continuous(breaks = c(0.33, 0.4, 0.6, 0.8, 1), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(0, 90, 15), expand = c(0, 0)) +
  alxtheme + 
  coord_cartesian(ylim = c(0.25, 1.1), xlim = c(0.5, 7.5)) +
  geom_segment(x = 1, xend = 7, y = 0.25, yend = 0.25,
               size = 1, color = "black") +
  geom_segment(x = 0.5, xend = 0.5, y = 0.33, yend = 1,
               size = 1, color = "black") +
  #ggtitle("Fovea") +
  theme(plot.title = element_text(color = colorblind[1]))
plotFOV.LR

# ..........Horiz -----------------------------------------------------------------------------------------------------

plotHORIZ.LR <- averagesLR %>% 
  filter(meridian == "HorizontalMeridian") %>% 
  ggplot(
    aes(
      x = phase,
      y = accuracy,
      color = inversion,
      fill = inversion,
      group = inversion)
  ) +
  geom_point (shape = 21, size = 2.5) +
  geom_errorbar(aes(ymin = accuracy - ci,
                    ymax = accuracy + ci),
                width = 0,
                size = 1) +
  labs (y = "Accuracy") +
  geom_line(
    data = predictionsLR %>% filter(meridian == "Horizontal Meridian"),
    aes(
      x = phase,
      y = predicted,
      color = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = 2
  ) +
  geom_ribbon(
    data = predictionsLR %>% filter(meridian == "Horizontal Meridian"),
    aes(
      x = phase,
      ymin = conf.low,
      ymax = conf.high,
      fill = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    alpha = 0.3
  ) +
  scale_color_manual (values = color.hor) +
  scale_fill_manual (values = color.hor) +
  scale_y_continuous(breaks = c(0.33, 0.4, 0.6, 0.8, 1), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(0, 90, 15), expand = c(0, 0)) +
  alxtheme + 
  coord_cartesian(ylim = c(0.25, 1.1), xlim = c(0.5, 7.5)) +
  geom_segment(x = 1, xend = 7, y = 0.25, yend = 0.25,
               size = 1, color = "black") +
  #ggtitle("Horizontal Meridian") +
  theme(plot.title = element_text(color = colorblind[2]),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank() 
  )
plotHORIZ.LR

# ..........Vertical -----------------------------------------------------------------------------------------------------

plotVERT.LR <- averagesLR %>% 
  filter(meridian == "VerticalMeridian") %>% 
  ggplot(
    aes(
      x = phase,
      y = accuracy,
      color = inversion,
      fill = inversion,
      group = inversion)
  ) +
  geom_point (shape = 21, size = 2.5) +
  geom_errorbar(aes(ymin = accuracy - ci,
                    ymax = accuracy + ci),
                width = 0,
                size = 1) +
  labs (y = "Accuracy") +
  geom_line(
    data = predictionsLR %>% filter(meridian == "Vertical Meridian"),
    aes(
      x = phase,
      y = predicted,
      color = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = 2
  ) +
  geom_ribbon(
    data = predictionsLR %>% filter(meridian == "Vertical Meridian"),
    aes(
      x = phase,
      ymin = conf.low,
      ymax = conf.high,
      fill = inversion,
      group = inversion
    ),
    inherit.aes = FALSE,
    alpha = 0.3
  ) +
  scale_color_manual (values = color.vert) +
  scale_fill_manual (values = color.vert) +
  scale_y_continuous(breaks = c(0.33, 0.4, 0.6, 0.8, 1), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(0, 90, 15), expand = c(0, 0)) +
  alxtheme + 
  coord_cartesian(ylim = c(0.25, 1.1), xlim = c(0.5, 7.5)) +
  geom_segment(x = 1, xend = 7, y = 0.25, yend = 0.25,
               size = 1, color = "black") +
  #ggtitle("Vertical Meridian") +
  theme(plot.title = element_text(color = colorblind[3]),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank() 
  )
plotVERT.LR


# ............Inversion effect ----------------------------------------------------------------------------------------

predIE.LR <- predictionsLR %>%
  group_by(meridian, inversion, phase) %>%
  dplyr::summarise(
    accuracy = mean(predicted) # first summarize across trial (1 value per subj*condition)
  ) %>% 
  dcast(meridian + phase ~ inversion, value.var = "accuracy") %>% 
  mutate(IE = upright - inverted) %>% 
  select(-upright,
         -inverted) %>% 
  Rmisc::summarySE( # then compute CI etc.
    measurevar = "IE",
    groupvars = c("meridian", "phase"))
predIE.LR$meridian <- as.factor(predIE.LR$meridian)


y.fov.LR <- predIE.LR %>% filter(phase == 7, meridian == "Fovea")
y.fov.LR <- y.fov.LR[1,4]
y.hor.LR <- predIE.LR %>% filter(phase == 7, meridian == "Horizontal Meridian")
y.hor.LR <- y.hor.LR[1,4]
y.vert.LR <- predIE.LR %>% filter(phase == 7, meridian == "Vertical Meridian")
y.vert.LR <- y.vert.LR[1,4]

plotIEs.LR <- ggplot(
  predIE.LR,
  aes(
    x = phase,
    y = IE,
    color = meridian, 
    fill = meridian
  )
) +
  stat_smooth(se = FALSE, geom = "area",
              method = 'loess', alpha = .5) +
  geom_smooth (size = 2, se = FALSE) +
  # geom_line() +
  labs (y = "Predicted inversion effect") +
  scale_color_manual (values = colorblind) +
  scale_fill_manual (values = colorblind) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(0, 90, 15), expand = c(0, 0)) +
  alxtheme + 
  coord_cartesian(ylim = c(0, 0.24), xlim = c(1, 15)) +
  geom_segment(x = 1, xend = 7, y = 0, yend = 0,
               size = 1, color = "black") +
  geom_segment(x = 1, xend = 1, y = 0, yend = 0.2,
               size = 1, color = "black") +
  theme(axis.ticks = element_blank()) +
  #ggtitle("Inversion effect size") +
  annotate(geom = "text", label = "  Fovea", x = 7.7, y = 0.20, hjust = "left", color = colorblind[1], size = 7) +
  annotate(geom = "text", label = "  Horizontal meridian", x = 7.7, y = 0.17, hjust = "left", color = colorblind[2], size = 7) +
  annotate(geom = "text", label = "  Vertical meridian", x = 7.7, y = 0.14, hjust = "left", color = colorblind[3], size = 7)

plotIEs.LR










# Combine -------------------------------------------------------------------------------------------------------------


library(cowplot)

LR <-
  plot_grid(
    plotFOV.LR,
    plotHORIZ.LR,
    plotVERT.LR,
    NULL,
    plotIEs.LR,
    label_size = 24,
    rel_widths = c(1,0.8,0.8,0.3,2.2),
    ncol = 5,
    nrow = 1
  )
LR 


 UL <-
  plot_grid(
    plotFOV.UL,
    plotHORIZ.UL,
    plotVERT.UL,
    NULL,
    plotIEs.UL,
    label_size = 24,
    rel_widths = c(1,0.8,0.8,0.3,2.2),
    ncol = 5,
    nrow = 1
  )
UL 


# write figure in tiff

tiff("UL.tif", width = 8000, height = 1750, units = 'px', res = 600)
UL
dev.off()

tiff("LR.tif", width = 8000, height = 1750, units = 'px', res = 600)
LR
dev.off()










# Indiv SLopes --------------------------------------------------------------------------------------------------------


bgdat = data.frame( #for background color of the plot
  x = c(0, 0.3),
  xmin = 0,
  xmax = 0.3,
  y = c(0, 0.3)
)



pars_ranef <- bind_rows(UPLEFT_pars_ranef, LOWRIGHT_pars_ranef)




indivz <- ggplot(bgdat) +
  geom_ribbon(aes(xmin = x, xmax = xmax, y = y), fill = "grey90") +
  geom_ribbon(aes(xmin = xmin, xmax = x, y = y), fill = "grey70") +
  geom_point(
    data = pars_ranef,
    inherit.aes = FALSE,
    aes(x = wslope.horiz, 
        y = wslope.vert, 
        shape = group),
    size = 2,
    color = "black",
    stroke = 1.5
  ) +
  scale_shape_manual(values = c(1,4), name = "") +
  scale_x_continuous(limits = c(0, 0.3), 
                     expand = c(0, 0),
                     breaks = seq(0,0.3,0.1),
                     name = expression(atop("Horizontal meridian", paste(β[upright]~-~β[inverted])))) +
  scale_y_continuous(limits = c(0, 0.3), 
                     expand = c(0, 0),
                     breaks = seq(0.1,0.3,0.1),
                     labels = seq(0.1,0.3,0.1),
                     name = expression(atop("Vertical meridian", paste(β[upright]~-~β[inverted])))) +
  coord_cartesian(ylim = c(0, 0.3), xlim = c(0, 0.3)) +
  # scale_size(range = c(1, 8)) +
  # geom_text(x = 0.2, y = 0.1, 
  #           label = "FIE radial > FIE tangential", 
  #           angle = 45,
  #           fontface = "bold",
  #           size = 8,
  #           alpha = .1) +
  # geom_text(x = 0.1, y = 0.2, 
  #           label = "FIE tangential > FIE radial", 
  #           angle = 45,
  #           fontface = "bold",
  #           size = 8,
  #           alpha = .1) +
  # alxtheme + 
 theme_bw() +
  theme(axis.ticks = element_blank(),
        legend.position = "none",
        text = element_text(size = 20),
        panel.border = element_rect(size = 2)) 
 
indivz


# Combine -------------------------------------------------------------------------------------------------------------


avrgd <-
  plot_grid(
    plotFOV,
    plotHORIZ,
    plotVERT,
    plotIEs,
    label_size = 24,
    rel_widths = c(1,0.8,0.8,2),
    ncol = 4,
    nrow = 1
  )
avrgd 



# write figure in tiff

tiff("avrgd.tif", width = 8000, height = 1750, units = 'px', res = 600)
avrgd
dev.off()

tiff("indivz.tif", width = 3000, height = 3000, units = 'px', res = 600)
indivz
dev.off()

