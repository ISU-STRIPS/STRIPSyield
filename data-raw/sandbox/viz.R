library(ggplot2)
library(ggpubr)

# attach("./data/yieldExtra.rda")

# Locations ---------------------------------------------------------------
out1 <- ggplot(yieldExtra) +
  geom_point(aes(x, y), size = 0.3) +
  facet_wrap(~ year + site, scales = "free") +
  theme_bw()
# facet_grid(rows = vars(site), cols = vars(year), scales = "free") +
#

ggsave("./data-raw/sandbox/locations.png", out1, dpi = "print", width = 20, height = 16)

# Measurements ------------------------------------------------------------
dotplots <- function(df, y) {
  ggplot(df) +
    geom_violin(aes_string(x = y, y = "year"), draw_quantiles = c(0.5)) +
    # geom_point(
    #   geom_violin(x = "year", y = y)
    # ) +
    # geom_point(
    #   aes_string(x = "year", y = y),
    #   size = 0.1, position = position_jitter(width = 0.25)
    # ) +
    facet_wrap(~ site, ncol = 4) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

ind  <- yieldExtra$crop == "Soybeans" & yieldExtra$site != "OffBounds"
cols <- c(
  "swath", "elevation", "speed", "distance",
  "cycle", "flow", "moisture", "yield"
)
l    <- lapply(cols, function(y) {
  dotplots(yieldExtra[ind, ], y)
})

out2 <- ggarrange(plotlist = l, nrow = ceiling(length(l) / 2), ncol = 2)
ggsave("./data-raw/sandbox/measurements.png", out2, dpi = "print", width = 20, height = 16)

ggplot(yieldExtra[ind, ]) +
  geom_point(aes(x = year, y = yield), size = 0.1, position = position_jitter(width = 0.25)) +
  facet_wrap(~ site, scales = "free") +
  theme_bw()

ggplot(yieldExtra[ind, ]) +
  geom_violin(aes(y = year, x = yield), draw_quantiles = c(0.5)) +
  facet_wrap(~ site, scales = "free") +
  theme_bw()
