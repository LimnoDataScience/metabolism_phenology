#### Manuscript Figure 6 ####
# Load graphing libraries
library(ggdendro)
library(purrr)
library(broom)
library(lubridate)
library(tidyverse)
library(corrplot)

#### Flux + Correlation Plot: Manuscript Figure ####
scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

do.decompose <- function(df) {
  df2 = df %>% 
    select(-lake) %>% 
    group_by(id, year = year(date)) %>% mutate(n = n()) %>% 
    filter(n >= 365) 

  trend = df2 %>% 
    nest(data = -id) %>% 
    dplyr::mutate(
      ts =  purrr::map(data, ~ ts(.x$FnepTot, start = c(1982,1), frequency = 365)),
      decomp = purrr::map(ts, ~ decompose(.x, type = 'additive')),
      tidied = purrr::map(decomp, augment)
    ) %>% 
    unnest(tidied) %>% ungroup() %>% 
    mutate(date = df2$date) %>% 
    select(id, date, .trend)
  return(trend)
}

# fluxes = read_csv('/Users/hilarydugan/Dropbox/Paper_Publications/Ladwig (2021) Oxygen ODEM NTL/R/odemFluxes.csv') %>% 
#   rename(id = name, lake = lakeid)

fluxes = read_csv('Processed_Output/Fluxes_dailyFluxes.csv')
flux.nepTot = fluxes %>% 
  select(id, lake, date, FnepTot) %>% 
  left_join(do.decompose(.))  %>% 
  mutate(month = month(date), year = year(date), date = as.Date(date)) %>% 
  group_by(id, lake, year, month) %>% 
  # summarise(FnepTot = mean(.trend), date = first(date)) %>% 
  # mutate_if(is.numeric, scale2) %>% #scale data
  ungroup() %>% 
  mutate(lake = factor(lake, levels = c('AL','BM', 'CR', 'SP', 'TR','ME','MO','FI'))) %>%
  mutate(region = as.factor(if_else(lake %in% c('BM', 'TR', 'CR', 'SP', 'AL', 'N','S'), 'North','South')))

# Plot 
p1 = ggplot(flux.nepTot) +
  geom_path(aes(x = date, y = .trend, col = lake), size = 0.5) +
  scale_color_brewer(palette = 'Spectral', name = 'Lake') +
  facet_wrap(~region, nrow = 2) +
  theme_bw(base_size = 8) +
  # ylab('Trend signal of total NEP flux') +
  ylab(expression("Trend signal total NEP flux [g DO"*~m^{-2}*~d^{-1}*"]")) +
  # theme(axis.title.x = element_blank(), 
  #       legend.position = 'bottom',
  #       legend.title=element_blank(),
  #       legend.margin = margin(1, 1, 1, 1),
  #       legend.key.size = unit(0.3, "cm")) 
  theme(axis.title.x = element_blank(), 
        legend.box.background = element_rect(colour = "black"),
        legend.key.width =unit(0.3,"cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.position=c(.1,.2),
        #legend.position = 'bottom',
        # legend.title=element_blank());p1
  );p1
ggsave(p1, filename = 'Figures/fluxnep.pdf',width = 4, height = 4, dpi = 500)


flux.wide = flux.nepTot %>% select(lake, date, .trend) %>% 
  pivot_wider(names_from = lake, values_from = .trend, values_fn = mean) %>% 
  select(-date) %>% 
  select('AL','BM', 'CR', 'SP', 'TR','ME','MO','FI')

# Compute a correlation matrix
cor.mat <- flux.wide %>% cor(use = 'complete.obs')
round(cor.mat,2)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(flux.wide)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

dd <- dist(cor.mat, method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

g2 <- ggdendrogram(hc) +
  theme_bw(base_size = 8) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  xlab('') + ylab(''); g2
ggsave(g2, filename = 'Figures/fluxnep_dendro.pdf',width = 3, height = 1, dpi = 500)


pdf(file = "Figures/fluxCorr.pdf", width = 3, height = 3)
p.corr = corrplot(cor.mat, method = 'color', col = col(200),
                  type="full", order="hclust",
                  hclust.method = 'ward.D',
                  addCoef.col = "black", # Add coefficient of correlation
                  tl.col="black", tl.srt=45, #Text label color and rotation
                  tl.cex = 0.7, cl.cex = 0.5,
                  number.cex = 0.5,
                  # Combine with significance
                  p.mat = p.mat, sig.level = 0.01, insig = "blank",
                  # hide correlation coefficient on the principal diagonal
                  diag=TRUE, addrect = 3)
dev.off()
