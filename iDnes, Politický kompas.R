# iDnes, Politický kompas

# Projekt: http://volby.idnes.cz/kompas.aspx
# Data: http://zpravy.idnes.cz/otevrena-data-z-projektu-politicky-kompas-fjm-/prilohy.aspx?c=A160627_154053_prilohy_pak

# save.image("iDnes, Politický kompas.RData")

library(data.table)
library(ggplot2)

# Funkce pro hromadné nahrazování -----------------------------------------

mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}


# Data --------------------------------------------------------------------

u <- fread("http://data.idnes.cz/opendata/kompas/user.csv")
d <- fread("http://data.idnes.cz/opendata/kompas/user-data.csv")

d <- merge(d, u, by = "iduser", all = TRUE)

temp.party <- c("ano", "cssd", "kdu", "kscm", "ods", "person", "pir", "sso", "sz", "top09", "usvit")
temp.party.full <- c("Ano", "ČSSD", "KDU-ČSL", "KSČM", "ODS", "person", "Piráti", "Svobodní", "Zelení", "Top 09", "Úsvit")

d[, party.full := mgsub(temp.party, temp.party.full, party)]
rm(temp.party, temp.party.full)

d <- d[party != "person" & lastX != "-1" & lastY != "-1" & firstX != "-1" & firstY != "-1"]
# převrácení osy Y, aby byla stejná jako v grafu na iDnes
d[, lastY := abs(lastY - 100)]  
d[, firstY := abs(firstY - 100)]


# Základní graf --------------------------------------------------------------------

g1 <- ggplot(d, aes(x = lastX, y = lastY, group = party, col = party)) +
  geom_jitter(alpha = 0.1, size = 1, shape = 16) +
  facet_wrap(~ party.full, ncol = 5) +
  scale_x_continuous("", breaks = c(2, 96), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  ggtitle(paste0("\nPolitický kompas iDnes: ", length(unique(d$iduser)), " respondentů")) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 18, hjust = 0),
    panel.grid = element_blank()
  )

ggsave("Graf 1 - základní.png", plot = g1, width = 16, height = 7.5, dpi = 100)
# ggsave("Graf 1 - základní, menší.png", plot = g1, width = 13, height = 5.8, dpi = 85)


# Graf průměr a směrodatná odchylka -----------------------------------------------

t2 <- copy(d)
t2 <- t2[, .(mean.x = mean(lastX),
             mean.y = mean(lastY),
             sd.x = sd(lastX),
             sd.y = sd(lastY)), by = party.full]

g2 <- ggplot(t2, aes(group = party.full, col = party.full, fill = party.full)) +
  geom_rect(aes(xmin = mean.x - sd.x / 2,
                xmax = mean.x + sd.x / 2,
                ymin = mean.y - sd.y / 2,
                ymax = mean.y + sd.y / 2),
            alpha = 0.3, color = NA) +
  geom_point(aes(x = mean.x, y = mean.y), size = 3, shape = 16) +
  facet_wrap(~ party.full, ncol = 5) +
  scale_x_continuous("", limits = c(0, 100), breaks = c(2, 96), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", limits = c(0, 100), breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  ggtitle(paste0("\nPolitický kompas iDnes, průměr a směrodatná odchylka: ", length(unique(d$iduser)), " respondentů")) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 18, hjust = 0),
    panel.grid = element_blank()
  )

ggsave("Graf 2 - průměr, standardní odchylka.png", plot = g2, width = 16, height = 7.5, dpi = 100)
# ggsave("Graf 2 - průměr, standardní odchylka, menší.png", plot = g2, width = 13, height = 5.8, dpi = 85)


# Graf medián a Median absolute deviation -----------------------------------------------

t3 <- copy(d)
t3 <- t3[, .(median.x = median(as.numeric(lastX)),
             median.y = median(as.numeric(lastY)),
             mad.x = mad(lastX),
             mad.y = mad(lastY)), by = party.full]

g3 <- ggplot(t3, aes(group = party.full, col = party.full, fill = party.full)) +
  geom_rect(aes(xmin = median.x - mad.x / 2,
                xmax = median.x + mad.x / 2,
                ymin = median.y - mad.y / 2,
                ymax = median.y + mad.y / 2),
            alpha = 0.3, color = NA) +
  geom_point(aes(x = median.x, y = median.y), size = 3, shape = 16) +
  facet_wrap(~ party.full, ncol = 5) +
  scale_x_continuous("", limits = c(0, 100), breaks = c(2, 96), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", limits = c(0, 100), breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  ggtitle(paste0("\nPolitický kompas iDnes, medián a median absolute deviation: ", length(unique(d$iduser)), " respondentů")) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 18, hjust = 0),
    panel.grid = element_blank()
  )

ggsave("Graf 3 - medián, MAD.png", plot = g3, width = 16, height = 7.5, dpi = 100)
# ggsave("Graf 3 - medián, MAD, menší.png", plot = g3, width = 13, height = 5.8, dpi = 85)


# Společný graf medián a Median absolute deviation -----------------------------------------------

g4 <- ggplot(t3, aes(group = party.full, col = party.full, fill = party.full, label = party.full)) +
  geom_rect(aes(xmin = median.x - mad.x / 2,
                xmax = median.x + mad.x / 2,
                ymin = median.y - mad.y / 2,
                ymax = median.y + mad.y / 2),
            alpha = 0.3, color = NA) +
  geom_text(aes(x = median.x, y = median.y), fontface = "bold") +
  scale_x_continuous("", limits = c(0, 100), breaks = c(0, 100), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", limits = c(0, 100), breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  ggtitle(paste0("\nPolitický kompas iDnes, medián a median absolute deviation: ", length(unique(d$iduser)), " respondentů")) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 18, hjust = 0),
    panel.grid = element_blank()
  )

ggsave("Graf 4 - medián, MAD, společný.png", plot = g4, width = 11, height = 9, dpi = 100)
# ggsave("Graf 4 - medián, MAD, společný, menší.png", plot = g4, width = 11, height = 9, dpi = 85)

rm(g1, g2, g3, g4)

