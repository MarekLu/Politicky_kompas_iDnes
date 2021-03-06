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

# Úprava názvů stran
temp.party <- c("ano", "cssd", "kdu", "kscm", "ods", "person", "pir", "sso", "sz", "top09", "usvit")
temp.party.full <- c("Ano", "ČSSD", "KDU-ČSL", "KSČM", "ODS", "person", "Piráti", "Svobodní", "Zelení", "Top 09", "Úsvit")
d[, party.full := mgsub(temp.party, temp.party.full, party)]
# d[, party.full := iconv(party.full, from = "CP1250", to = "UTF-8")]
rm(temp.party, temp.party.full)

# Doplnění názvů stran, koho člověk volil
temp.party2 <- c(10:21)
temp.party.full2 <- c("0", "nevolil", "ČSSD", "Ano", "KSČM", "Top 09", "ODS", "Úsvit", "KDU-ČSL", "Zelení", "Piráti", "Svobodní")
d[, voter.full := voter + 10]
d[, voter.full := mgsub(temp.party2, temp.party.full2, voter.full)]
rm(temp.party2, temp.party.full2)

d[, is.voter := FALSE]
d[party.full == voter.full, is.voter := TRUE]

# Tabulka persons a promazání divných dat
person <- copy(d)
person <- person[party == "person"]
person[, lastY := abs(lastY - 100)]  
person[, firstY := abs(firstY - 100)]
person <- person[lastX != "-1" & lastY != "-1" & firstX != "-1" & firstY != "-1"]

d <- d[party != "person" & lastX != "-1" & lastY != "-1" & firstX != "-1" & firstY != "-1"]

# Převrácení osy Y, aby byla stejná jako v grafu na iDnes
d[, lastY := abs(lastY - 100)]  
d[, firstY := abs(firstY - 100)]


# Základní graf --------------------------------------------------------------------

g1 <- ggplot(d, aes(x = lastX, y = lastY, group = party, col = party)) +
  geom_jitter(alpha = 0.06, size = 1, shape = 16) +
  facet_wrap(~ party.full, ncol = 5) +
  scale_x_continuous("", breaks = c(2, 96), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  ggtitle(paste0("\nPolitický kompas iDnes: ", formatC(length(unique(d$iduser)), big.mark = " "), " respondentů")) +
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
            alpha = 0.2, color = NA) +
  geom_point(aes(x = mean.x, y = mean.y), size = 3, shape = 16) +
  facet_wrap(~ party.full, ncol = 5) +
  scale_x_continuous("", limits = c(0, 100), breaks = c(2, 96), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", limits = c(0, 100), breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  ggtitle(paste0("\nPolitický kompas iDnes, průměr a směrodatná odchylka: ", formatC(length(unique(d$iduser)), big.mark = " "), " respondentů")) +
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
            alpha = 0.2, color = NA) +
  geom_point(aes(x = median.x, y = median.y), size = 3, shape = 16) +
  facet_wrap(~ party.full, ncol = 5) +
  scale_x_continuous("", limits = c(0, 100), breaks = c(2, 96), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", limits = c(0, 100), breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  ggtitle(paste0("\nPolitický kompas iDnes, medián a median absolute deviation (MAD): ", formatC(length(unique(d$iduser)), big.mark = " "), " respondentů")) +
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
            alpha = 0.2, color = NA) +
  geom_text(aes(x = median.x, y = median.y), fontface = "bold") +
  scale_x_continuous("", limits = c(0, 100), breaks = c(0, 100), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", limits = c(0, 100), breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  ggtitle(paste0("\nPolitický kompas iDnes, medián a median absolute deviation: ", formatC(length(unique(d$iduser)), big.mark = " "), " respondentů")) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 18, hjust = 0),
    panel.grid = element_blank()
  )

ggsave("Graf 4 - medián, MAD, společný.png", plot = g4, width = 11, height = 9, dpi = 100)
# ggsave("Graf 4 - medián, MAD, společný, menší.png", plot = g4, width = 11, height = 9, dpi = 85)


# Graf medián a Median absolute deviation, voliči strany a ostatní -----------------------------------------------

t5 <- copy(d[voter != "0"])  # bez těch, kteří nevybrali, koho volili
t5 <- t5[, .(median.x = median(as.numeric(lastX)),
             median.y = median(as.numeric(lastY)),
             mad.x = mad(lastX),
             mad.y = mad(lastY)), by = c("party.full", "is.voter")]

t5[, is.voter := as.factor(is.voter)]
t5[, is.voter := factor(is.voter, levels = rev(levels(is.voter)))]

g5 <- ggplot(t5, aes(group = party.full, col = is.voter, fill = is.voter)) +
  geom_rect(aes(xmin = median.x - mad.x / 2,
                xmax = median.x + mad.x / 2,
                ymin = median.y - mad.y / 2,
                ymax = median.y + mad.y / 2),
            alpha = 0.2, color = NA) +
  geom_point(aes(x = median.x, y = median.y), size = 3, shape = 16) +
  facet_wrap(~ party.full, ncol = 5) +
  scale_x_continuous("", limits = c(0, 100), breaks = c(2, 96), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", limits = c(0, 100), breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  scale_fill_manual("", values = c("red2","dodgerblue4"), labels = c("volič strany", "ostatní hlasující")) +
  scale_color_manual("", values = c("red2", "dodgerblue4"), labels = c("volič strany", "ostatní hlasující")) +
  ggtitle(paste0("\nPolitický kompas iDnes, medián a MAD, voliči a ostatní: ", formatC(length(unique(d$iduser)), big.mark = " "), " respondentů")) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "top",
    plot.title = element_text(size = 18, hjust = 0),
    panel.grid = element_blank(),
    legend.key = element_blank()
  )

ggsave("Graf 5 - medián, MAD, podle voličů.png", plot = g5, width = 16, height = 8, dpi = 100)
# ggsave("Graf 5 - medián, MAD, podle voličů, menší.png", plot = g5, width = 13, height = 6.5, dpi = 85)


# Graf medián a Median absolute deviation, muži a ženy -----------------------------------------------

t6 <- copy(d[sex != "0"])  # bez těch, kteří nevybrali pohlaví
t6 <- t6[, .(median.x = median(as.numeric(lastX)),
             median.y = median(as.numeric(lastY)),
             mad.x = mad(lastX),
             mad.y = mad(lastY)), by = c("party.full", "sex")]

g6 <- ggplot(t6, aes(group = party.full, col = as.factor(sex), fill = as.factor(sex))) +
  geom_rect(aes(xmin = median.x - mad.x / 2,
                xmax = median.x + mad.x / 2,
                ymin = median.y - mad.y / 2,
                ymax = median.y + mad.y / 2),
            alpha = 0.2, color = NA) +
  geom_point(aes(x = median.x, y = median.y), size = 3, shape = 16) +
  facet_wrap(~ party.full, ncol = 5) +
  scale_x_continuous("", limits = c(0, 100), breaks = c(2, 96), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", limits = c(0, 100), breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  scale_fill_manual("", values = c("red2","dodgerblue4"), labels = c("ženy", "muži")) +
  scale_color_manual("", values = c("red2", "dodgerblue4"), labels = c("ženy", "muži")) +
  ggtitle(paste0("\nPolitický kompas iDnes, medián a MAD, podle pohlaví: ", formatC(length(unique(d$iduser)), big.mark = " "), " respondentů")) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "top",
    plot.title = element_text(size = 18, hjust = 0),
    panel.grid = element_blank(),
    legend.key = element_blank()
  )

ggsave("Graf 6 - medián, MAD, podle pohlaví.png", plot = g6, width = 16, height = 8, dpi = 100)
# ggsave("Graf 6 - medián, MAD, podle pohlaví, menší.png", plot = g6, width = 13, height = 6.5, dpi = 85)


# Graf medián a Median absolute deviation, vzdělání -----------------------------------------------

t7 <- copy(d[education != "0"])  # bez těch, kteří nevybrali vzdělání
t7[, educ2 := "nižší"]
t7[education >= 3, educ2 := "vyšší"]
t7 <- t7[, .(median.x = median(as.numeric(lastX)),
             median.y = median(as.numeric(lastY)),
             mad.x = mad(lastX),
             mad.y = mad(lastY)), by = c("party.full", "educ2")]

g7 <- ggplot(t7, aes(group = party.full, col = educ2, fill = educ2)) +
  geom_rect(aes(xmin = median.x - mad.x / 2,
                xmax = median.x + mad.x / 2,
                ymin = median.y - mad.y / 2,
                ymax = median.y + mad.y / 2),
            alpha = 0.2, color = NA) +
  geom_point(aes(x = median.x, y = median.y), size = 3, shape = 16) +
  facet_wrap(~ party.full, ncol = 5) +
  scale_x_continuous("", limits = c(0, 100), breaks = c(2, 96), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", limits = c(0, 100), breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  scale_fill_manual("", values = c("red2","dodgerblue4"), labels = c("nižší vzdělání (ZŠ, SŠ bez maturity)", "vyšší vzdělání (SŠ s maturitou, VŠ)")) +
  scale_color_manual("", values = c("red2", "dodgerblue4"), labels = c("nižší vzdělání (ZŠ, SŠ bez maturity)", "vyšší vzdělání (SŠ s maturitou, VŠ)")) +
  ggtitle(paste0("\nPolitický kompas iDnes, medián a MAD, podle vzdělání: ", formatC(length(unique(d$iduser)), big.mark = " "), " respondentů")) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "top",
    plot.title = element_text(size = 18, hjust = 0),
    panel.grid = element_blank(),
    legend.key = element_blank()
  )

ggsave("Graf 7 - medián, MAD, podle vzdělání.png", plot = g7, width = 16, height = 8, dpi = 100)
# ggsave("Graf 7 - medián, MAD, podle vzdělání, menší.png", plot = g7, width = 13, height = 6.5, dpi = 85)


# Graf medián a Median absolute deviation, věk -----------------------------------------------

t8 <- copy(d[age != "0"])  # bez těch, kteří nevybrali věk
t8[, age2 := "do 29 let"]
t8[age >= 3, age2 := "30–49 let"]
t8[age >= 5, age2 := "nad 50 let"]
t8 <- t8[, .(median.x = median(as.numeric(lastX)),
             median.y = median(as.numeric(lastY)),
             mad.x = mad(lastX),
             mad.y = mad(lastY)), by = c("party.full", "age2")]

t8[, age2 := as.factor(age2)]
t8[, age2 := factor(age2, levels = c("do 29 let", "30–49 let", "nad 50 let"))]

g8 <- ggplot(t8, aes(group = party.full, col = age2, fill = age2)) +
  geom_rect(aes(xmin = median.x - mad.x / 2,
                xmax = median.x + mad.x / 2,
                ymin = median.y - mad.y / 2,
                ymax = median.y + mad.y / 2),
            alpha = 0.2, color = NA) +
  geom_point(aes(x = median.x, y = median.y), size = 3, shape = 16) +
  facet_wrap(~ party.full, ncol = 5) +
  scale_x_continuous("", limits = c(0, 100), breaks = c(2, 96), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", limits = c(0, 100), breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  scale_fill_manual("", values = c("red2","dodgerblue4", "goldenrod1")) +
  scale_color_manual("", values = c("red2", "dodgerblue4", "goldenrod1")) +
  ggtitle(paste0("\nPolitický kompas iDnes, medián a MAD, podle věku: ", formatC(length(unique(d$iduser)), big.mark = " "), " respondentů")) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "top",
    plot.title = element_text(size = 18, hjust = 0),
    panel.grid = element_blank(),
    legend.key = element_blank()
  )

ggsave("Graf 8 - medián, MAD, podle věku.png", plot = g8, width = 16, height = 8, dpi = 100)
# ggsave("Graf 8 - medián, MAD, podle věku, menší.png", plot = g8, width = 13, height = 6.5, dpi = 85)


# Základní graf --------------------------------------------------------------------

t9 <- copy(person)
t9 <- t9[, .(median.x = median(as.numeric(lastX)),
             median.y = median(as.numeric(lastY)),
             mad.x = mad(lastX),
             mad.y = mad(lastY))]

g9 <- ggplot() +
  geom_jitter(data = person, aes(x = lastX, y = lastY), alpha = 0.08, size = 1, shape = 16, col = "black") +
  geom_rect(data = t9, aes(xmin = median.x - mad.x / 2,
                xmax = median.x + mad.x / 2,
                ymin = median.y - mad.y / 2,
                ymax = median.y + mad.y / 2),
            alpha = 0.2, color = NA, fill = "red") +
  geom_point(data = t9, aes(x = median.x, y = median.y), size = 5, shape = 16, col = "red") +
  scale_x_continuous("", breaks = c(2, 96), labels = c("Levice", "Pravice")) +
  scale_y_continuous("", breaks = c(0, 100), labels = c("Konzervativní", "Liberální")) +
  ggtitle(paste0("\nPolitický kompas iDnes: Kde se vidí ", formatC(length(unique(d$iduser)), big.mark = " "), " hlasujících (body, medián, MAD)")) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 18, hjust = 0),
    panel.grid = element_blank()
  )

ggsave("Graf 9 - pozice respondentů, body.png", plot = g9, width = 11, height = 9, dpi = 100)
# ggsave("Graf 9 - pozice respondentů, body, menší.png", plot = g9, width = 11, height = 9, dpi = 85)

rm(g1, g2, g3, g4, g5, g6, g7, g8, g9)