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


# Základní graf --------------------------------------------------------------------

ggplot(d[party != "person"], aes(x = lastX, y = lastY, group = party, col = party)) +
  geom_jitter(alpha = 0.1, size = 1, shape = 16) +
  facet_wrap(~ party.full, ncol = 5) +
  scale_x_continuous("", breaks = c(2, 50, 96), labels = c("Levice", "", "Pravice")) +
  scale_y_continuous("", breaks = c(2, 50, 96), labels = c("Liberální", "", "Konzervativní")) +
  ggtitle(paste0("\nPolitický kompas iDnes: ", length(unique(d$iduser)), " respondentů")) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 18, hjust = 0),
    panel.grid = element_blank()
  )

ggsave("Graf 1 - základní.png", width = 15, height = 9, dpi = 100)
ggsave("Graf 1 - základní, menší.png", width = 13, height = 7, dpi = 85)
