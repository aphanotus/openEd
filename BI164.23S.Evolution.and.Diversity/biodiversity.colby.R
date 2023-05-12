# Analysis for Biodiversity Colby Project

library(tidyverse)
library(ggpubr)

{
target.taxa <- c(
  mammals = "Mammalia",
  birds = "Aves",
  reptiles = "Reptilia",
  amphibians = "Amphibia",
  ray_finned_fish = "Actinopterygii",
  cartilaginous_fish = "Elasmobranchii",
  urochordates = "Tunicata",
  echinoderms = "Echinodermata",
  insects = "Insecta",
  crustaceans = "Crustacea", # NEW
  myriapods = "Myriapoda", # NEW
  spiders = "Chelicerata", # NEW
  annelids = "Annelida",
  molluscs = "Mollusca",
  nematodes = "Nematoda",
  dicots = "Magnoliopsida",
  monocots = "Liliopsida",
  conifers = "Pinopsida",
  ferns = "Polypodiopsida",
  liverworts = "Marchantiophyta", # NEW
  mosses = "Bryophyta",
  basidomycetes = "Basidiomycota",
  ascomycetes = "Ascomycota",
  lichens = "Lecanoromycetes",
  chromists = "Chromista",
  red_algae = "Rhodophyta",
  dinoflagellates = "Dinoflagellata",
  slime_molds = "Mycetozoa",
  archaea = "Archaea",
  bacteria = "Bacteria"
)
}

{
silly.award.titles <- c(
  mammals = "Master of Mammals",
  birds = "Dinosaur Hunter",
  herps = "Herp Hero",
  fish = "Award for Aquatic Awesomeness",
  urochordates = "Urovision Award",
  echinoderms = "Excellence in Echinoderms",
  holometabolous_insects = "Master of Metamorphosis",
  hemimetabolous_insects = "Best in Bugs",
  crustaceans = "Exoskeletal Extravert",
  myriapods = "Award for Most Legs",
  spiders = "Amazing Spiderperson",
  annelids = "Worm Warrior",
  molluscs = "Clam Champ",
  nematodes = "Finding Nemo Award",
  dicots = "Green Beret",
  monocots = "Greatest in Grass",
  conifers = "Pine Tree State Champion",
  ferns = "Fern Fiend",
  liverworts = "Moss Master",
  mosses = "Chopped Liver",
  basidomycetes = "Super Spore",
  ascomycetes = "Master of Mycology",
  lichens = "Most Symbiotic",
  chromists = "Kelp Keeper",
  red_algae = "Red Badge of Courage",
  dinoflagellates = "Alveolate Ally",
  slime_molds = "Super Slime",
  prokaryotes = "Woese Award for Unusual Effort"
)
}

# file.choose()
setwd("~/Documents/5.teaching/BI164-S23/Biodiversity Colby/")

bc <- read.csv("observations-322862.csv")
# head(bc)
# names(bc)

# The Black List: Invalid observations to remove
black.list <- c("151628989","151869385","151937263","151937865","151937934","151938579","151940072","152935704",
                "151936943","151936389","151936477","151936512","151936647","151936672","151936724","151936844",
                "151937001","151937296","151937325","151937375","151937391")
bc <- bc %>% filter(!(id %in% black.list))
i <- which(bc$user_login=="coleiannuzzi" & bc$place_country_name == "Canada" & !(bc$taxon_id=="4343" | bc$taxon_id=="122767"))
bc <- bc[-i,]

# Filter out Chris!
bc <- bc %>% filter(user_login!="ecologistchris")

# Filter out observations without pictures
bc <- bc %>% filter(image_url!="")

# Filter out completely unidentified observations
bc <- bc %>% filter(taxon_kingdom_name!="")

# BI164 taxa counts
bc$bi164taxon <- bc$iconic_taxon_name
i <- which(!bc$bi164taxon %in% target.taxa)
bc$bi164taxon[i] <- bc$taxon_phylum_name[i]
i <- which(!bc$bi164taxon %in% target.taxa)
bc$bi164taxon[i] <- bc$taxon_subphylum_name[i]
i <- which(!bc$bi164taxon %in% target.taxa)
bc$bi164taxon[i] <- bc$taxon_class_name[i]
i <- which(!bc$bi164taxon %in% target.taxa)
bc$bi164taxon[i] <- NA

# lichens
i <- which(bc$taxon_class_name=="Lecanoromycetes")
bc$bi164taxon[i] <- "Lecanoromycetes"

# Parse insects into Holo- and hemimetabolous
holometabolous.orders <- c("Coleoptera","Diptera","Hymenoptera","Lepidoptera","Mecoptera","Megaloptera","Neuroptera","Raphidioptera","Siphonaptera","Strepsiptera","Trichoptera")
i <- which(bc$bi164taxon == "Insecta")
bc$bi164taxon[i] <- "hemimetabolous_insects"
i <- which(bc$taxon_order_name %in% holometabolous.orders)
bc$bi164taxon[i] <- "holometabolous_insects"

# Combine reptiles and amphibians into herp category
i <- which(bc$bi164taxon %in% c("Amphibia","Reptilia"))
bc$bi164taxon[i] <- "herps"

# Combine fish
i <- which(bc$bi164taxon %in% c("Actinopterygii","Elasmobranchii"))
bc$bi164taxon[i] <- "fish"

# Combine prokaryotes
i <- which(bc$bi164taxon %in% c("Archaea","Bacteria"))
bc$bi164taxon[i] <- "prokaryotes"

# Counts for most target groups
taxon.counts <-
  unlist(lapply(target.taxa, function(taxon) {
    sum(apply(bc[,grep("^taxon_",names(bc))], 1, function(x) { any(taxon == x, na.rm = TRUE) }))
  }))

# Split the insects
i <- which(names(taxon.counts)=="insects")
taxon.counts <- taxon.counts[-i]
x <- c(with(bc, by(bi164taxon,bi164taxon,length)))
taxon.counts <- c(taxon.counts,x[grep("insects",names(x))])

plot.observations.by.taxon <-
  tibble(
    taxon = gsub("_"," ",names(taxon.counts)),
    observations = taxon.counts
  ) %>%
    ggplot(aes(x=reorder(taxon, observations), y=observations)) +
    theme_bw()+
    geom_bar(stat = "identity") +
    coord_flip() +
    xlab("")

plot.observations.by.taxon

ggsave("plot.observations.by.taxon.png",plot.observations.by.taxon, width = 8, height = 4, scale = 1)

# Observations per user
median.observations.per.user <- bc %>%
  group_by(user_login) %>%
  summarise(observations = n()) %>%
  pull(observations) %>%
  median()

# Initial definition of the User Points table
user.points <- bc %>%
  group_by(user_login) %>%
  summarise(observations = n()) %>%
  arrange(desc(observations)) %>%
  mutate(
    atleast3 = observations>=3,
    aboveaverage = observations>median.observations.per.user
  ) %>%
  mutate(pts = if_else(atleast3,3,0)) %>%
  mutate(pts = if_else(aboveaverage,pts+2,pts))

# user.points %>% print(n = Inf)

plot.observations.by.user <- bc %>%
  group_by(user_login) %>%
  summarise(observations = n()) %>%
  ggplot(aes(x=reorder(user_login, observations), y=observations)) +
  theme_bw()+
  geom_hline(yintercept = 3, color = "darkred") +
  geom_hline(yintercept = median.observations.per.user, color = "darkred", linetype = 2) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("username")

plot.observations.by.user

ggsave("plot.observations.by.user.png",plot.observations.by.user, width = 8, height = 4, scale = 1)


# Most observations made before March 20
award.value <- 5
x <- bc %>%
  filter(strtrim(sub("2023-0","",observed_on),1)=="3") %>%
  filter(as.numeric(sub("2023-03-","",observed_on))<20) %>%
  group_by(user_login) %>%
  summarise(observations = n()) %>%
  filter(species == max(species)) %>%
  pull(user_login)
x
# "dakshprashar"

i <- which(user.points$user_login %in% x)
user.points$pts[i] <- user.points$pts[i] + ceiling(award.value/length(i))

# runner-up
x <- bc %>%
  filter(strtrim(sub("2023-0","",observed_on),1)=="3") %>%
  filter(as.numeric(sub("2023-03-","",observed_on))<20) %>%
  group_by(user_login, taxon_id) %>%
  summarise(observations = n()) %>%
  # arrange(desc(observations)) %>%
  group_by(user_login) %>%
  summarise(species = n()) %>%
  # arrange(desc(species)) %>%
  filter(species != max(species)) %>%
  filter(species == max(species)) %>%
  pull(user_login)
x
# "hannahw6"

i <- which(user.points$user_login %in% x)
user.points$pts[i] <- user.points$pts[i] + ceiling((award.value-1)/length(i))

# Most animal phyla
award.value <- 5
x <- bc %>%
  filter(taxon_kingdom_name == "Animalia") %>%
  group_by(user_login, taxon_phylum_name) %>%
  summarise(observations = n()) %>%
  arrange(desc(observations)) %>%
  group_by(user_login) %>%
  summarise(phyla = n()) %>%
  filter(phyla == max(phyla)) %>%
  pull(user_login)
paste0(x, collapse = ", ")

i <- which(user.points$user_login %in% x)
user.points$pts[i] <- user.points$pts[i] + ceiling(award.value/length(i))

# runners up
x <- bc %>%
  filter(taxon_kingdom_name == "Animalia") %>%
  group_by(user_login, taxon_phylum_name) %>%
  summarise(observations = n()) %>%
  arrange(desc(observations)) %>%
  group_by(user_login) %>%
  summarise(phyla = n()) %>%
  filter(phyla != max(phyla)) %>%
  filter(phyla == max(phyla)) %>%
  pull(user_login)
paste0(x, collapse = ", ")

i <- which(user.points$user_login %in% x)
user.points$pts[i] <- user.points$pts[i] + ceiling((award.value-1)/length(i))

# Most species in each target taxon
unique.targets <- sort(unique(bc$bi164taxon))
unique.targets <- unique.targets[which(!is.na(unique.targets))]

award.value <- 5
taxon.awards <- tibble(
  taxon = unique.targets,
  winner = NA,    winning.number = NA,
  runner.up = NA, ru.number = NA,
)

# backup.user.points <- user.points

for (i in 1:length(unique.targets)) {
  bc.i <- bc %>%
    filter(bi164taxon == unique.targets[i]) %>%
    group_by(user_login, taxon_id) %>%
    summarise(observations = n()) %>%
    arrange(desc(observations)) %>%
    group_by(user_login) %>%
    summarise(species = n()) %>%
    filter(species == max(species))

  x <- bc.i %>% pull(user_login)
  j <- which(user.points$user_login %in% x)
  user.points$pts[j] <- user.points$pts[j] + ceiling(award.value/length(j))

  taxon.awards$winner[i] <- paste(x, collapse = ", ")
  taxon.awards$winning.number[i] <- bc.i$species[1]

  bc.i <- bc %>%
    filter(bi164taxon == unique.targets[i]) %>%
    group_by(user_login, taxon_id) %>%
    summarise(observations = n()) %>%
    arrange(desc(observations)) %>%
    group_by(user_login) %>%
    summarise(species = n()) %>%
    filter(species != max(species)) %>%
    filter(species == max(species))

  x <- bc.i %>% pull(user_login)
  j <- which(user.points$user_login %in% x)
  user.points$pts[j] <- user.points$pts[j] + ceiling((award.value-1)/length(j))

  taxon.awards$runner.up[i] <- paste(x, collapse = ", ")
  taxon.awards$ru.number[i] <- bc.i$species[1]

}

taxon.awards %>% print(n=Inf)
write_tsv(taxon.awards, "taxon.awards.tsv")

award.species.numbers <- unlist(lapply(unique.targets, function(x){
  bc %>%
    filter(bi164taxon == x) %>%
    group_by(user_login, taxon_id) %>%
    summarise(observations = n()) %>%
    # arrange(desc(observations)) %>%
    group_by(user_login) %>%
    summarise(species = n()) %>%
    pull(species) %>%
    max()
}))
names(award.species.numbers) <- unique.targets


plot.award.species.numbers <- data.frame(
  taxon = names(award.species.numbers),
  species = award.species.numbers
) %>%
  ggplot(aes(x=reorder(taxon, species), y=species)) +
  theme_bw()+
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("")

plot.award.species.numbers
ggsave("plot.award.species.numbers.png", plot.award.species.numbers, width = 8, height = 4, scale = 1)

iNat.plots <- ggarrange(plot.observations.by.taxon, plot.award.species.numbers, plot.observations.by.user, ncol = 3)
ggsave("iNat.plots.png", iNat.plots, width = 9, height = 4, scale = 1.1)

# Misc Awards

# Herald of Spring (Global)	5	Earliest bumble bee observation of the year, outside Maine
award.value <- 5
x <- bc %>%
  filter(taxon_genus_name == "Bombus") %>%
  filter(place_admin1_name != "Maine") %>%
  arrange(observed_on) %>%
  slice_head(n=2) %>%
  select(user_login, observed_on, taxon_species_name)
x

i <- which(user.points$user_login == x$user_login[1])
user.points$pts[i] <- user.points$pts[i] + award.value
if (x$user_login[1] != x$user_login[2]) {
  i <- which(user.points$user_login == x$user_login[2])
  user.points$pts[i] <- user.points$pts[i] + award.value-1
}

# Herald of Spring (Local)	5	Earliest bumble bee observation of the year, in Maine
award.value <- 5
x <- bc %>%
  filter(taxon_genus_name == "Bombus") %>%
  filter(place_admin1_name == "Maine") %>%
  arrange(observed_on) %>%
  slice_head(n=2) %>%
  select(user_login, observed_on, taxon_species_name)
x

i <- which(user.points$user_login == x$user_login[1])
user.points$pts[i] <- user.points$pts[i] + award.value
if (x$user_login[1] != x$user_login[2]) {
  i <- which(user.points$user_login == x$user_login[2])
  user.points$pts[i] <- user.points$pts[i] + award.value-1
}

# Local Legend	5	Most observations made on Colby’s campus grounds
award.value <- 5

lat.max <- 44.570120; lat.min <- 44.557084
long.max <- -69.654505; long.min <- -69.668110
x <- bc %>%
  filter(latitude > lat.min & latitude < lat.max &
         longitude > long.min & longitude < long.max) %>%
  group_by(user_login) %>%
  summarise(observations = n()) %>%
  arrange(desc(observations)) %>%
  slice_head(n=2)
x

i <- which(user.points$user_login == x$user_login[1])
user.points$pts[i] <- user.points$pts[i] + award.value
if (x$user_login[1] != x$user_login[2]) {
  i <- which(user.points$user_login == x$user_login[2])
  user.points$pts[i] <- user.points$pts[i] + award.value-1
}

# World Traveler	3	Most distant or remote observation
award.value <- 3
colby.lat <- 44.564348
colby.long <- -69.660707
plot(bc$longitude, bc$latitude)
points(colby.long, colby.lat, pch = 16, col = "darkred")

i <- which.max(bc$longitude)
bc[i,]
# aashep26, Caryophyllales, Casablanca, 5369 km from campus
i <- which(user.points$user_login == bc$user_login[i])
user.points$pts[i] <- user.points$pts[i] + award.value

i <- which.min(bc$latitude)
bc[i,]
# lucygoodman26, Pavo cristatus, DR 2883 km from campus
i <- which(user.points$user_login == bc$user_login[i])
user.points$pts[i] <- user.points$pts[i] + award.value-1

# Welcoming Committee	5	Most introduced species observed
award.value <- 5
# Add "introduced=true" to the URL when downloading iNat observations

bc.i <- read.csv("observations-322865.csv")

# Filters
bc.i <- bc.i %>% filter(!(id %in% black.list))
bc.i <- bc.i %>% filter(user_login!="ecologistchris")
bc.i <- bc.i %>% filter(image_url!="")
bc.i <- bc.i %>% filter(taxon_kingdom_name!="")

x <- bc.i %>%
  group_by(user_login, taxon_id) %>%
  summarise(observations = n()) %>%
  group_by(user_login) %>%
  summarise(species = n()) %>%
  arrange(desc(species)) %>%
  slice_head(n=2)
x

i <- which(user.points$user_login == x$user_login[1])
user.points$pts[i] <- user.points$pts[i] + award.value
if (x$user_login[1] != x$user_login[2]) {
  i <- which(user.points$user_login == x$user_login[2])
  user.points$pts[i] <- user.points$pts[i] + award.value-1
}

# Super Duper Cuteness Award	3	Cutest organism
award.value <- 3
x <- bc %>%
  filter(grepl("Lepus",taxon_genus_name)) %>%
  pull(user_login)
x

i <- which(user.points$user_login %in% x)
user.points$pts[i] <- user.points$pts[i] + ceiling(award.value/length(i))

# Runner-up
x <- bc %>%
  filter(grepl("Bombus terricola",taxon_species_name)) %>%
  pull(user_login)
x

i <- which(user.points$user_login %in% x)
user.points$pts[i] <- user.points$pts[i] + ceiling((award.value-1)/length(i))


# Red in Tooth & Claw	3	Most dangerous organism (Please, don’t do anything dangerous!)
award.value <- 3
x <- bc %>%
  filter(grepl("Agkistrodon",taxon_genus_name)) %>%
  pull(user_login)
x

i <- which(user.points$user_login %in% x)
user.points$pts[i] <- user.points$pts[i] + ceiling(award.value/length(i))


# Final results!
user.points %>%
  arrange(desc(pts)) %>%
  filter(pts > 0) %>%
  select(user_login, observations, pts) %>%
  print(n=Inf)

x <- user.points %>%
  filter(pts > 0) %>%
  select(user_login, observations, pts) %>%
  mutate(efficiency = pts/observations) %>%
  arrange(desc(efficiency))

x %>%
  print(n=Inf)

plot(x$observations, x$pts)
abline(h=25, col="darkred")

# Enforce the point cap
x <- user.points %>%
  filter(pts > 0) %>%
  mutate(pts = if_else(pts > 25, 25, pts)) %>%
  arrange(desc(pts), desc(observations)) %>%
  select(user_login, observations, pts) %>%
  print(n=Inf)

write_tsv(x,"final.project.point.totals.tsv")
