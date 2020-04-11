# Integrate county and COC data

cocs <- read.csv("coc_projections.csv", stringsAsFactors = FALSE) %>%
  mutate(community = paste(coc_number, coc_name),
         community_type = "HUD Continuum of Care (CoC)")


counties <- read.csv("county_counts.csv", stringsAsFactors = FALSE) 
names(counties) <- tolower(names(counties))

counties <- counties %>%
  mutate(community = paste(state_name, ",", county),
         community_type = "County") %>%
  arrange(community)

all_communities <- bind_rows(cocs, counties) %>%
  mutate(community_type = ifelse(community == " United States, nationwide",  
         "United States, nationwide", community_type)) %>%
  arrange(community_type, community)


