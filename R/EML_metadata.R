# Prepare Metadata ####
# Markus Bauer


### Packages ###
library(EML)
library(here)

### Start ###
setwd(here())
rm(list = ls())



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Create infos about persons #########################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


creator <- eml$creator(
  individualName = eml$individualName(
    givenName = "Markus",
    surName = "Bauer"),
  electronicMailAddress = "markusbauer@mailbox.org"
)

address <- list(
  deliveryPoint = "Emil-Ramann-Strasse 6",
  city = "Freising",
  administrativeArea = "Bayern",
  postalCode = "85354",
  country = "Germany")

contact <- 
  list(
    individualName = creator$individualName,
    electronicMailAddress = creator$electronicMailAddress,
    address = address,
    organizationName = "Technical University of Munich",
    phone = "0049-152-56391781"
    )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create site and experiment infos ###################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


abstract <- "Most alpine floodplains are heavily modified and have lost their natural habitat dynamics and biodi-
versity. However, little is known about the consequences for floodplain forests. To study this topic, the 
Bavarian River Ammer was selected. An analysis of the effects of river regulation was possible within 
the ‘Schnalzaue’ where 55 years ago a weir and a dam had been built separating an active and inactive 
section of the floodplain. A comparison between both sections helps assessing the effect of the dam on 
the  alluvial  grey-alder  forests  (Alnetum  incanae),  as  basis  for  future  restoration.  We  expected  lower 
disturbance, lower moisture and higher nutrient supply in the inactive floodplain and thus, a transition 
to a maple-ash forest (Adoxo-Aceretum). Therefore, we analysed species composition, functional plant 
traits of the leaf-height-seed scheme (leaf dry matter content, plant height, seed mass) and Ellenberg 
indicators (indicator values for moisture and nutrients, indicator species for flooding and for periodical 
wet  conditions).  The  ordination  results  indicated  different  species  compositions  in  both  floodplain 
sections, while this was poorly reflected by the results of a syntaxonomic analysis. The latter suggested 
presence of a rarely flooded Asarum-subassociation of grey-alder forest in both floodplain sections. In 
the sections, similar site conditions prevailed regarding moisture and nutrient supply, while plant height 
indicated slightly more disturbance in the active floodplain. Overall, the results reveal low disturbance 
in both sections, even though the active one is connected with the river. A reason for this finding could 
be increased erosion of River Ammer downstream the weir with respective lower water levels. Accord-
ingly, restoration should improve river dynamics in the entire floodplain to allow more frequent dis-
turbance as a measure to maintain the grey-alder forest in the long term."

keywordSet <- list(
    keywordThesaurus = "LTER controlled vocabulary",
    keyword = list(
      "floodplain",
      "species diversity",
      "forest ecosystems",
      "plant species composition",
      "species abundance")
)

geographicDescription <- "Floodplain Schnalz of River Ammer near Peiting"

coverage <- set_coverage(
  begin = "2017-04-01", end = "2017-07-31",
  sci_names = list(list(
    Kingdom = "Plantae",
    Division = "Tracheophyta"
    )),
  geographicDescription = geographicDescription,
  west = 10.9544, east = 10.9649,
  north = 47.7744, south = 47.7727,
  altitudeMin = 638, altitudeMaximum = 644,
  altitudeUnits = "meter"
  )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C finalize EML #######################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


dataset <- list(
    title = "Alluvial forest vegetation in an active and inactive alpine 
    floodplain – a case study from River Ammer (Bavaria)",
    creator = creator,
    pubDate = "2018",
    language = "English",
    intellectualRights = "CC BY 4.0",
    abstract = abstract,
    keywordSet = keywordSet,
    coverage = coverage,
    contact = contact
    )

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset
  )

write_eml(eml, "METADATA.xml")
eml_validate("METADATA.xml")
