# Title: FMMO project
# Author: Pierre Merel
# Date: July 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


#load packages
options(dplyr.summarise.inform= FALSE)
suppressPackageStartupMessages({library(tidyverse)
                                library(sf) })


FO1STATES<-c("09","10","25","33","34","44","50","11")
FO1MARYMINUS<-c("Allegany","Garrett")
FO1NEWYMINUS<-c("Allegany","Cattaraugus","Chautauqua","Erie","Genesee","Livingston","Monroe",
                 "Niagara","Ontario","Orleans","Seneca","Wayne","Wyoming","Cayuga","Steuben","Yates")
###,"Cayuga","Steuben","Yates" 
## the townships of Conquest, Montezuma, Sterling and Victory in Cayuga County; the city of Hornell, and the 
##townships of Avoca, Bath, Bradford, Canisteo, Cohocton, Dansville, Fremont, Pulteney, Hartsville, Hornellsville, 
##Howard, Prattsburgh, Urbana, Wayland, Wayne and Wheeler in Steuben County; and the townships of Italy, Middlesex, and Potter in Yates County.
FO1PENNPLUS<-c("Adams","Bucks","Chester","Cumberland","Dauphin","Delaware","Franklin","Fulton",
                "Juniata","Lancaster","Lebanon","Montgomery","Perry","Philadelphia","York")
FO1VIRGPLUS<-c("Arlington","Fairfax","Loudoun","Prince William","Alexandria","Fairfax","Falls Church","Manassas","Manassas Park")
##, and the cities of Alexandria, Fairfax, Falls Church, Manassas, and Manassas Park (count as counties in Virginia)
FO5GEORPLUS<-c("Catoosa","Chattooga","Dade","Fannin","Murray","Walker","Whitfield")
FO5INDIPLUS<-c("Clark","Crawford","Daviess","Dubois","Floyd","Gibson","Greene","Harrison","Knox","Martin",
               "Orange","Perry","Pike","Posey","Scott","Spencer","Sullivan","Vanderburgh","Warrick","Washington")
FO5KENTPLUS<-c("Adair","Anderson","Bath","Bell","Bourbon","Boyle","Breathitt","Breckinridge","Bullitt",
               "Butler","Carroll","Carter","Casey","Clark","Clay","Clinton","Cumberland","Daviess",
               "Edmonson","Elliott","Estill","Fayette","Fleming","Franklin","Gallatin","Garrard",
               "Grayson","Green","Hancock","Hardin","Harlan","Hart","Henderson","Henry","Hopkins","Jackson",
               "Jefferson","Jessamine","Knott","Knox","Larue","Laurel","Lee","Leslie","Letcher","Lincoln",
               "Madison","Marion","McCreary","McLean","Meade","Menifee","Mercer","Montgomery","Morgan","Muhlenberg",
               "Nelson","Nicholas","Ohio","Oldham","Owen","Owsley","Perry","Powell","Pulaski","Rockcastle",
               "Rowan","Russell","Scott","Shelby","Spencer","Taylor","Trimble","Union","Washington","Wayne",
               "Webster","Whitley","Wolfe","Woodford")
FO5STATES<-c("37","45")
FO5TENNEPLUS<-c("Anderson","Blount","Bradley","Campbell","Carter","Claiborne","Cocke",
                 "Cumberland","Grainger","Greene","Hamblen","Hamilton","Hancock","Hawkins", 
                 "Jefferson","Johnson","Knox","Loudon","Marion","McMinn","Meigs","Monroe",
                 "Morgan","Polk","Rhea","Roane","Scott","Sequatchie","Sevier","Sullivan",
                 "Unicoi","Union","Washington")
FO5VIRGPLUS<-c("Buchanan","Dickenson","Lee","Russell","Scott","Tazewell","Washington","Wise","Grayson","Smyth",
               "Carroll","Wythe","Pulaski","Bland","Floyd","Franklin","Patrick","Henry","Pittsylvania","Campbell",
               "Giles","Montgomery","Craig","Roanoke","Botetourt","Bedford","Alleghany","Bath","Highland","Rockbridge",
               "Augusta","Rockingham","Amherst","Bristol","Norton","Galax","Radford","Covington","Buena Vista","Lexington",
               "Salem","Waynesboro","Martinsville","Danville","Lynchburg","Staunton","Harrisonburg")
###and the cities of Bristol and Norton
###counties and cities have been added based on FMMO map by USDA.
F05VIRGEXCL<-"51620" 
##exclude city of Franklin while Franklin County should be included
FO5WVIRGPLUS<-c("McDowell","Mercer")
FO6FLORMINUS<-c("Escambia","Okaloosa","Santa Rosa","Walton")

FO7STATES<-c("01","05","22","28")
FO7FLORPLUS<-c("Escambia","Okaloosa","Santa Rosa","Walton")
FO7KENTPLUS<-c("Allen","Ballard","Barren","Caldwell","Calloway","Carlisle","Christian",
              "Crittenden","Fulton","Graves","Hickman","Livingston","Logan","Lyon",
              "Marshall","McCracken","Metcalfe","Monroe","Simpson","Todd","Trigg","Warren")
FO7MISSOPLUS<-c("Barry","Barton","Bollinger","Butler","Cape Girardeau","Carter","Cedar",
                "Christian","Crawford","Dade","Dallas","Dent","Douglas","Dunklin","Greene",
                "Howell","Iron","Jasper","Laclede","Lawrence","Madison","McDonald","Mississippi",
                "New Madrid","Newton","Oregon","Ozark","Pemiscot","Perry","Polk","Reynolds",
                "Ripley","Scott","Shannon","St. Francois","Stoddard","Stone","Taney","Texas",
                "Vernon","Washington","Wayne","Webster","Wright")
FO30ILLIPLUS<-c("Boone","Carroll","Cook","DeKalb","DuPage","Jo Daviess","Kane","Kendall", 
                "Lake","Lee","McHenry","Ogle","Stephenson","Will","Winnebago")
FO30IOWAPLUS<-c("Howard","Kossuth","Mitchell","Winnebago","Winneshiek","Worth")
FO30MICHPLUS<-c("Delta","Dickinson","Gogebic","Iron","Menominee","Ontonagon")
FO30MINNEMINUS<-c("Lincoln","Nobles","Pipestone","Rock")
FO30NDAKOPLUS<-c("Barnes","Cass","Cavalier","Dickey","Grand Forks","Griggs","La Moure",
                 "Nelson","Pembina","Ramsey","Ransom","Richland","Sargent","Steele","Traill",
                  "Walsh")
FO30SDAKOPLUS<-c("Brown","Day","Edmunds","Grant","Marshall","McPherson","Roberts","Walworth")
FO30WISCMINUS<-c("Crawford","Grant")
FO32STATES<-c("20","40")
FO32COLOPLUS<-c("Adams","Arapahoe","Baca","Bent","Boulder","Broomfield","Chaffee",
                "Clear Creek","Cheyenne","Crowley","Custer","Delta","Denver","Douglas",
                "Eagle","El Paso","Elbert","Fremont","Garfield","Gilpin","Gunnison","Huerfano",
                "Jefferson","Kiowa","Kit Carson","Lake","Larimer","Las Animas","Lincoln","Logan",
                "Mesa","Montrose","Morgan","Otero","Park","Phillips","Pitkin","Prowers","Pueblo",
                "Sedgwick","Summit","Teller","Washington","Weld","Yuma")
FO32ILLIPLUS<-c("Adams","Alexander","Bond","Brown","Bureau","Calhoun","Cass","Champaign",
                "Christian","Clark","Clay","Clinton","Coles","Crawford","Cumberland","De Witt",
                "Douglas","Edgar","Edwards","Effingham","Fayette","Ford","Franklin","Fulton","Gallatin",
                "Greene","Grundy","Hamilton","Hancock","Hardin","Henderson","Henry","Iroquois",
                "Jackson","Jasper","Jefferson","Jersey","Johnson","Kankakee","Knox","LaSalle",
                "Lawrence","Livingston","Logan","McDonough","McLean","Macon","Macoupin","Madison",
                "Marion","Marshall","Mason","Massac","Menard","Mercer","Monroe","Montgomery","Morgan",
                "Moultrie","Peoria","Perry","Piatt","Pike","Pope","Pulaski","Putnam","Randolph","Richland",
                "Rock Island","Saline","Sangamon","Schuyler","Scott","Shelby","St. Clair","Stark","Tazewell",
                "Union","Vermilion","Wabash","Warren","Washington","Wayne","White","Whiteside",
                "Williamson","Woodford")
FO32MISSOPLUS<-c("Andrew","Atchison","Bates","Buchanan","Caldwell","Carroll","Cass","Clay","Clinton","Daviess",
                 "DeKalb","Franklin","Gentry","Grundy","Harrison","Henry","Hickory","Holt","Jackson","Jefferson",
                 "Johnson","Lafayette","Lincoln","Livingston","Mercer","Nodaway","Pettis","Platte","Putnam",
                 "Ray","Saline","Schuyler","St. Charles","St. Clair","Ste. Genevieve","St. Louis","Sullivan",
                 "Warren","Worth") ##includes Saint Louis county and Saint Louis city
FO32NEBRPLUS<-c("Adams","Antelope","Boone","Buffalo","Burt","Butler","Cass","Cedar","Chase","Clay","Colfax",
                "Cuming","Custer","Dakota","Dawson","Dixon","Dodge","Douglas","Dundy","Fillmore","Franklin","Frontier",
                "Furnas","Gage","Gosper","Greeley","Hall","Hamilton","Harlan","Hayes","Hitchcock","Howard","Jefferson",
                "Johnson","Kearney","Keith","Knox","Lancaster","Lincoln","Madison","Merrick","Nance","Nemaha","Nuckolls",
                "Otoe","Pawnee","Perkins","Phelps","Pierce","Platte","Polk","Red Willow","Richardson","Saline","Sarpy",
                "Saunders","Seward","Sherman","Stanton","Thayer","Thurston","Valley","Washington","Wayne","Webster","York")
FO32SDAKOPLUS<-c("Aurora","Beadle","Bon Homme","Brookings","Clark","Clay","Codington","Davison","Deuel","Douglas","Hamlin",
                 "Hanson","Hutchinson","Jerauld","Kingsbury","Lake","Lincoln","McCook","Miner","Minnehaha","Moody","Sanborn",
                 "Spink","Turner","Union","Yankton")
FO33INDIPLUS<-c("Adams","Allen","Bartholomew","Benton","Blackford","Boone","Brown","Carroll","Cass","Clay","Clinton",
                "Dearborn","Decatur","DeKalb","Delaware","Elkhart","Fayette","Fountain","Franklin","Fulton","Grant",
                "Hamilton","Hancock","Hendricks","Henry","Howard","Huntington","Jackson","Jasper","Jay","Jefferson",
                "Jennings","Johnson","Kosciusko","LaGrange","Lake","LaPorte","Lawrence","Madison","Marion","Marshall",
                "Miami","Monroe","Montgomery","Morgan","Newton","Noble","Ohio","Owen","Parke","Porter","Pulaski",
                "Putnam","Randolph","Ripley","Rush","Shelby","St. Joseph","Starke","Steuben","Switzerland","Tippecanoe",
                "Tipton","Union","Vermillion","Vigo","Wabash","Warren","Wayne","Wells","White","Whitley")
FO33KENTPLUS<-c("Boone","Boyd","Bracken","Campbell","Floyd","Grant","Greenup","Harrison","Johnson","Kenton","Lawrence",
                "Lewis","Magoffin","Martin","Mason","Pendleton","Pike","Robertson")
FO33OHIOMINUS<-c("Erie","Huron","Ottawa") ##part of Sandusky county should not technically be included
FO33PENNPLUS<-c("Allegheny","Armstrong","Beaver","Butler","Crawford","Erie","Fayette","Greene","Lawrence","Mercer",
                "Venango","Washington","Clarion","Westmoreland") 
##In Clarion County only the townships of Ashland, Beaver, Licking, Madison, Perry, Piney, Richland, Salem, and Toby. All of Westmoreland County except the townships of Cook, Donegal, Fairfield, Ligonier, and St. Clair, and the boroughs of Bolivar, Donegal, Ligonier, New Florence, and Seward
FO33WVIRGPLUS<-c("Barbour","Boone","Brooke","Cabell","Calhoun","Doddridge","Fayette","Gilmer","Hancock","Harrison",
                 "Jackson","Kanawha","Lewis","Lincoln","Logan","Marion","Marshall","Mason","Mingo","Monongalia",
                 "Ohio","Pleasants","Preston","Putnam","Raleigh","Randolph","Ritchie","Roane","Taylor","Tucker",
                 "Tyler","Upshur","Wayne","Wetzel","Wirt","Wood","Wyoming")

FO124IDAHPLUS<-c("Benewah","Bonner","Boundary","Kootenai","Latah","Shoshone")
FO124OREGPLUS<-c("Benton","Clackamas","Clatsop","Columbia","Coos","Crook","Curry","Deschutes","Douglas","Gilliam","Hood River",
                 "Jackson","Jefferson","Josephine","Klamath","Lake","Lane","Lincoln","Linn","Marion","Morrow","Multnomah","Polk",
                 "Sherman","Tillamook","Umatilla","Wasco","Washington","Wheeler","Yamhill")
FO126COLOPLUS<-c("Archuleta","La Plata","Montezuma")
FO126STATES<-c("35","48")

NYORKCOUNTIES<-c("36101","36011","36123")

pcounties<- read_sf(dsn = "Shape files/cb_2014_us_county_500k", layer = "cb_2014_us_county_500k")

pcountiescontinental<-pcounties %>%
  mutate(STATEFIPS=as.numeric(STATEFP)) %>%
  filter(STATEFIPS %in% c(1,3:14,16:56)) %>%
  dplyr::select(STATEFP,COUNTYFP,GEOID,NAME,geometry) %>%
  st_zm(pcountiescontinental,drop=T,what="ZM")

pstates<-pcountiescontinental %>%
  group_by(STATEFP) %>%
  dplyr::summarize()

pcountiescontinentalminus<-pcountiescontinental %>%
  filter(!GEOID %in% NYORKCOUNTIES)

pNYstate<-pcountiescontinental %>%
  filter(STATEFP=="36")

F01NEWYCITIESMINUS<-c("3610103342","3610104770","3610107740","3610112265","3610116738","3601117849",
                      "3610119675","3610127551","3610132567","3610135672","3610135683","3610135837",
                      "3612338044","3612347020","3601148131","3612359597","3610159718","3610159982",
                      "3601171146","3610176496","3601177420","3610178861","3610178883","3610181457")

F01NEWY3COUNTYFP<-c("101","011","123")

pnewyorkstate<- read_sf(dsn="Shape files/New_York_State_Municipal_Civil_Boundaries", layer = "New_York_State_Municipal_Civil_Boundaries")
pnewyork3counties<-pnewyorkstate %>%
  filter(COUNTY %in% c("Cayuga","Steuben","Yates")) %>%
  mutate(GEOID=FIPS_CODE) %>%
  mutate(STATEFP="36") %>%
  mutate(COUNTYFP=str_sub(GEOID,3,5)) %>%
  dplyr::select(STATEFP,COUNTYFP,GEOID,NAME,geometry)

pnewyork3countiestagged<-pnewyork3counties %>% 
  mutate(Region=case_when(GEOID %in% F01NEWYCITIESMINUS ~ NA, T ~ "Northeast"))

pcountiesNY3counties<-rbind(as.data.frame(pcountiescontinentalminus),as.data.frame(pnewyork3counties))

pregions<-pcountiesNY3counties %>% 
  mutate(Region=case_when(STATEFP %in% FO1STATES ~ "Northeast",
                              STATEFP=="24" & !NAME %in% FO1MARYMINUS ~ "Northeast",
                              STATEFP=="36" & !COUNTYFP %in% F01NEWY3COUNTYFP & !NAME %in% FO1NEWYMINUS ~ "Northeast",
                              STATEFP=="36" & COUNTYFP %in% F01NEWY3COUNTYFP & !GEOID %in% F01NEWYCITIESMINUS ~ "Northeast",
                              STATEFP=="42" & NAME %in% FO1PENNPLUS ~ "Northeast",
                              STATEFP=="51" & NAME %in% FO1VIRGPLUS ~ "Northeast",
                              STATEFP=="13" & NAME %in% FO5GEORPLUS ~ "Appalachian",
                              STATEFP=="18" & NAME %in% FO5INDIPLUS ~ "Appalachian",
                              STATEFP=="21" & NAME %in% FO5KENTPLUS ~ "Appalachian",
                              STATEFP %in% FO5STATES ~ "Appalachian",
                              STATEFP=="47" & NAME %in% FO5TENNEPLUS ~ "Appalachian",
                              STATEFP=="51" & NAME %in% FO5VIRGPLUS & !GEOID=="51620" ~ "Appalachian",
                              STATEFP=="54" & NAME %in% FO5WVIRGPLUS ~ "Appalachian",
                              STATEFP=="12" & !NAME %in% FO6FLORMINUS ~ "Florida",
                              STATEFP %in% FO7STATES ~ "Southeast",
                              STATEFP=="12" & NAME %in% FO7FLORPLUS ~ "Southeast",
                              STATEFP=="13" & !NAME %in% FO5GEORPLUS ~ "Southeast",
                              STATEFP=="21" & NAME %in% FO7KENTPLUS ~ "Southeast",
                              STATEFP=="29" & NAME %in% FO7MISSOPLUS ~ "Southeast",
                              STATEFP=="47" & !NAME %in% FO5TENNEPLUS ~ "Southeast",
                              STATEFP=="17" & NAME %in% FO30ILLIPLUS ~ "Upper Midwest",
                              STATEFP=="19" & NAME %in% FO30IOWAPLUS ~ "Upper Midwest",
                              STATEFP=="26" & NAME %in% FO30MICHPLUS ~ "Upper Midwest",
                              STATEFP=="27" & !NAME %in% FO30MINNEMINUS ~ "Upper Midwest",
                              STATEFP=="38" & NAME %in% FO30NDAKOPLUS ~ "Upper Midwest",
                              STATEFP=="46" & NAME %in% FO30SDAKOPLUS ~ "Upper Midwest",
                              STATEFP=="55" & !NAME %in% FO30WISCMINUS ~ "Upper Midwest",
                              STATEFP=="08" & NAME %in% FO32COLOPLUS ~ "Central",
                              STATEFP=="17" & NAME %in% FO32ILLIPLUS ~ "Central",
                              STATEFP=="19" & !NAME %in% FO30IOWAPLUS ~ "Central",
                              STATEFP %in% FO32STATES ~ "Central",
                              STATEFP=="27" & NAME %in% FO30MINNEMINUS ~ "Central",
                              STATEFP=="29" & NAME %in% FO32MISSOPLUS ~ "Central",
                              STATEFP=="31" & NAME %in% FO32NEBRPLUS ~ "Central",
                              STATEFP=="46" & NAME %in% FO32SDAKOPLUS ~ "Central",
                              STATEFP=="55" & NAME %in% FO30WISCMINUS ~ "Central",
                              STATEFP=="18" & NAME %in% FO33INDIPLUS ~ "Mideast",
                              STATEFP=="21" & NAME %in% FO33KENTPLUS ~ "Mideast",
                              STATEFP=="26" & !NAME %in% FO30MICHPLUS ~ "Mideast",
                              STATEFP=="39" & !NAME %in% FO33OHIOMINUS ~ "Mideast",
                              STATEFP=="42" & NAME %in% FO33PENNPLUS ~ "Mideast",
                              STATEFP=="54" & NAME %in% FO33WVIRGPLUS ~ "Mideast",
                              STATEFP=="06" ~ "California",
                              STATEFP=="16" & NAME %in% FO124IDAHPLUS ~ "Pacific Northwest",
                              STATEFP=="41" & NAME %in% FO124OREGPLUS ~ "Pacific Northwest",
                              STATEFP=="53" ~ "Pacific Northwest",
                              STATEFP=="08" & NAME %in% FO126COLOPLUS ~ "Southwest",
                              STATEFP %in% FO126STATES ~ "Southwest",
                              STATEFP=="04" ~ "Arizona",
                              T ~ NA,
                              ))
pregions<-st_as_sf(pregions,sf_column_name="geometry")

pregionsFMMO<-pregions %>%
  group_by(Region) %>%
  dplyr::summarize()   

targetnames=c("Northeast","Appalachian","Florida","Southeast","Upper Midwest","Central",
              "Mideast","California","Pacific Northwest","Southwest","Arizona",NA)

newnames=c("Northeast","Appalachian","Florida","Southeast","Upper Midwest","Central",
              "Mideast","California","Pacific Northwest","Southwest","Arizona","Unregulated")

pregionsFMMOreorder<-pregionsFMMO %>%
  dplyr::arrange(factor(Region, levels=targetnames)) %>%
  dplyr::mutate(ID = case_when(is.na(Region) ~ NA, T ~ row_number())) %>%
  dplyr::select(ID,Region,geometry)


fmmoregions<-ggplot(data = pregionsFMMOreorder) +
  geom_sf(aes(fill = factor(ID)),color="black",linewidth = 0.5) +
  ggtitle("Actual delimitation of FMMO regions") +
  scale_fill_discrete(name="Region",
    labels = function(breaks) {breaks<- newnames},
    na.value = "gray50") +
  geom_sf(data=pstates, fill=NA,color="black") +
  theme_void()

if (!dir.exists("../../MANUSCRIPT FIGURES/Figure A.1")) {
  dir.create("../../MANUSCRIPT FIGURES/Figure A.1")
}

ggsave(filename = "../../MANUSCRIPT FIGURES/Figure A.1/fmmoregions.png",plot = fmmoregions)


############################################
###APPROXIMATING REGIONS USING STATES
############################################

FO1STATESAPP<-c("09","10","23","24","25","33","34","36","42","44","50","11")  
FO5STATESAPP<-c("21","37","45","47","51")
FO7STATESAPP<-c("01","05","13","22","28")
F030STATESAPP<-c("27","38","55")
FO32STATESAPP<-c("08","17","19","20","29","31","40","46")
FO33STATESAPP<-c("18","26","39","54")
FO124STATESAPP<-c("41","53")
FO126STATESAPP<-c("35","48")
  
pregionsapprox<-pstates %>%
  mutate(Region=case_when(STATEFP %in% FO1STATESAPP ~ "Northeast" ,
                          STATEFP %in% FO5STATESAPP ~ "Appalachian" ,
                          STATEFP == "12" ~ "Florida" ,
                          STATEFP %in% FO7STATESAPP ~ "Southeast" ,
                          STATEFP %in% F030STATESAPP ~ "Upper Midwest" ,
                          STATEFP %in% FO32STATESAPP ~ "Central" ,
                          STATEFP %in% FO33STATESAPP ~ "Mideast" ,
                          STATEFP == "06" ~ "California" ,
                          STATEFP %in% FO124STATESAPP ~ "Pacific Northwest" ,
                          STATEFP %in% FO126STATESAPP ~ "Southwest" ,
                          STATEFP == "04" ~ "Arizona" , 
                          T ~ NA
                          )) %>%
  group_by(Region) %>%
  dplyr::summarize()

pregionsapproxreorder<-pregionsapprox %>%
  dplyr::arrange(factor(Region, levels=targetnames)) %>%
  dplyr::mutate(ID = case_when(is.na(Region) ~ NA, T ~ row_number())) %>%
  dplyr::select(ID,Region,geometry)


fmmoapproximations<-ggplot(data = pregionsapproxreorder) +
  geom_sf(aes(fill = factor(ID)),color="black",linewidth = 0.5) +
  ggtitle("Approximation of FMMO regions") +
  scale_fill_discrete(
    name="Region",
    labels = function(breaks) {breaks <- newnames},
    na.value = "gray50") +
  geom_sf(data=pstates, fill=NA,color="black") +
  theme_void()

ggsave(filename = "../../MANUSCRIPT FIGURES/Figure A.1/fmmoapproximations.png",plot = fmmoapproximations)


