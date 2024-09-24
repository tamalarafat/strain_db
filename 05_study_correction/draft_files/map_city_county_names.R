for (i in c(1:nrow(temp_source_tab))){
  if (temp_source_tab$names[i] == "BOTSWANA"){
    temp_source_tab$names[i] = "Botswana"
  } else if (temp_source_tab$names[i] == "ITALIEN"){
    temp_source_tab$names[i] = "Italy"
  } else if (temp_source_tab$names[i] == "DRESDEN"){
    temp_source_tab$names[i] = "Dresden"
  } else if (temp_source_tab$names[i] == "HCU (UNI GIEßEN)"){
    temp_source_tab$names[i] = "HCU (Uni Gießen)"
  } else if (temp_source_tab$names[i] == "BORSTEL"){
    temp_source_tab$names[i] = "Borstel"
  } else if (temp_source_tab$names[i] == "BERLIN"){
    temp_source_tab$names[i] = "Berlin"
  } else if (temp_source_tab$names[i] == "BASEL"){
    temp_source_tab$names[i] = "Basel"
  } else if (temp_source_tab$names[i] == "ESSEN"){
    temp_source_tab$names[i] = "Essen"
  } else if (temp_source_tab$names[i] %in% c("BUKINA FASO", "BUKINA-FASO")){
    temp_source_tab$names[i] = "Burkina Faso"
  } else if (temp_source_tab$names[i] == "BELARUS"){
    temp_source_tab$names[i] = "Belarus"
  } else if (temp_source_tab$names[i] %in% c("KONGO", "CONGO")){
    temp_source_tab$names[i] = "Congo"
  } else if (temp_source_tab$names[i] %in% c("GEORGIEN", "GEORGIA")){
    temp_source_tab$names[i] = "Georgia"
  } else if (temp_source_tab$names[i] %in% c("KASACHSTAN", "KAZAKHSTAN")){
    temp_source_tab$names[i] = "Kazakhstan"
  } else if (temp_source_tab$names[i] %in% c("MOLDAWIEN", "MOLDOVA")){
    temp_source_tab$names[i] = "Moldova"
  } else if (temp_source_tab$names[i] %in% c("UKRAINE", "UKRAINIAN")){
    temp_source_tab$names[i] = "Ukraine"
  } else if (temp_source_tab$names[i] == "NIGERIA"){
    temp_source_tab$names[i] = "Nigeria"
  } else if (temp_source_tab$names[i] %in% c("ROMANIA", "RUMANIA", "RUMÄNIEN")){
    temp_source_tab$names[i] = "Romania"
  } else if (temp_source_tab$names[i] == "CZECHIA"){
    temp_source_tab$names[i] = "Czechia"
  } else if (temp_source_tab$names[i] == "BUCHAREST"){
    temp_source_tab$names[i] = "Bucharest"
  } else if (temp_source_tab$names[i] == "SERBIA"){
    temp_source_tab$names[i] = "Serbia"
  } else if (temp_source_tab$names[i] == "PERU"){
    temp_source_tab$names[i] = "Peru"
  } else if (temp_source_tab$names[i] == "DENVER"){
    temp_source_tab$names[i] = "Denver"
  } else if (temp_source_tab$names[i] == "GAMBIA"){
    temp_source_tab$names[i] = "Gambia"
  } else if (temp_source_tab$names[i] %in% c("MOZAMBIQUE", "MOSAMBIK")){
    temp_source_tab$names[i] = "Mozambique"
  } else if (temp_source_tab$names[i] %in% c("SOUTHAFRICA", "SOUTH AFRICA", "S. AFRICA", "SOUTH-AFRICA", "SÜD AFRIKA")){
    temp_source_tab$names[i] = "South Africa"
  } else if (temp_source_tab$names[i] %in% c("TUNESIEN", "TUNISIA")){
    temp_source_tab$names[i] = "Tunisia"
  } else if (temp_source_tab$names[i] %in% c("TURKMENISTAN", "TURMENISTAN")){
    temp_source_tab$names[i] = "Turkmenistan"
  } else if (temp_source_tab$names[i] == "BAKU"){
    temp_source_tab$names[i] = "Baku"
  } else if (temp_source_tab$names[i] == "BW"){
    temp_source_tab$names[i] = "Bundeswehr"
  } else if (temp_source_tab$names[i] == "BEIJING"){
    temp_source_tab$names[i] = "Beijing"
  } else if (temp_source_tab$names[i] %in% c("TANZANIA", "TANSANIA")){
    temp_source_tab$names[i] = "Tanzania"
  } else if (temp_source_tab$names[i] %in% c("DEUSTCHLAND", "DEUTSCHLAND")){
    temp_source_tab$names[i] = "Germany"
  } else if (temp_source_tab$names[i] == "ESTONIA"){
    temp_source_tab$names[i] = "Estonia"
  } else if (temp_source_tab$names[i] == "FRANKFURT"){
    temp_source_tab$names[i] = "Frankfurt"
  } else if (temp_source_tab$names[i] == "HAMBURG"){
    temp_source_tab$names[i] = "Hamburg"
  } else if (temp_source_tab$names[i] == "HOMBURG"){
    temp_source_tab$names[i] = "Homburg"
  } else if (temp_source_tab$names[i] == "IRAK"){
    temp_source_tab$names[i] = "Iraq"
  } else if (temp_source_tab$names[i] == "IRAN"){
    temp_source_tab$names[i] = "Iran"
  } else if (temp_source_tab$names[i] == "ZÜRICH"){
    temp_source_tab$names[i] = "Zurich"
  } else if (temp_source_tab$names[i] == "MAILAND"){
    temp_source_tab$names[i] = "Milan"
  } else if (temp_source_tab$names[i] %in% c("ESWATINI", "SWAZILAND")){
    temp_source_tab$names[i] = "Eswatini"
  } else if (temp_source_tab$names[i] == "THAILAND"){
    temp_source_tab$names[i] = "Thailand"
  } else if (temp_source_tab$names[i] %in% c("ANTWERPEN", "ANTWEERPEN")){
    temp_source_tab$names[i] = "Antwerp"
  } else if (temp_source_tab$names[i] == "Sambia"){
    temp_source_tab$names[i] = "Zambia"
  } else if (temp_source_tab$names[i] == "ARGENTINA"){
    temp_source_tab$names[i] = "Argentina"
  } else if (temp_source_tab$names[i] == "ARMENIEN"){
    temp_source_tab$names[i] = "Armenia"
  } else if (temp_source_tab$names[i] %in% c("ASERBAIDSCHAN", "AZERBAIJAN")){
    temp_source_tab$names[i] = "Azerbaijan"
  } else if (temp_source_tab$names[i] %in% c("BADEN-WÜRTTEMBERG", "BADEN-WÜTTEMBERG", "BADEN-WÜRTEMBERG")){
    temp_source_tab$names[i] = "Baden-Württemberg"
  } else if (temp_source_tab$names[i] == "BELGRADE"){
    temp_source_tab$names[i] = "Belgrade"
  } else if (temp_source_tab$names[i] == "KIEL"){
    temp_source_tab$names[i] = "Kiel"
  } else if (temp_source_tab$names[i] %in% c("CROATIA", "KROATIEN")){
    temp_source_tab$names[i] = "Croatia"
  } else if (temp_source_tab$names[i] == "EASTGREENLAND"){
    temp_source_tab$names[i] = "East Greenland"
  } else if (temp_source_tab$names[i] == "GREENLAND"){
    temp_source_tab$names[i] = "Greenland"
  } else if (temp_source_tab$names[i] == "SIERRA LEONE"){
    temp_source_tab$names[i] = "Sierra Leone"
  } else if (temp_source_tab$names[i] == "VIETNAM"){
    temp_source_tab$names[i] = "Vietnam"
  } else if (temp_source_tab$names[i] == "HANNOVER"){
    temp_source_tab$names[i] = "Hannover"
  } else if (temp_source_tab$names[i] %in% c("KENIA", "KENYA")){
    temp_source_tab$names[i] = "Kenya"
  } else if (temp_source_tab$names[i] == "MAPUTO"){
    temp_source_tab$names[i] = "Maputo"
  } else if (temp_source_tab$names[i] == "NAMIBIA"){
    temp_source_tab$names[i] = "Namibia"
  } else if (temp_source_tab$names[i] == "NUKUS"){
    temp_source_tab$names[i] = "Nukus"
  } else if (temp_source_tab$names[i] == "SENEGAL"){
    temp_source_tab$names[i] = "Senegal"
  } else if (temp_source_tab$names[i] == "SUDAN"){
    temp_source_tab$names[i] = "Sudan"
  } else if (temp_source_tab$names[i] == "MEXICO"){
    temp_source_tab$names[i] = "Mexico"
  } else if (temp_source_tab$names[i] == "ÄGYPTEN"){
    temp_source_tab$names[i] = "Egypt"
  } else if (temp_source_tab$names[i] == "ALMATY"){
    temp_source_tab$names[i] = "Almaty"
  } else if (temp_source_tab$names[i] == "PARIS"){
    temp_source_tab$names[i] = "Paris"
  } else if (temp_source_tab$names[i] == "CAMEROON"){
    temp_source_tab$names[i] = "Cameroon"
  } else if (temp_source_tab$names[i] %in% c("COLUMBIA", "COLOMBIA")){
    temp_source_tab$names[i] = "Colombia"
  } else if (temp_source_tab$names[i] == "LESOTHO"){
    temp_source_tab$names[i] = "Lesotho"
  } else if (temp_source_tab$names[i] == "ETHIOPIA"){
    temp_source_tab$names[i] = "Ethiopia"
  } else if (temp_source_tab$names[i] == "FREIBURG"){
    temp_source_tab$names[i] = "Freiburg"
  } else if (temp_source_tab$names[i] == "GHANA"){
    temp_source_tab$names[i] = "Ghana"
  } else if (temp_source_tab$names[i] == "HEILBRONN"){
    temp_source_tab$names[i] = "Heilbronn"
  } else if (temp_source_tab$names[i] == "KIRIBATI"){
    temp_source_tab$names[i] = "Kiribati"
  } else if (temp_source_tab$names[i] == "KÖLN"){
    temp_source_tab$names[i] = "Köln"
  } else if (temp_source_tab$names[i] == "KYRGYZSTAN"){
    temp_source_tab$names[i] = "Kyrgyzstan"
  } else if (temp_source_tab$names[i] == "LAMBARENE"){
    temp_source_tab$names[i] = "Lambarene"
  } else if (temp_source_tab$names[i] == "BAYERN"){
    temp_source_tab$names[i] = "Bayern"
  } else if (temp_source_tab$names[i] == "LONDON"){
    temp_source_tab$names[i] = "London"
  } else if (temp_source_tab$names[i] == "MALAVI"){
    temp_source_tab$names[i] = "Malawi"
  } else if (temp_source_tab$names[i] == "KINSHASA"){
    temp_source_tab$names[i] = "Kinshasa"
  } else if (temp_source_tab$names[i] == "MOROCCO"){
    temp_source_tab$names[i] = "Morocco"
  } else if (temp_source_tab$names[i] == "MÜNSTER"){
    temp_source_tab$names[i] = "Münster"
  } else if (temp_source_tab$names[i] == "NÜRNBERG"){
    temp_source_tab$names[i] = "Nürnberg"
  } else if (temp_source_tab$names[i] == "OSLO"){
    temp_source_tab$names[i] = "Oslo"
  } else if (temp_source_tab$names[i] == "PAKISTAN"){
    temp_source_tab$names[i] = "Pakistan"
  } else if (temp_source_tab$names[i] == "SAUDI-ARABIEN"){
    temp_source_tab$names[i] = "Saudi Arabia"
  } else if (temp_source_tab$names[i] == "LÜBECK"){
    temp_source_tab$names[i] = "Lübeck"
  } else if (temp_source_tab$names[i] == "STUTTGART"){
    temp_source_tab$names[i] = "Stuttgart"
  } else if (temp_source_tab$names[i] == "TURK"){
    temp_source_tab$names[i] = "Turkey"
  } else if (temp_source_tab$names[i] == "UZBEKISTAN"){
    temp_source_tab$names[i] = "Uzbekistan"
  } else if (temp_source_tab$names[i] == "WIEN"){
    temp_source_tab$names[i] = "Vienna"
  }
}