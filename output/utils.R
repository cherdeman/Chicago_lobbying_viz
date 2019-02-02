# Define cleaning function for client names

client_name_cleaning <- function(df) {
  if ("CLIENT_NAME" %in% colnames(df)) {
    
    df %<>% 
      mutate(CLIENT_NAME = str_trim(toupper(CLIENT_NAME), side = c("both")),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, c("^S\\. ", "^S[^A-Z0-9]"), "SOUTH "),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, c(" S\\. ", " S[^A-Z0-9]"), " SOUTH "),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, c("^N\\. ", "^N[^A-Z0-9]"), "NORTH "),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, c(" N\\. ", " N[^A-Z0-9]"), " NORTH "),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, c("^E\\. ", "^E[^A-Z0-9]"), "EAST "),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, c(" E\\. ", " E[^A-Z0-9]"), " EAST "),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, c("^W\\. ", "^W[^A-Z0-9]"), "WEST "), 
             CLIENT_NAME = str_replace_all(CLIENT_NAME, c(" W\\. ", " W[^A-Z0-9]"), " WEST "),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, "ST\\.", "STREET"),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, "L.L.C.", "LLC"),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, ", LLC", " LLC"),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, "L.L.P.", "LLP"),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, ", LLP", " LLP"),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, "N.A.", "NA"),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, "INC\\.", "INC"),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, ", INC", " INC"),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, c("CORP\\. ", "CORP "), "CORPORATION "),
             # Specific hacks to group bigger players
             CLIENT_NAME = str_replace_all(CLIENT_NAME, "^KPMG CORPORATE FINANCE LLC$", 
                                           "KPMG LLP"),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, "^PRICEWATERHOUSECOOPERS$", 
                                           "PRICEWATERHOUSECOOPERS LLP"),
             CLIENT_NAME = str_replace_all(CLIENT_NAME, "^UNITED PARCEL SERVICE INC$", 
                                           "UNITED PARCEL SERVICE"),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "JCDECAUX"), "JCDECAUX GROUP", CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "^CITIBANK"), "CITIBANK, NA", CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "^HERTZ"), "HERTZ GLOBAL", CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "^UNITED AIRLINES"), "UNITED AIRLINES", CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "^AIRBNB INC$"), "AIRBNB", CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "^SP PLUS CORP$"), "SP PLUS CORPORATION", CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "^CAR2GO NORTH A. LLC"), "CAR2GO NA LLC", CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "^LYFT INC$"), "LYFT", CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "^CVS HEALTH$"), "CVS CAREMARK CORP", CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "^NRG ENERGY SERVICES LLC$"), "NRG ENERGY INC",
                                   CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "^PHILIPS LIGHTING NORTH AMERICA CORP$"), "PHILIPS LIGHTING",
                                   CLIENT_NAME),
             
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "CLEAR CHANNEL"), "CLEAR CHANNEL", CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "^THE BARRACK OBAMA FOUNAION"), 
                                   "THE BARACK OBAMA FOUNDATION", CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "^AMAZON.COM$"), 
                                   "AMAZON", CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, '"I AM" TEMPLE OF CHICAGO INC'), 
                                   '"I AM" TEMPLE OF CHICAGO', CLIENT_NAME),
             CLIENT_NAME = if_else(str_detect(CLIENT_NAME, "1100 E 47TH STREET LLC"), 
                                   "1100 EAST 47TH STREET LLC", CLIENT_NAME)
      )
    
    return(df)
    
  } else {
    
    stop("Requires a column named CLIENT_NAME")
    
  }
  
}

# Define vector of vectors representing missing clients and related industries
missing_clients <- c(c("ARLINGTON PARK","RACING & WAGERING"),
                      c("CITIBANK, NORTH A.", "FINANCIAL / BANKING"),
                      c("HERTZ GLOBAL", "TRANSPORTATION"),
                      c("UNITED AIRLINES INC", "TRANSPORTATION"),
                      c("NORTH HIGHLAND", "OTHER"),
                      c("SONDER", "TOURISM & TRAVEL"),
                      c("ASSURED GUARANTY CORP.", "FINANCIAL / BANKING"),
                      c("TUK TUK CHICAGO", "TRANSPORTATION"),
                      c("BLACKSTONE ADMINISTRATIVE SERVICES L. P.", "FINANCIAL / BANKING"),
                      c("COMPUTER AID INC", "INFORMATION / TECHNOLOGY PRODUCTS OR SERVICES"),
                      c("LOGAN JONES LP", "OTHER"),
                      c("ASSOCIATION FOR ACCESSIBLE MEDICINES", "HEALTH/ MEDICAL / HOSPITAL"),
                      c("DELTA AIR LINES", "TRANSPORTATION"),
                      c("MEGABUS USA LLC", "TRANSPORTATION"),
                      c("WEC ENERGY GROUP INC", "PUBLIC UTILITIES"),
                      c("CONDUENT INC AND ITS AFFILIATES", "INFORMATION / TECHNOLOGY PRODUCTS OR SERVICES"),
                      c("LYFT INC", "TRANSPORTATION"),
                      c("CVS HEALTH", "HEALTH/ MEDICAL / HOSPITAL"),
                      c("NRG ENERGY SERVICES LLC", "PUBLIC UTILITIES"),
                      c("THE BARRACK OBAMA FOUNAION", "RELIGIOUS / NON-PROFIT ORGANIZATIONS"),
                      c("VENDOR ASSISTANCE PROGRAM LLC", "OTHER"),
                      c("DCI GROUP AZ LLC", "PUBLIC RELATIONS & ADVERTISING"),
                      c("VMWARE", "INFORMATION / TECHNOLOGY PRODUCTS OR SERVICES"),
                      c("SPOTHERO INC", "TRANSPORTATION"),
                      c("SENTINEL TECHNOLOGIES", "INFORMATION / TECHNOLOGY PRODUCTS OR SERVICES"),
                      c("ALLIEDBARTON SECURITY SERVICES LLC", "OTHER")
                     )

