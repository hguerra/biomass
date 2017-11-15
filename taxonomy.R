require("RPostgreSQL")
require("BIOMASS")

createConnection <- function(host = "150.163.58.218", user = "eba", password = "ebaeba18", dbname = "simple_plotdata") {
  dbConnect(PostgreSQL(), host = host, user = user, password = password, dbname = dbname)
}

query <- function(sql, connection) {
  if (missing(connection)) {
    connection = createConnection()
  }
  
  rs <- dbSendQuery(connection, sql)
  fetch(rs, n = -1)
}

getPossiblesCommonNames <- function(con, commonName) {
  sqlIntro <- "SELECT * FROM fselect_scientific_name('"
  sqlEnd <- "');"
  
  sql <- paste(sqlIntro, commonName, sqlEnd, sep = "")
  query(sql, con)
}

getWoodDensityFromDB <- function(con, family_name, genus_name, species_name) {
  sqlIntro <- "select * from fget_wood_density('"
  sqlMiddle <- "'"
  sqlComma <- ","
  sqlEnd <- "');"
  
  sql <- paste(sqlIntro, family_name, sqlMiddle, sqlComma, sqlMiddle, genus_name, sqlMiddle, sqlComma, sqlMiddle, species_name, sqlEnd, sep="")
  query(sql, con)
}

getTableFromDB <- function(con, table) {
  sqlIntro <- "select * from"
  sqlEnd <- ";"
  
  sql <- paste(sqlIntro, table, sqlEnd)
  query(sql, con)
}

normalize <- function(s) {
  emptyStr <- ""
  blankStr <- " "
  prepositionsPt = c("da", "das", "de", "do", "dos")
  
  s <- iconv(s, to='ASCII//TRANSLIT')
  s <- tolower(s)
  s <- gsub("^\\s+|\\s+$", emptyStr, s)
  s <- gsub("'", emptyStr, s)
  s <- gsub('"', emptyStr, s)
  s <- gsub("-", blankStr, s)
  s <- gsub("  ", blankStr, s)
  
  splitted <- strsplit(s, blankStr)[[1]]
  result <- c()
  for(sp in splitted) {
    if(!sp %in% prepositionsPt) {
      result <- c(result, sp)
    }
  }
  
  do.call(paste, c(as.list(result), sep=blankStr))
}

getTaxonomyFromDB <- function(dataset, col_family="Familia", col_genus="Genero", col_species="Especie", col_common_name="Nome_comum") {
  print("Checking columns...")
  if (!(col_species %in% colnames(dataset)) && !(col_genus %in% colnames(dataset)) && !(col_family %in% colnames(dataset)) && !(col_common_name %in% colnames(dataset))) {
    stop("Error to find columns of: 'species', 'genus', 'family' or 'common_name'.")
  }

  print("Loading taxonomy from DB...")
  connection = createConnection()
  taxonomy <- getTableFromDB(connection, "taxonomy")
  species <- getTableFromDB(connection, "species")
  genus <- getTableFromDB(connection, "genus")
  family <- getTableFromDB(connection, "family")
  
  validTaxonomyRegions = c("south america (tropical)", "south america (extratropical)", "central america (tropical)")
  data("sd_10")
  
  dataset$species <- c()
  dataset$genus <- c()
  dataset$family <- c()
  dataset$species_density <- c()
  dataset$genus_density <- c()
  dataset$family_density <- c()
  
  for(i in 1:nrow(dataset)){
    row <- dataset[i,]

    species_name <- NA;
    genus_name <- NA;
    family_name <- NA;
    common_name <- NA;
    species_id <- NA;
    genus_id <- NA;
    family_id <- NA;
    species_mean <- NA;
    genus_mean <- NA;
    family_mean <- NA;
    empty = "";
    
    if (col_species %in% colnames(row)) {
      species_name <- row[[col_species]];
    }
    
    if (col_genus %in% colnames(row)) {
      genus_name <- row[[col_genus]];
    }
    
    if (col_family %in% colnames(row)) {
      family_name <- row[[col_family]];
    }
  
    if (col_common_name %in% colnames(row)) {
      common_name <- row[[col_common_name]]
    }
    
    if (!is.na(species_name) && species_name != empty) {
      species_name <- normalize(species_name)
      search_db <- species[species$name == species_name,]
      if (nrow(search_db) > 0) {
        species_id = search_db$id
      }
    }
    
    if (!is.na(genus_name) && genus_name != empty) {
      genus_name <- normalize(genus_name)
      search_db <- genus[genus$name == genus_name,]
      if (nrow(search_db) > 0) {
        genus_id = search_db$id
      }
    }
    
    if (!is.na(family_name) && family_name != empty) {
      family_name <- normalize(family_name)
      search_db <- family[family$name == family_name,]
      if (nrow(search_db) > 0) {
        family_id = search_db$id
      }
    }
    
    if (!is.finite(species_id) && !is.finite(genus_id) && !is.finite(family_id)) {
      if (!is.na(common_name)) {
        common_name <- normalize(common_name)
      }
      
      poss_names <- getPossiblesCommonNames(connection, common_name)
      if(is.finite(poss_names$species_id)) {
        species_id <- poss_names$species_id
        search_db <- species[species$id == species_id,]
        if (nrow(search_db) > 0) {
          species_name <- search_db$name
        } 
      }
      
      if(is.finite(poss_names$genus_id)) {
        genus_id <- poss_names$genus_id
        search_db <- genus[genus$id == genus_id,]
        if (nrow(search_db) > 0) {
          genus_name <- search_db$name
        } 
      }
      
      if(is.finite(poss_names$family_id)) {
        family_id <- poss_names$family_id
        search_db <- family[family$id == family_id,]
        if (nrow(search_db) > 0) {
          family_name <- search_db$name
        } 
      }
    }
   
    if(is.finite(species_id)) {
      species_mean <- mean(taxonomy$density[(taxonomy$species_id == species_id) & (taxonomy$region %in% validTaxonomyRegions)])
    }
    
    if(is.finite(genus_id)) {
      genus_mean <- mean(taxonomy$density[(taxonomy$genus_id == genus_id) & (taxonomy$region %in% validTaxonomyRegions)])
    }
    
    if(is.finite(family_id)) {
      family_mean <- mean(taxonomy$density[(taxonomy$family_id == family_id) & (taxonomy$region %in% validTaxonomyRegions)])
    }

    dataset$species[i] <- species_name
    dataset$genus[i] <- genus_name
    dataset$family[i] <- family_name
    
    if (is.finite(species_mean)) {
      dataset$density[i] <- species_mean
      dataset$densitySD[i] <- sd_10$sd[sd_10$taxo == "species"]
    } else if(is.finite(genus_mean)) {
      dataset$density[i] <- genus_mean
      dataset$densitySD[i] <- sd_10$sd[sd_10$taxo == "genus"]
    } else if(is.finite(family_mean)) {
      dataset$density[i] <- family_mean
      dataset$densitySD[i] <- sd_10$sd[sd_10$taxo == "family"]
    } else {
      dataset$density[i] <- NA
      dataset$densitySD[i] <- NA
    }
    
    dataset$species_density[i] <- species_mean
    dataset$genus_density[i] <- genus_mean
    dataset$family_density[i] <- family_mean
  }
  
  for(i in 1:nrow(dataset)){
    row <- dataset[i,]
    if (!is.finite(row$density)) {
      dataset$density[i] <- mean(dataset[dataset$Parcela == row$Parcela,]$density, na.rm = TRUE)
    }
  }
  
  dataset
}