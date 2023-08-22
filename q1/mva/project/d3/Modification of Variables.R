

## Change host_since variable
host_since_changed <- format(host_since, format = "%Y")

## Change property_type variable

for(i in 1:length(df$property_type)) {         
  if(df$property_type[i]!= "Entire rental unit" & df$property_type[i]!= "Private room in rental unit"){
    property_type[i] = 'Others'
  }
}


## Change neighbourhood_group_cleansed variable

for(i in 1:length(neighbourhood_group_cleansed)) {         
  if(substr(neighbourhood_group_cleansed[i], 1, 2) == "Gr"){
    neighbourhood_group_cleansed[i] = "Gracia"
  }
  else if (neighbourhood_group_cleansed[i] == "Horta-GuinardÃ³"){
    neighbourhood_group_cleansed[i] = "Horta-Guinardo"
  }
  else if (substr(neighbourhood_group_cleansed[i], 1, 6) == "Sant M"){
    neighbourhood_group_cleansed[i] = "Sant Marti"
  }
  else if (neighbourhood_group_cleansed[i] == "Sants-MontjuÃ¯c"){
    neighbourhood_group_cleansed[i] = "Sants-Montjuic"
  }
  else if (substr(neighbourhood_group_cleansed[i], 1, 5) == "Sarri"){
    neighbourhood_group_cleansed[i] = "Sarria-Sant Gervasi"
  }
}



## Change license variable
## There are listings with 3 licenses bc they are reting three apartments
## What does HB mean?

for(i in 1:length(license)){
  if(substr(license[i], 1, 4) == 'HUIB'){
    license[i] = 'HUIB'
  }
}




##Change bathrooms_text

num_baths <- bathrooms_text

for(i in 1:length(bathrooms_text)){
    if (bathrooms_text[i] != 'Half-bath' | bathrooms_text[i]!= 'Private half-bath' | bathrooms_text[i]!='Shared half_bath'| !is.null(bathrooms_text[i])){
      num_baths[i] = strsplit(df$bathrooms_text[i], split = ' ')[[1]][1]
      temp <- strsplit(df$bathrooms_text[i], split = ' ')[[1]]
      if("shared" %in% temp) {
        bathrooms_text[i] = "shared"
      } else if("private" %in% temp) {
        bathrooms_text[i] = "private"
      } else {
        bathrooms_text[i] = "uknown"
      }
    }
}


for(i in 1:length(bathrooms_text)){
  if(grepl('shared bath', bathrooms_text[i], ignore.case = TRUE)){
    bathrooms_text[i] = "Shared bathroom"
  }
}

summary(factor(bathrooms_text))




