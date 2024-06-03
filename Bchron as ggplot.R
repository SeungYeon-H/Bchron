convertBchronToDataFrame <- function(bchronOutput) {
  results_list <- lapply(bchronOutput, function(x) {
    # Check if densities and ageGrid are non-empty and have the same length
    if (!is.null(x$densities) && !is.null(x$ageGrid) && length(x$densities) == length(x$ageGrid)) {
      # Both vectors have data and the same length
      return(data.frame(age = x$ageGrid, density = x$densities, id = ifelse(is.null(x$id), NA, x$id)))
    } else {
      # Log the issue or handle cases where densities are empty or lengths mismatch
      message("Mismatch or empty data found for id: ", ifelse(is.null(x$id), "Unknown", x$id))
      # Return NULL or an empty dataframe with the same structure but no rows
      return(NULL)  # or return(data.frame(age = numeric(0), density = numeric(0), id = ifelse(is.null(x$id), NA, x$id)))
    }
  })
  
  # Remove NULL entries from the list before combining
  results_list <- Filter(Negate(is.null), results_list)
  
  # Combine all results into one dataframe if not empty
  if (length(results_list) > 0) {
    results_df <- do.call(rbind, results_list)
    return(results_df)
  } else {
    return(data.frame(age = numeric(), density = numeric(), id = character()))
  }
}

# Assuming age2 is the output from BchronCalibrate that needs processing
age2_df <- convertBchronToDataFrame(age2)

age2_df <- age2_df %>%  mutate(RowID = row_number()) 

age2_df <- tibble::rownames_to_column(age2_df, var = "RowName")

age2_df <- age2_df %>%group_by(RowName) %>% mutate(density_sequence = row_number()) %>%  ungroup()

age2_df <- age2_df %>%mutate(DensityID = paste(RowName, density_sequence, sep = "."))



#age2_df <- age2_df %>% mutate(NormalizedRowName = str_trim(NormalizedRowName))
age2_df <- age2_df %>%  mutate(across(where(is.character), ~str_replace_all(., "호 수혈", "pit")))

age2_df <- age2_df %>%mutate( NormalizedRowName = str_replace_all(RowName, "\\s*\\.\\d+", ""))

age2_df <- age2_df %>%
  mutate(
    # Extract the number for sorting
    SortNumber = as.numeric(str_extract(NormalizedRowName, "\\d+")),
    # Convert NormalizedRowName to a factor and order it based on SortNumber
    NormalizedRowName = fct_reorder(NormalizedRowName, SortNumber)
  )

age2_df$NormalizedRowName[is.na(age2_df$NormalizedRowName)]<-"District 2- Road Side Gutter"

age2_df$NormalizedRowName <- gsub("2지구 고구려 개축 도로 측구", "District 2- Road Side Gutter", age2_df$NormalizedRowName) 

age2_df <- age2_df %>%
  mutate( SortNumber = as.numeric(str_extract(NormalizedRowName, "\\d+")),
          NormalizedRowName = fct_reorder(NormalizedRowName, SortNumber) )


# sorting 
age2_df <- age2_df %>%  mutate(NormalizedRowName2 = str_extract(RowName, "^[0-9]+pit"),    NormalizedRowName2 = ifelse(is.na(NormalizedRowName2), str_extract(RowName, "^[0-9]+pit"), NormalizedRowName2)  )

age2_df$NormalizedRowName2[is.na(age2_df$NormalizedRowName2)]<-"District 2- Road Side Gutter"

age2_df <- age2_df %>%
  mutate( SortNumber = as.numeric(str_extract(NormalizedRowName2, "\\d+")),
    NormalizedRowName2 = fct_reorder(NormalizedRowName2, SortNumber)
  )


ggplot(age2_df, aes(x = age, y = density,fill=NormalizedRowName2)) + 
  geom_area(color= "gray40", alpha = 0.7) +
  scale_x_continuous(name = "Calibrated Age (years BP)",breaks=seq(250,600,50),sec.axis = dup_axis()) +
  scale_fill_viridis(discrete=TRUE,option="inferno")+
  coord_cartesian(xlim=c(280,600))+
  facet_grid(rows = vars(NormalizedRowName),switch = "y", scales = "free") + 
  theme_minimal() +
  theme(axis.title.x=element_text(face="bold",size=13),
        axis.text.x=element_text(size=11,face="bold"),
        strip.text.y.left = element_text(angle = 0),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank())+
  geom_vline(xintercept=500, color='black', size = 0.6)
