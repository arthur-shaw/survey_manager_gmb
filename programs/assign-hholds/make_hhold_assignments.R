# =============================================================================
# Setup
# =============================================================================

# -----------------------------------------------------------------------------
# Load necessary libraries
# -----------------------------------------------------------------------------

# packages needed for this program 
packagesNeeded <- c(
    "purrr",    # to facilitate loops
    "haven", 	# to injest Stata and SPSS files
	"readxl",   # to ingest Excel
    "tidyr",    # to reshape data set
    "stringr",  # to manipulate strings
	"dplyr",    # to do basic data wrangling
    "rlang",    # to convert user inputs into different object types
    "glue"      # to compose error messages easily
)

# identify and install those packages that are not already installed
packagesToInstall <- packagesNeeded[!(packagesNeeded %in% installed.packages()[,"Package"])]
if(length(packagesToInstall)) 
	install.packages(packagesToInstall, quiet = TRUE, 
		repos = 'https://cloud.r-project.org/', dep = TRUE)

# load all needed packages
lapply(packagesNeeded, library, character.only = TRUE)

# -----------------------------------------------------------------------------
# Load helper functions
# -----------------------------------------------------------------------------

helper_functions <- c(
    "create_preload_files.R",
    "allocate_assignments.R"
)

purrr::walk(
    .x = helper_functions, 
    .f = ~ source(paste0(assignHHDir, .x)))

# =============================================================================
# Prepare files and folders
# =============================================================================

# folders
assign_data_dir <- paste0(assignHHDir, "data/")
assign_temp_dir <- paste0(assignHHDir, "temp/")
assign_out_dir <- paste0(projDir, "outputs/", "assign-hholds/")

print("data_entered")
print("starting...")
# allocation data entered in Shiny
# check that it exists
if (!exists("data_entered")) {
    stop("Data does not exist")
# check that expected columns exist
} else {
    # determine whether any columns are missing, and if so which
    cols_expected = c("interviewer", "num_to_assign")
    cols_found <- !(cols_expected %in% colnames(data_entered))
    cols_missing <- cols_expected[cols_found]
    if (any(cols_found)) {
        stop(paste(
            paste0("The following columns are expected in `data_entered`: ", 
                glue_collapse(single_quote(cols_expected), 
                sep = ", ", last = ", and ")), 
            paste0("The following columns are missing: ", 
                glue_collapse(single_quote(cols_missing), 
                sep = ", ", last = " and ")), 
            sep = "\n")
        )
    }

    data_entered <- rename(data_entered, interviewer_name = interviewer)

}
print("ending...")

# household sample
hholds_sampled <- haven::read_dta(file = paste0(assign_data_dir, sample_preload_file)) %>%
    mutate(hhid = paste0(
        str_pad(hh1, width = 3, side = "left", pad = "0"),
        str_pad(hh2, width = 2, side = "left", pad = "0"))
    ) %>%
    select(hhid)

# split LFS datafile into hhold and member pieces
data <- haven::read_dta(file = paste0(assign_data_dir, hhold_preload_file)) %>%
    mutate(hhid = paste0(
        str_pad(hh1, width = 3, side = "left", pad = "0"),
        str_pad(hh2, width = 2, side = "left", pad = "0"))
    )
# ... hhold file
hholds <- data %>%
    mutate(
        language = case_when(
            hl11 == 1 ~ 2, # Mandingka/Jahanka -> MANDINKA
            hl11 == 2 ~ 4, # Fula/Tukulur/Loroba -> FULA
            hl11 == 3 ~ 3, # Wolof -> WOLLOF
            hl11 == 4 ~ 5, # Jola/Karoninka -> JOLA
            hl11 == 5 ~ 6, # Sarahule -> SARAHULE
            hl11 == 6 ~ 7, # Serere -> SERERE
            hl11 == 7 ~ 9, # Creole/Aku/Marabout -> CREOLE/ AKU MARABOUT
            hl11 == 8 ~ 8, # Manjako -> MANJAGO
            hl11 == 9 ~ 10, # Bambara -> BAMBARA
            hl11 == 96 ~ 1), # Other -> ENGLISH (FOR NOW)
        language = case_when(
            is.na(hl11) & hl10 == 2 ~ 3, # if Senegalese, Wollof
            is.na(hl11) & hl10 == 3 ~ 1, # if Nigerian, English
            is.na(hl11) & hl10 == 4 ~ 1, # if Sierra Leonean, English
            is.na(hl11) & hl10 == 5 ~ 1, # if Sierra Leonean, English
            is.na(hl11) & hl10 == 6 ~ 1, # if Ghanaian, English
            is.na(hl11) & hl10 == 7 ~ 4, # if Guinean, Fula (largest language)
            is.na(hl11) & hl10 == 8 ~ 1, # if Bissau Guinean, English (for lack of dominant lang)
            is.na(hl11) & hl10 == 9 ~ 1, # if Mauritanian, English
            is.na(hl11) & hl10 == 10 ~ 1, # if other West African, English
            is.na(hl11) & hl10 == 11 ~ 1, # if other African, English
            is.na(hl11) & hl10 == 12 ~ 1, # if non-African, English
            is.na(hl11) & is.na(hl10) ~ 1, # if no nationality or ethnicity, English
            !is.na(hl11) ~ language
        ),
        language = if_else(language == 10, 1, language), # recode Bambara to English, for lack of interviewer who speaks language
        dteasg = format(Sys.Date(), "%d-%b"),
        hh5_1_2 = hh5_1,
        has_hh_contacts = (!is.na(hh5_2_1) | !is.na(hh5_2_2)) # for allocate function, even though all in GMB have hhold contacts
    ) %>%
    filter(hl3 == 1) %>%
    mutate_at(
        .vars = vars(hh5_2_1, hh5_2_2),
        .funs = as.character
    ) %>%
    select(
        hhid, hh7, hh8, hh1, hh2,   # hhold identifiers: area, LGA, cluster, household number
        hh5_1, hh5_2_1, hh5_2_2,    # contacts: head name and phone contacts
        hh5_1_2,                    # duplicate head name
        dteasg, language, has_hh_contacts
        # strata - TODO: create        
    )

# ... member file
members <- data %>%
    select(
        hhid, hh7, hh8, hh1, hh2, # hhold identifiers: area, LGA, cluster, household number
        hl1, uid,  # ID
        hl2,    # name
        hl4,    # gender
        hl6,    # age
        hl3     # relationship
    )

# compute household size
hhold_size <- members %>%
    group_by(hhid) %>%
    summarize(hhsize = n()) %>%
    ungroup() %>%
    select(hhid, hhsize)

# create final hhold file by adding size and filtering to sampled hholds
hholds <- hholds %>%
    left_join(hhold_size, by = "hhid") %>%
    semi_join(hholds_sampled, by = "hhid")

# households already assigned
# if no assignments made, create an empty file
# else if assignments made, load file containing them
if (file.exists(paste0(assign_temp_dir, "old_assignments.dta"))) {
    old_assignments <- haven::read_dta(file = paste0(assign_temp_dir, "old_assignments.dta"))
} else {
    old_assignments <- tibble(
        !!sym(hhold_preload_id) := NA_character_,
        `_responsible` = NA_character_,
        `_record_audio` = NA_real_,
        .rows = 0
    )

    haven::write_dta(
        data = old_assignments,
        path = paste0(assign_temp_dir, "old_assignments.dta"))
}

# interviewer attributes
interviewer_attributes <- readxl::read_excel(
        path = paste0(assign_data_dir, "interviewer_languages.xlsx"), 
        n_max = 16
    ) %>%
    pivot_longer(
        cols = starts_with("language"), 
        names_to = "lang", 
        values_to = "language",
        values_drop_na = TRUE) %>%
    mutate(
        language = str_extract(lang, "(?<=language_)[0-9]+(?= )"),
        language = as.numeric(language), 
        interviewer = login
        ) %>%
    select(interviewer, language)    

# =============================================================================
# Make random assignments
# =============================================================================

new_assignments <- tibble(
        !!sym(hhold_preload_id) := NA_character_,
        `_responsible` = NA_character_,
        `_record_audio` = NA_real_,
        .rows = 0
    )

for (i in seq(from = 1, to = nrow(data_entered))) {

    interviewer_name <- data_entered$interviewer_name[[i]]
    num_to_assign <- data_entered$num_to_assign[[i]]

    print(paste0("INTERVIEWER: ", interviewer_name))
    print(paste0("TO ASSIGN: ", num_to_assign))

    hholds_assigned_to_int <- allocate(
        df_sample = hholds,
        df_attrib = interviewer_attributes,
        df_assigned = old_assignments,
        interviewer_name = interviewer_name,
        num_to_assign = num_to_assign,
        audio = TRUE,
        audio_interval = 5       
    )

    print(paste0("ROWS ASSIGNED: ", nrow(hholds_assigned_to_int)))

    new_assignments <- rbind(new_assignments, hholds_assigned_to_int)
    old_assignments <- rbind(old_assignments, hholds_assigned_to_int)

}

write_dta(data = new_assignments, path = paste0(assign_temp_dir, "new_assignments.dta"), version = 14)

# =============================================================================
# Create preload files based on these assignments
# =============================================================================

# define preload output files based on export file names
out_hhold_preload_file <- str_replace(hhold_file, "\\.dta$", "\\.tab")
out_number_preload_file <- str_replace(number_file, "\\.dta$", "\\.tab")
out_member_preload_file <- str_replace(member_file, "\\.dta$", "\\.tab")

# TODO: redo for Gambia

# define how phone numbers should be changed to number types
mutates <- quos(
    source_var %in% c("hh5_1", "hh5_2_1") ~ 1,
    source_var %in% c("hh5_2_1", "hh5_2_2") ~ 1 
)

# identify number groups for purposes of merging numbers and names in reshape
num_group_mutates <- quos(
    source_var %in% c("hh5_1", "hh5_2_1") ~ 1,
    source_var %in% c("hh5_2_1", "hh5_2_2") ~ 1  
)

# create preload assignments
create_preload(
    df_hhold = hholds, 
    hhold_id = hhid, 
    hhold_mutate = quos(head_name = hh5_1),
    hhold_rename = c(
        "hhid" = "hhid", 
        "lga" = "hh8",
        "sector" = "hh7",
        "language" = "language",
        "dteasg" = "dteasg"),
    hh_phone_vars = vars(hh5_2_1, hh5_2_2),
    hh_name_vars = vars(hh5_1, hh5_1_2),
    number_mutate = mutates,
    num_group_mutate = num_group_mutates,
    number_type_var = number_member,
    number_var = number_list,
    name_mutate = mutates,
    number_name_var = number_owner_txt,
    df_member = members, 
    mem_indiv_id = uid,
    mem_mutate = quos(
        preload_pid = uid, 
        s2q1_open = hl2, 
        preload_sex = hl4, 
        preload_age = hl6, 
        preload_relation = hl3,
        s2q7 = hl3,    # relationship to head
        s2q6 = hl6,  # age
        s2q5 = hl4,    # gender
        s2q1 = hl2),  # name
    mem_name = s2q1,
    df_assignments = new_assignments, 
    assign_rename = c(
        "hhid" = "hhid", 
        "_responsible" = "_responsible"),
    out_dir = assign_out_dir,
    out_hhold = out_hhold_preload_file,     
    out_member = out_member_preload_file,      
    out_number = out_number_preload_file      
)

# add new assignments to old assignments
write_dta(data = old_assignments, path = paste0(assign_temp_dir, "old_assignments.dta"), version = 14)
