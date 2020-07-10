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

# split file into hhold and member pieces
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
        dteasg = format(Sys.Date(), "%b-%m"),
        hh5_1_2 = hh5_1
    ) %>%
    filter(hl3 == 1) %>%
    select(
        hhid, hh7, hh8, hh1, hh2,   # hhold identifiers: area, LGA, cluster, household number
        hh5_1, hh5_2_1, hh5_2_2,    # contacts: head name and phone contacts
        hh5_1_2,                    # duplicate head name
        dteasg, language
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
hhold_size <- members
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
        !!sym(hhold_preload_id) := NA_real_,
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
        path = paste0(assign_data_dir, "agents_langue.xlsx"), 
        n_max = 18
    ) %>%
    pivot_longer(
        cols = starts_with("s00q28"), 
        names_to = "langue", 
        values_to = "s00q28",
        values_drop_na = TRUE) %>%
    mutate(
        s00q28 = str_extract(langue, "(?<=s00q28_)[0-9]+(?= )"),
        s00q28 = as.numeric(s00q28), 
        interviewer = login
        ) %>%
    select(interviewer, s00q28)    

# =============================================================================
# Make random assignments
# =============================================================================

new_assignments <- tibble(
        !!sym(hhold_preload_id) := NA_real_,
        `_responsible` = NA_character_,
        `_record_audio` = NA_real_,
        .rows = 0
    )

for (i in seq(from = 1, to = nrow(data_entered))) {

    interviewer_name <- data_entered$interviewer_name[[i]]
    num_to_assign <- data_entered$num_to_assign[[i]]

    hholds_assigned_to_int <- allocate(
        df_sample = hholds,
        df_attrib = interviewer_attributes,
        df_assigned = old_assignments,
        interviewer_name = interviewer_name,
        num_to_assign = num_to_assign,
        audio = TRUE,
        audio_interval = 5       
    )
    
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
    number_type_var = numero_membre,
    number_var = number_list,
    name_mutate = mutates,
    number_name_var = number_owner_txt,
    df_member = members, 
    mem_indiv_id = uid,
    mem_mutate = quos(
        preload_pid = uid, 
        s02q01_open = hl2, 
        preload_sex = hl4, 
        preload_age = hl6, 
        preload_relation = hl3,
        s02q07 = hl3,    # relationship to head
        s02q06 = hl6,  # age
        s02q05 = hl4,    # gender
        s02q01 = hl2),  # name
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
