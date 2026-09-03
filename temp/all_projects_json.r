library(stringr)
library(jsonlite)

clean_latex <- function(text) {
  if (is.na(text) || length(text) == 0) return("")
  
  # Remove LaTeX textbackslash commands (e.g. \textbackslash{} or \textbackslash)
  text <- str_replace_all(text, "\\\\textbackslash\\{\\}?", "")
  
  # Unwrap LaTeX inline formatting commands
  text <- str_replace_all(text, "\\\\url\\{([^}]+)\\}", "\\1")
  text <- str_replace_all(text, "\\\\textit\\{([^}]+)\\}", "\\1")
  text <- str_replace_all(text, "\\\\textbf\\{([^}]+)\\}", "\\1")
  
  # Unescape LaTeX special characters: \&, \%, \$, \_, \#, \{, \}
  text <- str_replace_all(text, "\\\\([&%$#_{}])", "\\1")
  
  # Replace LaTeX line breaks (\\) with space
  text <- str_replace_all(text, "\\\\\\\\", " ")
  
  # Remove any remaining stray backslashes
  text <- str_replace_all(text, "\\\\", "")
  
  # Normalize whitespace (replace tabs/newlines/multiple spaces with a single space)
  text <- str_replace_all(text, "\\s+", " ")
  
  str_trim(text)
}

parse_tex_to_json <- function(input_tex, output_json) {
  if (!file.exists(input_tex)) {
    stop(sprintf("Input file not found: %s", input_tex))
  }
  
  # Read the entire LaTeX file into a single string
  raw_text <- readLines(input_tex, warn = FALSE, encoding = "UTF-8")
  full_text <- paste(raw_text, collapse = "\n")
  
  # Extract only the Projects section
  projects_section <- str_extract(full_text, "(?s)\\\\section\\{Projects\\}.*?(?=\\\\section\\{|\\\\end\\{document\\})")
  if (is.na(projects_section)) {
    stop(sprintf("No '\\section{Projects}' section found in %s", input_tex))
  }
  
  # Split text into individual project blocks using the subsection tag
  project_blocks <- str_split(projects_section, "\\\\subsection\\{")[[1]][-1]
  
  # Parse each block into a list of single-row data frames
  projects_list <- lapply(seq_along(project_blocks), function(i) {
    block <- project_blocks[i]
    
    # 1. Separate the subsection header from the body using \textbf{Client:}
    client_pos <- str_locate(block, "(?s)\\\\textbf\\{Client:\\}")[1, 1]
    if (is.na(client_pos)) {
      warning(sprintf("Block %d: '\\textbf{Client:}' not found; skipping.", i))
      return(NULL)
    }
    
    header_raw <- str_trim(substr(block, 1, client_pos - 1))
    if (str_ends(header_raw, "\\}")) {
      header_raw <- str_sub(header_raw, 1, -2)
    }
    
    # Extract year: (YYYY) or (YYYY-YYYY) or (YYYY–YYYY) at the end of the header
    year_match <- str_match(header_raw, "\\(([0-9]{4}(?:[–-][0-9]{4})?)\\)\\s*$")
    year <- if (!is.na(year_match[1, 2])) year_match[1, 2] else ""
    
    # Title is everything before the final parenthesized year
    title <- str_replace(header_raw, "\\s*\\([0-9]{4}(?:[–-][0-9]{4})?\\)\\s*$", "")
    
    # Body containing Client, Location, and Description
    body <- substr(block, client_pos, nchar(block))
    
    # 2. Extract Client (everything after \textbf{Client:} up to \\)
    client_match <- str_match(body, "(?s)\\\\textbf\\{Client:\\}\\s*(.*?)\\\\\\\\")
    client <- if (!is.na(client_match[1, 2])) client_match[1, 2] else ""
    
    # 3. Extract Location (everything after \textbf{Location:} up to \\[...em\\] or \\)
    loc_match <- str_match(body, "(?s)\\\\textbf\\{Location:\\}\\s*(.*?)\\\\\\\\(\\[[0-9.]*em\\])?")
    location <- if (!is.na(loc_match[1, 2])) loc_match[1, 2] else ""
    
    # 4. Extract Description (everything after Location tag and its line break)
    desc_split <- str_split(body, "(?s)\\\\textbf\\{Location:\\}.*?\\\\\\\\(\\[[0-9.]*em\\])?")[[1]]
    description <- if (length(desc_split) > 1) desc_split[2] else ""
    
    data.frame(
      title = clean_latex(title),
      year = str_trim(year),
      client = clean_latex(client),
      location = clean_latex(location),
      description = clean_latex(description),
      stringsAsFactors = FALSE
    )
  })
  
  # Filter out any NULL elements and combine
  projects_list <- projects_list[!vapply(projects_list, is.null, logical(1))]
  portfolio <- do.call(rbind, projects_list)
  
  # Ensure target directory exists
  out_dir <- dirname(output_json)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Export to JSON
  write_json(portfolio, path = output_json, pretty = TRUE, auto_unbox = TRUE)
  message(sprintf("Successfully parsed %d projects from '%s' into '%s'", nrow(portfolio), input_tex, output_json))
}

# Resolve input filenames (handle curriculum_*.tex or capsus_*.tex)
resolve_input_file <- function(candidates) {
  for (cand in candidates) {
    if (file.exists(cand)) return(cand)
  }
  candidates[1]
}

# Execute extraction for English curriculum
input_en <- resolve_input_file(c("curriculum_en.tex", "capsus_en.tex"))
if (file.exists(input_en)) {
  parse_tex_to_json(input_en, "data/projects_en.json")
}

# Execute extraction for Spanish curriculum
input_es <- resolve_input_file(c("curriculum_es.tex", "capsus_es.tex"))
if (file.exists(input_es)) {
  parse_tex_to_json(input_es, "data/projects_es.json")
}