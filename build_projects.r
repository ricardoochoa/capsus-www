# Load required libraries
library(jsonlite)
library(glue)
library(fs)

# Ensure output directories exist and are clean
unlink(c("projects/en", "projects/es"), recursive = TRUE)
dir_create(c("projects/en", "projects/es"))

# Helper to remove Spanish accents
clean_accents <- function(x) {
  chartr("áéíóúÁÉÍÓÚñÑüÜ", "aeiouAEIOUnNuU", x)
}

# Generator function
generate_qmd <- function(json_path, lang) {
  projects <- fromJSON(json_path)
  seen_slugs <- character(0)
  
  for (i in seq_len(nrow(projects))) {
    p <- projects[i, ]
    
    # Create a URL-safe slug from the title without mangling accents
    base_slug <- clean_accents(p$title)
    base_slug <- tolower(base_slug)
    base_slug <- gsub("[^a-z0-9]+", "-", base_slug)
    base_slug <- gsub("^-+|-+$", "", base_slug)
    
    # Avoid collisions by appending a counter if duplicate slug exists
    slug <- base_slug
    counter <- 1
    while (slug %in% seen_slugs) {
      counter <- counter + 1
      slug <- paste0(base_slug, "-", counter)
    }
    seen_slugs <- c(seen_slugs, slug)
    
    # Sanitize double quotes to single quotes and remove backslashes
    safe_title <- gsub('"', "'", p$title)
    safe_title <- gsub("\\\\", "", safe_title)
    
    safe_client <- gsub('"', "'", p$client)
    safe_client <- gsub("\\\\", "", safe_client)
    
    safe_location <- gsub('"', "'", p$location)
    safe_location <- gsub("\\\\", "", safe_location)
    
    safe_theme <- gsub('"', "'", p$theme)
    safe_theme <- gsub("\\\\", "", safe_theme)
    
    # Build AI-readable JSON-LD metadata
    json_ld <- glue('
    <script type="application/ld+json">
    {{
      "@context": "https://schema.org",
      "@type": "Project",
      "name": "{safe_title}",
      "sponsor": "{safe_client}",
      "spatialCoverage": "{safe_location}"
    }}
    </script>
    ')
    
    # Construct the Quarto document
    content <- glue('
    ---
    title: "{safe_title}"
    client: "{safe_client}"
    location: "{safe_location}"
    year: "{p$year}"
    categories: ["{safe_theme}"]
    ---
    
    {json_ld}
    
    {p$description}
    ')
    
    # Write to file
    writeLines(content, glue("projects/{lang}/{slug}.qmd"))
  }
  message(glue("Successfully generated {length(seen_slugs)} {lang} projects."))
}

# Execute for both languages
generate_qmd("data/projects_en.json", "en")
generate_qmd("data/projects_es.json", "es")