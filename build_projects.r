# Load required libraries
library(jsonlite)
library(glue)
library(fs)

# Ensure output directories exist
dir_create(c("projects/en", "projects/es"))

# Generator function
generate_qmd <- function(json_path, lang) {
  projects <- fromJSON(json_path)
  
  for (i in seq_len(nrow(projects))) {
    p <- projects[i, ]
    
    # Create a URL-safe slug from the title
    slug <- gsub("[^a-z0-9]", "-", tolower(p$title))
    slug <- gsub("-+", "-", slug) 
    
    # Sanitize double quotes to single quotes to prevent YAML parsing errors
    safe_title <- gsub('"', "'", p$title)
    safe_client <- gsub('"', "'", p$client)
    safe_location <- gsub('"', "'", p$location)
    
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
    categories: ["{p$theme}"]
    ---
    
    {json_ld}
    
    {p$description}
    ')
    
    # Write to file
    writeLines(content, glue("projects/{lang}/{slug}.qmd"))
  }
  message(glue("Successfully generated {nrow(projects)} {lang} projects."))
}

# Execute for both languages
generate_qmd("data/projects_en.json", "en")
generate_qmd("data/projects_es.json", "es")