


# Custom function to replace the second occurrence of "-" with "_"
replace_second_occurrence <- function(names) {
  # Find the positions of all occurrences of "-"
  positions <- gregexpr("-", names)[[1]]
  # Check if there are at least two occurrences
  if (length(positions) >= 2) {
    # Replace the second occurrence with "_"
    substr(names, positions[2], positions[2]) <- "_"
  }
  return(names)
}
