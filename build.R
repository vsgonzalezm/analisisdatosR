
# install.packages(c("fs","rmarkdown"), repos="https://cloud.r-project.org")
library(fs)
library(rmarkdown)

src_root <- "slides_src"
out_root <- path("docs", "slides")

stopifnot(dir_exists(src_root))
dir_create(out_root)

clases <- dir_ls(src_root, type = "directory", recurse = FALSE)

for (cl in clases) {
  clase_name <- path_file(cl)                        # p.ej. "clase_1"
  rmds <- dir_ls(cl, regexp = "\\.Rmd$", type = "file", recurse = TRUE)
  if (!length(rmds)) next
  rmd <- rmds[1]                                     # usa el primer .Rmd
  
  out_dir <- path(out_root, clase_name)
  dir_create(out_dir)
  
  # --- COPIA RECURSIVA DE TODO EXCEPTO .Rmd ---
  paths <- dir_ls(cl, recurse = TRUE, type = "any")
  paths <- paths[!grepl("\\.Rmd$", paths, ignore.case = TRUE)]
  
  for (p in paths) {
    rel  <- path_rel(p, start = cl)                  # ruta relativa dentro de la clase
    dest <- path(out_dir, rel)
    if (dir_exists(p)) {
      dir_create(dest)
    } else {
      dir_create(path_dir(dest))
      file_copy(p, dest, overwrite = TRUE)
    }
  }
  
  message("Renderizando: ", rmd, "  -->  ", out_dir, "/index.html")
  render(
    input       = rmd,
    output_file = "index.html",
    output_dir  = out_dir,
    envir       = new.env()
  )
}

message("OK. Revisa docs/slides/<clase>/index.html (con libs/ si aplica)")