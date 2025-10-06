# build.R — publica todas las clases de slides_src/* en docs/slides/*
# Sin mover nada a mano.

library(fs)
library(rmarkdown)

src_root <- "slides_src"
out_root <- path("docs", "slides")

stopifnot(dir_exists(src_root))
dir_create(out_root)

clases <- dir_ls(src_root, type = "directory", recurse = FALSE)

for (cl in clases) {
  clase_name <- path_file(cl)                  # p.ej. "clase_1"
  rmds <- dir_ls(cl, regexp = "\\.Rmd$", type = "file")
  if (!length(rmds)) next
  rmd <- rmds[1]                               # usa el primer .Rmd de la carpeta
  
  out_dir <- path(out_root, clase_name)
  dir_create(out_dir)
  
  # Copia todos los assets que NO son Rmd al lado del HTML final
  assets <- dir_ls(cl, type = "file", recurse = FALSE)
  assets <- assets[!grepl("\\.Rmd$", assets, ignore.case = TRUE)]
  if (length(assets)) file_copy(assets, path(out_dir, path_file(assets)), overwrite = TRUE)
  
  message("Renderizando: ", rmd, "  -->  ", out_dir, "/index.html")
  render(
    input       = rmd,
    output_file = "index.html",
    output_dir  = out_dir,
    envir       = new.env()
  )
}
message("OK. Revisa docs/slides/<clase>/index.html")
