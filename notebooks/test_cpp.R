h1 <- himm:::Himm_NxN$new(10,10)
himm:::active_index()
himm:::show_pointer(1)

h2 <- himm:::Himm_NxN$new(10,10)
himm:::active_index()
himm:::show_pointer(2)

rm(h1)
gc()
himm:::active_index()
