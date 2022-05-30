#scrap for checking different allometric equations 

hyltemossa_dbh <- rlnorm(n=597, log(16.18), log(5.65))




mean(hyltemossa_dbh)

sd(hyltemossa_dbh)


dbh <- 16.18
B0 <- -1.577
B1 <- 10.892
B2 <- 15.61
  
  
h <- 1.3 + (dbh**2/((B0 + B1*dbh)**2))

Cstem_temp <- ((B0 *dbh**B1)*59.7)*0.5
Cstem_boreal <- ((B0 *dbh**B1)*59.7)*0.5

Cstem_boreal

Cstem_temp_mark <- exp(B0 + B1*(dbh/(dbh+B2)))*59.7*0.5
Cstem_boreal_mark <- exp(B0 + B1*(dbh/(dbh+B2)))*59.7*0.5
Cstem_boreal_mark
((Cstem_temp- Cstem_temp_mark) / 2) + Cstem_temp_mark
