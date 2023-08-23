#Henry's constant
#I'm having a hard time with Henry's constant, so here I explore
#this is for CO2


#Johnson et al 2010 gives examples of how mass equivilance changes with temperature. Here we check his work. First we
#will bild a data frame using numbers given in the manuscript
df <- data.frame(CO2_ppm = c(1000,1000,1000),
                CO2asC_mg  = c(2.84,2.01,1.50),
                temp_c = c(5,15,25)
)

#Plummer and Busenberg
# logK = A + BT + C/T + D log T + E/T^2
# T=temp in K
A = 108.3865
B = 0.01985076
C = -6919.53
D = -40.4515
E = 669365

df$temp_K <- df$temp_c + 273.15

df$Kh_molperKg_Plummer <-  exp(A + (B*df$temp_K) + (C/df$temp_K) + (D*log(df$temp_K)) + (E/df$temp_K^2))


#sanders 2017
# Kh = Kho * exp(D*(1/T - To))
Kho = .035
D = 2400
To = 298.15

df$kh_sanders <-  Kho * exp(D*(1/df$temp_K - 1/To))

#mass equivalence of dissolved CO2 - McDowell&Johnson
# C(1) = kH(1) × CO2(1) * 12 *10^-6
  #C(1) =	Mass equivalence of dissolved CO2 (g CO2‐C L−1) 
  #CO2(1) = pCO2 (atm) 
df$CO2asC_mg_mcdowell <- df$CO2_ppm * df$kh_sanders  * 10^-6 * 44 
plot(df$CO2asC_mg,df$CO2asC_mg_mcdowell)

#plummer is in mol/kg, so we have to convert kg to L based on temperature

df$CO2asC_mg_plummer <- df$CO2_ppm * df$Kh_Plummer * 12 * 10^-6 

plot(df$CO2asC_mg,df$Kh_Plummer) 


