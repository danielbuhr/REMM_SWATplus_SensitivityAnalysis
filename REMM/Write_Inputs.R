# Write_Inputs
# Creates new buffer, veg, rate, prj input files for REMM

file_buf = read.delim("C:\\REMMv1b\\bufferdata\\gibbs.buf", header = FALSE, sep = " ", col.names = paste0("V",seq_len(20)), fill = TRUE, stringsAsFactors = FALSE)
file_veg = read.delim("C:\\REMMv1b\\vegetationdata\\GIBBS091002SE.VEG", header = FALSE, sep = " ", col.names = paste0("V",seq_len(12)), fill = TRUE, stringsAsFactors = FALSE)
file_rte = read.delim("C:\\REMMv1b\\RateData\\Gibbs.rte", header = FALSE, sep = " ", col.names = paste0("V",seq_len(5)), fill = TRUE, stringsAsFactors = FALSE)
file_prj = read.delim("C:\\REMM2019\\REMM_Examples\\Projects\\Gibbs.prj", header = FALSE, fill = TRUE, stringsAsFactors = FALSE)

# Set all buf characteristics equivalent across zones
r <- 4 # row index
c <- 1 # column index
cols <- ncol(file_buf)

for (r in 4:49) {
  for (c in 1:cols) {
    file_buf[r+46,c] <- file_buf[r,c]
    file_buf[r+92,c] <- file_buf[r,c]
    c <- c+1
  }
  
  r <- r+1
}

# repeat for veg file
r_veg <- 1 # row index
c_veg <- 1 # column index
cols_veg <- ncol(file_veg)

for (r_veg in 1:462) {
  for (c_veg in 1:cols_veg) {
    file_veg[r_veg+462,c_veg] <- file_veg[r_veg,c_veg]
    file_veg[r_veg+924,c_veg] <- file_veg[r_veg,c_veg]
    c_veg <- c_veg+1
  }
  
  r_veg <- r_veg+1
}

i <- 1 # row index
while(i < n_sims+1) {
  
  # Buffer inputs
  file_buf[2,1] <- matrix_values[i,1] # surface drainage area
  file_buf[2,2] <- matrix_values[i,1] # subsurface drainage area
  file_buf[2,4] <- matrix_values[i,2] # stream depth
  file_buf[3,2] <- matrix_values[i,3] # soil temp
  file_buf[4,1] <- (matrix_values[i,4]) # Zone 1 width
  file_buf[50,1] <- (matrix_values[i,4]) # Zone 2 width
  file_buf[96,1] <- (matrix_values[i,4]) # Zone 3 width
  file_buf[4,3] <- matrix_values[i,5] # Zone 1 slope
  file_buf[50,3] <- matrix_values[i,5] # Zone 2 slope
  file_buf[96,3] <- matrix_values[i,5] # Zone 3 slope
  file_buf[5,1] <- matrix_values[i,6] # Zone 1 deep seepage
  file_buf[51,1] <- matrix_values[i,6] # Zone 2 deep seepage
  file_buf[97,1] <- matrix_values[i,6] # Zone 3 deep seepage
  file_buf[30,11] <- matrix_values[i,7] # Zone 1 Manning's n
  file_buf[76,11] <- matrix_values[i,7] # Zone 2 Manning's n
  file_buf[122, 11] <- matrix_values[i,7] # Zone 3 Manning's n
  file_buf[33,1] <- (matrix_values[i,8]/10) # Zone 1, litter layer ammonium
  file_buf[79,1] <- (matrix_values[i,8]/10) # Zone 2, litter layer ammonium
  file_buf[125,1] <- (matrix_values[i,8]/10) # Zone 3, litter layer ammonium
  file_buf[33,2] <- (matrix_values[i,9]/10) # Zone 1, litter layer nitrate
  file_buf[79,2] <- (matrix_values[i,9]/10) # Zone 2, litter layer nitrate
  file_buf[125,2] <- (matrix_values[i,9]/10) # Zone 3, litter layer nitrate
  file_buf[38,1] <- matrix_values[i,8] # Zone 1, layer 1 ammonium
  file_buf[84,1] <- matrix_values[i,8] # Zone 2, layer 1 ammonium
  file_buf[130,1] <- matrix_values[i,8] # Zone 3, layer 1 ammonium
  file_buf[38,2] <- matrix_values[i,9] # Zone 1, layer 1 nitrate
  file_buf[84,2] <- matrix_values[i,9] # Zone 2, layer 1 nitrate
  file_buf[130,2] <- matrix_values[i,9] # Zone 3, layer 1 nitrate
  file_buf[43,1] <- matrix_values[i,8] # Zone 1, layer 2 ammonium
  file_buf[89,1] <- matrix_values[i,8] # Zone 2, layer 2 ammonium
  file_buf[135,1] <- matrix_values[i,8] # Zone 3, layer 2 ammonium
  file_buf[43,2] <- matrix_values[i,9] # Zone 1, layer 2 nitrate
  file_buf[89,2] <- matrix_values[i,9] # Zone 2, layer 2 nitrate
  file_buf[135,2] <- matrix_values[i,9] # Zone 3, layer 2 nitrate
  file_buf[48,1] <- matrix_values[i,8] # Zone 1, layer 3 ammonium
  file_buf[94,1] <- matrix_values[i,8] # Zone 2, layer 3 ammonium
  file_buf[140,1] <- matrix_values[i,8] # Zone 3, layer 3 ammonium
  file_buf[48,2] <- matrix_values[i,9] # Zone 1, layer 3 nitrate
  file_buf[94,2] <- matrix_values[i,9] # Zone 2, layer 3 nitrate
  file_buf[140,2] <- matrix_values[i,9] # Zone 3, layer 3 nitrate
  file_buf[35,1] <- matrix_values[i,10] # Zone 1, layer 1 depth to bedrock
  file_buf[81,1] <- matrix_values[i,10] # Zone 2, layer 1 depth to bedrock
  file_buf[127,1] <- matrix_values[i,10] # Zone 3, layer 1 depth to bedrock
  file_buf[40,1] <- matrix_values[i,10] # Zone 1, layer 2 depth to bedrock
  file_buf[86,1] <- matrix_values[i,10] # Zone 2, layer 2 depth to bedrock
  file_buf[132,1] <- matrix_values[i,10] # Zone 3, layer 2 dpeth to bedrock
  file_buf[45,1] <- matrix_values[i,10] # Zone 1, layer 3 depth to bedrock
  file_buf[91,1] <- matrix_values[i,10] # Zone 2, layer 3 depth to bedrock
  file_buf[137,1] <- matrix_values[i,10] # Zone 3, layer 3 depth to bedrock
  file_buf[35,4] <- matrix_values[i,11] # Zone 1, layer 1 pore size distribution index
  file_buf[81,4] <- matrix_values[i,11] # Zone 2, layer 1 pore size distribution index
  file_buf[127,4] <- matrix_values[i,11] # Zone 3, layer 1 pore size distribution index
  file_buf[40,4] <- matrix_values[i,11] # Zone 1, layer 2 pore size distribution index
  file_buf[86,4] <- matrix_values[i,11] # Zone 2, layer 2 pore size distribution index
  file_buf[132,4] <- matrix_values[i,11] # Zone 3, layer 2 pore size distribution index
  file_buf[45,4] <- matrix_values[i,11] # Zone 1, layer 3 pore size distribution index
  file_buf[91,4] <- matrix_values[i,11] # Zone 2, layer 3 pore size distribution index
  file_buf[137,4] <- matrix_values[i,11] # Zone 3, layer 3 pore size distribution index
  file_buf[36,1] <- matrix_values[i,12] # Zone 1, layer 1 layer thickness
  file_buf[82,1] <- matrix_values[i,12] # Zone 2, layer 1 layer thickness
  file_buf[128,1] <- matrix_values[i,12] # Zone 3, layer 1 layer thickness
  file_buf[41,1] <- matrix_values[i,12] # Zone 1, layer 2 layer thickness
  file_buf[87,1] <- matrix_values[i,12] # Zone 2, layer 2 layer thickness
  file_buf[133,1] <- matrix_values[i,12] # Zone 3, layer 2 layer thickness
  file_buf[46,1] <- matrix_values[i,12] # Zone 1, layer 3 layer thickness
  file_buf[92,1] <- matrix_values[i,12] # Zone 2, layer 3 layer thickness
  file_buf[138,1] <- matrix_values[i,12] # Zone 3, layer 3 layer thickness
  file_buf[36,2] <- matrix_values[i,13] # Zone 1, layer 1 wilting point
  file_buf[82,2] <- matrix_values[i,13] # Zone 2, layer 1 wilting point
  file_buf[128,2] <- matrix_values[i,13] # Zone 3, layer 1 wilting point
  file_buf[41,2] <- matrix_values[i,13] # Zone 1, layer 2 wilting point
  file_buf[87,2] <- matrix_values[i,13] # Zone 2, layer 2 wilting point
  file_buf[133,2] <- matrix_values[i,13] # Zone 3, layer 2 wilting point
  file_buf[46,2] <- matrix_values[i,13] # Zone 1, layer 3 wilting point
  file_buf[92,2] <- matrix_values[i,13] # Zone 2, layer 3 wilting point
  file_buf[138,2] <- matrix_values[i,13] # Zone 3, layer 3 wilting point
  file_buf[36,3] <- matrix_values[i,14] # Zone 1, layer 1 field capacity
  file_buf[82,3] <- matrix_values[i,14] # Zone 2, layer 1 field capacity
  file_buf[128,3] <- matrix_values[i,14] # Zone 3, layer 1 field capacity
  file_buf[41,3] <- matrix_values[i,14] # Zone 1, layer 2 field capacity
  file_buf[87,3] <- matrix_values[i,14] # Zone 2, layer 2 field capacity
  file_buf[133,3] <- matrix_values[i,14] # Zone 3, layer 2 field capacity
  file_buf[46,3] <- matrix_values[i,14] # Zone 1, layer 3 field capacity
  file_buf[92,3] <- matrix_values[i,14] # Zone 2, layer 3 field capacity
  file_buf[138,3] <- matrix_values[i,14] # Zone 3, layer 3 field capacity
  file_buf[36,4] <- matrix_values[i,15] # Zone 1, layer 1 porosity
  file_buf[82,4] <- matrix_values[i,15] # Zone 2, layer 1 porosity
  file_buf[128,4] <- matrix_values[i,15] # Zone 3, layer 1 porosity
  file_buf[41,4] <- matrix_values[i,15] # Zone 1, layer 2 porosity
  file_buf[87,4] <- matrix_values[i,15] # Zone 2, layer 2 porosity
  file_buf[133,4] <- matrix_values[i,15] # Zone 3, layer 2 porosity
  file_buf[46,4] <- matrix_values[i,15] # Zone 1, layer 3 porosity
  file_buf[92,4] <- matrix_values[i,15] # Zone 2, layer 3 porosity
  file_buf[138,4] <- matrix_values[i,15] # Zone 3, layer 3 porosity
  file_buf[36,5] <- matrix_values[i,16] # Zone 1, layer 1 VWC
  file_buf[82,5] <- matrix_values[i,16] # Zone 2, layer 1 VWC
  file_buf[128,5] <- matrix_values[i,16] # Zone 3, layer 1 VWC
  file_buf[41,5] <- matrix_values[i,16] # Zone 1, layer 2 VWC
  file_buf[87,5] <- matrix_values[i,16] # Zone 2, layer 2 VWC
  file_buf[133,5] <- matrix_values[i,16] # Zone 3, layer 2 VWC
  file_buf[46,5] <- matrix_values[i,16] # Zone 1, layer 3 VWC
  file_buf[92,5] <- matrix_values[i,16] # Zone 2, layer 3 VWC
  file_buf[138,5] <- matrix_values[i,16] # Zone 3, layer 3 VWC
  file_buf[36,6] <- matrix_values[i,17] # Zone 1, layer 1 permeability
  file_buf[82,6] <- matrix_values[i,17] # Zone 2, layer 1 permeability
  file_buf[128,6] <- matrix_values[i,17] # Zone 3, layer 1 permeability
  file_buf[41,6] <- matrix_values[i,17] # Zone 1, layer 2 permeability
  file_buf[87,6] <- matrix_values[i,17] # Zone 2, layer 2 permeability
  file_buf[133,6] <- matrix_values[i,17] # Zone 3, layer 2 permeability
  file_buf[46,6] <- matrix_values[i,17] # Zone 1, layer 3 permeability
  file_buf[92,6] <- matrix_values[i,17] # Zone 2, layer 3 permeability
  file_buf[138,6] <- matrix_values[i,17] # Zone 3, layer 3 permeability
  file_buf[36,9] <- matrix_values[i,18] # Zone 1, layer 1 % clay
  file_buf[82,9] <- matrix_values[i,18] # Zone 2, layer 1 % clay
  file_buf[128,9] <- matrix_values[i,18] # Zone 3, layer 1 % clay
  file_buf[41,9] <- matrix_values[i,18] # Zone 1, layer 2 % clay
  file_buf[87,9] <- matrix_values[i,18] # Zone 2, layer 2 % clay
  file_buf[133,9] <- matrix_values[i,18] # Zone 2, layer 2 % clay
  file_buf[46,9] <- matrix_values[i,18] # Zone 1, layer 3 % clay
  file_buf[92,9] <- matrix_values[i,18] # Zone 2, layer 3 % clay
  file_buf[138,9] <- matrix_values[i,18] # Zone 3, layer 3 % clay
  file_buf[31,8] <- (bd[i]*0.625) # Zone 1, litter layer bulk density
  file_buf[77,8] <- (bd[i]*0.625) # Zone 2, litter layer bulk density
  file_buf[123,8] <- (bd[i]*0.625) # Zone 3, litter layer bulk density
  file_buf[36,10] <- bd_A[i] # Zone 1, layer 1 bulk density
  file_buf[82,10] <- bd_A[i] # Zone 2, layer 1 bulk density
  file_buf[128,10] <- bd_A[i] # Zone 3, layer 1 bulk density
  file_buf[41,10] <- bd_B[i] # Zone 1, layer 2 bulk density
  file_buf[87,10] <- bd_B[i] # Zone 2, layer 2 bulk density
  file_buf[133,10] <- bd_B[i] # Zone 3, layer 2 bulk density
  file_buf[46,10] <- bd_C[i] # Zone 1, layer 3 bulk density
  file_buf[92,10] <- bd_C[i] # Zone 2, layer 3 bulk density
  file_buf[138,10] <- bd_C[i] # Zone 3, layer 3 bulk density
  file_buf[31,14] <- matrix_values[i,20] # Zone 1, litter layer pH
  file_buf[77,14] <- matrix_values[i,20] # Zone 2, litter layer pH
  file_buf[123,14] <- matrix_values[i,20] # Zone 3, litter layer pH
  file_buf[36,16] <- matrix_values[i,20] # Zone 1, layer 1 pH
  file_buf[82,16] <- matrix_values[i,20] # Zone 2, layer 1 pH
  file_buf[128,16] <- matrix_values[i,20] # Zone 3, layer 1 pH
  file_buf[41,16] <- matrix_values[i,20] # Zone 1, layer 2 pH
  file_buf[87,16] <- matrix_values[i,20] # Zone 2, layer 2 pH
  file_buf[133,16] <- matrix_values[i,20] # Zone 3, layer 2 pH
  file_buf[46,16] <- matrix_values[i,20] # Zone 1, layer 3 pH
  file_buf[92,16] <- matrix_values[i,20] # Zone 2, layer 3 pH
  file_buf[138,16] <- matrix_values[i,20] # Zone 3, layer 3 pH
  file_buf[32,1] <- (matrix_values[i,42]*2.5) # Zone 1, litter layer structural carbon
  file_buf[78,1] <- (matrix_values[i,42]*2.5) # Zone 2, litter layer structural carbon
  file_buf[124,1] <- (matrix_values[i,42]*2.5) # Zone 3, litter layer structural carbon
  file_buf[37,1] <- matrix_values[i,42] # Zone 1, layer 1 structural carbon
  file_buf[83,1] <- matrix_values[i,42] # Zone 2, layer 1 structural carbon
  file_buf[129,1] <- matrix_values[i,42] # Zone 3, layer 1 structural carbon
  file_buf[42,1] <- matrix_values[i,42] # Zone 1, layer 2 structural carbon
  file_buf[88,1] <- matrix_values[i,42] # Zone 2, layer 2 structural carbon
  file_buf[134,1] <- matrix_values[i,42] # Zone 3, layer 2 structural carbon
  file_buf[47,1] <- matrix_values[i,42] # Zone 1, layer 3 structural carbon
  file_buf[93,1] <- matrix_values[i,42] # Zone 2, layer 3 structural carbon
  file_buf[139,1] <- matrix_values[i,42] # Zone 3, layer 3 structural carbon
  # Manual says that C:N for strucutural residue should be fixed at 150
  file_buf[33,3] <- (matrix_values[i,42]/(t[i]/2.5)) # Zone 1, litter layer structural nitrogen
  file_buf[79,3] <- (matrix_values[i,42]/(t[i]/2.5)) # Zone 2, litter layer structural nitrogen
  file_buf[125,3] <- (matrix_values[i,42]/(t[i]/2.5)) # Zone 3, litter layer structural nitrogen
  file_buf[38,3] <- (matrix_values[i,42]/t[i]) # Zone 1, layer 1 structural nitrogen
  file_buf[84,3] <- (matrix_values[i,42]/t[i]) # Zone 2, layer 1 structural nitrogen
  file_buf[130,3] <- (matrix_values[i,42]/t[i]) # Zone 3, layer 1 structural nitrogen
  file_buf[43,3] <- (matrix_values[i,42]/t[i]) # Zone 1, layer 2 structural nitrogen
  file_buf[89,3] <- (matrix_values[i,42]/t[i]) # Zone 2, layer 2 structural nitrogen
  file_buf[135,3] <- (matrix_values[i,42]/t[i]) # Zone 3, layer 2 structural nitrogen
  file_buf[48,3] <- (matrix_values[i,42]/t[i]) # Zone 1, layer 3 structural nitrogen
  file_buf[94,3] <- (matrix_values[i,42]/t[i]) # Zone 2, layer 3 structural nitrogen
  file_buf[140,3] <- (matrix_values[i,42]/t[i]) # Zone 3, layer 3 structural nitrogen
  # No constraints on metabolic carbon:nitrogen
  file_buf[32,2] <- (matrix_values[i,43]*2.5) # Zone 1, litter layer metabolic carbon
  file_buf[78,2] <- (matrix_values[i,43]*2.5) # Zone 2, litter layer metabolic carbon
  file_buf[124,2] <- (matrix_values[i,43]*2.5) # Zone 3, litter layer metabolic carbon
  file_buf[37,2] <- matrix_values[i,43] # Zone 1, layer 1 metabolic carbon
  file_buf[83,2] <- matrix_values[i,43] # Zone 2, layer 1 metabolic carbon
  file_buf[129,2] <- matrix_values[i,43] # Zone 3, layer 1 metabolic carbon
  file_buf[42,2] <- matrix_values[i,43] # Zone 1, layer 2 metabolic carbon
  file_buf[88,2] <- matrix_values[i,43] # Zone 2, layer 2 metabolic carbon
  file_buf[134,2] <- matrix_values[i,43] # Zone 3, layer 2 metabolic carbon
  file_buf[47,2] <- matrix_values[i,43] # Zone 1, layer 3 metabolic carbon
  file_buf[93,2] <- matrix_values[i,43] # Zone 2, layer 3 metabolic carbon
  file_buf[139,2] <- matrix_values[i,43] # Zone 3, layer 3 metabolic carbon
  file_buf[32,3] <- (matrix_values[i,44]/4) # Zone 1, litter layer active humus carbon
  file_buf[78,3] <- (matrix_values[i,44]/4) # Zone 2, litter layer active humus carbon
  file_buf[124,3] <- (matrix_values[i,44]/4) # Zone 3, litter layer active humus carbon
  file_buf[37,3] <- matrix_values[i,44] # Zone 1, layer 1 active humus carbon
  file_buf[83,3] <- matrix_values[i,44] # Zone 2, layer 1 active humus carbon
  file_buf[129,3] <- matrix_values[i,44] # Zone 3, layer 1 active humus carbon
  file_buf[42,3] <- matrix_values[i,44] # Zone 1, layer 2 active humus carbon
  file_buf[88,3] <- matrix_values[i,44] # Zone 2, layer 2 active humus carbon
  file_buf[134,3] <- matrix_values[i,44] # Zone 3, layer 2 active humus carbon
  file_buf[47,3] <- matrix_values[i,44] # Zone 1, layer 3 active humus carbon
  file_buf[93,3] <- matrix_values[i,44] # Zone 2, layer 3 active humus carbon
  file_buf[139,3] <- matrix_values[i,44] # Zone 3, layer 3 active humus carbon
  # Active humus nitrogen will be altered to adjust C:N
  file_buf[33,5] <- (matrix_values[i,44]/(u[i]*4)) # Zone 1, litter layer active humus nitrogen
  file_buf[79,5] <- (matrix_values[i,44]/(u[i]*4)) # Zone 2, litter layer active humus nitrogen
  file_buf[125,5] <- (matrix_values[i,44]/(u[i]*4)) # Zone 3, litter layer active humus nitrogen
  file_buf[38,5] <- (matrix_values[i,44]/u[i]) # Zone 1, layer 1 active humus nitrogen
  file_buf[84,5] <- (matrix_values[i,44]/u[i]) # Zone 2, layer 1 active humus nitrogen
  file_buf[130,5] <- (matrix_values[i,44]/u[i]) # Zone 3, layer 1 active humus nitrogen
  file_buf[43,5] <- (matrix_values[i,44]/u[i]) # Zone 1, layer 2 active humus nitrogen
  file_buf[89,3] <- (matrix_values[i,44]/u[i]) # Zone 2, layer 2 active humus nitrogen
  file_buf[135,5] <- (matrix_values[i,44]/u[i]) # Zone 3, layer 2 active humus nitrogen
  file_buf[48,5] <- (matrix_values[i,44]/u[i]) # Zone 1, layer 3 active humus nitrogen
  file_buf[94,5] <- (matrix_values[i,44]/u[i]) # Zone 2, layer 3 active humus nitrogen
  file_buf[140,5] <- (matrix_values[i,44]/u[i]) # Zone 3, layer 3 active humus nitrogen
  file_buf[32,4] <- (matrix_values[i,45]/4) # Zone 1, litter layer slow humus carbon
  file_buf[78,4] <- (matrix_values[i,45]/4) # Zone 2, litter layer slow humus carbon
  file_buf[124,4] <- (matrix_values[i,45]/4) # Zone 3, litter layer slow humus carbon
  file_buf[37,4] <- matrix_values[i,45] # Zone 1, layer 1 slow humus carbon
  file_buf[83,4] <- matrix_values[i,45] # Zone 2, layer 1 slow humus carbon
  file_buf[129,4] <- matrix_values[i,45] # Zone 3, layer 1 slow humus carbon
  file_buf[42,4] <- matrix_values[i,45] # Zone 1, layer 2 slow humus carbon
  file_buf[88,4] <- matrix_values[i,45] # Zone 2, layer 2 slow humus carbon
  file_buf[134,4] <- matrix_values[i,45] # Zone 3, layer 2 slow humus carbon
  file_buf[47,4] <- matrix_values[i,45] # Zone 1, layer 3 slow humus carbon
  file_buf[93,4] <- matrix_values[i,45] # Zone 2, layer 3 slow humus carbon
  file_buf[139,4] <- matrix_values[i,45] # Zone 3, layer 3 slow humus carbon
  # Slow humus nitrogen will be altered to adjust C:N
  file_buf[33,6] <- (matrix_values[i,45]/(w[i]*4)) # Zone 1, litter layer slow humus nitrogen
  file_buf[79,6] <- (matrix_values[i,45]/(w[i]*4)) # Zone 2, litter layer slow humus nitrogen
  file_buf[125,6] <- (matrix_values[i,45]/(w[i]*4)) # Zone 3, litter layer slow humus nitrogen
  file_buf[38,6] <- (matrix_values[i,45]/w[i]) # Zone 1, layer 1 slow humus nitrogen
  file_buf[84,6] <- (matrix_values[i,45]/w[i]) # Zone 2, layer 1 slow humus nitrogen
  file_buf[130,6] <- (matrix_values[i,45]/w[i]) # Zone 3, layer 1 slow humus nitrogen
  file_buf[43,6] <- (matrix_values[i,45]/w[i]) # Zone 1, layer 2 slow humus nitrogen
  file_buf[89,6] <- (matrix_values[i,45]/w[i]) # Zone 2, layer 2 slow humus nitrogen
  file_buf[135,6] <- (matrix_values[i,45]/w[i]) # Zone 3, layer 2 slow humus nitrogen
  file_buf[48,6] <- (matrix_values[i,45]/w[i]) # Zone 1, layer 3 slow humus nitrogen
  file_buf[94,6] <- (matrix_values[i,45]/w[i]) # Zone 2, layer 3 slow humus nitrogen
  file_buf[140,6] <- (matrix_values[i,45]/w[i]) # Zone 3, layer 3 slow humus nitrogen
  file_buf[32,5] <- (matrix_values[i,46]/4) # Zone 1, litter layer passive humus carbon
  file_buf[78,5] <- (matrix_values[i,46]/4) # Zone 2, litter layer passive humus carbon
  file_buf[124,5] <- (matrix_values[i,46]/4) # Zone 3, litter layer passive humus carbon
  file_buf[37,5] <- matrix_values[i,46] # Zone 1, layer 1 passive humus carbon
  file_buf[83,5] <- matrix_values[i,46] # Zone 2, layer 1 passive humus carbon
  file_buf[129,5] <- matrix_values[i,46] # Zone 3, layer 1 passive humus carbon
  file_buf[42,5] <- matrix_values[i,46] # Zone 1, layer 2 passive humus carbon
  file_buf[88,5] <- matrix_values[i,46] # Zone 2, layer 2 passive humus carbon
  file_buf[134,5] <- matrix_values[i,46] # Zone 3, layer 2 passive humus carbon
  file_buf[47,5] <- matrix_values[i,46] # Zone 1, layer 3 passive humus carbon
  file_buf[93,5] <- matrix_values[i,46] # Zone 2, layer 3 passive humus carbon
  file_buf[139,5] <- matrix_values[i,46] # Zone 3, layer 3 passive humus carbon
  # Passive humus nitrogen will be altered to adjust C:N
  file_buf[33,7] <- (matrix_values[i,46]/(v[i]*4)) # Zone 1, litter layer passive humus nitrogen
  file_buf[79,7] <- (matrix_values[i,46]/(v[i]*4)) # Zone 2, litter layer passive humus nitrogen
  file_buf[125,7] <- (matrix_values[i,46]/(v[i]*4)) # Zone 3, litter layer passive humus nitrogen
  file_buf[38,7] <- (matrix_values[i,46]/v[i]) # Zone 1, layer 1 passive humus nitrogen
  file_buf[84,7] <- (matrix_values[i,46]/v[i]) # Zone 2, layer 1 passive humus nitrogen
  file_buf[130,7] <- (matrix_values[i,46]/v[i]) # Zone 3, layer 1 passive humus nitrogen
  file_buf[43,7] <- (matrix_values[i,46]/v[i]) # Zone 1, layer 2 passive humus nitrogen
  file_buf[89,7] <- (matrix_values[i,46]/v[i]) # Zone 2, layer 2 passive humus nitrogen
  file_buf[135,7] <- (matrix_values[i,46]/v[i]) # Zone 3, layer 2 passive humus nitrogen
  file_buf[48,7] <- (matrix_values[i,46]/v[i]) # Zone 1, layer 3 passive humus nitrogen
  file_buf[94,7] <- (matrix_values[i,46]/v[i]) # Zone 2, layer 3 passive humus nitrogen
  file_buf[140,7] <- (matrix_values[i,46]/v[i]) # Zone 3, layer 3 passive humus nitrogen
  file_buf[32,6] <- (matrix_values[i,47]*2.5) # Zone 1, litter layer end lignin carbon
  file_buf[78,6] <- (matrix_values[i,47]*2.5) # Zone 2,  litter layer end lignin carbon
  file_buf[124,6] <- (matrix_values[i,47]*2.5) # Zone 3, litter layer end lignin carbon
  file_buf[37,6] <- matrix_values[i,47] # Zone 1, layer 1 end lignin carbon
  file_buf[83,6] <- matrix_values[i,47] # Zone 2, layer 1 end lignin carbon
  file_buf[129,6] <- matrix_values[i,47] # Zone 3, layer 1 end lignin carbon
  file_buf[42,6] <- matrix_values[i,47] # Zone 1, layer 2 end lignin carbon
  file_buf[88,6] <- matrix_values[i,47] # Zone 2, layer 2 end lignin carbon
  file_buf[134,6] <- matrix_values[i,47] # Zone 3, layer 2 end lignin carbon
  file_buf[47,6] <- matrix_values[i,47] # Zone 1, layer 3 end lignin carbon
  file_buf[93,6] <- matrix_values[i,47] # Zone 2, layer 3 end lignin carbon
  file_buf[139,6] <- matrix_values[i,47] # Zone 3, layer 3 end lignin carbon
  
  # Vegetation inputs
  # Changed values for the veg type 1, DecFallUC
  file_veg[3,8] <- matrix_values[i,21] # Zone 1, Rainfall Interception Ratio
  file_veg[465,8] <- matrix_values[i,21] # Zone 2, rainfall interception ratio
  file_veg[927,8] <- matrix_values[i,21] # Zone 3, rainfall interception ratio
  file_veg[21,1] <- matrix_values[i,22] # Zone 1, Maximum nitrogen growth concentration, buds and new leaves
  file_veg[483,1] <- matrix_values[i,22] # Zone 2, Maximum nitrogen growth concentration, buds and new leaves
  file_veg[945,1] <- matrix_values[i,22] # Zone 3, Maximum nitrogen growth concentration, buds and new leaves
  file_veg[21,2] <- matrix_values[i,28] # Zone 1, Minimum nitrogen growth concentration, buds and new leaves
  file_veg[483,2] <- matrix_values[i,28] # Zone 2, Minimum nitrogen growth concentration, buds and new leaves
  file_veg[945,2] <- matrix_values[i,28] # Zone 3, Minimum nitrogen growth concentration, buds and new leaves
  file_veg[21,3] <- matrix_values[i,23] # Zone 1, Maximum nitrogen growth concentration, leaves
  file_veg[483,3] <- matrix_values[i,23] # Zone 2, Maximum nitrogen growth concentration, leaves
  file_veg[945,3] <- matrix_values[i,23] # Zone 3, Maximum nitrogen growth concentration, leaves
  file_veg[21,4] <- matrix_values[i,29] # Zone 1, Minimum nitrogen growth concentration, leaves
  file_veg[483,4] <- matrix_values[i,29] # Zone 2, Minimum nitrogen growth concentration, leaves
  file_veg[945,4] <- matrix_values[i,29] # Zone 3, Minimum nitrogen growth concentration, leaves
  file_veg[21,5] <- matrix_values[i,25] # Zone 1, Maximum nitrogen growth concentration, branches
  file_veg[483,5] <- matrix_values[i,25] # Zone 2, Maximum nitrogen growth concentration, branches
  file_veg[945,5] <- matrix_values[i,25] # Zone 3, Maximum nitrogen growth concentration, branches
  file_veg[21,6] <- matrix_values[i,31] # Zone 1, Minimum nitrogen growth concentration, branches
  file_veg[483,6] <- matrix_values[i,31] # Zone 2, Minimum nitrogen growth concentration, branches
  file_veg[945,6] <- matrix_values[i,31] # Zone 3, Minimum nitrogen growth concentration, branches
  file_veg[21,7] <- matrix_values[i,24] # Zone 1, Maximum nitrogen growth concentration, stems
  file_veg[483,7] <- matrix_values[i,24] # Zone 2, Maximum nitrogen growth concentration, stems
  file_veg[945,7] <- matrix_values[i,24] # Zone 3, Maximum nitrogen growth concentration, stems
  file_veg[21,8] <- matrix_values[i,30] # Zone 1, Minimum nitrogen growth concentration, stems
  file_veg[483,8] <- matrix_values[i,30] # Zone 2, Minimum nitrogen growth concentration, stems
  file_veg[945,8] <- matrix_values[i,30] # Zone 3, Minimum nitrogen growth concentration, stems
  file_veg[21,9] <- matrix_values[i,26] # Zone 1, Maximum nitrogen growth concentration, coarse roots
  file_veg[483,9] <- matrix_values[i,26] # Zone 2, Maximum nitrogen growth concentration, coarse roots
  file_veg[945,9] <- matrix_values[i,26] # Zone 3, Maximum nitrogen growth concentration, coarse roots
  file_veg[21,10] <- matrix_values[i,32] # Zone 1, Minimum nitrogen growth concentration, coarse roots
  file_veg[483,10] <- matrix_values[i,32] # Zone 2, Minimum nitrogen growth concentration, coarse roots
  file_veg[945,10] <- matrix_values[i,32] # Zone 3, Minimum nitrogen growth concentration, coarse roots
  file_veg[21,11] <- matrix_values[i,27] # Zone 1, Maximum nitrogen growth concentration, fine roots
  file_veg[483,11] <- matrix_values[i,27] # Zone 2, Maximum nitrogen growth concentration, fine roots
  file_veg[945,11] <- matrix_values[i,27] # Zone 3, Maximum nitrogen growth concentration, fine roots
  file_veg[21,12] <- matrix_values[i,33] # Zone 1, Minimum nitrogen growth concentration, fine roots
  file_veg[483,12] <- matrix_values[i,33] # Zone 2, Minimum nitrogen growth concentration, fine roots
  file_veg[945,12] <- matrix_values[i,33] # Zone 3, Minimum nitrogen growth concentration, fine roots
  file_veg[6,2] <- matrix_values[i,34] # Zone 1, Maximum rooting depth
  file_veg[468,2] <- matrix_values[i,34] # Zone 2, Maximum rooting depth
  file_veg[930,2] <- matrix_values[i,34] # Zone 3, Maximum rooting depth
  file_veg[6,4] <- matrix_values[i,35] # Zone 1, Actual rooting depth
  file_veg[468,4] <- matrix_values[i,35] # Zone 2, Actual rooting depth
  file_veg[930,4] <- matrix_values[i,35] # Zone 3, Actual rooting depth
  
  # Rate inputs
  file_rte[1:12,1] <- matrix_values[i,37] # Zones 1-3, Residue Structural C Release Factor for Litter ( l = 1) to Layer 3 (l = 4)
  file_rte[1:12,2] <- matrix_values[i,36] # Zones 1-3, Residue Metabolic C Release Factor for Litter (m = 1) to Layer 3 (m = 4)
  file_rte[1:12,3] <- matrix_values[i,38] # Zones 1-3, Humus Active C Release Factor for Litter (n = 1) to Layer 3 (n = 4)
  file_rte[1:12,4] <- matrix_values[i,39] # Zones 1-3, Humus Slow C Release Factor for Litter (o = 1) to Layer 3 (o = 4)
  file_rte[1:12,5] <- matrix_values[i,40] # Zones 1-3, Humus Passive C Release Factor for Litter (m = 1) to Layer 3 (m = 4)
  file_rte[13,1:4] <- matrix_values[i,41] # Zone 1 Denitrification Rate Constant
  file_rte[14,1:4] <- matrix_values[i,41] # Zone 2 Denitrification Rate Constant
  file_rte[15,1:4] <- matrix_values[i,41] # Zone 3 Denitrification Rate Constant
  
  write.table(file_buf, file= paste0(path, "\\Input_Files\\BUF\\SensAnalysis_",mytime, "_", i,".buf"), quote = FALSE, na = "", row.names = FALSE, col.names = FALSE)
  write.table(file_veg, file= paste0(path, "\\Input_Files\\VEG\\SensAnalysis_",mytime, "_", i,".veg"), quote = FALSE, na = "", row.names = FALSE, col.names = FALSE)
  write.table(file_rte, file= paste0(path, "\\Input_Files\\RTE\\SensAnalysis_",mytime, "_", i,".rte"), quote = FALSE, na = "", row.names = FALSE, col.names = FALSE)
  
  file_prj[10,1] <- paste0(path, "\\Input_Files\\BUF\\SensAnalysis_",mytime,"_", i,".buf")
  file_prj[11,1] <- paste0(path, "\\Input_Files\\VEG\\SensAnalysis_",mytime,"_", i,".veg")
  file_prj[12,1] <- paste0(path, "\\Input_Files\\RTE\\SensAnalysis_",mytime,"_", i,".rte")
  file_prj[13,1] <- paste0(path, "\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_",mytime,"_", i,".OUT")
  file_prj[14,1] <- paste0(path, "\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_",mytime,"_", i,".DTB")
  file_prj[15,1] <- paste0(path, "\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_",mytime,"_", i,".MTB")
  file_prj[16,1] <- paste0(path, "\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_",mytime,"_", i,".YTB")
  file_prj[17,1] <- paste0(path, "\\Outputs\\PSTBaseFieldInputNewDeg_SensAnalysis_",mytime,"_", i,".ETB")
  file_prj[19,1] <- paste0(path, "\\Outputs\\Nitrogen_SensAnalysis_",mytime,"_", i,".bal")
  file_prj[20,1] <- paste0(path, "\\Outputs\\Erosion_SensAnalysis_",mytime,"_", i,".bal")
  file_prj[21,1] <- paste0(path, "\\Outputs\\Hydrology_SensAnalysis_",mytime,"_", i,".bal")
  file_prj[22,1] <- paste0(path, "\\Outputs\\Vegetation_SensAnalysis_",mytime,"_", i,".bal")
  
  write.table(file_prj, file=paste0(path, "\\Input_Files\\PRJ\\SensAnalysis_",mytime, "_", i,".prj"), quote = FALSE, na = "", row.names = FALSE, col.names = FALSE)
  
  i = i+1
}

# Create a batch file (.bat) to run all projects sequentially from the command line
batch_matrix <- matrix(nrow = n_sims, ncol = 2)
batch_matrix[,1] <- "c:\\remm2019\\remm2019\\bin\\debug\\remm2019.exe" # REMM executable location
batch_matrix[,2] <- list.files(path = paste0(path,"\\Input_Files\\PRJ\\"), pattern = mytime, full.names = TRUE)
write.table(batch_matrix, file=paste0(path, "\\", "SensAnalysis_batch_", mytime, ".txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(batch_matrix, file=paste0(path, "\\", "SensAnalysis_batch_", mytime, ".bat"), quote = FALSE, row.names = FALSE, col.names = FALSE)
# Run this batch file in the command window to complete all simulations