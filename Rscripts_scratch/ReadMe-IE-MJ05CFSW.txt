I am trying to keep straight what order I need to run r files in

For synoptic data:

1. Synoptic: these scripts combine synoptic measuremnts of CO2 concentration and flux collected in the field data by site
	Synoptic_ANTE_script_2022-04-19.R
		WriteOut: "Synoptic/ANTE_2022-04-19.csv"
	Synoptic_GAVI_script_2022-04-19.R
		WriteOut: "Synoptic/GAVI_2022-04-19.csv"
	Synoptic_COLMILLO_script_2022-04-19
		WriteOut: "Synoptic/COLMILLO_2022-04-19.csv"

2. Geomorphology: these script use the data collected with handheld GPS to map the stream lat, lon, and elevation. 
		a. Script interpolates in between gaps calculates distance between points. 
		b. It combines the synoptic data compiled in previous script. 
		c. also calculates slope
	ANTE_Geomorphology_2022-03-24.Rmd
		WriteOut: "ProcessedData/ANTE_synoptic_2022-04-19.csv"
		WriteOut: "ProcessedData/ANTE_WaterChem_synop_2022-04-19.csv"
	GAVI_Geomorphology_2022-02-16.Rmd
		WriteOut: "ProcessedData/GAVI_synoptic_2022-04-19.csv"
	COLM_Geomorphology_2022-02-17.Rmd
		WriteOut: "ProcessedData/COLMILLO_synoptic_2022-04-19.csv"
	GAVItrib1_Geomorphology_2022-03-30.Rmd
		WriteOut: "ProcessedData/GAVI_Trib1_synoptic_2022-04-19.csv"
	GAVItrib2_Geomorphology_2022-04-10.Rmd
		WriteOut: "ProcessedData/GAVI_Trib2_synoptic_2022-04-19.csv"
	GAVItrib3_Geomorphology_2022-04-10.Rmd
		WriteOut: "ProcessedData/GAVI_Trib3_synoptic_2022-04-19.csv"
3. More Geomorphology: This script combines geomorphology with width and distance data. It calculates a column for surface area represented by each CO2 measurment based on distance between points and width 
	ANTE_moreGeomorph_2022-02-16.Rmd
		WriteOut: "ProcessedData/ANTE_synopticGeom_2022-04-19.csv"
	GAVI_moreGeomorph_2022-02-16.Rmd
		WriteOut: "ProcessedData/GAVI_synopticGeom_2022-04-19.csv"
	COLM_moreGeomorph_2022-02-16.Rmd
		WriteOut: "ProcessedData/COLM_synopticGeom_2022-04-19.csv"

4. synoptic_k600_2022-02-11: this script calculates k600 for all sites
		WriteOut: "ProcessedData/ALL_synoptic_2022-04-19.csv"
		WriteOut: "ProcessedData/ALL_synopticNoNA_2022-04-19.csv" - this is for reading into arcpro

5. Put into ArcPro to get cathment size
		Process as in Stnoptic_FlowAccumulation.txt
