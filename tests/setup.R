if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
lodown( "pisa" , output_dir = file.path( getwd() ) )
library(lodown)
# examine all available PISA microdata files
pisa_cat <-
	get_catalog( "pisa" ,
		output_dir = file.path( getwd() ) )

# 2015 only
pisa_cat <- subset( pisa_cat , year == 2015 )
# download the microdata to your local computer


library(DBI)
library(RSQLite)
library(survey)
library(mitools)

pisa_design <- readRDS( file.path( getwd() , "2015 cmb_stu_qqq design.rds" ) )

pisa_design <- lodown:::svyMDBdesign( pisa_design )
pisa_design <- 
	update( 
		pisa_design , 
		
		gender = factor( st004d01t , labels = c( "male" , "female" ) ) ,
		
		how_many_computers_at_home = 
			factor( 
				st012q06na , 
				labels = c( "none" , "one" , "two" , "three or more" ) 
			)
 
	)
MIcombine( with( pisa_design , svyby( ~ one , ~ one , unwtd.count ) ) )

MIcombine( with( pisa_design , svyby( ~ one , ~ gender , unwtd.count ) ) )
MIcombine( with( pisa_design , svytotal( ~ one ) ) )

MIcombine( with( pisa_design ,
	svyby( ~ one , ~ gender , svytotal )
) )
MIcombine( with( pisa_design , svymean( ~ scie ) ) )

MIcombine( with( pisa_design ,
	svyby( ~ scie , ~ gender , svymean )
) )
MIcombine( with( pisa_design , svymean( ~ how_many_computers_at_home ) ) )

MIcombine( with( pisa_design ,
	svyby( ~ how_many_computers_at_home , ~ gender , svymean )
) )
MIcombine( with( pisa_design , svytotal( ~ scie ) ) )

MIcombine( with( pisa_design ,
	svyby( ~ scie , ~ gender , svytotal )
) )
MIcombine( with( pisa_design , svytotal( ~ how_many_computers_at_home ) ) )

MIcombine( with( pisa_design ,
	svyby( ~ how_many_computers_at_home , ~ gender , svytotal )
) )
MIcombine( with( pisa_design , svyquantile( ~ scie , 0.5 , se = TRUE ) ) )

MIcombine( with( pisa_design ,
	svyby( 
		~ scie , ~ gender , svyquantile , 0.5 ,
		se = TRUE , keep.var = TRUE , ci = TRUE 
) ) )
MIcombine( with( pisa_design ,
	svyratio( numerator = ~ math , denominator = ~ reading )
) )
sub_pisa_design <- subset( pisa_design , cnt == "ALB" )
MIcombine( with( sub_pisa_design , svymean( ~ scie ) ) )
this_result <-
	MIcombine( with( pisa_design ,
		svymean( ~ scie )
	) )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	MIcombine( with( pisa_design ,
		svyby( ~ scie , ~ gender , svymean )
	) )

coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( pisa_design$designs[[1]] )
MIcombine( with( pisa_design , svyvar( ~ scie ) ) )
# SRS without replacement
MIcombine( with( pisa_design ,
	svymean( ~ scie , deff = TRUE )
) )

# SRS with replacement
MIcombine( with( pisa_design ,
	svymean( ~ scie , deff = "replace" )
) )
lodown:::MIsvyciprop( ~ oecd , pisa_design ,
	method = "likelihood" )
lodown:::MIsvyttest( scie ~ oecd , pisa_design )
lodown:::MIsvychisq( ~ oecd + how_many_computers_at_home , pisa_design )
glm_result <- 
	MIcombine( with( pisa_design ,
		svyglm( scie ~ oecd + how_many_computers_at_home )
	) )
	
summary( glm_result )

