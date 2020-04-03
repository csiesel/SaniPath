%macro removeOldFile(bye); %if %sysfunc(exist(&bye.)) %then %do; proc delete data=&bye.; run; %end; %mend removeOldFile; %removeOldFile(work.redcap); data REDCAP; %let _EFIERR_ = 0;
infile 'SaniPathRiskAssessme_DATA_NOHDRS_2017-04-04_1747.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=1 ; 
	informat record_id $500. ;
	informat redcap_survey_identifier $500. ;
	informat my_first_instrument_timestamp $500. ;
	informat understood_consent best32. ;
	informat above_age18 best32. ;
	informat hh_dd $500. ;
	informat neighbor best32. ;
	informat address1 $500. ;
	informat address2 $500. ;
	informat address3 $500. ;
	informat address4 $500. ;
	informat hh_q1 best32. ;
	informat hh_q2 best32. ;
	informat hh_q3 best32. ;
	informat hh_q4 best32. ;
	informat hh_q5 best32. ;
	informat hh_q6 best32. ;
	informat hh_q7 best32. ;
	informat hh_q8 best32. ;
	informat hh_q9 best32. ;
	informat hh_q10 best32. ;
	informat hh_q11 best32. ;
	informat hh_q12 best32. ;
	informat hh_q13 best32. ;
	informat hh_q14 best32. ;
	informat hh_q14a $5000. ;
	informat hh_q15 best32. ;
	informat hh_q16 best32. ;
	informat hh_q17a best32. ;
	informat hh_q17b best32. ;
	informat hh_q17c best32. ;
	informat hh_q17d best32. ;
	informat hh_q17e best32. ;
	informat hh_q17f best32. ;
	informat hh_q17g best32. ;
	informat hh_q17h best32. ;
	informat hh_q17i best32. ;
	informat hh_q17j best32. ;
	informat hh_q17k best32. ;
	informat hh_q17l best32. ;
	informat hh_q18a best32. ;
	informat hh_q18b best32. ;
	informat hh_q18c best32. ;
	informat hh_q18d best32. ;
	informat hh_q18e best32. ;
	informat hh_q18f best32. ;
	informat hh_q18g best32. ;
	informat hh_q18h best32. ;
	informat hh_q18i best32. ;
	informat hh_q18j best32. ;
	informat hh_q18k best32. ;
	informat hh_q18l best32. ;
	informat hh_q18m best32. ;
	informat hh_q18n best32. ;
	informat hh_q18o best32. ;
	informat hh_q19 best32. ;
	informat hh_q20 best32. ;
	informat hh_q21___1 best32. ;
	informat hh_q21___2 best32. ;
	informat hh_q21___3 best32. ;
	informat hh_q21___4 best32. ;
	informat hh_q21a $500. ;
	informat hh_q22 best32. ;
	informat hh_q23 best32. ;
	informat hh_q24 best32. ;
	informat hh_q25___1 best32. ;
	informat hh_q25___2 best32. ;
	informat hh_q25___3 best32. ;
	informat hh_q25___4 best32. ;
	informat hh_q25___5 best32. ;
	informat hh_q26 best32. ;
	informat hh_q27 best32. ;
	informat age best32. ;
	informat gender best32. ;
	informat race best32. ;
	informat race_other $5000. ;
	informat hh_q28 best32. ;
	informat hh_q29 best32. ;
	informat hh_q29a $500. ;
	informat hh_q30 best32. ;
	informat hh_q31 best32. ;
	informat hh_q31a $500. ;
	informat hh_q31b $500. ;
	informat hh_q31c $500. ;
	informat my_first_instrument_complete best32. ;

	format record_id $500. ;
	format redcap_survey_identifier $500. ;
	format my_first_instrument_timestamp $500. ;
	format understood_consent best12. ;
	format above_age18 best12. ;
	format hh_dd $500. ;
	format neighbor best12. ;
	format address1 $500. ;
	format address2 $500. ;
	format address3 $500. ;
	format address4 $500. ;
	format hh_q1 best12. ;
	format hh_q2 best12. ;
	format hh_q3 best12. ;
	format hh_q4 best12. ;
	format hh_q5 best12. ;
	format hh_q6 best12. ;
	format hh_q7 best12. ;
	format hh_q8 best12. ;
	format hh_q9 best12. ;
	format hh_q10 best12. ;
	format hh_q11 best12. ;
	format hh_q12 best12. ;
	format hh_q13 best12. ;
	format hh_q14 best12. ;
	format hh_q14a $5000. ;
	format hh_q15 best12. ;
	format hh_q16 best12. ;
	format hh_q17a best12. ;
	format hh_q17b best12. ;
	format hh_q17c best12. ;
	format hh_q17d best12. ;
	format hh_q17e best12. ;
	format hh_q17f best12. ;
	format hh_q17g best12. ;
	format hh_q17h best12. ;
	format hh_q17i best12. ;
	format hh_q17j best12. ;
	format hh_q17k best12. ;
	format hh_q17l best12. ;
	format hh_q18a best12. ;
	format hh_q18b best12. ;
	format hh_q18c best12. ;
	format hh_q18d best12. ;
	format hh_q18e best12. ;
	format hh_q18f best12. ;
	format hh_q18g best12. ;
	format hh_q18h best12. ;
	format hh_q18i best12. ;
	format hh_q18j best12. ;
	format hh_q18k best12. ;
	format hh_q18l best12. ;
	format hh_q18m best12. ;
	format hh_q18n best12. ;
	format hh_q18o best12. ;
	format hh_q19 best12. ;
	format hh_q20 best12. ;
	format hh_q21___1 best12. ;
	format hh_q21___2 best12. ;
	format hh_q21___3 best12. ;
	format hh_q21___4 best12. ;
	format hh_q21a $500. ;
	format hh_q22 best12. ;
	format hh_q23 best12. ;
	format hh_q24 best12. ;
	format hh_q25___1 best12. ;
	format hh_q25___2 best12. ;
	format hh_q25___3 best12. ;
	format hh_q25___4 best12. ;
	format hh_q25___5 best12. ;
	format hh_q26 best12. ;
	format hh_q27 best12. ;
	format age best12. ;
	format gender best12. ;
	format race best12. ;
	format race_other $5000. ;
	format hh_q28 best12. ;
	format hh_q29 best12. ;
	format hh_q29a $500. ;
	format hh_q30 best12. ;
	format hh_q31 best12. ;
	format hh_q31a $500. ;
	format hh_q31b $500. ;
	format hh_q31c $500. ;
	format my_first_instrument_complete best12. ;

input
		record_id $
		redcap_survey_identifier $
		my_first_instrument_timestamp $
		understood_consent
		above_age18
		hh_dd $
		neighbor
		address1 $
		address2 $
		address3 $
		address4 $
		hh_q1
		hh_q2
		hh_q3
		hh_q4
		hh_q5
		hh_q6
		hh_q7
		hh_q8
		hh_q9
		hh_q10
		hh_q11
		hh_q12
		hh_q13
		hh_q14
		hh_q14a $
		hh_q15
		hh_q16
		hh_q17a
		hh_q17b
		hh_q17c
		hh_q17d
		hh_q17e
		hh_q17f
		hh_q17g
		hh_q17h
		hh_q17i
		hh_q17j
		hh_q17k
		hh_q17l
		hh_q18a
		hh_q18b
		hh_q18c
		hh_q18d
		hh_q18e
		hh_q18f
		hh_q18g
		hh_q18h
		hh_q18i
		hh_q18j
		hh_q18k
		hh_q18l
		hh_q18m
		hh_q18n
		hh_q18o
		hh_q19
		hh_q20
		hh_q21___1
		hh_q21___2
		hh_q21___3
		hh_q21___4
		hh_q21a $
		hh_q22
		hh_q23
		hh_q24
		hh_q25___1
		hh_q25___2
		hh_q25___3
		hh_q25___4
		hh_q25___5
		hh_q26
		hh_q27
		age
		gender
		race
		race_other $
		hh_q28
		hh_q29
		hh_q29a $
		hh_q30
		hh_q31
		hh_q31a $
		hh_q31b $
		hh_q31c $
		my_first_instrument_complete
;
if _ERROR_ then call symput('_EFIERR_',"1");
run;

proc contents;run;


data redcap;
	set redcap;
	label record_id='Record ID';
	label redcap_survey_identifier='Survey Identifier';
	label my_first_instrument_timestamp='Survey Timestamp';
	label understood_consent='I have read the above consent form.  ';
	label above_age18='I certify that I am 18 years old or older.  ';
	label hh_dd='Please provide todays date (Month, Day, Year)';
	label neighbor='Which neighborhood are you from?';
	label address1='Please provide your address: Address ';
	label address2='City/ Town';
	label address3='State';
	label address4='ZIP/ Postal Code';
	label hh_q1='Do you have children between the ages of 5-12? ';
	label hh_q2='Think about whether you ever go into rivers, lakes, streams, creeks, or ponds in your neighborhood. This includes wading, swimming, splashing around, and fishing. How often do you go into the rivers, lakes, streams, creeks or ponds?  ';
	label hh_q3='Think about whether your children ever go into the rivers, lakes, streams, creeks, or ponds in your neighborhood. This includes wading, swimming, splashing around, and fishing. How often do your children go into the rivers, lakes, streams, creeks, or ponds?';
	label hh_q4='How often do you come into contact with floodwater during the rainy season?   Floodwater may be stagnant water, such as puddles after heavy rain, overflowing storm drains or flooded creeks or rivers.  ';
	label hh_q5='How often do your children come into contact with floodwater during the rainy season?   Floodwater may be stagnant water, such as puddles after heavy rain, overflowing storm drains or flooded creeks or rivers.';
	label hh_q6='How many days a week do you drink from a public water source?  A public water source includes public water fountains but does not include a water cooler or bottled water. ';
	label hh_q7='How many days a week do your children drink from a public water source?   A public water source includes public water fountains but does not include a water cooler or bottled water. ';
	label hh_q8='How many days during the week do you eat produce that is raw (uncooked)?   For this question, we are referring to any fruit or vegetable that does not grow on a tree, and that does not have a peel or shell. Please think about produce you eat whole and produce you cut up but eat raw. For example lettuce, tomatoes, bell peppers. ';
	label hh_q9='How many days during the week do your children eat produce that is raw (uncooked)?   For this question, we are referring to any fruit or vegetable that does not grow on a tree, and that does not have a peel or shell. Please think about produce you eat whole and produce you cut up but eat raw. For example lettuce, tomatoes, bell peppers.';
	label hh_q10='How often do you use a public toilet?   A public toilet includes a toilet in a public space such as a restaurant or gas station, or a toilet at school or work. ';
	label hh_q11='How often do your children use a public toilet?   A public toilet includes a toilet in a public space such as a restaurant or gas station, or a toilet at school or work. ';
	label hh_q12='Do you own the property where you are living?';
	label hh_q13='Are you renting the house/apartment/property where you are living?';
	label hh_q14='What type of property is it?';
	label hh_q14a='If other, specify';
	label hh_q15='How many people live with you?';
	label hh_q16='How long have you been living at your current address?';
	label hh_q17a='Flood water is safe to drink';
	label hh_q17b='Flood water can cause illnesses';
	label hh_q17c='Contact with flood water should be avoided';
	label hh_q17d='Flood water contains germs, which make me sick';
	label hh_q17e='Bleach solution should be used to clean surfaces after a flood event';
	label hh_q17f='During a flooding event, tap water in my house/apartment is safe to drink';
	label hh_q17g='There is no risk to my property during a flooding event';
	label hh_q17h='Flood water contains dangerous chemicals';
	label hh_q17i='Flood water contains sewage';
	label hh_q17j='There is no health risk associated with flood water';
	label hh_q17k='During a flooding event, I am worried about the safety of the drinking water';
	label hh_q17l='Sewage and storm water overflow after a heavy rainfall cause the flooding in my area';
	label hh_q18a='Flooding is a major issue in my community';
	label hh_q18b='My house/apartment floods regularly';
	label hh_q18c='My yard floods regularly';
	label hh_q18d='The streets near my house/apartment flood regularly';
	label hh_q18e='During a flooding event, I walk through the flood water';
	label hh_q18f='During a flooding event, my children play in the flood water';
	label hh_q18g='During a flooding event, my pets play in the flood water';
	label hh_q18h='During a flooding event, my pets drink the flood water';
	label hh_q18i='Flood water causes damage other than mold to my house/apartment';
	label hh_q18j='Flood water causes mold to grow in the flooded parts of the house/apartment';
	label hh_q18k='I wear protective equipment such as waterproof boots and plastic or rubber gloves when cleaning up after a flooding event';
	label hh_q18l='My tap water changes color during a flooding event';
	label hh_q18m='I boil my tap water when flooding occurs';
	label hh_q18n='After my house/apartment floods, I use bleach or Lysol to clean all the surfaces';
	label hh_q18o='I wash my hands with soap and water after cleaning up flood water';
	label hh_q19='Have you previously experienced any flooding in your neighborhood?';
	label hh_q20='How often do you hear about flooding in your neighborhood each year? ';
	label hh_q21___1='When it is flooding in your area, which of the following areas flood: (choice=Street nearby the property/house/ apartment)';
	label hh_q21___2='When it is flooding in your area, which of the following areas flood: (choice=Yard)';
	label hh_q21___3='When it is flooding in your area, which of the following areas flood: (choice=House)';
	label hh_q21___4='When it is flooding in your area, which of the following areas flood: (choice=Other)';
	label hh_q21a='If other, specify';
	label hh_q22='How often does the street(s) by your house flood in one year?';
	label hh_q23='How often does your yard flood in one year?';
	label hh_q24='How often does your house flood in one year?';
	label hh_q25___1='If flooding occurs, ... (choice=I walk through flooded areas)';
	label hh_q25___2='If flooding occurs, ... (choice=My children play in flood water)';
	label hh_q25___3='If flooding occurs, ... (choice=I have direct skin contact with flood water)';
	label hh_q25___4='If flooding occurs, ... (choice=I touch the flood water with my hands)';
	label hh_q25___5='If flooding occurs, ... (choice=None of the above)';
	label hh_q26='During a flooding event, I spend __ minutes walking through flood water.';
	label hh_q27='During a flooding event, my children play __ minutes in flood water.';
	label age='How old are you?';
	label gender='What is your gender?';
	label race='What is your race or ethnicity?  ';
	label race_other='If other, specify';
	label hh_q28='What is the highest level of education that you have completed?';
	label hh_q29='Do you have a job?';
	label hh_q29a='If other, specify';
	label hh_q30='Which of the following describes your household income per year? ';
	label hh_q31='Are you willing to be contacted via email/ phone for follow up questions?';
	label hh_q31a='If yes, please provide your email or phone number for follow up questions  Name (optional):   ';
	label hh_q31b='Email:';
	label hh_q31c='Phone:';
	label my_first_instrument_complete='Complete?';
	run;

proc format;
	value understood_consent_ 1='Yes' 0='No';
	value above_age18_ 1='Yes' 0='No';
	value neighbor_ 1='Peoplestown' 2='English Ave' 
		3='Proctor Creek';
	value hh_q1_ 1='Yes' 0='No';
	value hh_q2_ 1='More than 10 times total every month.' 2='6 to 10 times total every month.' 
		3='1 to 5 times total every month.' 4='I never go into the rivers, lakes, streams, creeks or ponds.';
	value hh_q3_ 1='More than 10 times total every month.' 2='6 to 10 times total every month.' 
		3='1 to 5 times total every month.' 4='My children never go into the rivers, lakes, streams, creeks or ponds.' 
		5='I do not known how often my children go into the rivers, lakes, streams, creeks or ponds.';
	value hh_q4_ 1='More than 10 times total every month during the rainy season.' 2='6 to 10 times total every month during the rainy season.' 
		3='1 to 5 times total every month during the rainy season.' 4='I never come into contact with floodwater.';
	value hh_q5_ 1='More than 10 times total every month during the rainy season.' 2='6 to 10 times total every month during the rainy season.' 
		3='1 to 5 times total every month during the rainy season.' 4='My children never come into contact with floodwater.' 
		5='I do not know how often my children come into contact with floodwater.';
	value hh_q6_ 1='Everyday.' 2='4 to 6 days a week.' 
		3='1 to 3 days a week.' 4='I never drink water from a public water source.' 
		5='I do not know if I drink water from a public water source.';
	value hh_q7_ 1='Everyday.' 2='4 to 6 days a week.' 
		3='1 to 3 days a week.' 4='My children never drink water from a public water source.' 
		5='I do not know how often my children drink water from a public water source.';
	value hh_q8_ 1='Everyday.' 2='4 to 6 days a week.' 
		3='1 to 3 days a week.' 4='I never eat raw produce.';
	value hh_q9_ 1='Everyday.' 2='4 to 6 days a week.' 
		3='1 to 3 days a week.' 4='My children never eat raw produce.' 
		5='I do not know if my children eat raw produce.';
	value hh_q10_ 1='More than 10 times total every week.' 2='6 to 10 times total every week.' 
		3='1 to 5 times total every week.' 4='I never use a public toilet.';
	value hh_q11_ 1='More than 10 times total every week.' 2='6 to 10 times total every week.' 
		3='1 to 5 times total every week.' 4='My children never use a public toilet.' 
		5='I do not know how often my children use a public toilet.';
	value hh_q12_ 1='Yes' 0='No';
	value hh_q13_ 1='Yes' 0='No';
	value hh_q14_ 1='Residential (House/Apartment)' 2='Business' 
		3='Other';
	value hh_q15_ 1='0-2' 2='3-5' 
		3='6-8' 4='More than 9';
	value hh_q16_ 1='less than one year' 2='1-5 years' 
		3='6-9 years' 4='10-14 years' 
		5='15-19 years' 6='Longer than 20 years';
	value hh_q17a_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q17b_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q17c_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q17d_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q17e_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q17f_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q17g_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q17h_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q17i_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q17j_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q17k_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q17l_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18a_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18b_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18c_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18d_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18e_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18f_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18g_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18h_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18i_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18j_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18k_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18l_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18m_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18n_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q18o_ 1='Strongly Agree' 2='Agree' 
		3='Neutral' 4='Disagree' 
		5='Strongly disagree' 6='Not applicable';
	value hh_q19_ 1='Yes' 0='No';
	value hh_q20_ 1='Never' 2='1 or 2 times each year' 
		3='3-6 times each year' 4='7-12 times each year' 
		5='More than 12 times each year';
	value hh_q21___1_ 0='Unchecked' 1='Checked';
	value hh_q21___2_ 0='Unchecked' 1='Checked';
	value hh_q21___3_ 0='Unchecked' 1='Checked';
	value hh_q21___4_ 0='Unchecked' 1='Checked';
	value hh_q22_ 1='Never' 2='1-2 times each year' 
		3='3-6 times each year' 4='7-12 times each year' 
		5='More than 12 times';
	value hh_q23_ 1='Never' 2='1-2 times each year' 
		3='3-6 times each year' 4='7-12 times each year' 
		5='More than 12 times';
	value hh_q24_ 1='Never' 2='1-2 times each year' 
		3='3-6 times each year' 4='7-12 times each year' 
		5='More than 12 times';
	value hh_q25___1_ 0='Unchecked' 1='Checked';
	value hh_q25___2_ 0='Unchecked' 1='Checked';
	value hh_q25___3_ 0='Unchecked' 1='Checked';
	value hh_q25___4_ 0='Unchecked' 1='Checked';
	value hh_q25___5_ 0='Unchecked' 1='Checked';
	value hh_q26_ 1='I don''t walk in flood water' 2='1-10 minutes' 
		3='11-30 minutes' 4='31-60 minutes' 
		5='More than one hour';
	value hh_q27_ 1='My children don''t play in flood water' 2='1-10 minutes' 
		3='11-30 minutes' 4='31-60 minutes' 
		5='More than one hour';
	value age_ 1='Under 12 years' 2='12 - 17 years old' 
		3='18 - 24 years old' 4='25 - 34 years old' 
		5='35 - 44 years old' 6='45 to 54 years old' 
		7='55 - 64 years old' 8='64 - 74 years old' 
		9='75 years or older';
	value gender_ 1='Female' 2='Male' 
		3='Prefer not to answer';
	value race_ 1='White' 2='Hispanic or Latino' 
		3='Black or African American' 4='Native American or American Indian' 
		5='Asian / Pacific Islander' 6='Other' 
		7='Prefer not to answer';
	value hh_q28_ 1='No schooling completed' 2='Nursery school to 8th grade' 
		3='Some high school, no diploma' 4='High school graduate, diploma or the equivalent (for example: GED)' 
		5='Some college credit, no degree' 6='Trade/technical/vocational training' 
		7='College degree' 8='Post-graduate degree';
	value hh_q29_ 1='Yes' 2='No' 
		3='Retired' 4='Other';
	value hh_q30_ 1='Less than $15,000' 2='$15,001 - $30,000' 
		3='$30,001 - $50,000' 4='$50,001 - $75,000' 
		5='More than $75,000';
	value hh_q31_ 1='Yes' 0='No';
	value my_first_instrument_complete_ 0='Incomplete' 1='Unverified' 
		2='Complete';
	run;

data redcap;
	set redcap;

	format understood_consent understood_consent_.;
	format above_age18 above_age18_.;
	format neighbor neighbor_.;
	format hh_q1 hh_q1_.;
	format hh_q2 hh_q2_.;
	format hh_q3 hh_q3_.;
	format hh_q4 hh_q4_.;
	format hh_q5 hh_q5_.;
	format hh_q6 hh_q6_.;
	format hh_q7 hh_q7_.;
	format hh_q8 hh_q8_.;
	format hh_q9 hh_q9_.;
	format hh_q10 hh_q10_.;
	format hh_q11 hh_q11_.;
	format hh_q12 hh_q12_.;
	format hh_q13 hh_q13_.;
	format hh_q14 hh_q14_.;
	format hh_q15 hh_q15_.;
	format hh_q16 hh_q16_.;
	format hh_q17a hh_q17a_.;
	format hh_q17b hh_q17b_.;
	format hh_q17c hh_q17c_.;
	format hh_q17d hh_q17d_.;
	format hh_q17e hh_q17e_.;
	format hh_q17f hh_q17f_.;
	format hh_q17g hh_q17g_.;
	format hh_q17h hh_q17h_.;
	format hh_q17i hh_q17i_.;
	format hh_q17j hh_q17j_.;
	format hh_q17k hh_q17k_.;
	format hh_q17l hh_q17l_.;
	format hh_q18a hh_q18a_.;
	format hh_q18b hh_q18b_.;
	format hh_q18c hh_q18c_.;
	format hh_q18d hh_q18d_.;
	format hh_q18e hh_q18e_.;
	format hh_q18f hh_q18f_.;
	format hh_q18g hh_q18g_.;
	format hh_q18h hh_q18h_.;
	format hh_q18i hh_q18i_.;
	format hh_q18j hh_q18j_.;
	format hh_q18k hh_q18k_.;
	format hh_q18l hh_q18l_.;
	format hh_q18m hh_q18m_.;
	format hh_q18n hh_q18n_.;
	format hh_q18o hh_q18o_.;
	format hh_q19 hh_q19_.;
	format hh_q20 hh_q20_.;
	format hh_q21___1 hh_q21___1_.;
	format hh_q21___2 hh_q21___2_.;
	format hh_q21___3 hh_q21___3_.;
	format hh_q21___4 hh_q21___4_.;
	format hh_q22 hh_q22_.;
	format hh_q23 hh_q23_.;
	format hh_q24 hh_q24_.;
	format hh_q25___1 hh_q25___1_.;
	format hh_q25___2 hh_q25___2_.;
	format hh_q25___3 hh_q25___3_.;
	format hh_q25___4 hh_q25___4_.;
	format hh_q25___5 hh_q25___5_.;
	format hh_q26 hh_q26_.;
	format hh_q27 hh_q27_.;
	format age age_.;
	format gender gender_.;
	format race race_.;
	format hh_q28 hh_q28_.;
	format hh_q29 hh_q29_.;
	format hh_q30 hh_q30_.;
	format hh_q31 hh_q31_.;
	format my_first_instrument_complete my_first_instrument_complete_.;
	run;

proc contents data=redcap;
proc print data=redcap;
run;
quit;