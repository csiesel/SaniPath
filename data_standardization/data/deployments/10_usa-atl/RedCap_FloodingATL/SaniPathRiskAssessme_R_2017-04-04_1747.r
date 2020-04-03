#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('SaniPathRiskAssessme_DATA_2017-04-04_1747.csv')
#Setting Labels

label(data$record_id)="Record ID"
label(data$redcap_survey_identifier)="Survey Identifier"
label(data$my_first_instrument_timestamp)="Survey Timestamp"
label(data$understood_consent)="I have read the above consent form.  "
label(data$above_age18)="I certify that I am 18 years old or older.  "
label(data$hh_dd)="Please provide todays date (Month, Day, Year)"
label(data$neighbor)="Which neighborhood are you from?"
label(data$address1)="Please provide your address: Address "
label(data$address2)="City/ Town"
label(data$address3)="State"
label(data$address4)="ZIP/ Postal Code"
label(data$hh_q1)="Do you have children between the ages of 5-12? "
label(data$hh_q2)="Think about whether you ever go into rivers, lakes, streams, creeks, or ponds in your neighborhood. This includes wading, swimming, splashing around, and fishing. How often do you go into the rivers, lakes, streams, creeks or ponds?  "
label(data$hh_q3)="Think about whether your children ever go into the rivers, lakes, streams, creeks, or ponds in your neighborhood. This includes wading, swimming, splashing around, and fishing. How often do your children go into the rivers, lakes, streams, creeks, or ponds?"
label(data$hh_q4)="How often do you come into contact with floodwater during the rainy season?   Floodwater may be stagnant water, such as puddles after heavy rain, overflowing storm drains or flooded creeks or rivers.  "
label(data$hh_q5)="How often do your children come into contact with floodwater during the rainy season?   Floodwater may be stagnant water, such as puddles after heavy rain, overflowing storm drains or flooded creeks or rivers."
label(data$hh_q6)="How many days a week do you drink from a public water source?  A public water source includes public water fountains but does not include a water cooler or bottled water. "
label(data$hh_q7)="How many days a week do your children drink from a public water source?   A public water source includes public water fountains but does not include a water cooler or bottled water. "
label(data$hh_q8)="How many days during the week do you eat produce that is raw (uncooked)?   For this question, we are referring to any fruit or vegetable that does not grow on a tree, and that does not have a peel or shell. Please think about produce you eat whole and produce you cut up but eat raw. For example lettuce, tomatoes, bell peppers. "
label(data$hh_q9)="How many days during the week do your children eat produce that is raw (uncooked)?   For this question, we are referring to any fruit or vegetable that does not grow on a tree, and that does not have a peel or shell. Please think about produce you eat whole and produce you cut up but eat raw. For example lettuce, tomatoes, bell peppers."
label(data$hh_q10)="How often do you use a public toilet?   A public toilet includes a toilet in a public space such as a restaurant or gas station, or a toilet at school or work. "
label(data$hh_q11)="How often do your children use a public toilet?   A public toilet includes a toilet in a public space such as a restaurant or gas station, or a toilet at school or work. "
label(data$hh_q12)="Do you own the property where you are living?"
label(data$hh_q13)="Are you renting the house/apartment/property where you are living?"
label(data$hh_q14)="What type of property is it?"
label(data$hh_q14a)="If other, specify"
label(data$hh_q15)="How many people live with you?"
label(data$hh_q16)="How long have you been living at your current address?"
label(data$hh_q17a)="Flood water is safe to drink"
label(data$hh_q17b)="Flood water can cause illnesses"
label(data$hh_q17c)="Contact with flood water should be avoided"
label(data$hh_q17d)="Flood water contains germs, which make me sick"
label(data$hh_q17e)="Bleach solution should be used to clean surfaces after a flood event"
label(data$hh_q17f)="During a flooding event, tap water in my house/apartment is safe to drink"
label(data$hh_q17g)="There is no risk to my property during a flooding event"
label(data$hh_q17h)="Flood water contains dangerous chemicals"
label(data$hh_q17i)="Flood water contains sewage"
label(data$hh_q17j)="There is no health risk associated with flood water"
label(data$hh_q17k)="During a flooding event, I am worried about the safety of the drinking water"
label(data$hh_q17l)="Sewage and storm water overflow after a heavy rainfall cause the flooding in my area"
label(data$hh_q18a)="Flooding is a major issue in my community"
label(data$hh_q18b)="My house/apartment floods regularly"
label(data$hh_q18c)="My yard floods regularly"
label(data$hh_q18d)="The streets near my house/apartment flood regularly"
label(data$hh_q18e)="During a flooding event, I walk through the flood water"
label(data$hh_q18f)="During a flooding event, my children play in the flood water"
label(data$hh_q18g)="During a flooding event, my pets play in the flood water"
label(data$hh_q18h)="During a flooding event, my pets drink the flood water"
label(data$hh_q18i)="Flood water causes damage other than mold to my house/apartment"
label(data$hh_q18j)="Flood water causes mold to grow in the flooded parts of the house/apartment"
label(data$hh_q18k)="I wear protective equipment such as waterproof boots and plastic or rubber gloves when cleaning up after a flooding event"
label(data$hh_q18l)="My tap water changes color during a flooding event"
label(data$hh_q18m)="I boil my tap water when flooding occurs"
label(data$hh_q18n)="After my house/apartment floods, I use bleach or Lysol to clean all the surfaces"
label(data$hh_q18o)="I wash my hands with soap and water after cleaning up flood water"
label(data$hh_q19)="Have you previously experienced any flooding in your neighborhood?"
label(data$hh_q20)="How often do you hear about flooding in your neighborhood each year? "
label(data$hh_q21___1)="When it is flooding in your area, which of the following areas flood: (choice=Street nearby the property/house/ apartment)"
label(data$hh_q21___2)="When it is flooding in your area, which of the following areas flood: (choice=Yard)"
label(data$hh_q21___3)="When it is flooding in your area, which of the following areas flood: (choice=House)"
label(data$hh_q21___4)="When it is flooding in your area, which of the following areas flood: (choice=Other)"
label(data$hh_q21a)="If other, specify"
label(data$hh_q22)="How often does the street(s) by your house flood in one year?"
label(data$hh_q23)="How often does your yard flood in one year?"
label(data$hh_q24)="How often does your house flood in one year?"
label(data$hh_q25___1)="If flooding occurs, ... (choice=I walk through flooded areas)"
label(data$hh_q25___2)="If flooding occurs, ... (choice=My children play in flood water)"
label(data$hh_q25___3)="If flooding occurs, ... (choice=I have direct skin contact with flood water)"
label(data$hh_q25___4)="If flooding occurs, ... (choice=I touch the flood water with my hands)"
label(data$hh_q25___5)="If flooding occurs, ... (choice=None of the above)"
label(data$hh_q26)="During a flooding event, I spend __ minutes walking through flood water."
label(data$hh_q27)="During a flooding event, my children play __ minutes in flood water."
label(data$age)="How old are you?"
label(data$gender)="What is your gender?"
label(data$race)="What is your race or ethnicity?  "
label(data$race_other)="If other, specify"
label(data$hh_q28)="What is the highest level of education that you have completed?"
label(data$hh_q29)="Do you have a job?"
label(data$hh_q29a)="If other, specify"
label(data$hh_q30)="Which of the following describes your household income per year? "
label(data$hh_q31)="Are you willing to be contacted via email/ phone for follow up questions?"
label(data$hh_q31a)="If yes, please provide your email or phone number for follow up questions  Name (optional):   "
label(data$hh_q31b)="Email:"
label(data$hh_q31c)="Phone:"
label(data$my_first_instrument_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$understood_consent.factor = factor(data$understood_consent,levels=c("1","0"))
data$above_age18.factor = factor(data$above_age18,levels=c("1","0"))
data$neighbor.factor = factor(data$neighbor,levels=c("1","2","3"))
data$hh_q1.factor = factor(data$hh_q1,levels=c("1","0"))
data$hh_q2.factor = factor(data$hh_q2,levels=c("1","2","3","4"))
data$hh_q3.factor = factor(data$hh_q3,levels=c("1","2","3","4","5"))
data$hh_q4.factor = factor(data$hh_q4,levels=c("1","2","3","4"))
data$hh_q5.factor = factor(data$hh_q5,levels=c("1","2","3","4","5"))
data$hh_q6.factor = factor(data$hh_q6,levels=c("1","2","3","4","5"))
data$hh_q7.factor = factor(data$hh_q7,levels=c("1","2","3","4","5"))
data$hh_q8.factor = factor(data$hh_q8,levels=c("1","2","3","4"))
data$hh_q9.factor = factor(data$hh_q9,levels=c("1","2","3","4","5"))
data$hh_q10.factor = factor(data$hh_q10,levels=c("1","2","3","4"))
data$hh_q11.factor = factor(data$hh_q11,levels=c("1","2","3","4","5"))
data$hh_q12.factor = factor(data$hh_q12,levels=c("1","0"))
data$hh_q13.factor = factor(data$hh_q13,levels=c("1","0"))
data$hh_q14.factor = factor(data$hh_q14,levels=c("1","2","3"))
data$hh_q15.factor = factor(data$hh_q15,levels=c("1","2","3","4"))
data$hh_q16.factor = factor(data$hh_q16,levels=c("1","2","3","4","5","6"))
data$hh_q17a.factor = factor(data$hh_q17a,levels=c("1","2","3","4","5","6"))
data$hh_q17b.factor = factor(data$hh_q17b,levels=c("1","2","3","4","5","6"))
data$hh_q17c.factor = factor(data$hh_q17c,levels=c("1","2","3","4","5","6"))
data$hh_q17d.factor = factor(data$hh_q17d,levels=c("1","2","3","4","5","6"))
data$hh_q17e.factor = factor(data$hh_q17e,levels=c("1","2","3","4","5","6"))
data$hh_q17f.factor = factor(data$hh_q17f,levels=c("1","2","3","4","5","6"))
data$hh_q17g.factor = factor(data$hh_q17g,levels=c("1","2","3","4","5","6"))
data$hh_q17h.factor = factor(data$hh_q17h,levels=c("1","2","3","4","5","6"))
data$hh_q17i.factor = factor(data$hh_q17i,levels=c("1","2","3","4","5","6"))
data$hh_q17j.factor = factor(data$hh_q17j,levels=c("1","2","3","4","5","6"))
data$hh_q17k.factor = factor(data$hh_q17k,levels=c("1","2","3","4","5","6"))
data$hh_q17l.factor = factor(data$hh_q17l,levels=c("1","2","3","4","5","6"))
data$hh_q18a.factor = factor(data$hh_q18a,levels=c("1","2","3","4","5","6"))
data$hh_q18b.factor = factor(data$hh_q18b,levels=c("1","2","3","4","5","6"))
data$hh_q18c.factor = factor(data$hh_q18c,levels=c("1","2","3","4","5","6"))
data$hh_q18d.factor = factor(data$hh_q18d,levels=c("1","2","3","4","5","6"))
data$hh_q18e.factor = factor(data$hh_q18e,levels=c("1","2","3","4","5","6"))
data$hh_q18f.factor = factor(data$hh_q18f,levels=c("1","2","3","4","5","6"))
data$hh_q18g.factor = factor(data$hh_q18g,levels=c("1","2","3","4","5","6"))
data$hh_q18h.factor = factor(data$hh_q18h,levels=c("1","2","3","4","5","6"))
data$hh_q18i.factor = factor(data$hh_q18i,levels=c("1","2","3","4","5","6"))
data$hh_q18j.factor = factor(data$hh_q18j,levels=c("1","2","3","4","5","6"))
data$hh_q18k.factor = factor(data$hh_q18k,levels=c("1","2","3","4","5","6"))
data$hh_q18l.factor = factor(data$hh_q18l,levels=c("1","2","3","4","5","6"))
data$hh_q18m.factor = factor(data$hh_q18m,levels=c("1","2","3","4","5","6"))
data$hh_q18n.factor = factor(data$hh_q18n,levels=c("1","2","3","4","5","6"))
data$hh_q18o.factor = factor(data$hh_q18o,levels=c("1","2","3","4","5","6"))
data$hh_q19.factor = factor(data$hh_q19,levels=c("1","0"))
data$hh_q20.factor = factor(data$hh_q20,levels=c("1","2","3","4","5"))
data$hh_q21___1.factor = factor(data$hh_q21___1,levels=c("0","1"))
data$hh_q21___2.factor = factor(data$hh_q21___2,levels=c("0","1"))
data$hh_q21___3.factor = factor(data$hh_q21___3,levels=c("0","1"))
data$hh_q21___4.factor = factor(data$hh_q21___4,levels=c("0","1"))
data$hh_q22.factor = factor(data$hh_q22,levels=c("1","2","3","4","5"))
data$hh_q23.factor = factor(data$hh_q23,levels=c("1","2","3","4","5"))
data$hh_q24.factor = factor(data$hh_q24,levels=c("1","2","3","4","5"))
data$hh_q25___1.factor = factor(data$hh_q25___1,levels=c("0","1"))
data$hh_q25___2.factor = factor(data$hh_q25___2,levels=c("0","1"))
data$hh_q25___3.factor = factor(data$hh_q25___3,levels=c("0","1"))
data$hh_q25___4.factor = factor(data$hh_q25___4,levels=c("0","1"))
data$hh_q25___5.factor = factor(data$hh_q25___5,levels=c("0","1"))
data$hh_q26.factor = factor(data$hh_q26,levels=c("1","2","3","4","5"))
data$hh_q27.factor = factor(data$hh_q27,levels=c("1","2","3","4","5"))
data$age.factor = factor(data$age,levels=c("1","2","3","4","5","6","7","8","9"))
data$gender.factor = factor(data$gender,levels=c("1","2","3"))
data$race.factor = factor(data$race,levels=c("1","2","3","4","5","6","7"))
data$hh_q28.factor = factor(data$hh_q28,levels=c("1","2","3","4","5","6","7","8"))
data$hh_q29.factor = factor(data$hh_q29,levels=c("1","2","3","4"))
data$hh_q30.factor = factor(data$hh_q30,levels=c("1","2","3","4","5"))
data$hh_q31.factor = factor(data$hh_q31,levels=c("1","0"))
data$my_first_instrument_complete.factor = factor(data$my_first_instrument_complete,levels=c("0","1","2"))

levels(data$understood_consent.factor)=c("Yes","No")
levels(data$above_age18.factor)=c("Yes","No")
levels(data$neighbor.factor)=c("Peoplestown","English Ave","Proctor Creek")
levels(data$hh_q1.factor)=c("Yes","No")
levels(data$hh_q2.factor)=c("More than 10 times total every month.","6 to 10 times total every month.","1 to 5 times total every month.","I never go into the rivers, lakes, streams, creeks or ponds.")
levels(data$hh_q3.factor)=c("More than 10 times total every month.","6 to 10 times total every month.","1 to 5 times total every month.","My children never go into the rivers, lakes, streams, creeks or ponds.","I do not known how often my children go into the rivers, lakes, streams, creeks or ponds.")
levels(data$hh_q4.factor)=c("More than 10 times total every month during the rainy season.","6 to 10 times total every month during the rainy season.","1 to 5 times total every month during the rainy season.","I never come into contact with floodwater.")
levels(data$hh_q5.factor)=c("More than 10 times total every month during the rainy season.","6 to 10 times total every month during the rainy season.","1 to 5 times total every month during the rainy season.","My children never come into contact with floodwater.","I do not know how often my children come into contact with floodwater.")
levels(data$hh_q6.factor)=c("Everyday.","4 to 6 days a week.","1 to 3 days a week.","I never drink water from a public water source.","I do not know if I drink water from a public water source.")
levels(data$hh_q7.factor)=c("Everyday.","4 to 6 days a week.","1 to 3 days a week.","My children never drink water from a public water source.","I do not know how often my children drink water from a public water source.")
levels(data$hh_q8.factor)=c("Everyday.","4 to 6 days a week.","1 to 3 days a week.","I never eat raw produce.")
levels(data$hh_q9.factor)=c("Everyday.","4 to 6 days a week.","1 to 3 days a week.","My children never eat raw produce.","I do not know if my children eat raw produce.")
levels(data$hh_q10.factor)=c("More than 10 times total every week.","6 to 10 times total every week.","1 to 5 times total every week.","I never use a public toilet.")
levels(data$hh_q11.factor)=c("More than 10 times total every week.","6 to 10 times total every week.","1 to 5 times total every week.","My children never use a public toilet.","I do not know how often my children use a public toilet.")
levels(data$hh_q12.factor)=c("Yes","No")
levels(data$hh_q13.factor)=c("Yes","No")
levels(data$hh_q14.factor)=c("Residential (House/Apartment)","Business","Other")
levels(data$hh_q15.factor)=c("0-2","3-5","6-8","More than 9")
levels(data$hh_q16.factor)=c("less than one year","1-5 years","6-9 years","10-14 years","15-19 years","Longer than 20 years")
levels(data$hh_q17a.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q17b.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q17c.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q17d.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q17e.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q17f.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q17g.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q17h.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q17i.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q17j.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q17k.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q17l.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18a.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18b.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18c.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18d.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18e.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18f.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18g.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18h.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18i.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18j.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18k.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18l.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18m.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18n.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q18o.factor)=c("Strongly Agree","Agree","Neutral","Disagree","Strongly disagree","Not applicable")
levels(data$hh_q19.factor)=c("Yes","No")
levels(data$hh_q20.factor)=c("Never","1 or 2 times each year","3-6 times each year","7-12 times each year","More than 12 times each year")
levels(data$hh_q21___1.factor)=c("Unchecked","Checked")
levels(data$hh_q21___2.factor)=c("Unchecked","Checked")
levels(data$hh_q21___3.factor)=c("Unchecked","Checked")
levels(data$hh_q21___4.factor)=c("Unchecked","Checked")
levels(data$hh_q22.factor)=c("Never","1-2 times each year","3-6 times each year","7-12 times each year","More than 12 times")
levels(data$hh_q23.factor)=c("Never","1-2 times each year","3-6 times each year","7-12 times each year","More than 12 times")
levels(data$hh_q24.factor)=c("Never","1-2 times each year","3-6 times each year","7-12 times each year","More than 12 times")
levels(data$hh_q25___1.factor)=c("Unchecked","Checked")
levels(data$hh_q25___2.factor)=c("Unchecked","Checked")
levels(data$hh_q25___3.factor)=c("Unchecked","Checked")
levels(data$hh_q25___4.factor)=c("Unchecked","Checked")
levels(data$hh_q25___5.factor)=c("Unchecked","Checked")
levels(data$hh_q26.factor)=c("I dont walk in flood water","1-10 minutes","11-30 minutes","31-60 minutes","More than one hour")
levels(data$hh_q27.factor)=c("My children dont play in flood water","1-10 minutes","11-30 minutes","31-60 minutes","More than one hour")
levels(data$age.factor)=c("Under 12 years","12 - 17 years old","18 - 24 years old","25 - 34 years old","35 - 44 years old","45 to 54 years old","55 - 64 years old","64 - 74 years old","75 years or older")
levels(data$gender.factor)=c("Female","Male","Prefer not to answer")
levels(data$race.factor)=c("White","Hispanic or Latino","Black or African American","Native American or American Indian","Asian / Pacific Islander","Other","Prefer not to answer")
levels(data$hh_q28.factor)=c("No schooling completed","Nursery school to 8th grade","Some high school, no diploma","High school graduate, diploma or the equivalent (for example: GED)","Some college credit, no degree","Trade/technical/vocational training","College degree","Post-graduate degree")
levels(data$hh_q29.factor)=c("Yes","No","Retired","Other")
levels(data$hh_q30.factor)=c("Less than $15,000","$15,001 - $30,000","$30,001 - $50,000","$50,001 - $75,000","More than $75,000")
levels(data$hh_q31.factor)=c("Yes","No")
levels(data$my_first_instrument_complete.factor)=c("Incomplete","Unverified","Complete")
