***           use sql to select phase 2 Vellore Data from rapid tool            ***;
***                                 Created by:Yuke Wang                        ***;
***                                 Date: Jan 12th, 2015                        ***;

libname S "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\Phase 2\Support for colleague\David Berendes\Vellore Public Domain Data\data";
libname D "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\Phase 2\Support for colleague\David Berendes\Vellore Public Domain Data";

/*
proc sql;
   create table D.sample1 as
   select sam.*, st.sample_type_name, st.sample_type_abbrev, sq.sample_question_id, sq.question_text, sa.answer_text
   from S.sample as sam, S.sampletype as st, S.samplequestion as sq, S.sampleanswer as sa
   where sam.sample_type=st.sample_type_id & sam.sample_type=sq.sample_type & sq.sample_question_id=sa.question & sam.sample_id=sa.response
   order by sample_id;
quit;

proc sql;
   create table D.sample2 as
   select sam.*, st.sample_type_name, st.sample_type_abbrev, sq.sample_question_id, sq.question_text, sc.choice, sqc.sample_choice_text
   from S.sample as sam, S.sampletype as st, S.samplequestion as sq, S.samplequestionchoice as sqc, S.samplechoice as sc
   where sam.sample_type=st.sample_type_id & sam.sample_type=sq.sample_type & sam.sample_id=sc.response & sc.choice=sqc.sample_choice_id & sq.sample_question_id=sqc.question
   order by sample_id;
quit;

proc sql;
   create table D.sample_test as
   select *
   from D.sample1
   outer union corr
   select *
   from D.sample2
   order by sample_id;
quit;
*/

proc sql;
   create table D.sample as
   select sam.*, st.sample_type_name, st.sample_type_abbrev, sq.sample_question_id, sq.question_text, sa.answer_text
   from S.sample as sam, S.sampletype as st, S.samplequestion as sq, S.sampleanswer as sa
   where sam.sample_type=st.sample_type_id & sam.sample_type=sq.sample_type & sq.sample_question_id=sa.question & sam.sample_id=sa.response
   outer union corr
   select sam.*, st.sample_type_name, st.sample_type_abbrev, sq.sample_question_id, sq.question_text, sc.choice, sqc.sample_choice_text
   from S.sample as sam, S.sampletype as st, S.samplequestion as sq, S.samplequestionchoice as sqc, S.samplechoice as sc
   where sam.sample_type=st.sample_type_id & sam.sample_type=sq.sample_type & sam.sample_id=sc.response & sc.choice=sqc.sample_choice_id & sq.sample_question_id=sqc.question
   order by sample_id;
quit;

*************************************************************************************************************************************;
***transect walk***;

proc sql;
   create table D.transect as 
   select transob.*, tw.tw_neighborhood as neighborhood_id, neigh.neighborhood_name as neighborhood
   from S.transectwalkobservation as transob, S.transectwalk as tw, S.neighborhood as neigh
   where transob.transect_walk=tw.tw_id & tw.tw_neighborhood=neigh.neighborhood_id
   order by id;
quit;

**************************************************************************************************************************************;
***behavior***;
proc sql;
   create table D.behavior as
   select src.response as response_id, src.choice as choice_id, sr.*, sqc.question as question_id, sqc.choice_text, sq.question_text, neigh.neighborhood_name, survey.survey_type, survey.risk_response_table
   from S.surveyresponse as sr, S.survey, S.neighborhood as neigh, S.surveyresponsechoice as src, S.surveyquestionchoice as sqc, S.surveyquestion as sq
   where sr.survey=survey.survey_id & sr.neighborhood=neigh.neighborhood_id & src.response=sr.response_id & src.choice=sqc.choice_id & sqc.question=sq.survey_question_id
   order by response_id;
quit;

proc sort data=D.behavior;
   by response_id question_id;
run;


***calculate the concentration***;
data sample;
   set D.sample;
   if count1="None" & plate1_tntc="TRUE" then count_num1=999;
   else if count1="None" & plate1_tntc="FALSE" then count_num1=.;
   else count_num1=count1*1;
   if count2="None" & plate2_tntc="TRUE" then count_num2=999;
   else if count2="None" & plate2_tntc="FALSE" then count_num2=.;
   else count_num2=count2*1;
   eq_vol1=100/dilution1;
   eq_vol2=100/dilution2;
   if sample_type in (4,5,6,7,8) then ec_denom=100;
   ********************************************************************************************;
   if sample_type=1 then ec_denom=2;
   else if sample_type=2 then ec_denom=12;
   ********************************************************************************************;
   else if sample_type=3 then ec_denom=500;
   if count_num1=999 & count_num2=999 then conc=200/eq_vol2*ec_denom;
   else if count_num1=999 & count_num2>=10 & count_num2<=200 then conc=count_num2/eq_vol2*ec_denom;
   else if count_num1=999 & count_num2>=1 & count_num2<=9 then conc=count_num2/eq_vol2*ec_denom;
   else if count_num1>=10 & count_num1<=200 & count_num2>=10 & count_num2<=200 then conc=(count_num1+count_num2)/(eq_vol1+eq_vol2)*ec_denom;
   else if count_num1>=10 & count_num1<=200 & count_num2>=1 & count_num2<=9 then conc=count_num1/eq_vol1*ec_denom;
   else if count_num1>=10 & count_num1<=200 & count_num2=0 then conc=count_num1/eq_vol1*ec_denom;
   else if count_num1>=1 & count_num1<=9 & count_num2>=1 & count_num2<=9 then conc=(count_num1+count_num2)/(eq_vol1+eq_vol2)*ec_denom;
   else if count_num1>=1 & count_num1<=9 & count_num2=0 then conc=count_num1/eq_vol1*ec_denom;
   else if count_num1=0 & count_num2=0 then conc=0.5/(eq_vol1+eq_vol2)*ec_denom;
   else conc=.; ***records doesn't make sense***;
run;

data D.sample_w_conc;
   set sample;
run;
