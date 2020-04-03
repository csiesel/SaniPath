*** import rapid tool data for sql from phase 2 Vellore   ***;
***                   created by: Yuke Wang               ***;
***                   Date: Jan 12th, 2014                ***;
libname S "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\Phase 2\Support for colleague\David Berendes\Vellore Public Domain Data\data";
***import the data***;

PROC IMPORT OUT= S.AnswerType 
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Answer Types.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC IMPORT OUT= S.ExposureFrequencies 
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Exposure Frequencies.
csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/*
PROC IMPORT OUT= S.Keyinformantcity20
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\Phase 2\SQL for rapid tool result validation\
Andrew Validation\SaniPath_Database_Andrew_Validation\Key Informant City question 20 responses_adjusted.csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.Keyinformantcity23
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Key Informant City question 23 responses_adjusted.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.Keyinformantcommunity05
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Key Informant Community question 5 responses_adjusted.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.Keyinformantcommunity05
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Key Informant Community question 5 responses_adjusted.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.Keyinformantcommunity13
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Key Informant Community question 13 responses_adjusted.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.Keyinformantinterviewanswer
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Key Informant Interview Answers_adjusted.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.Keyinformantinterviewchoice
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Key Informant Interview Choices.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.Keyinformantinterview
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Key Informant Interviews.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

data S.Keyinformantquestionchoice;
   infile "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Key Informant Question Choices.
csv" delimiter="," missover dsd lrecl=32767 firstobs=2;
   informat choice_text $30.;
   format choice_text $30.;
   input ki_choice_id question choice_text $;
run;

data S.Keyinformantquestion;
   infile "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Key Informant Questions.
csv" delimiter="," missover dsd lrecl=32767 firstobs=2;
   informat question_text $255.;
   informat answer_required $5.;
   informat ki_type $9.;
   format question_text $255.;
   format answer_required $5.;
   informat ki_type $9.;
   input ki_choice_id answer_type question_text $ answer_required $ display_order ki_type $;
run;

*/

PROC IMPORT OUT= S.modulequestionanswer
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Module Question Answers.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.moduleselectionquestion
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Module Selection Questions.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.module
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Modules.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.neighborhood
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Neighborhoods.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.pathway
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Pathways.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.projectmodule
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Project-Modules.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.project
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Projects.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.sampleanswer
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Sample Answers.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.samplechoice
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Sample Choices.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.samplequestionchoice
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Sample Question Choices.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.samplequestion
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Sample Questions.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.sampletype
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Sample Types.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.Sample
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Samples.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.surveyquestionchoice
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Survey Question Choices.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.surveyquestion
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Survey Questions.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

data S.SURVEYRESPONSEANSWER;      
   infile 'T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Survey Response Answers.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
      informat id best32. ;
      informat question best32. ;
      informat response best32. ;
      informat answer_text $4. ;
      informat count_yes $4. ;
      informat count_missing $4. ;
      format id best12. ;
      format question best12. ;
      format response best12. ;
      format answer_text $4. ;
      format count_yes $4. ;
      format count_missing $4. ;
      input
         id
         question
         response
         answer_text $
         count_yes $
         count_missing $
         ;
run;

PROC IMPORT OUT= S.surveyresponsechoice
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Survey Response Choices.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.surveyresponse
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Survey Responses.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

data S.SURVEYRESPONSE;
   infile 'T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Survey Responses.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
      informat response_id best32. ;
      informat project best32. ;
      informat survey best32. ;
      informat neighborhood best32. ;
      informat response_date yymmdd10. ;
      informat entity_name $50. ;
      informat number_participant best32. ;
      informat number_female best32. ;
      informat number_male best32. ;
      informat GPS_latitude $10. ;
      informat GPS_longitude $10. ;
      informat use_adult $5. ;
      format response_id best12. ;
      format project best12. ;
      format survey best12. ;
      format neighborhood best12. ;
      format response_date yymmdd10. ;
      format entity_name $50. ;
      format number_participant best12. ;
      format number_female best12. ;
      format number_male best12. ;
      format GPS_latitude $10. ;
      format GPS_longitude $10. ;
      format use_adult $5. ;
   input
      response_id
      project
      survey
      neighborhood
      response_date
      entity_name $
      number_participant
      number_female
      number_male
      GPS_latitude $
      GPS_longitude $
      use_adult $
      ;
run;

PROC IMPORT OUT= S.survey
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Surveys.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

/*
***no data for this***;
PROC IMPORT OUT= S.transectwalkboundary
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Transect Walk Boundary Checklist Items.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.transectwalkobservation
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Transect Walk Observations.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;

PROC IMPORT OUT= S.transectwalk
            DATAFILE= "T:\IHProjs\Moe Research\Projects\SaniPath\Andrew\
Phase 2\SQL for rapid tool result validation\Andrew Validation\SaniPath_Database_Andrew_Validation\Transect Walks.
csv" 
            DBMS=csv replace;
     GETNAMES=YES;
     DATAROW=2;
RUN;
*/
