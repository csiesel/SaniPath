proc sql;
   create table D.school as
   select sr.response_id, sr.neighborhood as neighborhood_id, sr.survey as survey_id, sr.number_participant, sr.number_male, sr.number_female, 
          sra.question as question_id, sra.count_yes, sra.count_missing, sq.question_text, 
          case when question_id>=27 & question_id<=30 then 2
		       when question_id>=32 & question_id<=36 then 1
			   when question_id>=38 & question_id<=41 then 4
			   when question_id>=43 & question_id<=47 then 3
			   when question_id>=49 & question_id<=53 then 6
			   when question_id>=55 & question_id<=59 then 5
			   when question_id>=62 & question_id<=65 then 8
			   when question_id>=67 & question_id<=71 then 7
			   when question_id>=73 & question_id<=76 then 10
			   when question_id>=78 & question_id<=82 then 9
			   end as question_tool
   from S.surveyresponseanswer as sra, S.surveyresponse as sr, S.surveyquestion as sq
   where sr.survey=1 & sra.response=sr.response_id & sra.question=sq.survey_question_id
   order by response_id, question_id;
quit;

proc sql;
   create table D.community as
   select sr.response_id, sr.neighborhood as neighborhood_id, sr.survey as survey_id, sr.number_participant, sr.number_male, sr.number_female, 
          sra.question as question_id, sra.count_yes, sra.count_missing, sq.question_text,
          case when question_id>=227 & question_id<=230 then 1
		       when question_id>=232 & question_id<=236 then 2
			   when question_id>=238 & question_id<=241 then 3
			   when question_id>=243 & question_id<=247 then 4
			   when question_id>=249 & question_id<=253 then 5
			   when question_id>=255 & question_id<=259 then 6
			   when question_id>=262 & question_id<=265 then 7
			   when question_id>=267 & question_id<=271 then 8
			   when question_id>=273 & question_id<=276 then 9
			   when question_id>=278 & question_id<=282 then 10
			   end as question_tool
   from S.surveyresponseanswer as sra, S.surveyresponse as sr, S.surveyquestion as sq
   where sr.survey=2 & sra.response=sr.response_id & sra.question=sq.survey_question_id
   order by response_id, question_id;
quit;

proc sql;
   create table D.household as
   select src.response as response_id, src.choice as choice_id, sr.neighborhood as neighborhood_id, sr.survey as survey_id, sqc.question as question_id,
          sqc.choice_text, sq.question_text,
		  case when question_id=405 then 1
		       when question_id=406 then 2
			   when question_id=407 then 3
			   when question_id=408 then 4
			   when question_id=409 then 5
			   when question_id=410 then 6
			   when question_id=412 then 7
			   when question_id=413 then 8
			   when question_id=414 then 9
			   when question_id=415 then 10
			   end as question_tool
   from S.surveyresponsechoice as src, S.surveyresponse as sr, S.surveyquestionchoice as sqc, S.surveyquestion as sq
   where src.response=sr.response_id & src.choice=sqc.choice_id & sqc.question=sq.survey_question_id
   order by response_id, question_id;
quit;
