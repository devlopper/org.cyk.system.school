SELECT studentgid.CODE AS 'Student',levelgid.CODE AS 'Level',IFNULL(classroomsessionsuffixgid.CODE,'') AS 'Suffix'
,IFNULL(studentresults.evaluationAverageValue,'') AS 'Average',IFNULL(SUBSTRING(table_intervalgid.CODE,23),'') AS 'Promotion'
FROM studentclassroomsession

INNER JOIN classroomsession ON classroomsession.IDENTIFIER = studentclassroomsession.classroomSession
INNER JOIN leveltimedivision ON leveltimedivision.IDENTIFIER = classroomsession.levelTimeDivision
INNER JOIN level ON level.IDENTIFIER = leveltimedivision.LEVEL_IDENTIFIER
INNER JOIN globalidentifier AS levelgid ON levelgid.IDENTIFIER = level.GLOBALIDENTIFIER_IDENTIFIER

LEFT JOIN classroomsessionsuffix ON classroomsessionsuffix.IDENTIFIER = classroomsession.suffix
LEFT JOIN globalidentifier AS classroomsessionsuffixgid ON classroomsessionsuffixgid.IDENTIFIER = classroomsessionsuffix.GLOBALIDENTIFIER_IDENTIFIER

INNER JOIN student ON student.IDENTIFIER = studentclassroomsession.student
INNER JOIN globalidentifier AS studentgid ON studentgid.IDENTIFIER = student.GLOBALIDENTIFIER_IDENTIFIER

INNER JOIN studentresults ON studentresults.IDENTIFIER = studentclassroomsession.RESULTS_IDENTIFIER

LEFT JOIN table_interval ON table_interval.IDENTIFIER = studentresults.evaluationPromotedAverageInterval
LEFT JOIN globalidentifier AS table_intervalgid ON table_intervalgid.IDENTIFIER = table_interval.GLOBALIDENTIFIER_IDENTIFIER