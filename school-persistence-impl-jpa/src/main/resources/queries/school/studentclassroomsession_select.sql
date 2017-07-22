SELECT levelgid.CODE AS 'Level',classroomsessionsuffixgid.CODE AS 'Suffix',studentgid.CODE AS 'Student',studentresults.evaluationAverageValue AS 'Average'
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