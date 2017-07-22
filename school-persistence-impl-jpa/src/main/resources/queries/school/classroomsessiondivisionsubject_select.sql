SELECT levelgid.CODE AS 'Level',IFNULL(classroomsessionsuffixgid.CODE,'') AS 'Suffix'
,subjectgid.CODE AS 'Subject',partygid.CODE AS 'Teacher'

FROM classroomsessiondivisionsubject

INNER JOIN subject ON subject.IDENTIFIER = classroomsessiondivisionsubject.subject
INNER JOIN globalidentifier AS subjectgid ON subjectgid.IDENTIFIER = subject.GLOBALIDENTIFIER_IDENTIFIER

INNER JOIN globalidentifier AS classroomsessiondivisionsubjectgid ON classroomsessiondivisionsubjectgid.IDENTIFIER = classroomsessiondivisionsubject.GLOBALIDENTIFIER_IDENTIFIER

INNER JOIN classroomsessiondivision ON classroomsessiondivision.IDENTIFIER = classroomsessiondivisionsubject.classroomSessionDivision
INNER JOIN globalidentifier AS classroomsessiondivisiongid ON classroomsessiondivisiongid.IDENTIFIER = classroomsessiondivision.GLOBALIDENTIFIER_IDENTIFIER

INNER JOIN classroomsession ON classroomsession.IDENTIFIER = classroomsessiondivision.CLASSROOMSESSION_IDENTIFIER
INNER JOIN globalidentifier AS classroomsessiongid ON classroomsessiongid.IDENTIFIER = classroomsession.GLOBALIDENTIFIER_IDENTIFIER

LEFT JOIN classroomsessionsuffix ON classroomsessionsuffix.IDENTIFIER = classroomsession.suffix
LEFT JOIN globalidentifier AS classroomsessionsuffixgid ON classroomsessionsuffixgid.IDENTIFIER = classroomsessionsuffix.GLOBALIDENTIFIER_IDENTIFIER

INNER JOIN leveltimedivision ON leveltimedivision.IDENTIFIER = classroomsession.levelTimeDivision
INNER JOIN globalidentifier AS leveltimedivisiongid ON leveltimedivisiongid.IDENTIFIER = leveltimedivision.GLOBALIDENTIFIER_IDENTIFIER

INNER JOIN level ON level.IDENTIFIER = levelTimeDivision.LEVEL_IDENTIFIER
INNER JOIN globalidentifier AS levelgid ON levelgid.IDENTIFIER = level.GLOBALIDENTIFIER_IDENTIFIER

LEFT JOIN teacher ON teacher.IDENTIFIER = classroomsessiondivisionsubject.TEACHER_IDENTIFIER
LEFT JOIN party ON party.IDENTIFIER = teacher.PERSON_IDENTIFIER
LEFT JOIN globalidentifier AS partygid ON partygid.IDENTIFIER = party.GLOBALIDENTIFIER_IDENTIFIER

WHERE classroomsessiondivisiongid.ORDERNUMBER = 3