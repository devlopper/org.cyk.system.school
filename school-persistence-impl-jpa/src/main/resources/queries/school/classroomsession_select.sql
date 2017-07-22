SELECT levelgid.CODE AS 'Level',IFNULL(classroomsessionsuffixgid.CODE,'') AS 'Suffix',partygid.CODE AS 'Coordinator'

FROM classroomsession

INNER JOIN globalidentifier AS classroomsessiongid ON classroomsessiongid.IDENTIFIER = classroomsession.GLOBALIDENTIFIER_IDENTIFIER

LEFT JOIN classroomsessionsuffix ON classroomsessionsuffix.IDENTIFIER = classroomsession.suffix
LEFT JOIN globalidentifier AS classroomsessionsuffixgid ON classroomsessionsuffixgid.IDENTIFIER = classroomsessionsuffix.GLOBALIDENTIFIER_IDENTIFIER

INNER JOIN leveltimedivision ON leveltimedivision.IDENTIFIER = classroomsession.levelTimeDivision
INNER JOIN globalidentifier AS leveltimedivisiongid ON leveltimedivisiongid.IDENTIFIER = leveltimedivision.GLOBALIDENTIFIER_IDENTIFIER

INNER JOIN level ON level.IDENTIFIER = levelTimeDivision.LEVEL_IDENTIFIER
INNER JOIN globalidentifier AS levelgid ON levelgid.IDENTIFIER = level.GLOBALIDENTIFIER_IDENTIFIER

LEFT JOIN teacher ON teacher.IDENTIFIER = classroomsession.COORDINATOR_IDENTIFIER
LEFT JOIN party ON party.IDENTIFIER = teacher.PERSON_IDENTIFIER
LEFT JOIN globalidentifier AS partygid ON partygid.IDENTIFIER = party.GLOBALIDENTIFIER_IDENTIFIER
