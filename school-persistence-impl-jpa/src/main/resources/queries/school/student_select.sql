SELECT IFNULL(leveltimedivisiongid.CODE,'') AS 'Admission level',studentgid.CODE AS 'Student',student.GLOBALIDENTIFIER_IDENTIFIER AS 'Global identifier'
FROM student

INNER JOIN globalidentifier AS studentgid ON studentgid.IDENTIFIER = student.GLOBALIDENTIFIER_IDENTIFIER

LEFT JOIN leveltimedivision ON leveltimedivision.IDENTIFIER = student.ADMISSIONLEVELTIMEDIVISION_IDENTIFIER
LEFT JOIN globalidentifier AS leveltimedivisiongid ON leveltimedivisiongid.IDENTIFIER = leveltimedivision.GLOBALIDENTIFIER_IDENTIFIER

LEFT JOIN party ON party.GLOBALIDENTIFIER_IDENTIFIER = student.PERSON_IDENTIFIER
LEFT JOIN globalidentifier AS partygid ON partygid.IDENTIFIER = party.GLOBALIDENTIFIER_IDENTIFIER