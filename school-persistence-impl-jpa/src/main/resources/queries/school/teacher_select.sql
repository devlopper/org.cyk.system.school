SELECT teachergid.CODE AS 'Code',partygid.CODE AS 'Person',teacher.GLOBALIDENTIFIER_IDENTIFIER AS 'Global identifier'
FROM teacher

INNER JOIN globalidentifier AS teachergid ON teachergid.IDENTIFIER = teacher.GLOBALIDENTIFIER_IDENTIFIER

INNER JOIN party ON party.IDENTIFIER = teacher.PERSON_IDENTIFIER
INNER JOIN globalidentifier AS partygid ON partygid.IDENTIFIER = party.GLOBALIDENTIFIER_IDENTIFIER