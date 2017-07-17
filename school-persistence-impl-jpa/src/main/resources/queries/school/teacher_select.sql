SELECT teachergid.CODE AS 'Teacher',teacher.GLOBALIDENTIFIER_IDENTIFIER AS 'Global identifier'
FROM teacher

INNER JOIN globalidentifier AS teachergid ON teachergid.IDENTIFIER = teacher.GLOBALIDENTIFIER_IDENTIFIER