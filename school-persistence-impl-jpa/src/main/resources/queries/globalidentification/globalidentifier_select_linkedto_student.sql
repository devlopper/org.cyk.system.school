SELECT globalidentifier.IDENTIFIER AS 'Identifier',IFNULL(globalidentifier.CODE,'') AS 'Code',IFNULL(globalidentifier.NAME,'') AS 'Name'
,IFNULL(globalidentifier.DESCRIPTION,'') AS 'Description',IFNULL(globalidentifier.ABBREVIATION,'') AS 'Abbreviation'
,IFNULL(globalidentifier.OTHERDETAILS,'') AS 'Other details',IFNULL(globalidentifier.EXTERNALIDENTIFIER,'') AS 'External identifier'
,IFNULL(globalidentifier.ORDERNUMBER,'') AS 'Order number',IFNULL(globalidentifier.WEIGHT,'') AS 'Weight'
,IFNULL(globalidentifier.USABLE,'') AS 'Usable',IFNULL(globalidentifier.FROMDATE,'') AS 'Birth date'
,IFNULL(globalidentifier.TODATE,'') AS 'Death date',IFNULL(file.IDENTIFIER,'') AS 'Image file name'
,IFNULL(file.EXTENSION,'') AS 'Image file extension'
FROM globalidentifier

INNER JOIN student ON student.GLOBALIDENTIFIER_IDENTIFIER = globalidentifier.IDENTIFIER
LEFT JOIN file ON file.IDENTIFIER = globalidentifier.IMAGE_IDENTIFIER