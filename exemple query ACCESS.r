## EXEMPLE QUERY AMB ACCESS ##

query<-"SELECT
RECLUTAMENT.idhermes,
[dades actualitzades].LOCALITA AS 'poble',
[dades actualitzades].NOM_LOC,
[dades actualitzades].SEXE,
RECLUTAMENT.dnaix,
RECLUTAMENT.dexam,
RECLUTAMENT.infota,
RECLUTAMENT.dietta,
RECLUTAMENT.medita,
RECLUTAMENT.tas_ul,
RECLUTAMENT.tad_ul,
RECLUTAMENT.imc,
RECLUTAMENT.fuma,
RECLUTAMENT.COL,
RECLUTAMENT.HDL,
RECLUTAMENT.TRIGLI,
RECLUTAMENT.GLUCOSA,
RECLUTAMENT.colester,
RECLUTAMENT.presdiet,
RECLUTAMENT.medicol,
RECLUTAMENT.infoglu,
RECLUTAMENT.dietglu,
RECLUTAMENT.mediglu,
RECLUTAMENT.insuli,
RECLUTAMENT.infoiam,
RECLUTAMENT.infoang,
RECLUTAMENT.infoict,
RECLUTAMENT.psisesdd,
RECLUTAMENT.psiseidd,
RECLUTAMENT.psiseied,
RECLUTAMENT.itb1_2,
RECLUTAMENT.itb2_2,
DOPPLERSrepetits.psisesdd,
DOPPLERSrepetits.psiseidd,
DOPPLERSrepetits.psiseied
FROM
DOPPLERSrepetits RIGHT JOIN
([dades actualitzades] RIGHT JOIN RECLUTAMENT ON [dades actualitzades].CIP = RECLUTAMENT.cip)
ON DOPPLERSrepetits.idhermes = RECLUTAMENT.idhermes
WHERE ((([dades actualitzades].LOCALITA)='1704900' Or ([dades actualitzades].LOCALITA)='1718600' Or ([dades actualitzades].LOCALITA)='1716900'))
ORDER BY RECLUTAMENT.idhermes;"



query<-"SELECT x.idhermes FROM x;"

file<-"U:\\Estudis\\Epidemiologia\\HERMES\\Dades\\JGarcia\\Jgarcia.mdb"


canal <- odbcConnectAccess(file)
sqlTables(canal)    # llista els noms de les taules
taula<-sqlQuery(canal,query) # guarda el resultat de la consulta (query) en un data.frame anomenat alim.

odbcGetInfo(canal)

odbcClose(canal)







