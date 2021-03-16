; *** Astar.lisp                              ***
; *** Código lisp para encontrar la ruta más  ***
; *** corta entre ciudades de México.         ***

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; **** CIUDADES Y DISTANCIAS ****
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((ciudades (make-hash-table :size 20)))              ; Tabla de ciudades
  (defun set-distancia (x y) (setf (gethash x ciudades) y))
  (defun get-distancia (x) (gethash x ciudades)))

(let ((caminos (make-hash-table :size 20)))               ; Tabla de caminos recorridas
  (defun set-ancestro (x y) (setf (gethash x caminos) y))
  (defun get-ancestro (x) (gethash x caminos)))

  ;Ciudades de Aguascalientes
  (set-distancia 'Aguascalientes '((Calvillo . 51)(Jesus-Maria . 11)(San-Francisco-de-Los-Romo . 23)(Asientos . 101)(El-Llano . 41)(San-Miguel-el-Alto . 111)))
  (set-distancia 'Calvillo '((San-Jose-de-Gracia . 87)(Jesus-Maria . 53)(Aguascalientes . 51)(Nochistlan-de-Mejia . 91)))
  (set-distancia 'Jesus-Maria '((Calvillo . 53)(San-Jose-de-Gracia . 34)(Pabellon-de-Arteaga . 31)(San-Francisco-de-Los-Romo . 17)(Aguascalientes . 11)))
  (set-distancia 'San-Francisco-de-Los-Romo '((Jesus-Maria . 17)(Pabellon-de-Arteaga . 10)(Asientos . 40)(Aguascalientes . 23)))
  (set-distancia 'Asientos '((Pabellon-de-Arteaga . 32)(Tepezala . 13)(El-Llano . 48)(Aguascalientes . 101)(San-Francisco-de-Los-Romo . 40)))
  (set-distancia 'El-Llano '((Aguascalientes . 41)(Asientos . 48)))
  (set-distancia 'San-Jose-de-Gracia '((Calvillo . 87)(Rincon-de-Romos . 24)(Pabellon-de-Arteaga . 15)(Jesus-Maria . 34)))
  (set-distancia 'Pabellon-de-Arteaga '((San-Jose-de-Gracia . 15)(Rincon-de-Romos . 12)(Tepezala . 19)(Asientos . 32)(San-Francisco-de-Los-Romo . 10)(Jesus-Maria . 31)))
  (set-distancia 'Rincon-de-Romos '((San-Jose-de-Gracia . 24)(Cosio . 17)(Tepezala . 17)(Pabellon-de-Arteaga . 12)(Zacatecas . 78)))
  (set-distancia 'Tepezala '((Rincon-de-Romos . 17)(Pabellon-de-Arteaga . 19)(Asientos . 13)))
  (set-distancia 'Cosio '((Rincon-de-Romos . 17)(Villa-Gonzalez-Ortega . 73)))
  ;Ciudades de Baja California
  (set-distancia 'Tijuana '((Playas-de-Rosarito . 21)(Ensenada . 100)(Tecate . 50)))
  (set-distancia 'Playas-de-Rosarito '((Tijuana . 21)(Ensenada . 84)))
  (set-distancia 'Ensenada '((Tijuana . 100)(Playas-de-Rosarito . 84)(Tecate . 108)(Mexicali . 238)(Ciudad-Progreso . 221)(Ciudad-Guadalupe-Victoria . 284)(Punta-Prieta . 480)(Bahia-de-los-Angeles . 520)))
  (set-distancia 'Tecate '((Tijuana . 50)(Ensenada . 108)(Ciudad-Progreso . 120)(Mexicali . 130)))
  (set-distancia 'Ciudad-Progreso '((Ensenada . 221)(Tecate . 120)(Mexicali . 15)(Ciudad-Guadalupe-Victoria . 75)(Bahia-de-los-Angeles . 537)(Punta-Prieta . 485)))
  (set-distancia 'Mexicali '((Ensenada . 238)(Tecate . 130)(Ciudad-Progreso . 15)(Ciudad-Guadalupe-Victoria . 62)(Golfo-de-Santa-Clara . 146)))
  (set-distancia 'Ciudad-Guadalupe-Victoria '((Ensenada . 284)(Mexicali . 62)(Bahia-de-los-Angeles . 501)(Punta-Prieta . 459)(Golfo-de-Santa-Clara . 97)))
  (set-distancia 'Punta-Prieta '((Ensenada . 480)(Ciudad-Progreso . 485)(Ciudad-Guadalupe-Victoria . 459)(Bahia-de-los-Angeles . 79)(Santo-Domingo . 85)))
  (set-distancia 'Bahia-de-los-Angeles '((Ensenada . 520)(Ciudad-Progreso . 537)(Ciudad-Guadalupe-Victoria . 501)(Punta-Prieta . 79)(El-Arco . 165)))
  (set-distancia 'Santo-Domingo '((Punta-Prieta . 85)(El-Arco . 108)(Benito-Juarez . 65)))
  (set-distancia 'El-Arco '((Bahia-de-los-Angeles . 165)(Santo-Domingo . 108)(Benito-Juarez . 49)))
  ;Ciudades de Baja California Sur
  (set-distancia 'Bahia-Tortugas '((Benito-Juarez . 226)(San-Ignacio . 245)))
  (set-distancia 'Benito-Juarez '((Bahia-Tortugas . 226)(San-Ignacio . 126)(El-Arco . 49)(Santo-Domingo . 65)))
  (set-distancia 'San-Ignacio '((Bahia-Tortugas . 245)(Benito-Juarez . 126)(Mulege . 138)(Comondu . 246)(Ciudad-Insurgentes . 310)))
  (set-distancia 'Mulege '((Comondu . 157)(Ciudad-Insurgentes . 250)(Loreto . 135)))
  (set-distancia 'Comondu '((San-Ignacio . 246)(Mulege . 157)(Ciudad-Insurgentes . 109)(Loreto . 75)))
  (set-distancia 'Loreto '((Mulege . 135)(Comondu . 75)(Ciudad-Insurgentes . 122)))
  (set-distancia 'Ciudad-Insurgentes '((San-Ignacio . 310)(Mulege . 250)(Comondu . 109)(Loreto . 122)(La-Paz . 236)))
  (set-distancia 'La-Paz '((Ciudad-Insurgentes . 236)(Todos-Santos . 82)(Santiago . 134)(La-Ribera . 136)))
  (set-distancia 'Todos-Santos '((La-Paz . 82)(Cabo-San-Lucas . 78)(Santiago . 155)(La-Ribera . 156)))
  (set-distancia 'Cabo-San-Lucas '((Santiago . 89)(Todos-Santos . 78)))
  (set-distancia 'Santiago '((Cabo-San-Lucas . 89)(La-Paz . 134)(Todos-Santos . 155)(La-Ribera . 23)))
  (set-distancia 'La-Ribera '((La-Paz . 136)(Santiago . 23)(Todos-Santos . 156)))
  ;Ciudades de Campeche
  (set-distancia 'Calkini '((Hopelchen . 106)(Hecelchakan . 30)(Merida . 90)))
  (set-distancia 'Hecelchakan '((Campeche . 60)(Hopelchen . 93)(Calkini . 30)))
  (set-distancia 'Campeche '((Hopelchen . 87)(Champoton . 63)(Hecelchakan . 60)))
  (set-distancia 'Hopelchen '((Campeche . 87)(Champoton . 134)(Hecelchakan . 93)(Calkini . 106)(Xpujil . 181)(Chacsinkin . 147)(Jose-Maria-Morelos . 160)))
  (set-distancia 'Champoton '((Campeche . 63)(Ciudad-del-Carmen . 147)(Hopelchen . 134)(Francisco-Escarcega . 86)(Xpujil . 209)))
  (set-distancia 'Ciudad-del-Carmen '((Francisco-Escarcega . 164)(Champoton . 147)(Frontera . 97)(Macuspana . 156)))
  (set-distancia 'Francisco-Escarcega '((Ciudad-del-Carmen . 164)(Champoton . 86)(Xpujil . 153)))
  (set-distancia 'Xpujil '((Hopelchen . 181)(Champoton . 209)(Francisco-Escarcega . 153)(Chetumal . 158)))
  ;Ciudades de Chiapas
  (set-distancia 'Chapultenango '((Comitan-de-Dominguez . 270)(Tuxtla-Gutierrez . 151)(Acala . 234)(Simojovel . 146)(San-Juan-Chamula . 171)(Minatitlan . 300)(Coatzacoalcos . 250)(Huimanguillo . 98)))
  (set-distancia 'Simojovel '((Comitan-de-Dominguez . 177)(Tuxtla-Gutierrez . 124)(Acala . 141)(Chapultenango . 146)(San-Juan-Chamula . 78)(Huimanguillo . 140)(Jalapa . 111)))
  (set-distancia 'San-Juan-Chamula '((Comitan-de-Dominguez . 99)(Tuxtla-Gutierrez . 69)(Acala . 65)(Simojovel . 78)(Chapultenango . 171)))
  (set-distancia 'Tuxtla-Gutierrez '((Comitan-de-Dominguez . 146)(Tapachula . 369)(Acala . 56)(Simojovel . 124)(Villaflores . 91)(Chapultenango . 151)(El-Parral . 62)(San-Juan-Chamula . 69)(San-Pedro-Tapantepec . 140)))
  (set-distancia 'Acala '((Comitan-de-Dominguez . 123)(Tuxtla-Gutierrez . 56)(Simojovel . 141)(Villaflores . 11)(Chapultenango . 234)(El-Parral . 68)(San-Juan-Chamula . 65)))
  (set-distancia 'El-Parral '((Comitan-de-Dominguez . 191)(Tuxtla-Gutierrez . 62)(Acala . 68)(Villaflores . 43)(Siltepec . 197)))
  (set-distancia 'Comitan-de-Dominguez '((Tuxtla-Gutierrez . 146)(Tapachula . 244)(Acala . 123)(Simojovel . 177)(Villaflores . 226)(Siltepec . 159)(Chapultenango . 270)(El-Parral . 191)(San-Juan-Chamula . 99)))
  (set-distancia 'Villaflores '((Comitan-de-Dominguez . 226)(Tuxtla-Gutierrez . 91)(Tapachula . 355)(Acala . 11)(Siltepec . 199)(El-Parral . 43)))
  (set-distancia 'Siltepec '((Comitan-de-Dominguez . 159)(Tapachula . 142)(Villaflores . 199)(El-Parral . 197)))
  (set-distancia 'Tapachula '((Comitan-de-Dominguez . 244)(Tuxtla-Gutierrez . 369)(Villaflores . 355)(Siltepec . 142)(San-Pedro-Tapantepec . 287)))
  ;Ciudades de Chihuahua
  (set-distancia 'Jimenez '((Hidalgo-del-Parral . 81)(Cuauhtemoc . 323)(Chihuahua . 224)(Ciudad-Juarez . 584)(Nuevo-Casas-Grandes . 522)(Camargo . 72)(Delicias . 140)(Yepachic . 540)(Pascual-Orozco . 387)(Inde . 235)(Topia . 515)(Ciudad-Lerdo . 240)(Monclova . 430)(Parras-de-la-Fuente . 510)))
  (set-distancia 'Hidalgo-del-Parral '((Jimenez . 81)(Cuauhtemoc . 254)(Chihuahua . 228)(Ciudad-Juarez . 578)(Nuevo-Casas-Grandes . 516)(Camargo . 147)(Delicias . 215)(Yepachic . 472)(Pascual-Orozco . 318)(Navojoa . 669)(Gustavo-Diaz-Ordaz . 721)(Culiacan . 703)(Inde . 180)(Topia . 460)(Ciudad-Lerdo . 336)))
  (set-distancia 'Cuauhtemoc '((Jimenez . 323)(Hidalgo-del-Parral . 254)(Chihuahua . 107)(Ciudad-Juarez . 457)(Nuevo-Casas-Grandes . 301)(Camargo . 252)(Delicias . 187)(Yepachic . 218)(Pascual-Orozco . 64)))
  (set-distancia 'Chihuahua '((Jimenez . 224)(Hidalgo-del-Parral . 228)(Cuauhtemoc . 107)(Ciudad-Juarez . 353)(Nuevo-Casas-Grandes . 291)(Camargo . 153)(Delicias . 88)(Yepachic . 325)(Pascual-Orozco . 171)))
  (set-distancia 'Ciudad-Juarez '((Jimenez . 584)(Hidalgo-del-Parral . 578)(Cuauhtemoc . 457)(Chihuahua . 353)(Nuevo-Casas-Grandes . 269)(Camargo . 516)(Delicias . 451)(Yepachic . 676)(Pascual-Orozco . 523)(Agua-Prieta . 360)))
  (set-distancia 'Nuevo-Casas-Grandes '((Jimenez . 522)(Hidalgo-del-Parral . 516)(Cuauhtemoc . 301)(Chihuahua . 291)(Ciudad-Juarez . 269)(Camargo . 453)(Delicias . 388)(Yepachic . 460)(Pascual-Orozco . 299)(Agua-Prieta . 221)))
  (set-distancia 'Camargo '((Jimenez . 72)(Hidalgo-del-Parral . 147)(Cuauhtemoc . 252)(Chihuahua . 153)(Ciudad-Juarez . 516)(Nuevo-Casas-Grandes . 453)(Delicias . 69)(Yepachic . 469)(Pascual-Orozco . 316)(Monclova . 501)))
  (set-distancia 'Delicias '((Jimenez . 140)(Hidalgo-del-Parral . 215)(Cuauhtemoc . 187)(Chihuahua . 88)(Ciudad-Juarez . 451)(Nuevo-Casas-Grandes . 388)(Camargo . 69)(Yepachic . 404)(Pascual-Orozco . 250)))
  (set-distancia 'Yepachic '((Jimenez . 540)(Hidalgo-del-Parral . 472)(Cuauhtemoc . 218)(Chihuahua . 325)(Ciudad-Juarez . 676)(Nuevo-Casas-Grandes . 460)(Camargo . 469)(Delicias . 404)(Pascual-Orozco . 172)(Hermosillo . 360)(Ciudad-Obregon . 306)(Navojoa . 320)(Los-Mochis . 620)))
  (set-distancia 'Pascual-Orozco '((Jimenez . 387)(Hidalgo-del-Parral . 318)(Cuauhtemoc . 64)(Chihuahua . 171)(Ciudad-Juarez . 523)(Nuevo-Casas-Grandes . 299)(Camargo . 316)(Delicias . 250)(Yepachic . 172)))
  ;Ciudades de Ciudad de Mexico
  (set-distancia 'CDMX '((Iztapalapa . 14)(Gustavo-A-Madero . 9)(Magdalena-Contreras . 27)(Milpa-Alta . 39)(Azcapotzalco . 13)(Venustiano-Carranza . 7)(Miguel-Hidalgo . 9)(Coyoacan . 13)(Alvaro-Obregon . 16)))
  (set-distancia 'Iztapalapa '((CDMX . 14)(Gustavo-A-Madero . 20)(Magdalena-Contreras . 25)(Milpa-Alta . 32)(Azcapotzalco . 26)(Venustiano-Carranza . 11)(Miguel-Hidalgo . 21)(Coyoacan . 11)(Alvaro-Obregon . 19)))
  (set-distancia 'Gustavo-A-Madero '((CDMX . 9)(Iztapalapa . 20)(Magdalena-Contreras . 35)(Milpa-Alta . 50)(Azcapotzalco . 15)(Venustiano-Carranza . 11)(Miguel-Hidalgo . 15)(Coyoacan . 25)(Alvaro-Obregon . 24)(Ecatepec . 16)))
  (set-distancia 'Magdalena-Contreras '((CDMX . 27)(Iztapalapa . 25)(Gustavo-A-Madero . 35)(Milpa-Alta . 38)(Azcapotzalco . 30)(Venustiano-Carranza . 30)(Miguel-Hidalgo . 23)(Coyoacan . 15)(Alvaro-Obregon . 16)(Cuernavaca . 70)(Toluca-de-Lerdo . 54)(Santiago-Tianguistenco . 49)))
  (set-distancia 'Milpa-Alta '((CDMX . 39)(Iztapalapa . 32)(Gustavo-A-Madero . 50)(Magdalena-Contreras . 38)(Azcapotzalco . 51)(Venustiano-Carranza . 42)(Miguel-Hidalgo . 49)(Coyoacan . 29)(Alvaro-Obregon . 44)(Cuernavaca . 77)(Yecapixtla . 57)))
  (set-distancia 'Azcapotzalco '((CDMX . 13)(Iztapalapa . 26)(Gustavo-A-Madero . 15)(Magdalena-Contreras . 30)(Milpa-Alta . 51)(Venustiano-Carranza . 15)(Miguel-Hidalgo . 8)(Coyoacan . 23)(Alvaro-Obregon . 18)(Jiquipilco . 72)))
  (set-distancia 'Venustiano-Carranza '((CDMX . 7)(Iztapalapa . 11)(Gustavo-A-Madero . 11)(Magdalena-Contreras . 30)(Milpa-Alta . 42)(Azcapotzalco . 15)(Miguel-Hidalgo . 15)(Coyoacan . 17)(Alvaro-Obregon . 20)(Texcoco . 28)))
  (set-distancia 'Miguel-Hidalgo '((CDMX . 9)(Iztapalapa . 21)(Gustavo-A-Madero . 15)(Magdalena-Contreras . 23)(Milpa-Alta . 49)(Azcapotzalco . 8)(Venustiano-Carranza . 15)(Coyoacan . 17)(Alvaro-Obregon . 13)(Jiquipilco . 73)))
  (set-distancia 'Coyoacan '((CDMX . 13)(Iztapalapa . 11)(Gustavo-A-Madero . 25)(Magdalena-Contreras . 15)(Milpa-Alta . 29)(Azcapotzalco . 23)(Venustiano-Carranza . 17)(Miguel-Hidalgo . 17)(Alvaro-Obregon . 12)))
  (set-distancia 'Alvaro-Obregon '((CDMX . 16)(Iztapalapa . 19)(Gustavo-A-Madero . 24)(Magdalena-Contreras . 16)(Milpa-Alta . 44)(Azcapotzalco . 18)(Venustiano-Carranza . 20)(Miguel-Hidalgo . 13)(Coyoacan . 12)))
  ;Ciudades de Coahuila
  (set-distancia 'Saltillo '((Monclova . 191)(Torreon . 255)(Ciudad-Acuña . 487)(Piedras-Negras . 432)(Gomez-Farias . 68)(Parras-de-la-Fuente . 150)(Nueva-Rosita . 320)(Ramos-Arizpe . 16)(Monterrey . 86)(El-Salvador . 140)))
  (set-distancia 'Torreon '((Saltillo . 255)(Monclova . 364)(Ciudad-Acuña . 599)(Piedras-Negras . 544)(Gomez-Farias . 315)(Parras-de-la-Fuente . 149)(Nueva-Rosita . 432)(Ramos-Arizpe . 267)(Ciudad-Lerdo . 15)(San-Juan-de-Guadalupe . 156)))
  (set-distancia 'Parras-de-la-Fuente '((Saltillo . 150)(Monclova . 258)(Torreon . 149)(Ciudad-Acuña . 553)(Piedras-Negras . 498)(Gomez-Farias . 207)(Nueva-Rosita . 387)(Ramos-Arizpe . 158)(Jimenez . 510)(San-Juan-de-Guadalupe . 200)(Rio-Grande . 318)(El-Salvador . 213)))
  (set-distancia 'Ramos-Arizpe '((Saltillo . 16)(Monclova . 181)(Torreon . 267)(Ciudad-Acuña . 476)(Piedras-Negras . 421)(Gomez-Farias . 85)(Parras-de-la-Fuente . 158)(Nueva-Rosita . 310)(Monterrey . 74)))
  (set-distancia 'Monclova '((Saltillo . 191)(Torreon . 364)(Ciudad-Acuña . 295)(Piedras-Negras . 240)(Gomez-Farias . 256)(Parras-de-la-Fuente . 258)(Nueva-Rosita . 121)(Ramos-Arizpe . 181)(Camargo . 501)(Jimenez . 430)(Monterrey . 196)(Villaldama . 152)))
  (set-distancia 'Gomez-Farias '((Saltillo . 68)(Monclova . 256)(Torreon . 315)(Ciudad-Acuña . 551)(Piedras-Negras . 496)(Parras-de-la-Fuente . 207)(Nueva-Rosita . 385)(Ramos-Arizpe . 85)(La-Cruz-de-Elorza . 205)(El-Salvador . 77)(Villa-de-La-Paz . 206)))
  (set-distancia 'Ciudad-Acuña '((Saltillo . 487)(Monclova . 295)(Torreon . 599)(Piedras-Negras . 89)(Gomez-Farias . 551)(Parras-de-la-Fuente . 553)(Nueva-Rosita . 171)(Ramos-Arizpe . 476)))
  (set-distancia 'Piedras-Negras '((Saltillo . 432)(Monclova . 240)(Torreon . 544)(Ciudad-Acuña . 89)(Gomez-Farias . 496)(Parras-de-la-Fuente . 498)(Nueva-Rosita . 116)(Ramos-Arizpe . 421)))
  (set-distancia 'Nueva-Rosita '((Saltillo . 320)(Monclova . 121)(Torreon . 432)(Ciudad-Acuña . 171)(Piedras-Negras . 116)(Gomez-Farias . 385)(Parras-de-la-Fuente . 387)(Ramos-Arizpe . 310)(Villaldama . 257)))
  ;Ciudades de Colima
  (set-distancia 'Colima '((Manzanillo . 97)(Cuyutlan . 65)(Queseria . 28)(Cuauhtemoc . 18)(Camotlan-de-Miraflores . 115)(Coquimatlan . 13)(Ciudad-de-Armeria . 53)(Tecoman . 50)(Purificacion . 170)(Apatzingan . 240)))
  (set-distancia 'Manzanillo '((Colima . 97)(Cuyutlan . 33)(Queseria . 122)(Cuauhtemoc . 112)(Camotlan-de-Miraflores . 26)(Coquimatlan . 102)(Ciudad-de-Armeria . 47)(Tecoman . 58)))
  (set-distancia 'Cuyutlan '((Colima . 65)(Manzanillo . 33)(Queseria . 90)(Cuauhtemoc . 79)(Camotlan-de-Miraflores . 52)(Coquimatlan . 69)(Ciudad-de-Armeria . 14)(Tecoman . 25)))
  (set-distancia 'Queseria '((Colima . 28)(Manzanillo . 122)(Cuyutlan . 90)(Cuauhtemoc . 14)(Camotlan-de-Miraflores . 140)(Coquimatlan . 43)(Ciudad-de-Armeria . 78)(Tecoman . 75)(Ciudad-Guzaman . 49)))
  (set-distancia 'Cuauhtemoc '((Colima . 18)(Manzanillo . 112)(Cuyutlan . 79)(Queseria . 14)(Camotlan-de-Miraflores . 129)(Coquimatlan . 33)(Ciudad-de-Armeria . 67)(Tecoman . 64)))
  (set-distancia 'Camotlan-de-Miraflores '((Colima . 115)(Manzanillo . 26)(Cuyutlan . 52)(Queseria . 140)(Cuauhtemoc . 129)(Coquimatlan . 121)(Ciudad-de-Armeria . 66)(Tecoman . 77)(Purificacion . 112)))
  (set-distancia 'Coquimatlan '((Colima . 13)(Manzanillo . 102)(Cuyutlan . 69)(Queseria . 43)(Cuauhtemoc . 33)(Camotlan-de-Miraflores . 121)(Ciudad-de-Armeria . 54)(Tecoman . 51)))
  (set-distancia 'Ciudad-de-Armeria '((Colima . 53)(Manzanillo . 47)(Cuyutlan . 14)(Queseria . 78)(Cuauhtemoc . 67)(Camotlan-de-Miraflores . 66)(Coquimatlan . 54)(Tecoman . 14)))
  (set-distancia 'Tecoman '((Colima . 50)(Manzanillo . 58)(Cuyutlan . 25)(Queseria . 75)(Cuauhtemoc . 64)(Camotlan-de-Miraflores . 77)(Coquimatlan . 51)(Ciudad-de-Armeria . 14)(Lazaro-Cardenas . 285)))
  ;Ciudades de Durango
  (set-distancia 'Ciudad-Lerdo '((Victoria-de-Durango . 235)(Tamazula . 740)(Francisco-I-Madero . 176)(Topia . 560)(Inde . 247)(San-Juan-de-Guadalupe . 153)(La-Ciudad . 361)(Hidalgo-del-Parral . 336)(Jimenez . 240)))
  (set-distancia 'Victoria-de-Durango '((Ciudad-Lerdo . 235)(Tamazula . 510)(Francisco-I-Madero . 72)(Topia . 377)(Inde . 318)(San-Juan-de-Guadalupe . 311)(La-Ciudad . 130)(Chalchihuites . 121)(Rio-Grande . 210)))
  (set-distancia 'Tamazula '((Ciudad-Lerdo . 740)(Victoria-de-Durango . 510)(Francisco-I-Madero . 579)(Topia . 125)(Inde . 383)(San-Juan-de-Guadalupe . 817)(La-Ciudad . 420)(Culiacan . 79)(Cosala . 120)))
  (set-distancia 'Francisco-I-Madero '((Ciudad-Lerdo . 176)(Victoria-de-Durango . 72)(Tamazula . 579)(Topia . 383)(Inde . 275)(San-Juan-de-Guadalupe . 254)(La-Ciudad . 199)))
  (set-distancia 'Topia '((Ciudad-Lerdo . 560)(Victoria-de-Durango . 377)(Tamazula . 125)(Francisco-I-Madero . 383)(Inde . 295)(San-Juan-de-Guadalupe . 637)(La-Ciudad . 506)(Hidalgo-del-Parral . 460)(Jimenez . 515)))
  (set-distancia 'Inde '((Ciudad-Lerdo . 247)(Victoria-de-Durango . 318)(Tamazula . 383)(Francisco-I-Madero . 275)(Topia . 295)(San-Juan-de-Guadalupe . 395)(La-Ciudad . 446)(Hidalgo-del-Parral . 180)(Jimenez . 235)))
  (set-distancia 'San-Juan-de-Guadalupe '((Ciudad-Lerdo . 153)(Victoria-de-Durango . 311)(Tamazula . 817)(Francisco-I-Madero . 254)(Topia . 637)(Inde . 395)(La-Ciudad . 438)(Torreon . 156)(Parras-de-la-Fuente . 200)(Rio-Grande . 129)(El-Salvador . 268)))
  (set-distancia 'La-Ciudad '((Acaponeta . 251)(Ciudad-Lerdo . 361)(Victoria-de-Durango . 130)(Tamazula . 420)(Francisco-I-Madero . 199)(Topia . 506)(Inde . 446)(San-Juan-de-Guadalupe . 438)(Mazatlan . 192)))
  ;Ciudades de Guanajuato
  (set-distancia 'Leon '((Irapuato . 69)(Salamanca . 89)(Celaya . 127)(Guanajuato . 57)(Moroleon . 147)(Paso-de-Vaqueros . 176)(La-Angostura . 128)(Ciudad-Manuel-Doblado . 59)(Salvatierra . 150)(San-Miguel-el-Alto . 87)(San-Luis-Potosi . 188)))
  (set-distancia 'Irapuato '((Leon . 69)(Salamanca . 22)(Celaya . 64)(Guanajuato . 48)(Moroleon . 81)(Paso-de-Vaqueros . 168)(La-Angostura . 61)(Ciudad-Manuel-Doblado . 73)(Salvatierra . 84)(San-Miguel-el-Alto . 160)))
  (set-distancia 'Salamanca '((Leon . 89)(Irapuato . 22)(Celaya . 42)(Guanajuato . 68)(Moroleon . 60)(Paso-de-Vaqueros . 197)(La-Angostura . 42)(Ciudad-Manuel-Doblado . 97)(Salvatierra . 63)(Zamora . 160)))
  (set-distancia 'Celaya '((Leon . 127)(Irapuato . 64)(Salamanca . 42)(Guanajuato . 105)(Moroleon . 89)(Paso-de-Vaqueros . 156)(La-Angostura . 53)(Ciudad-Manuel-Doblado . 138)(Salvatierra . 39)(Queretaro . 78)))
  (set-distancia 'Guanajuato '((Leon . 57)(Irapuato . 48)(Salamanca . 68)(Celaya . 105)(Moroleon . 126)(Paso-de-Vaqueros . 123)(La-Angostura . 107)(Ciudad-Manuel-Doblado . 94)(Salvatierra . 129)(San-Luis-Potosi . 190)(Rancho-del-Puente . 238)))
  (set-distancia 'Moroleon '((Leon . 147)(Irapuato . 81)(Salamanca . 60)(Celaya . 89)(Guanajuato . 126)(Paso-de-Vaqueros . 247)(La-Angostura . 32)(Ciudad-Manuel-Doblado . 155)(Salvatierra . 42)(Queretaro . 136)(Zamora . 180)(Morelia . 55)(Zitacuaro . 182)))
  (set-distancia 'Paso-de-Vaqueros '((Leon . 176)(Irapuato . 168)(Salamanca . 197)(Celaya . 156)(Guanajuato . 123)(Moroleon . 247)(La-Angostura . 209)(Ciudad-Manuel-Doblado . 211)(Salvatierra . 193)(Rancho-del-Puente . 210)(Parque-Industrial-Queretaro . 87)(Jalpan-de-Serra . 190)))
  (set-distancia 'La-Angostura '((Leon . 128)(Irapuato . 61)(Salamanca . 42)(Celaya . 53)(Guanajuato . 107)(Moroleon . 32)(Paso-de-Vaqueros . 209)(Ciudad-Manuel-Doblado . 136)(Salvatierra . 30)))
  (set-distancia 'Ciudad-Manuel-Doblado '((Leon . 59)(Irapuato . 73)(Salamanca . 97)(Celaya . 138)(Guanajuato . 94)(Moroleon . 155)(Paso-de-Vaqueros . 211)(La-Angostura . 136)(Salvatierra . 162)(San-Miguel-el-Alto . 95)(Cuquio . 190)(Guadalajara . 190)(El-Chante . 218)))
  (set-distancia 'Salvatierra '((Leon . 150)(Irapuato . 84)(Salamanca . 63)(Celaya . 39)(Guanajuato . 129)(Moroleon . 42)(Paso-de-Vaqueros . 193)(La-Angostura . 30)(Ciudad-Manuel-Doblado . 162)(Zitacuaro . 190)))
  ;Ciudades de Guerrero
  (set-distancia 'Acapulco '((Chilpancingo . 105)(Tepetixtla . 74)(Cuajinicuilapa . 206)(Zumpango-del-Rio . 122)(Iguala . 211)(Arcelia . 342)(Tuliman . 174)(Pueblo-Viejo . 57)(Olinala . 249)))
  (set-distancia 'Chilpancingo '((Acapulco . 105)(Tepetixtla . 177)(Cuajinicuilapa . 263)(Zumpango-del-Rio . 18)(Iguala . 106)(Arcelia . 238)(Tuliman . 69)(Pueblo-Viejo . 161)(Olinala . 154)))
  (set-distancia 'Tepetixtla '((Acapulco . 74)(Chilpancingo . 177)(Cuajinicuilapa . 279)(Zumpango-del-Rio . 195)(Iguala . 283)(Arcelia . 415)(Tuliman . 246)(Pueblo-Viejo . 38)(Olinala . 322)(Lazaro-Cardenas . 332)))
  (set-distancia 'Cuajinicuilapa '((Acapulco . 206)(Chilpancingo . 263)(Tepetixtla . 279)(Zumpango-del-Rio . 280)(Iguala . 369)(Arcelia . 500)(Tuliman . 332)(Pueblo-Viejo . 262)(Olinala . 272)(Oaxaca . 300)(Pinotepa . 51)))
  (set-distancia 'Zumpango-del-Rio '((Acapulco . 122)(Chilpancingo . 18)(Tepetixtla . 195)(Cuajinicuilapa . 280)(Iguala . 91)(Arcelia . 222)(Tuliman . 63)(Pueblo-Viejo . 173)(Olinala . 155)))
  (set-distancia 'Iguala '((Acapulco . 211)(Chilpancingo . 106)(Tepetixtla . 283)(Cuajinicuilapa . 369)(Zumpango-del-Rio . 91)(Arcelia . 132)(Tuliman . 77)(Pueblo-Viejo . 266)(Olinala . 162)(Puente-de-Ixtla . 65)(Toluca-de-Lerdo . 175)))
  (set-distancia 'Arcelia '((Acapulco . 342)(Chilpancingo . 238)(Tepetixtla . 415)(Cuajinicuilapa . 500)(Zumpango-del-Rio . 222)(Iguala . 132)(Tuliman . 209)(Pueblo-Viejo . 397)(Olinala . 293)(Churumuco-de-Morelos . 250)))
  (set-distancia 'Tuliman '((Acapulco . 174)(Chilpancingo . 69)(Tepetixtla . 246)(Cuajinicuilapa . 332)(Zumpango-del-Rio . 63)(Iguala . 77)(Arcelia . 209)(Pueblo-Viejo . 278)(Olinala . 140)(Puente-de-Ixtla . 103)(Izucar-de-Matamoros . 175)))
  (set-distancia 'Pueblo-Viejo '((Acapulco . 57)(Chilpancingo . 161)(Tepetixtla . 38)(Cuajinicuilapa . 262)(Zumpango-del-Rio . 173)(Iguala . 266)(Arcelia . 397)(Tuliman . 278)(Olinala . 305)(Lazaro-Cardenas . 356)))
  (set-distancia 'Olinala '((Acapulco . 249)(Chilpancingo . 154)(Tepetixtla . 322)(Cuajinicuilapa . 272)(Zumpango-del-Rio . 155)(Iguala . 162)(Arcelia . 293)(Tuliman . 140)(Pueblo-Viejo . 305)(Tulancingo . 75)))
  ;Ciudades de Hidalgo
  (set-distancia 'Pachuca-de-Soto '((Progreso . 63)(Tizayuca . 41)(Ixmiquilpan . 76)(Ciudad-Sahagun . 46)(Tepeji-de-Ocampo . 91)(Tlaxcoapan . 73)(Tulancingo . 47)))
  (set-distancia 'Progreso '((Pachuca-de-Soto . 63)(Tizayuca . 79)(Ixmiquilpan . 32)(Tlaxcoapan . 21)(Tequisquiapan . 110)(Jalpan-de-Serra . 360)))
  (set-distancia 'Tizayuca '((Pachuca-de-Soto . 41)(Progreso . 79)(Ciudad-Sahagun . 57)(Tepeji-de-Ocampo . 51)(Tlaxcoapan . 59)(Tecamac . 15)))
  (set-distancia 'Ixmiquilpan '((Pachuca-de-Soto . 76)(Progreso . 32)))
  (set-distancia 'Ciudad-Sahagun '((Pachuca-de-Soto . 46)(Tizayuca . 57)(Apan . 19)(Tlaxcoapan . 86)(Tulancingo . 51)(Calpulalpan . 29)(Ixtlahuaca . 37)))
  (set-distancia 'Apan '((Ciudad-Sahagun . 19)(Tulancingo . 63)(Calpulalpan . 23)))
  (set-distancia 'Tepeji-de-Ocampo '((Pachuca-de-Soto . 91)(Tizayuca . 51)(Tlaxcoapan . 32)(San-Juan-del-Rio . 96)))
  (set-distancia 'Tlaxcoapan '((Pachuca-de-Soto . 73)(Progreso . 21)(Tizayuca . 59)(Ciudad-Sahagun . 86)(Tepeji-de-Ocampo . 32)))
  (set-distancia 'Tulancingo '((Pachuca-de-Soto . 47)(Ciudad-Sahagun . 51)(Apan . 63)(Zacatlan . 72)(Poza-Rica . 149)))
  ;Ciudades de Jalisco
  (set-distancia 'Puerto-Vallarta '((Guadalajara . 332)(Ameca . 224)(Purificacion . 197)(Cuquio . 403)(San-Miguel-el-Alto . 451)(El-Chante . 264)(El-Rosario . 210)(Amatlan-de-Cañas . 159)))
  (set-distancia 'Guadalajara '((Puerto-Vallarta . 332)(Ameca . 86)(Purificacion . 254)(Cuquio . 79)(San-Miguel-el-Alto . 122)(El-Chante . 202)(Amatlan-de-Cañas . 159)(Ciudad-Manuel-Doblado . 190)(Ixtlan-de-los-Hervores . 131)))
  (set-distancia 'Ameca '((Puerto-Vallarta . 224)(Guadalajara . 86)(Purificacion . 204)(Cuquio . 156)(San-Miguel-el-Alto . 204)(El-Chante . 152)(Amatlan-de-Cañas . 120)))
  (set-distancia 'Purificacion '((Puerto-Vallarta . 197)(Guadalajara . 254)(Ameca . 204)(Cuquio . 327)(San-Miguel-el-Alto . 371)(El-Chante . 82)(Camotlan-de-Miraflores . 112)(Colima . 170)))
  (set-distancia 'Cuquio '((Puerto-Vallarta . 403)(Guadalajara . 79)(Ameca . 156)(Purificacion . 327)(San-Miguel-el-Alto . 106)(El-Chante . 274)(Nochistlan-de-Mejia . 104)(Ciudad-Manuel-Doblado . 190)))
  (set-distancia 'San-Miguel-el-Alto '((Puerto-Vallarta . 451)(Guadalajara . 122)(Ameca . 204)(Purificacion . 371)(Cuquio . 106)(El-Chante . 318)(Nochistlan-de-Mejia . 96)(Aguascalientes . 111)(Leon . 87)(Irapuato . 160)(Ciudad-Manuel-Doblado . 95)))
  (set-distancia 'El-Chante '((Puerto-Vallarta . 264)(Guadalajara . 202)(Ameca . 152)(Purificacion . 82)(Cuquio . 274)(San-Miguel-el-Alto . 318)(Ciudad-Manuel-Doblado . 218)(San-Jose-de-Gracia . 90)))
  (set-distancia 'Villa-Guerrero '((Guadalajara . 210)(Puente-de-Camotlan . 145)(Jerez . 132)))
  (set-distancia 'Huejuquilla-el-Alto '((Villa-Guerrero . 139)(Puente-de-Camotlan . 202)(Tepic . 335)(Villa-Hidalgo . 332)(Fresnillo . 139)(Jerez . 137)))
  (set-distancia 'Ciudad-Guzaman '((Purificacion . 108)(El-Chante . 116)(Ameca . 144)(Queseria . 49)(Tacintaro . 217)))
  ;Ciudades de Estado de México
  (set-distancia 'Toluca-de-Lerdo '((Santiago-Tianguistenco . 32)(Jiquipilco . 44)(San-Francisco-Coacalco . 93)(Zitacuaro . 89)(Iguala . 175)(Magdalena-Contreras . 54)))
  (set-distancia 'Santiago-Tianguistenco '((Toluca-de-Lerdo . 32)(Jiquipilco . 59)(Santa-Martha . 19)(Magdalena-Contreras . 49)))
  (set-distancia 'Ixtlahuaca '((Tecamac . 140)(Ecatepec . 125)(Texcoco . 129)(San-Francisco-Coacalco . 126)(Ciudad-Sahagun . 37)(Calpulalpan . 50)))
  (set-distancia 'Jiquipilco '((Toluca-de-Lerdo . 44)(Santiago-Tianguistenco . 59)(Tecamac . 102)(San-Francisco-Coacalco . 80)(Zitacuaro . 103)(San-Juan-del-Rio . 127)(Azcapotzalco . 72)(Miguel-Hidalgo . 73)))
  (set-distancia 'Tecamac '((Ixtlahuaca . 140)(Jiquipilco . 102)(Ecatepec . 16)(Texcoco . 33)(San-Francisco-Coacalco . 22)(Tizayuca . 15)))
  (set-distancia 'Ecatepec '((Ixtlahuaca . 125)(Tecamac . 16)(Texcoco . 24)(San-Francisco-Coacalco . 8)(Gustavo-A-Madero . 16)))
  (set-distancia 'Texcoco '((Ixtlahuaca . 129)(Tecamac . 33)(Ecatepec . 24)(Calpulalpan . 39)(Puebla . 120)(Totolapan . 83)(Venustiano-Carranza . 28)))
  (set-distancia 'San-Francisco-Coacalco '((Toluca-de-Lerdo . 93)(Ixtlahuaca . 126)(Jiquipilco . 80)(Tecamac . 22)(Ecatepec . 8)(Santa-Martha . 99)(San-Juan-del-Rio . 120)))
  (set-distancia 'Santa-Martha '((Santiago-Tianguistenco . 19)(San-Francisco-Coacalco . 99)(Cuernavaca . 40)))
  ;Ciudad de Michoacán
  (set-distancia 'Morelia '((Lazaro-Cardenas . 314)(Apatzingan . 186)(Zitacuaro . 137)(Zamora . 176)(Uruapan . 109)(Tacintaro . 163)(Churumuco-de-Morelos . 228)(Ixtlan-de-los-Hervores . 177)(San-Jose-de-Gracia . 271)(Moroleon . 55)))
  (set-distancia 'Lazaro-Cardenas '((Morelia . 314)(Apatzingan . 193)(Zitacuaro . 483)(Zamora . 325)(Uruapan . 234)(Tacintaro . 234)(Churumuco-de-Morelos . 212)(Ixtlan-de-los-Hervores . 355)(San-Jose-de-Gracia . 382)(Tecoman . 285)(Tepetixtla . 332)(Pueblo-Viejo . 356)))
  (set-distancia 'Apatzingan '((Morelia . 186)(Lazaro-Cardenas . 193)(Zitacuaro . 356)(Zamora . 159)(Uruapan . 107)(Tacintaro . 43)(Churumuco-de-Morelos . 108)(Ixtlan-de-los-Hervores . 188)(San-Jose-de-Gracia . 190)(Colima . 240)))
  (set-distancia 'Zitacuaro '((Morelia . 137)(Lazaro-Cardenas . 483)(Apatzingan . 356)(Zamora . 301)(Uruapan . 279)(Tacintaro . 334)(Churumuco-de-Morelos . 273)(Ixtlan-de-los-Hervores . 302)(San-Jose-de-Gracia . 397)(Moroleon . 182)(Salvatierra . 190)(San-Juan-del-Rio . 208)(Toluca-de-Lerdo . 89)(Jiquipilco . 103)))
  (set-distancia 'Zamora '((Morelia . 176)(Lazaro-Cardenas . 325)(Apatzingan . 159)(Zitacuaro . 301)(Uruapan . 92)(Tacintaro . 108)(Churumuco-de-Morelos . 240)(Ixtlan-de-los-Hervores . 32)(San-Jose-de-Gracia . 99)(Salamanca . 160)(Moroleon . 180)))
  (set-distancia 'Uruapan '((Morelia . 109)(Lazaro-Cardenas . 234)(Apatzingan . 107)(Zitacuaro . 279)(Zamora . 92)(Tacintaro . 56)(Churumuco-de-Morelos . 149)(Ixtlan-de-los-Hervores . 122)(San-Jose-de-Gracia . 178)))
  (set-distancia 'Tacintaro '((Morelia . 163)(Lazaro-Cardenas . 234)(Apatzingan . 43)(Zitacuaro . 334)(Zamora . 108)(Uruapan . 56)(Churumuco-de-Morelos . 149)(Ixtlan-de-los-Hervores . 136)(San-Jose-de-Gracia . 138)(Ciudad-Guzaman . 217)))
  (set-distancia 'Churumuco-de-Morelos '((Morelia . 228)(Lazaro-Cardenas . 212)(Apatzingan . 108)(Zitacuaro . 273)(Zamora . 240)(Uruapan . 149)(Tacintaro . 149)(Ixtlan-de-los-Hervores . 270)(San-Jose-de-Gracia . 297)(Arcelia . 250)))
  (set-distancia 'Ixtlan-de-los-Hervores '((Morelia . 177)(Lazaro-Cardenas . 355)(Apatzingan . 188)(Zitacuaro . 302)(Zamora . 32)(Uruapan . 122)(Tacintaro . 136)(Churumuco-de-Morelos . 270)(San-Jose-de-Gracia . 88)(Guadalajara . 131)))
  (set-distancia 'San-Jose-de-Gracia '((Morelia . 271)(Lazaro-Cardenas . 382)(Apatzingan . 190)(Zitacuaro . 397)(Zamora . 99)(Uruapan . 178)(Tacintaro . 138)(Churumuco-de-Morelos . 297)(Ixtlan-de-los-Hervores . 88)(El-Chante . 90)))
  ;Ciudades de Morelos
  (set-distancia 'Cuernavaca '((Puente-de-Ixtla . 39)(Yautepec . 270)(Temixco . 9)(Miacatlan . 43)(Totolapan . 59)(Tlayacapan . 50)(Yecapixtla . 61)(Milpa-Alta . 77)(Magdalena-Contreras . 70)(Santa-Martha . 40)))
  (set-distancia 'Puente-de-Ixtla '((Cuernavaca . 39)(Yautepec . 62)(Temixco . 32)(Miacatlan . 21)(Totolapan . 102)(Tlayacapan . 93)(Yecapixtla . 104)(Iguala . 65)(Tuliman . 103)))
  (set-distancia 'Yautepec '((Cuernavaca . 270)(Puente-de-Ixtla . 62)(Temixco . 33)(Miacatlan . 67)(Totolapan . 29)(Tlayacapan . 20)(Yecapixtla . 27)(Izucar-de-Matamoros . 89)))
  (set-distancia 'Temixco '((Cuernavaca . 9)(Puente-de-Ixtla . 32)(Yautepec . 33)(Miacatlan . 36)(Totolapan . 75)(Tlayacapan . 65)(Yecapixtla . 77)))
  (set-distancia 'Miacatlan '((Cuernavaca . 43)(Puente-de-Ixtla . 21)(Yautepec . 67)(Temixco . 36)(Totolapan . 107)(Tlayacapan . 98)(Yecapixtla . 109)))
  (set-distancia 'Totolapan '((Cuernavaca . 59)(Puente-de-Ixtla . 102)(Yautepec . 29)(Temixco . 75)(Miacatlan . 107)(Tlayacapan . 11)(Yecapixtla . 17)(Texcoco . 83)))
  (set-distancia 'Tlayacapan '((Cuernavaca . 50)(Puente-de-Ixtla . 93)(Yautepec . 20)(Temixco . 65)(Miacatlan . 98)(Totolapan . 11)(Yecapixtla . 23)))
  (set-distancia 'Yecapixtla '((Cuernavaca . 61)(Puente-de-Ixtla . 104)(Yautepec . 27)(Temixco . 77)(Miacatlan . 109)(Totolapan . 17)(Tlayacapan . 23)(Atlixco . 82)(Izucar-de-Matamoros . 67)(Milpa-Alta . 57)))
  ;Ciudades de Nayarit
  (set-distancia 'Acaponeta '((Campo-de-los-Limones . 120)(Villa-Hidalgo . 110)(Tepic . 133)(Mazatlan . 163)(La-Ciudad . 251)))
  (set-distancia 'Tepic '((Puente-de-Camotlan . 277)(Acaponeta . 133)(Compostela . 38)(El-Rosario . 107)(Campo-de-los-Limones . 82)(Amatlan-de-Cañas . 121)(San-Blas . 54)(Villa-Hidalgo . 60)(Huejuquilla-el-Alto . 335)))
  (set-distancia 'Compostela '((Puente-de-Camotlan . 278)(Tepic . 38)(El-Rosario . 97)(Campo-de-los-Limones . 119)(Amatlan-de-Cañas . 111)(San-Blas . 91)(Villa-Hidalgo . 97)))
  (set-distancia 'El-Rosario '((Puente-de-Camotlan . 240)(Tepic . 107)(Compostela . 97)(Campo-de-los-Limones . 192)(Amatlan-de-Cañas . 13)(San-Blas . 164)(Puerto-Vallarta . 210)))
  (set-distancia 'Campo-de-los-Limones '((Acaponeta . 120)(Tepic . 82)(Compostela . 119)(El-Rosario . 192)(Amatlan-de-Cañas . 210)(San-Blas . 65)(Villa-Hidalgo . 35)))
  (set-distancia 'Amatlan-de-Cañas '((Tepic . 121)(Compostela . 11)(El-Rosario . 13)(Campo-de-los-Limones . 210)(San-Blas . 177)(Puerto-Vallarta . 159)(Ameca . 120)(Guadalajara . 159)))
  (set-distancia 'San-Blas '((Tepic . 54)(Compostela . 91)(El-Rosario . 164)(Campo-de-los-Limones . 65)(Amatlan-de-Cañas . 177)(Villa-Hidalgo . 31)))
  (set-distancia 'Villa-Hidalgo '((Acaponeta . 110)(Tepic . 60)(Compostela . 97)(Campo-de-los-Limones . 35)(San-Blas . 31)(Huejuquilla-el-Alto . 332)))
  (set-distancia 'Puente-de-Camotlan '((Tepic . 277)(Compostela . 278)(El-Rosario . 240)(Huejuquilla-el-Alto . 202)(Villa-Guerrero . 145)))
  ;Ciudades de Nuevo Leon
  (set-distancia 'Monterrey '((Linares . 130)(La-Cruz-de-Elorza . 297)(Aramberri . 321)(Agualeguas . 128)(China . 123)(Villaldama . 99)(Apodaca . 23)(Monclova . 196)(Saltillo . 86)(Ramos-Arizpe . 74)))
  (set-distancia 'La-Cruz-de-Elorza '((Linares . 205)(Monterrey . 297)(Aramberri . 118)(Agualeguas . 421)(China . 346)(Villaldama . 382)(Apodaca . 319)(Gomez-Farias . 205)(Villa-de-La-Paz . 38)))
  (set-distancia 'Aramberri '((Linares . 172)(Monterrey . 321)(La-Cruz-de-Elorza . 118)(Agualeguas . 444)(China . 312)(Villaldama . 405)(Apodaca . 342)(Tula . 210)(Ciudad-Victoria . 232)))
  (set-distancia 'Agualeguas '((Monterrey . 128)(La-Cruz-de-Elorza . 421)(Aramberri . 444)(China . 103)(Villaldama . 114)(Apodaca . 107)(Nuevo-Laredo . 174)))
  (set-distancia 'China '((Linares . 143)(Monterrey . 123)(La-Cruz-de-Elorza . 346)(Aramberri . 312)(Agualeguas . 103)(Villaldama . 201)(Apodaca . 116)(Reynosa . 108)(Nuevo-Laredo . 270)))
  (set-distancia 'Villaldama '((Monterrey . 99)(La-Cruz-de-Elorza . 382)(Aramberri . 405)(Agualeguas . 114)(China . 201)(Apodaca . 89)(Monclova . 152)(Nueva-Rosita . 257)(Nuevo-Laredo . 161)))
  (set-distancia 'Apodaca '((Monterrey . 23)(La-Cruz-de-Elorza . 319)(Aramberri . 342)(Agualeguas . 107)(China . 116)(Villaldama . 89)))
  (set-distancia 'Linares '((Monterrey . 130)(China . 143)(Aramberri . 172)(La-Cruz-de-Elorza . 205)(Ciudad-Victoria . 155)(Reynosa . 250)))
  ;Ciudades de Oaxaca
  (set-distancia 'Oaxaca '((Santo-Domingo-Ingenio . 320)(San-Pedro-Tapantepec . 388)(Ciudad-Ixtepec . 280)(Santo-Domingo-Zanatepec . 365)(Loma-Bonita . 253)(Pinotepa . 327)(Salina-Cruz . 266)(Cuajinicuilapa . 300)(San-Sebastian-Zinacatepec . 196)))
  (set-distancia 'Santo-Domingo-Ingenio '((Oaxaca . 320)(San-Pedro-Tapantepec . 73)(Ciudad-Ixtepec . 48)(Santo-Domingo-Zanatepec . 50)(Loma-Bonita . 288)(Pinotepa . 476)(Salina-Cruz . 81)))
  (set-distancia 'San-Pedro-Tapantepec '((Oaxaca . 388)(Santo-Domingo-Ingenio . 73)(Ciudad-Ixtepec . 117)(Santo-Domingo-Zanatepec . 24)(Loma-Bonita . 337)(Pinotepa . 544)(Salina-Cruz . 149)(Tapachula . 287)(Villaflores . 140)))
  (set-distancia 'Ciudad-Ixtepec '((Oaxaca . 280)(Santo-Domingo-Ingenio . 48)(San-Pedro-Tapantepec . 117)(Santo-Domingo-Zanatepec . 94)(Loma-Bonita . 274)(Pinotepa . 436)(Salina-Cruz . 56)))
  (set-distancia 'Santo-Domingo-Zanatepec '((Oaxaca . 365)(Santo-Domingo-Ingenio . 50)(San-Pedro-Tapantepec . 24)(Ciudad-Ixtepec . 94)(Loma-Bonita . 314)(Pinotepa . 521)(Salina-Cruz . 127)))
  (set-distancia 'Loma-Bonita '((Oaxaca . 253)(Santo-Domingo-Ingenio . 288)(San-Pedro-Tapantepec . 337)(Ciudad-Ixtepec . 274)(Santo-Domingo-Zanatepec . 314)(Pinotepa . 578)(Salina-Cruz . 321)(San-Sebastian-Zinacatepec . 221)(Tierra-Blanca . 72)(Santiago-Tuxtla . 93)(Minatitlan . 150)))
  (set-distancia 'Pinotepa '((Oaxaca . 327)(Santo-Domingo-Ingenio . 476)(San-Pedro-Tapantepec . 544)(Ciudad-Ixtepec . 436)(Santo-Domingo-Zanatepec . 521)(Loma-Bonita . 578)(Salina-Cruz . 392)(Cuajinicuilapa . 51)))
  (set-distancia 'Salina-Cruz '((Oaxaca . 266)(Santo-Domingo-Ingenio . 81)(San-Pedro-Tapantepec . 149)(Ciudad-Ixtepec . 56)(Santo-Domingo-Zanatepec . 127)(Loma-Bonita . 321)(Pinotepa . 392)))
  ;Ciudades de Puebla
  (set-distancia 'Zacatlan '((Oriental . 265)(Tlaxco . 55)(Tulancingo . 72)(Poza-Rica . 162)))
  (set-distancia 'Puebla '((Izucar-de-Matamoros . 68)(Tehuacan . 132)(Cuacnopalan . 88)(Tulcingo-de-Valle . 165)(San-Jose-Ixtapa . 98)(Oriental . 84)(Atlixco . 33)(Tlaxcala . 37)(Teolocholco . 27)(Huamantla . 55)(Texcoco . 120)))
  (set-distancia 'Izucar-de-Matamoros '((Puebla . 68)(Tehuacan . 197)(Cuacnopalan . 153)(Tulcingo-de-Valle . 98)(San-Jose-Ixtapa . 164)(Oriental . 150)(Atlixco . 37)(Yecapixtla . 67)(Yautepec . 89)(Tuliman . 175)))
  (set-distancia 'Tehuacan '((Puebla . 132)(Izucar-de-Matamoros . 197)(Cuacnopalan . 49)(Tulcingo-de-Valle . 207)(San-Sebastian-Zinacatepec . 24)(San-Jose-Ixtapa . 36)(Oriental . 135)(Atlixco . 162)))
  (set-distancia 'Cuacnopalan '((Puebla . 88)(Izucar-de-Matamoros . 153)(Tehuacan . 49)(Tulcingo-de-Valle . 251)(San-Jose-Ixtapa . 16)(Oriental . 92)(Atlixco . 119)(Huamantla . 99)))
  (set-distancia 'Tulcingo-de-Valle '((Puebla . 165)(Izucar-de-Matamoros . 98)(Tehuacan . 207)(Cuacnopalan . 251)(San-Sebastian-Zinacatepec . 213)(San-Jose-Ixtapa . 224)(Atlixco . 138)(Olinala . 75)))
  (set-distancia 'San-Sebastian-Zinacatepec '((Tehuacan . 24)(Tulcingo-de-Valle . 213)(San-Jose-Ixtapa . 61)(Oaxaca . 196)(Loma-Bonita . 221)))
  (set-distancia 'San-Jose-Ixtapa '((Puebla . 98)(Izucar-de-Matamoros . 164)(Tehuacan . 36)(Cuacnopalan . 16)(Tulcingo-de-Valle . 224)(San-Sebastian-Zinacatepec . 61)(Oriental . 102)(Atlixco . 129)))
  (set-distancia 'Oriental '((Zacatlan . 265)(Puebla . 84)(Izucar-de-Matamoros . 150)(Tehuacan . 135)(Cuacnopalan . 92)(San-Jose-Ixtapa . 102)(Atlixco . 120)(Huamantla . 39)(Villa-de-El-Carmen-Tequexquitla . 7)(Xalapa . 99)(Orizaba . 130)))
  (set-distancia 'Atlixco '((Puebla . 33)(Izucar-de-Matamoros . 37)(Tehuacan . 162)(Cuacnopalan . 119)(Tulcingo-de-Valle . 138)(San-Jose-Ixtapa . 129)(Oriental . 120)(Yecapixtla . 82)))
  ;Ciudades de Queretaro
  (set-distancia 'Queretaro '((San-Juan-del-Rio . 53)(Tequisquiapan . 62)(Jalpan-de-Serra . 186)(Ezequiel-Montes . 62)(Santa-Rosa-Jauregui . 21)(Parque-Industrial-Queretaro . 31)(Moroleon . 136)(Celaya . 78)))
  (set-distancia 'San-Juan-del-Rio '((Queretaro . 53)(Tequisquiapan . 21)(Jalpan-de-Serra . 181)(Ezequiel-Montes . 37)(Santa-Rosa-Jauregui . 72)(Parque-Industrial-Queretaro . 72)(Zitacuaro . 208)(Tepeji-de-Ocampo . 96)(San-Francisco-Coacalco . 120)(Jiquipilco . 127)))
  (set-distancia 'Tequisquiapan '((Queretaro . 62)(San-Juan-del-Rio . 21)(Jalpan-de-Serra . 162)(Ezequiel-Montes . 18)(Santa-Rosa-Jauregui . 76)(Parque-Industrial-Queretaro . 76)(Progreso . 110)))
  (set-distancia 'Jalpan-de-Serra '((Queretaro . 186)(San-Juan-del-Rio . 181)(Tequisquiapan . 162)(Ezequiel-Montes . 142)(Santa-Rosa-Jauregui . 199)(Parque-Industrial-Queretaro . 199)(Paso-de-Vaqueros . 190)(Rancho-del-Puente . 158)(Cardenas . 123)(Tancahuitz-de-Santos . 124)(Matlapa . 115)(Progreso . 360)))
  (set-distancia 'Ezequiel-Montes '((Queretaro . 62)(San-Juan-del-Rio . 37)(Tequisquiapan . 18)(Jalpan-de-Serra . 142)(Santa-Rosa-Jauregui . 76)(Parque-Industrial-Queretaro . 76)))
  (set-distancia 'Santa-Rosa-Jauregui '((Queretaro . 21)(San-Juan-del-Rio . 72)(Tequisquiapan . 76)(Jalpan-de-Serra . 199)(Ezequiel-Montes . 76)(Parque-Industrial-Queretaro . 12)))
  (set-distancia 'Parque-Industrial-Queretaro '((Queretaro . 31)(San-Juan-del-Rio . 72)(Tequisquiapan . 76)(Jalpan-de-Serra . 199)(Ezequiel-Montes . 76)(Santa-Rosa-Jauregui . 12)(Paso-de-Vaqueros . 87)))
  ;Ciudades de Quintana Roo
  (set-distancia 'Cancun '((Playa-del-Carmen . 68)(Chetumal . 378)(Kantunilkin . 98)(Jose-Maria-Morelos . 309)(Puerto-Morelos . 38)(Tulum . 130)))
  (set-distancia 'Playa-del-Carmen '((Cancun . 68)(Chetumal . 312)(Kantunilkin . 79)(Jose-Maria-Morelos . 238)(Puerto-Morelos . 36)(Tulum . 64)))
  (set-distancia 'Chetumal '((Cancun . 378)(Playa-del-Carmen . 312)(Kantunilkin . 371)(Jose-Maria-Morelos . 198)(Puerto-Morelos . 346)(Tulum . 248)(Xpujil . 158)))
  (set-distancia 'Kantunilkin '((Cancun . 98)(Playa-del-Carmen . 79)(Chetumal . 371)(Jose-Maria-Morelos . 266)(Puerto-Morelos . 107)(Tulum . 123)(Tizimin . 75)(Valladolid . 117)))
  (set-distancia 'Jose-Maria-Morelos '((Cancun . 309)(Playa-del-Carmen . 238)(Chetumal . 198)(Kantunilkin . 266)(Puerto-Morelos . 272)(Tulum . 175)(Valladolid . 150)(Hopelchen . 160)))
  (set-distancia 'Puerto-Morelos '((Cancun . 38)(Playa-del-Carmen . 36)(Chetumal . 346)(Kantunilkin . 107)(Jose-Maria-Morelos . 272)(Tulum . 97)))
  (set-distancia 'Tulum '((Cancun . 130)(Playa-del-Carmen . 64)(Chetumal . 248)(Kantunilkin . 123)(Jose-Maria-Morelos . 175)(Puerto-Morelos . 97)))
  ;Ciudades de San Luis Potosi
  (set-distancia 'Ciudad-Valles '((San-Luis-Potosi . 252)(Rancho-del-Puente . 173)(Tancahuitz-de-Santos . 52)(Cardenas . 97)(Matlapa . 91)(Aldama . 205)(Gomez-Farias . 206)))
  (set-distancia 'San-Luis-Potosi '((Ciudad-Valles . 252)(Rancho-del-Puente . 88)(Tancahuitz-de-Santos . 298)(Cardenas . 187)(Matlapa . 337)(Venado . 109)(Villa-de-La-Paz . 203)(Tula . 191)(Villa-Gonzalez-Ortega . 145)(Leon . 188)(Guanajuato . 190)))
  (set-distancia 'Rancho-del-Puente '((Ciudad-Valles . 173)(San-Luis-Potosi . 88)(Tancahuitz-de-Santos . 220)(Cardenas . 108)(Matlapa . 258)(Venado . 190)(Villa-de-La-Paz . 268)(Tula . 219)(Guanajuato . 238)(Paso-de-Vaqueros . 210)(Jalpan-de-Serra . 158)))
  (set-distancia 'Tancahuitz-de-Santos '((Ciudad-Valles . 52)(San-Luis-Potosi . 298)(Rancho-del-Puente . 220)(Cardenas . 144)(Matlapa . 44)(Jalpan-de-Serra . 124)))
  (set-distancia 'Cardenas '((Ciudad-Valles . 97)(San-Luis-Potosi . 187)(Rancho-del-Puente . 108)(Tancahuitz-de-Santos . 144)(Matlapa . 183)(Venado . 236)(Tula . 199)(Jalpan-de-Serra . 123)))
  (set-distancia 'Matlapa '((Ciudad-Valles . 91)(San-Luis-Potosi . 337)(Rancho-del-Puente . 258)(Tancahuitz-de-Santos . 44)(Cardenas . 183)(Jalpan-de-Serra . 115)))
  (set-distancia 'Venado '((San-Luis-Potosi . 109)(Rancho-del-Puente . 190)(Cardenas . 236)(Villa-de-La-Paz . 122)(Fresnillo . 242)(Villa-Gonzalez-Ortega . 159)))
  (set-distancia 'Villa-de-La-Paz '((San-Luis-Potosi . 203)(Rancho-del-Puente . 268)(Venado . 122)(La-Cruz-de-Elorza . 38)(Ciudad-Victoria . 260)(El-Salvador . 122)(Fresnillo . 320)))
  ;Ciudades de Sinaloa
  (set-distancia 'Dimas '((Costa-Rica . 132)(Mazatlan . 72)(Cosala . 108)))
  (set-distancia 'La-Reforma '((Culiacan . 89)(Gabriel-Leyva-Solano . 109)(Gustavo-Diaz-Ordaz . 202)))
  (set-distancia 'Culiacan '((La-Reforma . 89)(Costa-Rica . 35)(Cosala . 123)(Gustavo-Diaz-Ordaz . 267)(Hidalgo-del-Parral . 703)(Tamazula . 79)))
  (set-distancia 'Los-Mochis '((Gabriel-Leyva-Solano . 43)(Navojoa . 163)(Yepachic . 620)))
  (set-distancia 'Costa-Rica '((Dimas . 132)(Culiacan . 35)(Cosala . 119)))
  (set-distancia 'Gabriel-Leyva-Solano '((La-Reforma . 109)(Los-Mochis . 43)(Gustavo-Diaz-Ordaz . 95)))
  (set-distancia 'Mazatlan '((Dimas . 72)(Cosala . 178)(La-Ciudad . 192)(Acaponeta . 163)))
  (set-distancia 'Cosala '((Dimas . 108)(Culiacan . 123)(Costa-Rica . 119)(Mazatlan . 178)(Tamazula . 120)))
  (set-distancia 'Gustavo-Diaz-Ordaz '((La-Reforma . 202)(Culiacan . 267)(Gabriel-Leyva-Solano . 95)(Hidalgo-del-Parral . 721)))
  ;Ciudades de Sonora
  (set-distancia 'Golfo-de-Santa-Clara '((Puerto-Peñasco . 148)(Heroica-Nogales . 635)(Ciudad-Guadalupe-Victoria . 97)(Mexicali . 146)))
  (set-distancia 'Navojoa '((Ciudad-Obregon . 68)(Hermosillo . 322)(Guaymas . 196)(Heroica-Ciudad-de-Ures . 397)(Agua-Prieta . 698)(Yepachic . 320)(Hidalgo-del-Parral . 669)(Los-Mochis . 163)))
  (set-distancia 'Puerto-Peñasco '((Golfo-de-Santa-Clara . 148)(Heroica-Caborca . 180)(Hermosillo . 456)(Guaymas . 592)(Heroica-Ciudad-de-Ures . 508)(Heroica-Nogales . 396)))
  (set-distancia 'Ciudad-Obregon '((Navojoa . 68)(Hermosillo . 254)(Guaymas . 128)(Heroica-Ciudad-de-Ures . 329)(Agua-Prieta . 630)(Yepachic . 306)))
  (set-distancia 'Heroica-Caborca '((Puerto-Peñasco . 180)(Hermosillo . 277)(Guaymas . 413)(Heroica-Ciudad-de-Ures . 328)(Heroica-Nogales . 216)(Agua-Prieta . 313)))
  (set-distancia 'Hermosillo '((Navojoa . 322)(Puerto-Peñasco . 456)(Ciudad-Obregon . 254)(Heroica-Caborca . 277)(Guaymas . 137)(Heroica-Ciudad-de-Ures . 76)(Heroica-Nogales . 281)(Agua-Prieta . 377)(Yepachic . 360)))
  (set-distancia 'Guaymas '((Navojoa . 196)(Puerto-Peñasco . 592)(Ciudad-Obregon . 128)(Heroica-Caborca . 413)(Hermosillo . 137)(Heroica-Ciudad-de-Ures . 212)(Heroica-Nogales . 417)(Agua-Prieta . 513)))
  (set-distancia 'Heroica-Ciudad-de-Ures '((Navojoa . 397)(Puerto-Peñasco . 508)(Ciudad-Obregon . 329)(Heroica-Caborca . 328)(Hermosillo . 76)(Guaymas . 212)(Heroica-Nogales . 331)(Agua-Prieta . 300)))
  (set-distancia 'Heroica-Nogales '((Golfo-de-Santa-Clara . 635)(Puerto-Peñasco . 396)(Heroica-Caborca . 216)(Hermosillo . 281)(Guaymas . 417)(Heroica-Ciudad-de-Ures . 331)(Agua-Prieta . 186)))
  (set-distancia 'Agua-Prieta '((Navojoa . 698)(Ciudad-Obregon . 630)(Heroica-Caborca . 313)(Hermosillo . 377)(Guaymas . 513)(Heroica-Ciudad-de-Ures . 300)(Heroica-Nogales . 186)(Nuevo-Casas-Grandes . 221)(Ciudad-Juarez . 360)))
  ;Ciudades de Tabasco
  (set-distancia 'Villahermosa '((Jalapa . 43)(Macuspana . 50)(Sanchez-Magallanes . 141)(Huimanguillo . 72)(Frontera . 84)(Cunduacan . 36)(Comalcalco . 61)(Heroica-Cardenas . 53)))
  (set-distancia 'Jalapa '((Villahermosa . 43)(Macuspana . 39)(Sanchez-Magallanes . 179)(Huimanguillo . 11)(Frontera . 125)(Cunduacan . 75)(Comalcalco . 100)(Heroica-Cardenas . 91)(Simojovel . 111)))
  (set-distancia 'Macuspana '((Villahermosa . 50)(Jalapa . 39)(Sanchez-Magallanes . 191)(Huimanguillo . 122)(Frontera . 123)(Cunduacan . 86)(Comalcalco . 111)(Heroica-Cardenas . 102)(Ciudad-del-Carmen . 156)))
  (set-distancia 'Sanchez-Magallanes '((Villahermosa . 141)(Jalapa . 179)(Macuspana . 191)(Benito-Juarez . 195)(Huimanguillo . 112)(Frontera . 151)(Cunduacan . 119)(Comalcalco . 95)(Heroica-Cardenas . 93)(Coatzacoalcos . 92)))
  (set-distancia 'Huimanguillo '((Villahermosa . 72)(Jalapa . 11)(Macuspana . 122)(Sanchez-Magallanes . 112)(Frontera . 150)(Cunduacan . 48)(Comalcalco . 60)(Heroica-Cardenas . 20)(Minatitlan . 154)(Chapultenango . 98)(Simojovel . 140)))
  (set-distancia 'Frontera '((Villahermosa . 84)(Jalapa . 125)(Macuspana . 123)(Sanchez-Magallanes . 151)(Huimanguillo . 150)(Cunduacan . 114)(Comalcalco . 94)(Heroica-Cardenas . 131)(Ciudad-del-Carmen . 97)))
  (set-distancia 'Cunduacan '((Villahermosa . 36)(Jalapa . 75)(Macuspana . 86)(Sanchez-Magallanes . 119)(Huimanguillo . 48)(Frontera . 114)(Comalcalco . 27)(Heroica-Cardenas . 29)))
  (set-distancia 'Comalcalco '((Villahermosa . 61)(Jalapa . 100)(Macuspana . 111)(Sanchez-Magallanes . 95)(Huimanguillo . 60)(Frontera . 94)(Cunduacan . 27)(Heroica-Cardenas . 41)))
  (set-distancia 'Heroica-Cardenas '((Villahermosa . 53)(Jalapa . 91)(Macuspana . 102)(Sanchez-Magallanes . 93)(Huimanguillo . 20)(Frontera . 131)(Cunduacan . 29)(Comalcalco . 41)(Minatitlan . 134)))
  ;Ciudades de Tamaulipas
  (set-distancia 'Aldama '((Tula . 237)(Reynosa . 397)(Ciudad-Victoria . 189)(Matamoros . 390)(Tampico . 118)(General-Francisco-Villa . 271)(Ciudad-Valles . 205)))
  (set-distancia 'Reynosa '((Aldama . 397)(Ciudad-Victoria . 329)(Matamoros . 90)(Nuevo-Laredo . 248)(Tampico . 500)(General-Francisco-Villa . 125)(Linares . 250)(China . 108)))
  (set-distancia 'Ciudad-Victoria '((Tula . 144)(Aldama . 189)(Reynosa . 329)(Matamoros . 320)(Nuevo-Laredo . 502)(Tampico . 242)(General-Francisco-Villa . 202)(Aramberri . 232)(Linares . 155)(Villa-de-La-Paz . 260)))
  (set-distancia 'Matamoros '((Aldama . 390)(Reynosa . 90)(Ciudad-Victoria . 320)(Nuevo-Laredo . 333)(Tampico . 494)(General-Francisco-Villa . 118)))
  (set-distancia 'Nuevo-Laredo '((Reynosa . 248)(Ciudad-Victoria . 502)(Matamoros . 333)(China . 270)(Agualeguas . 174)(Villaldama . 161)))
  (set-distancia 'Tampico '((Tula . 275)(Aldama . 118)(Reynosa . 500)(Ciudad-Victoria . 242)(Matamoros . 494)(General-Francisco-Villa . 375)(Poza-Rica . 222)))
  (set-distancia 'General-Francisco-Villa '((Aldama . 271)(Reynosa . 125)(Ciudad-Victoria . 202)(Matamoros . 118)(Tampico . 375)))
  (set-distancia 'Tula '((Ciudad-Victoria . 144)(Aldama . 237)(Tampico . 275)(Aramberri . 210)(San-Luis-Potosi . 191)(Rancho-del-Puente . 219)(Cardenas . 199)))
  ;Ciudades de Tlaxcala
  (set-distancia 'Tlaxcala '((Calpulalpan . 63)(Tlaxco . 41)(Villa-de-El-Carmen-Tequexquitla . 73)(Tetla . 23)(Huamantla . 43)(Teolocholco . 12)(Puebla . 37)))
  (set-distancia 'Calpulalpan '((Tlaxcala . 63)(Tlaxco . 59)(Villa-de-El-Carmen-Tequexquitla . 116)(Tetla . 65)(Huamantla . 86)(Teolocholco . 72)(Ciudad-Sahagun . 29)(Apan . 23)(Ixtlahuaca . 50)(Texcoco . 39)))
  (set-distancia 'Tlaxco '((Tlaxcala . 41)(Calpulalpan . 59)(Villa-de-El-Carmen-Tequexquitla . 74)(Tetla . 21)(Huamantla . 46)(Teolocholco . 47)(Zacatlan . 55)))
  (set-distancia 'Villa-de-El-Carmen-Tequexquitla '((Tlaxcala . 73)(Calpulalpan . 116)(Tlaxco . 74)(Tetla . 57)(Huamantla . 30)(Teolocholco . 80)(Oriental . 7)))
  (set-distancia 'Tetla '((Tlaxcala . 23)(Calpulalpan . 65)(Tlaxco . 21)(Villa-de-El-Carmen-Tequexquitla . 57)(Huamantla . 28)(Teolocholco . 30)))
  (set-distancia 'Huamantla '((Tlaxcala . 43)(Calpulalpan . 86)(Tlaxco . 46)(Villa-de-El-Carmen-Tequexquitla . 30)(Tetla . 28)(Teolocholco . 49)(Puebla . 55)(Cuacnopalan . 99)(Oriental . 39)))
  (set-distancia 'Teolocholco '((Tlaxcala . 12)(Calpulalpan . 72)(Tlaxco . 47)(Villa-de-El-Carmen-Tequexquitla . 80)(Tetla . 30)(Huamantla . 49)(Puebla . 27)))
  ;Ciudades de Veracruz
  (set-distancia 'Coatzacoalcos '((Veracruz . 313)(Poza-Rica . 566)(Orizaba . 328)(Minatitlan . 25)(Xalapa . 412)(Cordoba . 306)(Santiago-Tuxtla . 183)(Tierra-Blanca . 244)(Chapultenango . 250)(Sanchez-Magallanes . 92)))
  (set-distancia 'Veracruz '((Coatzacoalcos . 313)(Poza-Rica . 255)(Orizaba . 136)(Minatitlan . 294)(Xalapa . 101)(Cordoba . 114)(Santiago-Tuxtla . 146)(Tierra-Blanca . 101)))
  (set-distancia 'Poza-Rica '((Coatzacoalcos . 566)(Veracruz . 255)(Orizaba . 324)(Minatitlan . 538)(Xalapa . 211)(Cordoba . 358)(Santiago-Tuxtla . 389)(Tierra-Blanca . 345)(Tampico . 222)(Tulancingo . 149)(Zacatlan . 162)))
  (set-distancia 'Orizaba '((Coatzacoalcos . 328)(Veracruz . 136)(Poza-Rica . 324)(Minatitlan . 308)(Xalapa . 140)(Cordoba . 24)(Santiago-Tuxtla . 244)(Tierra-Blanca . 115)(Oriental . 130)))
  (set-distancia 'Minatitlan '((Coatzacoalcos . 25)(Veracruz . 294)(Poza-Rica . 538)(Orizaba . 308)(Xalapa . 392)(Cordoba . 286)(Santiago-Tuxtla . 162)(Tierra-Blanca . 224)(Loma-Bonita . 150)(Chapultenango . 300)(Heroica-Cardenas . 134)(Huimanguillo . 154)))
  (set-distancia 'Xalapa '((Coatzacoalcos . 412)(Veracruz . 101)(Poza-Rica . 211)(Orizaba . 140)(Minatitlan . 392)(Cordoba . 130)(Santiago-Tuxtla . 236)(Tierra-Blanca . 192)(Oriental . 99)))
  (set-distancia 'Cordoba '((Coatzacoalcos . 306)(Veracruz . 114)(Poza-Rica . 358)(Orizaba . 24)(Minatitlan . 286)(Xalapa . 130)(Santiago-Tuxtla . 222)(Tierra-Blanca . 94)))
  (set-distancia 'Santiago-Tuxtla '((Coatzacoalcos . 183)(Veracruz . 146)(Poza-Rica . 389)(Orizaba . 244)(Minatitlan . 162)(Xalapa . 236)(Cordoba . 222)(Tierra-Blanca . 159)(Loma-Bonita . 93)))
  (set-distancia 'Tierra-Blanca '((Coatzacoalcos . 244)(Veracruz . 101)(Poza-Rica . 345)(Orizaba . 115)(Minatitlan . 224)(Xalapa . 192)(Cordoba . 94)(Santiago-Tuxtla . 159)(Loma-Bonita . 72)))
  ;Ciudades de Yucatan
  (set-distancia 'Merida '((Izamal . 71)(Tizimin . 203)(Valladolid . 162)(Motul . 43)(Chacsinkin . 126)(Cenotillo . 118)(Calkini . 90)))
  (set-distancia 'Izamal '((Merida . 71)(Tizimin . 112)(Valladolid . 110)(Motul . 38)(Chacsinkin . 111)(Cenotillo . 47)))
  (set-distancia 'Tizimin '((Merida . 203)(Izamal . 112)(Valladolid . 52)(Motul . 123)(Chacsinkin . 184)(Cenotillo . 61)(Kantunilkin . 75)))
  (set-distancia 'Valladolid '((Merida . 162)(Izamal . 110)(Tizimin . 52)(Motul . 143)(Chacsinkin . 132)(Cenotillo . 62)(Kantunilkin . 117)(Jose-Maria-Morelos . 150)))
  (set-distancia 'Motul '((Merida . 143)(Izamal . 38)(Tizimin . 123)(Valladolid . 143)(Chacsinkin . 149)(Cenotillo . 84)))
  (set-distancia 'Chacsinkin '((Merida . 126)(Izamal . 111)(Tizimin . 184)(Valladolid . 132)(Motul . 149)(Cenotillo . 148)(Hopelchen . 147)))
  (set-distancia 'Cenotillo '((Merida . 118)(Izamal . 47)(Tizimin . 61)(Valladolid . 62)(Motul . 84)(Chacsinkin . 148)))
  ;Ciudades de Zacatecas
  (set-distancia 'Fresnillo '((Zacatecas . 61)(Jerez . 65)(El-Salvador . 309)(Nochistlan-de-Mejia . 280)(Villa-Gonzalez-Ortega . 139)(Chalchihuites . 157)(Rio-Grande . 78)(Huejuquilla-el-Alto . 139)(Villa-de-La-Paz . 320)(Venado . 242)))
  (set-distancia 'Zacatecas '((Fresnillo . 61)(Jerez . 55)(El-Salvador . 313)(Nochistlan-de-Mejia . 224)(Villa-Gonzalez-Ortega . 82)(Rio-Grande . 139)(Rincon-de-Romos . 78)))
  (set-distancia 'Jerez '((Fresnillo . 65)(Zacatecas . 55)(Nochistlan-de-Mejia . 222)(Villa-Gonzalez-Ortega . 136)(Chalchihuites . 222)(Huejuquilla-el-Alto . 137)(Villa-Guerrero . 132)))
  (set-distancia 'El-Salvador '((Fresnillo . 309)(Zacatecas . 313)(Villa-Gonzalez-Ortega . 390)(Rio-Grande . 367)(San-Juan-de-Guadalupe . 268)(Gomez-Farias . 77)(Parras-de-la-Fuente . 213)(Saltillo . 140)(Villa-de-La-Paz . 122)))
  (set-distancia 'Nochistlan-de-Mejia '((Fresnillo . 280)(Zacatecas . 224)(Jerez . 222)(Villa-Gonzalez-Ortega . 206)(Calvillo . 91)(Cuquio . 104)(San-Miguel-el-Alto . 96)))
  (set-distancia 'Villa-Gonzalez-Ortega '((Fresnillo . 139)(Zacatecas . 82)(Jerez . 136)(El-Salvador . 390)(Nochistlan-de-Mejia . 206)(Rio-Grande . 214)(Cosio . 73)(Rincon-de-Romos . 89)(Venado . 159)(San-Luis-Potosi . 145)))
  (set-distancia 'Chalchihuites '((Fresnillo . 157)(Jerez . 222)(Rio-Grande . 151)(Victoria-de-Durango . 121)))
  (set-distancia 'Rio-Grande '((Fresnillo . 78)(Zacatecas . 139)(El-Salvador . 367)(Villa-Gonzalez-Ortega . 214)(Chalchihuites . 151)(Victoria-de-Durango . 210)(San-Juan-de-Guadalupe . 129)(Parras-de-la-Fuente . 318)))

; *** Tablas de hash para recordar los valores           ***
; *** F y G que se vayan generando, según el             ***
; *** algoritmo de Geeks for Geeks:                      ***
; *** https://www.geeksforgeeks.org/a-search-algorithm/  ***
(let ((tabla-f (make-hash-table :size 20)))
  (defun set-f (x y) (setf (gethash x tabla-f) y))
  (defun get-f (x) (gethash x tabla-f))
)

(let ((tabla-g (make-hash-table :size 20)))
  (defun set-g (x y) (setf (gethash x tabla-g) y))
  (defun get-g (x) (gethash x tabla-g))
)

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
; **** TABLA DE COORDENADAS ****
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
(let ((cooredenadas (make-hash-table :size 20)))
  (defun set-coordenadas (x y) (setf (gethash x cooredenadas) y))
  (defun get-coordenadas (x) (gethash x cooredenadas)))

;Ciudades de Aguascalientes
(set-coordenadas 'Aguascalientes '(21.88 102.29))
(set-coordenadas 'Calvillo '(21.84 102.71))
(set-coordenadas 'Jesus-Maria '(21.96 102.34))
(set-coordenadas 'San-Francisco-de-Los-Romo '(22.07 102.26))
(set-coordenadas 'Asientos '(22.23 102.08))
(set-coordenadas 'El-Llano '(21.91 101.96))
(set-coordenadas 'San-Jose-de-Gracia '(22.14 102.41))
(set-coordenadas 'Pabellon-de-Arteaga '(22.14 102.27))
(set-coordenadas 'Rincon-de-Romos '(22.22 102.32))
(set-coordenadas 'Tepezala '(22.22 102.17))
(set-coordenadas 'Cosio '(22.36 102.29))
;Ciudades de Baja California
(set-coordenadas 'Tijuana '(32.51 117.05))
(set-coordenadas 'Playas-de-Rosarito '(32.36 117.06))
(set-coordenadas 'Ensenada '(31.86 116.59))
(set-coordenadas 'Tecate '(32.56 116.62))
(set-coordenadas 'Ciudad-Progreso '(32.58 115.58))
(set-coordenadas 'Mexicali '(32.60 115.46))
(set-coordenadas 'Ciudad-Guadalupe-Victoria '(32.29 115.10))
(set-coordenadas 'Punta-Prieta '(28.92 114.15))
(set-coordenadas 'Bahia-de-los-Angeles '(28.95 113.56))
(set-coordenadas 'Santo-Domingo '(28.29 114.02))
(set-coordenadas 'El-Arco '(28.02 113.39))
;Ciudades de Baja California Sur
(set-coordenadas 'Bahia-Tortugas '(27.69 114.89))
(set-coordenadas 'Benito-Juarez '(27.88 113.78))
(set-coordenadas 'San-Ignacio '(27.28 112.87))
(set-coordenadas 'Mulege '(26.88 111.98))
(set-coordenadas 'Comondu '(26.06 111.84))
(set-coordenadas 'Loreto '(26.01 111.35))
(set-coordenadas 'Ciudad-Insurgentes '(25.26 111.77))
(set-coordenadas 'La-Paz '(24.14 110.31))
(set-coordenadas 'Todos-Santos '(23.44 110.22))
(set-coordenadas 'Cabo-San-Lucas '(22.89 109.91))
(set-coordenadas 'Santiago '(23.47 109.71))
(set-coordenadas 'La-Ribera '(23.59 109.58))
;Ciudades de Campeche
(set-coordenadas 'Calkini '(20.37 90.05))
(set-coordenadas 'Hecelchakan '(20.18 90.13))
(set-coordenadas 'Campeche '(19.83 90.53))
(set-coordenadas 'Hopelchen '(19.74 89.84))
(set-coordenadas 'Champoton '(19.34 90.72))
(set-coordenadas 'Ciudad-del-Carmen '(18.65 91.80))
(set-coordenadas 'Francisco-Escarcega '(18.61 90.73))
(set-coordenadas 'Xpujil '(18.51 89.39))
;Ciudades de Chiapas
(set-coordenadas 'Chapultenango '(17.33 93.12))
(set-coordenadas 'Simojovel '(17.13 92.71))
(set-coordenadas 'San-Juan-Chamula '(16.78 92.68))
(set-coordenadas 'Tuxtla-Gutierrez '(16.75 93.10))
(set-coordenadas 'Acala '(16.55 92.80))
(set-coordenadas 'El-Parral '(16.36 93.00))
(set-coordenadas 'Comitan-de-Dominguez '(16.23 92.11))
(set-coordenadas 'Villaflores '(16.23 93.27))
(set-coordenadas 'Siltepec '(15.55 92.32))
(set-coordenadas 'Tapachula '(14.90 92.26))
; Ciudades de Chihuahua
(set-coordenadas 'Jimenez '(27.13 104.92))
(set-coordenadas 'Hidalgo-del-Parral '(26.93 105.66))
(set-coordenadas 'Cuauhtemoc '(28.40 106.86))
(set-coordenadas 'Chihuahua '(28.63 106.06))
(set-coordenadas 'Ciudad-Juarez '(31.69 106.42))
(set-coordenadas 'Nuevo-Casas-Grandes '(30.41 107.91))
(set-coordenadas 'Camargo '(27.67 105.16))
(set-coordenadas 'Delicias '(28.18 105.45))
(set-coordenadas 'Yepachic '(28.42 108.37))
(set-coordenadas 'Pascual-Orozco '(28.57 107.44))
; Ciudades de Ciudad de Mexico
(set-coordenadas 'CDMX '(19.43 99.13))
(set-coordenadas 'Iztapalapa '(19.34 99.05))
(set-coordenadas 'Gustavo-A-Madero '(19.48 99.12))
(set-coordenadas 'Magdalena-Contreras '(19.31 99.24))
(set-coordenadas 'Milpa-Alta '(19.19 99.02))
(set-coordenadas 'Azcapotzalco '(19.48 99.18))
(set-coordenadas 'Venustiano-Carranza '(19.43 99.09))
(set-coordenadas 'Miguel-Hidalgo '(19.43 99.20))
(set-coordenadas 'Coyoacan '(19.34 99.15))
(set-coordenadas 'Alvaro-Obregon '(19.36 99.22))
; Ciudades de Coahuila
(set-coordenadas 'Saltillo '(25.43 100.97))
(set-coordenadas 'Monclova '(26.90 101.42))
(set-coordenadas 'Torreon '(25.54 103.40))
(set-coordenadas 'Ciudad-Acuña '(29.32 100.95))
(set-coordenadas 'Piedras-Negras '(28.69 100.54))
(set-coordenadas 'Gomez-Farias '(24.96 101.03))
(set-coordenadas 'Parras-de-la-Fuente '(25.44 102.17))
(set-coordenadas 'Nueva-Rosita '(27.93 101.21))
(set-coordenadas 'Ramos-Arizpe '(25.54 100.94))
; Ciudades de Colima
(set-coordenadas 'Colima '(19.24 103.72))
(set-coordenadas 'Manzanillo '(19.11 104.33))
(set-coordenadas 'Cuyutlan '(18.91 104.06))
(set-coordenadas 'Queseria '(19.38 103.57))
(set-coordenadas 'Cuauhtemoc '(19.33 103.59))
(set-coordenadas 'Camotlan-de-Miraflores '(19.21 104.23))
(set-coordenadas 'Coquimatlan '(19.20 103.80))
(set-coordenadas 'Ciudad-de-Armeria '(18.93 103.96))
(set-coordenadas 'Tecoman '(18.91 103.87))
; Ciudades de Durango
(set-coordenadas 'Ciudad-Lerdo '(25.53 103.53))
(set-coordenadas 'Victoria-de-Durango '(24.02 104.65))
(set-coordenadas 'Tamazula '(24.96 106.96))
(set-coordenadas 'Francisco-I-Madero '(24.40 104.31))
(set-coordenadas 'Topia '(25.21 106.57))
(set-coordenadas 'Inde '(25.91 105.22))
(set-coordenadas 'San-Juan-de-Guadalupe '(24.63 102.77))
(set-coordenadas 'La-Ciudad '(24.02 104.95))
; Ciudades de Guanajuato
(set-coordenadas 'Leon '(21.12 101.68))
(set-coordenadas 'Irapuato '(20.87 101.35))
(set-coordenadas 'Salamanca '(20.57 101.19))
(set-coordenadas 'Celaya '(20.52 100.81))
(set-coordenadas 'Guanajuato '(21.01 101.25))
(set-coordenadas 'Moroleon '(20.12 101.19))
(set-coordenadas 'Paso-de-Vaqueros '(21.35 100.38))
(set-coordenadas 'La-Angostura '(20.27 101.10))
(set-coordenadas 'Ciudad-Manuel-Doblado '(20.72 101.95))
(set-coordenadas 'Salvatierra '(20.20 100.88))
; Ciudades de Guerrero
(set-coordenadas 'Acapulco '(16.85 99.82))
(set-coordenadas 'Chilpancingo '(17.55 99.50))
(set-coordenadas 'Tepetixtla '(17.21 100.11))
(set-coordenadas 'Cuajinicuilapa '(16.47 98.41))
(set-coordenadas 'Zumpango-del-Rio '(17.65 99.52))
(set-coordenadas 'Iguala '(18.34 99.53))
(set-coordenadas 'Arcelia '(18.31 100.28))
(set-coordenadas 'Tuliman '(18.02 99.25))
(set-coordenadas 'Pueblo-Viejo '(17.53 100.11))
(set-coordenadas 'Olinala '(17.77 98.73))
; Ciudades de Hidalgo
(set-coordenadas 'Pachuca-de-Soto '(20.10 98.75))
(set-coordenadas 'Progreso '(20.34 99.18))
(set-coordenadas 'Tizayuca '(19.84 98.98))
(set-coordenadas 'Ixmiquilpan '(20.48 99.21))
(set-coordenadas 'Ciudad-Sahagun '(19.77 98.57))
(set-coordenadas 'Apan '(19.71 98.45))
(set-coordenadas 'Tepeji-de-Ocampo '(19.91 99.35))
(set-coordenadas 'Tlaxcoapan '(20.09 99.22))
(set-coordenadas 'Tulancingo '(20.09 98.36))
; Ciudades de Jalisco
(set-coordenadas 'Puerto-Vallarta '(20.65 105.22))
(set-coordenadas 'Guadalajara '(20.65 103.34))
(set-coordenadas 'Ameca '(20.54 104.04))
(set-coordenadas 'Purificacion '(19.71 104.60))
(set-coordenadas 'Cuquio '(20.92 103.02))
(set-coordenadas 'San-Miguel-el-Alto '(21.02 102.40))
(set-coordenadas 'El-Chante '(20.28 103.40))
(set-coordenadas 'Villa-Guerrero '(21.97 130.59))
(set-coordenadas 'Huejuquilla-el-Alto '(22.62 103.90))
(set-coordenadas 'Ciudad-Guzaman '(21.71 103.45))
; Ciudades de Estado de México
(set-coordenadas 'Toluca-de-Lerdo '(19.28 99.65))
(set-coordenadas 'Santiago-Tianguistenco '(19.18 99.46))
(set-coordenadas 'Ixtlahuaca '(19.57 99.76))
(set-coordenadas 'Jiquipilco '(19.55 99.60))
(set-coordenadas 'Tecamac '(19.71 98.96))
(set-coordenadas 'Ecatepec '(19.60 99.03))
(set-coordenadas 'Texcoco '(19.50 98.88))
(set-coordenadas 'San-Francisco-Coacalco '(19.63 99.09))
(set-coordenadas 'Santa-Martha '(19.49 99.72))
;Ciudades de Michoacán
(set-coordenadas 'Morelia '(19.70 101.19))
(set-coordenadas 'Lazaro-Cardenas '(17.95 102.19))
(set-coordenadas 'Apatzingan '(19.08 102.35))
(set-coordenadas 'Zitacuaro '(19.43 100.35))
(set-coordenadas 'Zamora '(19.99 102.28))
(set-coordenadas 'Uruapan '(19.40 102.04))
(set-coordenadas 'Tacintaro '(19.33 102.36))
(set-coordenadas 'Churumuco-de-Morelos '(18.66 101.64))
(set-coordenadas 'Ixtlan-de-los-Hervores '(20.16 102.39))
(set-coordenadas 'San-Jose-de-Gracia '(19.98 103.02))
;Ciudades de Morelos
(set-coordenadas 'Cuernavaca '(18.92 99.22))
(set-coordenadas 'Puente-de-Ixtla '(18.61 99.32))
(set-coordenadas 'Yautepec '(18.88 99.06))
(set-coordenadas 'Temixco '(18.85 99.23))
(set-coordenadas 'Miacatlan '(18.77 99.35))
(set-coordenadas 'Totolapan '(18.98 98.92))
(set-coordenadas 'Tlayacapan '(18.96 98.98))
(set-coordenadas 'Yecapixtla '(18.88 98.86))
;Ciudades de Nayarit
(set-coordenadas 'Acaponeta '(22.50 105.36))
(set-coordenadas 'Tepic '(21.50 104.89))
(set-coordenadas 'Compostela '(21.23 104.90))
(set-coordenadas 'El-Rosario '(20.89 104.46))
(set-coordenadas 'Campo-de-los-Limones '(21.80 105.41))
(set-coordenadas 'Amatlan-de-Cañas '(20.80 104.40))
(set-coordenadas 'San-Blas '(21.54 105.28))
(set-coordenadas 'Villa-Hidalgo '(21.74 105.23))
(set-coordenadas 'Puente-de-Camotlan '(21.70 104.08))
;Ciudades de Nuevo Leon
(set-coordenadas 'Monterrey '(25.68 100.31))
(set-coordenadas 'La-Cruz-de-Elorza '(23.83 100.46))
(set-coordenadas 'Aramberri '(24.09 99.81))
(set-coordenadas 'Agualeguas '(26.31 99.53))
(set-coordenadas 'China '(25.70 99.23))
(set-coordenadas 'Villaldama '(26.49 100.43))
(set-coordenadas 'Apodaca '(25.77 100.18))
(set-coordenadas 'Linares '(24.84 99.56))
;Ciudades de Oaxaca
(set-coordenadas 'Oaxaca '(17.07 96.72))
(set-coordenadas 'Santo-Domingo-Ingenio '(16.58 94.76))
(set-coordenadas 'San-Pedro-Tapantepec '(16.36 94.19))
(set-coordenadas 'Ciudad-Ixtepec '(16.56 95.10))
(set-coordenadas 'Santo-Domingo-Zanatepec '(16.47 94.35))
(set-coordenadas 'Loma-Bonita '(18.10 95.88))
(set-coordenadas 'Pinotepa '(16.34 98.05))
(set-coordenadas 'Salina-Cruz '(16.18 95.20))
;Ciudades de Puebla
(set-coordenadas 'Zacatlan '(19.93 97.96))
(set-coordenadas 'Puebla '(19.04 98.20))
(set-coordenadas 'Izucar-de-Matamoros '(18.59 98.46))
(set-coordenadas 'Tehuacan '(18.46 97.40))
(set-coordenadas 'Cuacnopalan '(18.81 97.51))
(set-coordenadas 'Tulcingo-de-Valle '(18.04 98.44))
(set-coordenadas 'San-Sebastian-Zinacatepec '(18.33 97.24))
(set-coordenadas 'San-Jose-Ixtapa '(18.71 97.45))
(set-coordenadas 'Oriental '(19.36 97.61))
(set-coordenadas 'Atlixco '(18.91 98.42))
;Ciudades de Queretaro
(set-coordenadas 'Queretaro '(20.58 100.38))
(set-coordenadas 'San-Juan-del-Rio '(20.39 99.98))
(set-coordenadas 'Tequisquiapan '(20.51 99.88))
(set-coordenadas 'Jalpan-de-Serra '(21.21 99.47))
(set-coordenadas 'Ezequiel-Montes '(20.66 99.90))
(set-coordenadas 'Santa-Rosa-Jauregui '(20.74 100.44))
(set-coordenadas 'Parque-Industrial-Queretaro '(20.56 100.25))
;Ciudades de Quintana Roo
(set-coordenadas 'Cancun '(21.16 86.85))
(set-coordenadas 'Playa-del-Carmen '(20.62 87.07))
(set-coordenadas 'Chetumal '(18.50 88.29))
(set-coordenadas 'Kantunilkin '(21.10 87.48))
(set-coordenadas 'Jose-Maria-Morelos '(19.74 88.70))
(set-coordenadas 'Puerto-Morelos '(20.84 86.87))
(set-coordenadas 'Tulum '(20.21 87.46))
;Ciudades de San Luis Potosi
(set-coordenadas 'Ciudad-Valles '(22.00 99.00))
(set-coordenadas 'San-Luis-Potosi '(22.15 100.98))
(set-coordenadas 'Rancho-del-Puente '(21.97 100.37))
(set-coordenadas 'Tancahuitz-de-Santos '(21.60 98.96))
(set-coordenadas 'Cardenas '(21.99 99.64))
(set-coordenadas 'Matlapa '(21.33 98.92))
(set-coordenadas 'Venado '(22.93 101.09))
(set-coordenadas 'Villa-de-La-Paz '(23.67 100.70))
;Ciudades de Sinaloa
(set-coordenadas 'Dimas '(23.72 106.78))
(set-coordenadas 'La-Reforma '(25.08 108.05))
(set-coordenadas 'Culiacan '(25.17 107.47))
(set-coordenadas 'Los-Mochis '(25.79 108.98))
(set-coordenadas 'Costa-Rica '(24.08 107.05))
(set-coordenadas 'Gabriel-Leyva-Solano '(25.66 108.63))
(set-coordenadas 'Mazatlan '(23.22 106.42))
(set-coordenadas 'Cosala '(24.41 106.69))
(set-coordenadas 'Gustavo-Diaz-Ordaz '(26.26 109.03))
;Ciudades de Sonora
(set-coordenadas 'Golfo-de-Santa-Clara '(31.68 114.49))
(set-coordenadas 'Navojoa '(27.07 109.44))
(set-coordenadas 'Puerto-Peñasco '(31.32 113.53))
(set-coordenadas 'Ciudad-Obregon '(27.48 109.93))
(set-coordenadas 'Heroica-Caborca '(30.69 112.18))
(set-coordenadas 'Hermosillo '(29.07 110.95))
(set-coordenadas 'Guaymas '(27.91 110.90))
(set-coordenadas 'Heroica-Ciudad-de-Ures '(29.42 110.38))
(set-coordenadas 'Heroica-Nogales '(31.30 110.93))
(set-coordenadas 'Agua-Prieta '(31.32 109.54))
;Ciudades de Tabasco
(set-coordenadas 'Villahermosa '(17.84 92.61))
(set-coordenadas 'Jalapa '(17.71 92.81))
(set-coordenadas 'Macuspana '(17.76 92.59))
(set-coordenadas 'Sanchez-Magallanes '(18.29 93.86))
(set-coordenadas 'Huimanguillo '(17.83 93.38))
(set-coordenadas 'Frontera '(18.53 92.64))
(set-coordenadas 'Cunduacan '(18.07 93.17))
(set-coordenadas 'Comalcalco '(18.26 93.22))
(set-coordenadas 'Heroica-Cardenas '(18.00 93.37))
;Ciudades de Tamaulipas
(set-coordenadas 'Aldama '(22.92 98.07))
(set-coordenadas 'Reynosa '(24.26 98.83))
(set-coordenadas 'Ciudad-Victoria '(23.73 99.14))
(set-coordenadas 'Matamoros '(25.86 97.50))
(set-coordenadas 'Nuevo-Laredo '(27.47 99.54))
(set-coordenadas 'Tampico '(22.23 97.86))
(set-coordenadas 'General-Francisco-Villa '(25.01 98.07))
(set-coordenadas 'Tula '(23.00 99.72))
;Ciudades de Tlaxcala
(set-coordenadas 'Tlaxcala '(19.31 98.23))
(set-coordenadas 'Calpulalpan '(19.58 98.56))
(set-coordenadas 'Tlaxco '(19.61 98.11))
(set-coordenadas 'Villa-de-El-Carmen-Tequexquitla '(19.31 97.65))
(set-coordenadas 'Tetla '(19.43 98.10))
(set-coordenadas 'Huamantla '(19.31 97.92))
(set-coordenadas 'Teolocholco '(19.23 98.18))
;Ciudades de Veracruz
(set-coordenadas 'Coatzacoalcos '(18.13 94.45))
(set-coordenadas 'Veracruz '(19.17 96.13))
(set-coordenadas 'Poza-Rica '(20.52 97.46))
(set-coordenadas 'Orizaba '(18.85 97.10))
(set-coordenadas 'Minatitlan '(17.99 94.54))
(set-coordenadas 'Xalapa '(19.54 96.91))
(set-coordenadas 'Cordoba '(18.88 96.92))
(set-coordenadas 'Santiago-Tuxtla '(18.46 95.30))
(set-coordenadas 'Tierra-Blanca '(18.44 96.35))
;Ciudades de Yucatan
(set-coordenadas 'Merida '(20.96 89.59))
(set-coordenadas 'Izamal '(20.93 89.02))
(set-coordenadas 'Tizimin '(21.14 88.14))
(set-coordenadas 'Valladolid '(20.68 88.20))
(set-coordenadas 'Motul '(21.09 89.28))
(set-coordenadas 'Chacsinkin '(20.17 89.01))
(set-coordenadas 'Cenotillo '(20.96 88.60))
;Ciudades de Zacatecas
(set-coordenadas 'Fresnillo '(23.17 102.86))
(set-coordenadas 'Zacatecas '(22.77 102.58))
(set-coordenadas 'Jerez '(22.64 102.98))
(set-coordenadas 'El-Salvador '(24.52 100.86))
(set-coordenadas 'Nochistlan-de-Mejia '(21.36 102.84))
(set-coordenadas 'Villa-Gonzalez-Ortega '(22.51 101.91))
(set-coordenadas 'Chalchihuites '(23.47 103.88))
(set-coordenadas 'Rio-Grande '(23.82 103.03))


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ************************************************ FUNCIONES EXTRA ************************************************
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; ;;;;;;;;;;;;;;
; **** META ****
; ;;;;;;;;;;;;;;
(let (meta)
  (defun set-meta (x) (setf meta x))
  (defun get-meta () meta) )

; ;;;;;;;;;;;;;;;;;;;;;;
; **** BUSCAR-MEJOR **** Rodrigo
; ;;;;;;;;;;;;;;;;;;;;;;
(defun buscar-mejor (fst lst)
  (cond
    ((null lst) fst)
	  ((< (get-f fst) (get-f (first lst))) fst)
    ((eql (first lst) (get-meta)) (first lst))
    (t (buscar-mejor fst (rest lst)))))

; ;;;;;;;;;;;;;;;;;;;;
; **** CALCULAR-H ****
; ;;;;;;;;;;;;;;;;;;;;
(defun h (n)
  (sqrt (+ (expt (* (- (car (get-coordenadas n))
                       (car (get-coordenadas (get-meta)))) 110) 2)
              (expt (* (- (car (cdr (get-coordenadas n)))
                       (car (cdr (get-coordenadas (get-meta))))) 85) 2))))

; ;;;;;;;;;;;;;;;;;;;;
; **** CALCULAR-F ****
; ;;;;;;;;;;;;;;;;;;;;
(defun calcular-f (n)
  (+ (get-g n) (h n)))

; ;;;;;;;;;;;;;;;;;;;;
; **** CALCULAR-G ****
; ;;;;;;;;;;;;;;;;;;;;
(defun calcular-g (x y)
  (+ (get-g y)
     (or (rest (assoc y (get-distancia x))) 99999999)))

; ;;;;;;;;;;;;;;;;
; **** COLOCA ****
; ;;;;;;;;;;;;;;;;
(defun coloca (nodo lst)
   (cond
     ((null lst)(list nodo))
     ((< (get-f nodo)
         (get-f (first lst)) )
      (cons nodo lst))
     (t (cons (first lst)
              (coloca nodo (rest lst))))))

; ;;;;;;;;;;;;;;;;;;;;;;
; **** CAMINO-FINAL **** Rodrigo
; ;;;;;;;;;;;;;;;;;;;;;;
; // Recibe el nodo actual (supuestamente el nodo final) y crea una lista n
; // añadiendo los nodos previos.
; Regresa una lista con el nodo inicial hasta la meta
(defun camino-final (n)
(cond
   ((null n) nil)
	(t (append (camino-final (get-ancestro n))
	           (list n)))))

; ;;;;;;;;;;;;;;;;;;;;;;
; **** NUMERO-NODOS ****
; ;;;;;;;;;;;;;;;;;;;;;;
(let (NUMERO-NODOS)
   (defun iniciar-cuenta-nodos () (setf NUMERO-NODOS 0))
   (defun incrementa-cuenta-nodos () (incf NUMERO-NODOS))
   (defun get-numero-nodos () NUMERO-NODOS))


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ********************************************* FUNCION A-ESTRELLA *********************************************
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun a-star (nodo-inicial nodo-final)                   ; algoritmo A*
  (set-meta nodo-final)                                   ; ¡OBJETO PARA GUARDAR EL NODO FINAL!
                                                          ; (PARA PODER ACCEDER A EL EN OTRAS FUNCIONES)

  (let ((abierto (list nodo-inicial))                       ; Inicializamos lista abierto y cerrado, el nodo x y sucesor
    (cerrado nil)
    nodo-actual
    sucesores)

    (set-ancestro nodo-inicial nil)                       ; Guardamos el nodo inicial en la tabla de caminos recorridos

    (set-g nodo-inicial 0)                                ; Inicializamos los valores g y f del primer nodo
    (set-f nodo-inicial (calcular-f nodo-inicial))        ; ¡FUNCIÓN PARA CALCULAR F y G!

    (loop                                                 ; Comienza propiamente el algoritmo
      (if (null abierto)(return 'imposible))              ; Si la lista abierta está vacía termina en fracaso

      (if (eql (first abierto) nodo-final)
          (setf nodo-actual (first abierto))
          (setf nodo-actual (buscar-mejor (first abierto) ; ¡FUNCIÓN RECURSIVA PARA BUSCAR UN NODO CON MENOR F!
                                          (rest abierto))))

      (setf abierto (remove nodo-actual abierto))         ; lo sacamos de abierto
      (push nodo-actual cerrado)                          ; y lo enviamos a cerrado

      (if (eql nodo-actual nodo-final)                    ; Si llegamos al nodo final terminamos
          (return (camino-final nodo-actual)))            ; ¡FUNCIÓN PARA REGRESAR UNA LISTA CON LOS NODOS DE LA RUTA!

      (setf sucesores                                     ; De otro modo, obtenemos los posibles sucesores de n
            (mapcar #'first (get-distancia nodo-actual))) ; Por medio de un mapcar de los primeros elementos de cada lista

      (dolist (sucesor sucesores)                         ; Para cada nodo sucesor revisa si aún no esta
        (if (not (or (member sucesor abierto)             ; ni en abierto
                     (member sucesor cerrado)))           ; ni en cerrado

          (progn                                          ; Si es TRUE :
           (incrementa-cuenta-nodos)                      ; aumenta un nodo a la cuenta
            (set-g sucesor
                (calcular-g sucesor nodo-actual))         ; Calculamos el valor g del nodo sucesor
            (set-f sucesor
                (calcular-f sucesor))                     ; Calculamos el valor f del nodo sucesor
            (setf abierto (coloca sucesor abierto))       ; Metemos el nodo sucesor a los abiertos
            (set-ancestro sucesor nodo-actual))           ; Guardamos nodo-actual como el ancestro de sucesor

          (let*                                           ; Si es FALSE (Esta en abierto o cerrado):
            ((ans-anterior (get-ancestro sucesor))        ; Recupero el nodo que antes fue su ancestro

             (nuevo-f (if ans-anterior                    ; Reviso si es el nodo inicial (que no tiene ancestro)
                          (+ (- (get-f sucesor)
                                (get-g ans-anterior)
                                (or (rest (assoc ans-anterior (get-distancia sucesor))) 99999999))
                             (get-g nodo-actual)
                             (or (rest (assoc nodo-actual (get-distancia sucesor))) 99999999))
                          (get-f sucesor))))
            (if (< nuevo-f (get-f sucesor))               ; En caso de que sea menor el nuevo-f que el anterior
                (progn
                  (set-g sucesor (+ (- (get-g sucesor)    ; Actualizo el valor de g al de la nueva conexión
                                       (get-f sucesor))
                                    nuevo-f))
                  (set-f sucesor nuevo-f)                 ; Actualizo el valor de f al del nuevo-f
                  (set-ancestro sucesor nodo-actual)      ; Cambio el nodo ancestro a este nodo

                  (if (member sucesor abierto)            ; Si estaba en abierto, lo saco y lo meto con su nuevo
                      (progn                              ; valor, en su nueva posición en la lista
                        (setf abierto
                              (remove sucesor abierto))
                        (setf abierto
                              (coloca sucesor abierto))))

                  (if (member sucesor cerrado)            ; Si estaba en cerrado, lo saco y lo regreso
                      (progn                              ; a abierto, para darle otra oportunidad
                        (setf abierto
                              (coloca sucesor abierto))
                        (setf cerrado
                              (remove sucesor cerrado))))

                ))                                        ; Aquí termina el if
          ))                                              ; Aquí termina el caso FALSE para abierto o cerrado
      )                                                   ; Aquí termina el dolist
    )                                                     ; Aquí termina el loop
))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; **** LLamada a la función ****
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test ()
  (iniciar-cuenta-nodos)

  (let* ((input (with-open-file (in "input.txt")
                  (loop repeat 2
                        collect (read in))))
         (path (a-star (car input) (car (cdr input))))
         (dist (get-f (car (cdr input)))))
  (with-open-file (str "output.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
                  (format str "~a~%" dist)
                  (dolist (city path)
                          (format str "~{~a~^,~}~%" (get-coordenadas city))))
))
