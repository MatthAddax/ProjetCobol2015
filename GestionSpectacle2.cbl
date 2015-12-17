      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. GestionSpectacle2.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       file-control.
         select FiSpectacle assign "../Fichiers/SPECTACLE.IND"
             organization is indexed access mode is dynamic
             record key is codeSpect
                 alternate record key is titre
                                       with duplicates
                 alternate record key is dateRepresentation
                                       with duplicates
                 file status is fs-fiSpectacle.

         select FiSalle assign "../Fichiers/SALLE.REL"
              organization is relative
              access mode is dynamic
              relative key is salleID
                  file status is fs-fiSalle.

         SELECT FiMaj assign "../Fichiers/maj.seq"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS fs-FiMaj.
         SELECT FiErreur assign "../Fichiers/erreurs.seq"
              ORGANIZATION IS LINE SEQUENTIAL.
         SELECT FiListingTitre assign "../Fichiers/ListingTitres.seq"
           organization is line sequential.
         SELECT FiListingOctobre assign "../Fichiers/ListingOctobre.seq"
           organization is line sequential.

           select optional debug assign "../debug.seq"
               organization is line sequential.
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       FD debug.
       01 EnregDebug                       pic x(100).
       FD FiErreur.
       01 EnregErreur.
           02 codeErreur                   pic x(2).
           02 ligneErreur                  pic x(80).
       FD FiListingTitre.
       01 EnreglistingTitre                pic x(100).
       FD FiListingOctobre.
       01 EnregListingOctobre.
           02 datesOctobre                 pic x(10).
           02 listeTitres                  pic x(1000).
           02 nbSpectacles                 pic 9(2).

       FD FiSpectacle.
       01 EnregSpectacle.
           02 codeSpect.
               03 codeGenre                pic x(5).
               03 codeNum                  pic 9(2).
           02 titre                        pic x(30).
           02 numSalle                     pic 9(2).
           02 dateRepresentation.
               03  moisRepresentation      pic 9(2).
               03  jourRepresentation      pic 9(2).
           02 tabReservationsCategories    pic 9(9).
           02 REDEFINES tabReservationsCategories.
               03 nbReservations           pic 9(3) OCCURS 3.
       FD FiSalle.
       01 EnregSalle.
           02 tabPlacesCategories          pic 9(9).
           02 REDEFINES tabPlacesCategories.
               03 nbPlaces                 pic 9(3) OCCURS 3.
       FD FiMaj.
       01 EnregMAJ.
           02 codeMaj                      pic x.
           02 informationsAction           pic x(37).
           02 REDEFINES informationsAction.
               03 codeGenreNouv            pic x(5).
               03 titreNouv                pic x(30).
           02 REDEFINES informationsAction.
               03 titreReserve             pic x(30).
               03 dateReserve              pic 9(4).
               03 categReserve             pic 9.
               03 nbPlacesReserve          pic 99.
           02 REDEFINES informationsAction.
               03 codeSpectacleAnnulation  pic x(7).
               03 categAnnulation          pic 9.
               03 nbPlacesAnnulation       pic 99.
       01 EnregDateRepresentation.
           02 codeGenreNouveau             pic x(5).
           02 dateRepresentationNouveau    pic 9(4).
           02 numSalleNouveau              pic 99.
       WORKING-STORAGE SECTION.
      *-----------------------
       77 fs-fiSpectacle                   pic x(2).
           88 finErreurFiSpectacle VALUES "10" THRU "99".
       77 fs-fiSalle                       pic x(2).
           88 finErreurFiSalle     VALUES "10" THRU "99".
       77 fs-fiMaj                         pic x(2).
           88 finFiMaj             VALUE "10".
       01 labelTitre                       pic x(21)
               VALUE "Titre du spectacle : ".
       01 ligneTitre                       pic x(51).
       01 LigneLabelsListingTitre.
           02                              pic x(25)
               VALUE "Dates des représentations".
           02                              pic x(3)
               VALUE SPACES.
           02                              pic x(26)
               VALUE "Nombre de places réservées".
           02                              pic x(3)
               VALUE SPACES.
           02                              pic x(19)
               VALUE "Taux de remplissage".
       01 LigneInfosListingTitre.
           02  dateRepresentationEd        pic x(10).
           02                              pic x(18)
               VALUE SPACES.
           02  nbPlacesReserveesEd         pic zzz9.
           02                              pic x(25)
               VALUE SPACES.
           02  tauxDeremplissage           pic zz9.
           02                              pic x VALUE "%".


       77 codeNumPrec                      pic 9(2).
       77 iCategorie                       pic 9.
       77 titreSave                        pic x(30).
       77 codeGenreSave                    pic x(5).
       77 placesTemp                       pic 9(3).
       77 choix                            pic x.
       77 annee                            pic 9999 value 2015.
       77 totalPlacesReservee              pic 9999.
       77 totalPlacesDisponibles           pic 9999.
       77 taux                             pic 9V99.
       77 dateSave                         pic 9999.
       77 nbSpectacleDuJour                pic 999.
       77 grandeStringSpectacle            pic x(10000).
       77 jourSauve                        pic 99.
       77 pointeur                         pic 99.
       77 salleID                          pic 99.
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      ************************************
           OPEN INPUT FiMaj FiSalle.
           OPEN I-O FiSpectacle.
           OPEN output debug FiErreur FiListingTitre FiListingOctobre.

           read FiMaj.

           perform miseAJour until finFiMaj.

           perform listingParTitre.

           perform listingOctobre.

           close FiMaj,
                 FiSpectacle,
                 debug,
                 FiSalle,
                 FiErreur,
                 FiListingTitre,
                 FiListingOctobre.
           stop run.

       miseAJour.
      ************************************
           move SPACES to EnregDebug.
           evaluate codeMaj
               WHEN 'N'
                   perform ajoutSpectacle
               WHEN 'R'
                   perform reservation
                   read FiMaj
               WHEN 'A'
                   perform annulation
                   read FiMaj
               WHEN OTHER
                   move 01 to codeErreur
                   perform writeErreur
                   read FiMaj
           end-evaluate.


       reservation.
      ************************************
      *    recherche si places disponibles

           move titreReserve to titre.
           start FiSpectacle key is = titre
               invalid key move 02 to codeErreur
                           perform writeErreur
               not invalid key perform reservationPlaces
           end-start.

       reservationPlaces.
      ************************************

           perform checkDateReservation.
           if dateReserve = dateRepresentation
               perform miseAJourPlaces
           else
               move 03 to codeErreur
               perform writeErreur
           end-if.

       checkDateReservation.
      ************************************
           perform until finErreurFiSpectacle
                           OR dateReserve = dateRepresentation
               read FiSpectacle next
           end-perform.

       miseAJourPlaces.
      ************************************
           add nbReservations(categReserve) to nbPlacesReserve
               giving placesTemp.
           move numSalle to salleID.
           read FiSalle.
          display "Nb reserv actuelles : " nbReservations(categReserve).
          DISPLAY "Nb reservations MAJ : " nbPlacesReserve.
          display "Total reservations  : " placesTemp "/"
               nbPlaces(categReserve).
           if placesTemp > nbPlaces(categReserve)
               move 04 to codeErreur
               perform writeErreur
           else
               move placesTemp to nbReservations(categReserve)
           end-if.

       annulation.
      ************************************
           move codeSpectacleAnnulation to codeSpect.

           start FiSpectacle key is = codeSpect
               invalid key move 5 to codeErreur
                           perform writeErreur
               not invalid key perform annuleSpectale
           end-start.

       annuleSpectale.
      ************************************
           SUBTRACT nbPlacesAnnulation
               from nbReservations(categAnnulation).
           if nbReservations(categAnnulation) < 0
              move zeroes to nbReservations(categAnnulation)
           end-if.

       ajoutSpectacle.
      ************************************
           move codeGenreNouv to codeGenre.
           move codeGenreNouv to codeGenreSave.
           start FiSpectacle key is = codeGenre
               invalid key perform nouveauSpectacle
               not invalid key
                       perform goDernierCodeNum
           end-start.


           move titreNouv to titreSave.

           read FiMaj.
           perform ajoutRepresentation until finFiMaj
                                OR codeGenreSave not = codeGenreNouveau.

       nouveauSpectacle.
      ************************************
           move 0 to codeNum.

       goDernierCodeNum.
      ************************************
           read FiSpectacle next
           perform until finErreurFiSpectacle
                           OR codeGenre not = codeGenreNouv
               move codeNum to codeNumPrec
               read FiSpectacle next
           end-perform.

           move codeNumPrec to codeNum.

       ajoutRepresentation.
      ************************************
           move "ajoute representation" to EnregDebug.
           write EnregDebug.
           move codeGenreSave to codeGenre.
           move titreSave to titre.
           move dateRepresentationNouveau to dateRepresentation.
           move numSalleNouveau to numSalle.
           add 1 to codeNum.


           perform varying iCategorie from 1 by 1 until iCategorie > 3
               MOVE 0 TO nbReservations(iCategorie)
           end-perform.

           move EnregSpectacle to EnregDebug.
           write EnregDebug.
           write EnregSpectacle.
           read FiMaj.

       listingParTitre.
      ************************************
      **********listing complet***********
      ************************************
           move spaces to titre.
           start FiSpectacle key is > titre
               invalid key display "fichier vide"
               not invalid key
                   read FiSpectacle next
                   perform listeTitre until finErreurFiSpectacle
           end-start.

       listeTitre.
           move titre to titreSave.
           STRING
               labelTitre titre
               into ligneTitre
           END-STRING.
           display ligneTitre
           move ligneTitre to EnreglistingTitre.
           write EnreglistingTitre.
           display LigneLabelsListingTitre
           move LigneLabelsListingTitre to EnreglistingTitre.
           write EnreglistingTitre.

           perform until finErreurFiSpectacle OR
                           titre not = titreSave
               perform embelliDate
               perform getSalle
               move 0 to totalPlacesReservee
               move 0 to totalPlacesDisponibles
               perform varying iCategorie
                   from 1 by 1 until iCategorie > 3
                       add nbReservations(iCategorie)
                           to totalPlacesReservee
                       add nbPlaces(iCategorie)
                           to totalPlacesDisponibles
               end-perform
               divide totalPlacesReservee by totalPlacesDisponibles
                   giving taux
               end-divide

               multiply 100 by taux giving tauxDeremplissage
               move totalPlacesReservee to nbPlacesReserveesEd
               display LigneInfosListingTitre
               move LigneInfosListingTitre to EnreglistingTitre
               write EnreglistingTitre

               read FiSpectacle next
           end-perform.

       listingOctobre.
      ************************************
      **********listing octobre***********
      ************************************
           move 1000 to dateRepresentation.
           start FiSpectacle key > dateRepresentation
               invalid key display "pas de spectacles en octobre"
               not invalid key read FiSpectacle next
                               perform listeOctobre
                                       until finErreurFiSpectacle
                                       OR dateRepresentation > 1100
           end-start.

       listeOctobre.
      ************************************
           move jourRepresentation to jourSauve.
           perform embelliDate.
           move dateRepresentationEd to datesOctobre.
           move spaces to listeTitres.
           move 1 to pointeur.
           move 0 to nbSpectacles.
           perform until finErreurFiSpectacle
                   OR jourSauve not = jourRepresentation
                   OR dateRepresentation > 1100
               string titre delimited by "  " "-"
                 into listeTitres with pointer pointeur
               end-string
               add 1 to nbSpectacles

               read FiSpectacle next
           end-perform.
           subtract 1 from pointeur.
           move " " to listeTitres(pointeur:1).

           display jourRepresentation "/" moisRepresentation.
           display listeTitres.
           display nbSpectacles.
           write EnregListingOctobre.
       embelliDate.
      ************************************
           string jourRepresentation "/" moisRepresentation "/" annee
               into dateRepresentationEd
           end-string.

       getSalle.
      ************************************
           move numSalle to salleID.
           start FiSalle key = salleID
               not invalid key
                   read FiSalle next
               invalid key display "Une erreur est survenue au niveau -
               du fichier SALLE.REL, veuillez le regénérer"
           end-start.


       writeErreur.
      ************************************
           move EnregMAJ to ligneErreur.
           write EnregErreur.
      ************************************
       END PROGRAM GestionSpectacle2.
