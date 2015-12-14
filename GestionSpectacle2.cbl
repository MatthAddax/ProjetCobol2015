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
          select OPTIONAL FiSpectacle assign "../Fichiers/SPECTACLE.IND"
             organization is indexed access mode is dynamic
             record key is codeSpect
                 alternate record key is titre with duplicates
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
           02 codeErreurEd                 pic x(2).
           02 ligneErreur                  pic x(80).
       FD FiSpectacle.
       01 EnregSpectacle.
           02 codeSpect.
               03 codeGenre                pic x(5).
               03 codeNum                  pic 9(2).
           02 titre                        pic x(30).
           02 numSalle                     pic 9(2).
           02 dateRepresentation           pic 9(4).
           02 tabReservationsCategories    pic 9(9).
           02 REDEFINES tabReservationsCategories.
               03 nbReservations           pic 9(3) OCCURS 3.
       FD FiSalle.
       01 EnregSalle.
           02 salleID                      pic 9(2).
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
           02 numSalleNouveau              pic 99.
           02 dateRepresentationNouveau    pic 9(4).
       WORKING-STORAGE SECTION.
      *-----------------------
       01 codeErreur                       pic xx.
       77 fs-fiSpectacle                   pic x(2).
           88 finErreurFiSpectacle VALUES "10" THRU "99".
       77 fs-fiSalle                       pic x(2).
           88 finErreurFiSalle     VALUES "10" THRU "99".
       77 fs-fiMaj                         pic x(2).
           88 finFiMaj             VALUE "10".
       77 codeNumPrec                      pic 9(2).
       77 iCategorie                       pic 9.
       77 titreSave                        pic x(30).
       77 codeGenreSave                    pic x(5).
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      ************************************
           OPEN INPUT FiMaj.
           OPEN I-O FiSpectacle.
           OPEN output debug.
           DISPLAY fs-FiSpectacle.
           display fs-FiMaj.
           read FiMaj.
           perform miseAJour until finFiMaj.

           close FiMaj, FiSpectacle, debug.
           stop run.
       miseAJour.
      ************************************
           evaluate codeMaj
               WHEN 'N'
                   display "ajoutSpectacle"
                   move "ajoutSpectacle" to EnregDebug
                   perform ajoutSpectacle
               WHEN 'R'
                   DISPLAY 'Reservation'
                   move 'Reservation' to EnregDebug
                   perform reservation
               WHEN 'A'
                   DISPLAY 'Annulation'
                   move 'Annulation' to EnregDebug
           end-evaluate.

           write EnregDebug.

           read FiMaj.

       reservation.
      ************************************
      *    recherche si places disponibles
           display titreReserve
                   dateReserve
                   categReserve
                   nbPlacesReserve.

           move titreReserve to titre.
           start FiSpectacle key is = titre
               invalid key display "error"
               not invalid key display
                                   "vérifier si date existe et est ok"
                               display
                                   "si oui vérifier places max de la"
                               DISPLAY
                                   "salle et si ok faire l'ajout"
                               DISPLAY
                                   "sinon faire les erreurs adaptées ;)"
           end-start.



































       ajoutSpectacle.
      ************************************
           move codeGenreNouv to codeGenre.
           move "set key codeGenre" to EnregDebug.
           write EnregDebug.
           start FiSpectacle key is = codeGenre
               invalid key perform nouveauSpectacle
               not invalid key
                       perform goDernierCodeNum
           end-start.

           move "fin start codeGenre" to EnregDebug.
           write EnregDebug.

           move codeGenre to codeGenreSave.
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
           move titreSave to titre.
           move dateRepresentationNouveau to dateRepresentation.
           move numSalleNouveau to numSalle.
           add 1 to codeNum.


           perform varying iCategorie from 1 by 1 until iCategorie > 3
               MOVE 0 TO nbReservations(iCategorie)
           end-perform.
           display EnregSpectacle.
           move EnregSpectacle to EnregDebug.
           write EnregDebug.
           write EnregSpectacle.
           read FiMaj.

       END PROGRAM GestionSpectacle2.
