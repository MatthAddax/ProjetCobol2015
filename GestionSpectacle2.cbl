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

       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
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
       77 fs-fiSpectacle                   pic x(2).
           88 finErreurFiSpectacle VALUES "10" THRU "99".
       77 fs-fiSalle                       pic x(2).
           88 finErreurFiSalle     VALUES "10" THRU "99".
       77 fs-fiMaj                         pic x(2).
           88 finFiMaj             VALUE "10".
       77 codeNumPrec                      pic 9(2).
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      ************************************
           OPEN INPUT FiMaj.
           OPEN I-O FiSpectacle.
           DISPLAY fs-FiSpectacle.
           display fs-FiMaj.
           read FiMaj.
           perform miseAJour until finFiMaj.

           close FiMaj, FiSpectacle.
           stop run.
       miseAJour.
      ************************************
           evaluate codeMaj
               WHEN 'N'
                   display "ajoutSpectacle"
               WHEN 'R'
                   DISPLAY 'Reservation'
               WHEN 'A'
                   DISPLAY 'Annulation'
           end-evaluate.

           read FiMaj.



       ajoutSpectacle.
      ************************************
           move codeGenreNouv to codeGenre.
           start FiSpectacle key is = codeGenre
               invalid key perform nouveauSpectacle
               not invalid key
                       read FiSpectacle next
                       perform goDernierCodeNum
           end-start.

       nouveauSpectacle.
      ************************************
           move 1 to codeNum.
       goDernierCodeNum.
      ************************************
           perform until finErreurFiSpectacle
                           OR codeGenre not = codeGenreNouv
               codeNumPrec = codeNum
               read FiSpectacle next
           end-perform.
       ajoutRepresentation.
      ************************************
           move titreNouv to titre.
           move dateRepresentationNouveau to dateRepresentation.
           move numSalleNouveau to numSalle.
       END PROGRAM GestionSpectacle2.
