      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. ReadFiSpectacles.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
      *-----------------------
          select OPTIONAL FiSpectacle assign
          "../Fichiers/SPECTACLE.IND"
                               organization is indexed
                               access mode is dynamic
                               record key is codeSpect
                               alternate record key is titre  with
                               duplicates
                               alternate record key is
                            dateRepresentation  with duplicates
                               file status is fs-fiSpectacle.
           select optional indLisible assign "../indLisible.seq"
               organization is line sequential.
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       FD indLisible.
       01 EnregIND                         pic x(100).
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
       WORKING-STORAGE SECTION.
      *-----------------------
       77 fs-fiSpectacle                   pic x(2).
           88 finErreurFiSpectacle VALUES "10" THRU "99".
       01 ligneAffichage.
           02                              pic x(17)
                                           VALUE "Code spectacle : ".
           02 codeSpectEd                  pic x(7).
           02                              pic x(9) VALUE " Titre : ".
           02 titreEd                      pic x(30).
           02                              pic x(13)
                                           VALUE " Num Salle : ".
           02 numSalleEd                   pic z9.
           02                              pic x(23)
                                        VALUE " Date representation : ".
           02 dateRepresentationEd         pic 9999.
           02                              pic x(23)
                                        VALUE " Reservations : ".
           02 tabReservationsCategoriesEd  pic 9(9).
           02 REDEFINES tabReservationsCategoriesEd.
               03 nbReservationsEd         pic ZZ9.


       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
           OPEN I-O FiSpectacle.
           OPEN output indLisible.
           DISPLAY fs-FiSpectacle.
           MOVE SPACES TO codeSpect.
           START FiSpectacle key is > codeSpect
                   INVALID KEY DISPLAY "Fichier vide"
                   not INVALID KEY READ FiSpectacle NEXT
           END-START.
           DISPLAY fs-FiSpectacle.
           PERFORM afficheSpectacle UNTIL finErreurFiSpectacle.
           DISPLAY fs-FiSpectacle.
           CLOSE FiSpectacle indLisible.
           STOP RUN.


       afficheSpectacle.
           move codeSpect to codeSpectEd.
           move titre to titreEd.
           move numSalle to numSalleEd.
           move dateRepresentation to dateRepresentationEd.
           move tabReservationsCategories
                           to tabReservationsCategoriesEd.
           display ligneAffichage.
           display EnregSpectacle.
           move EnregSpectacle to EnregIND.
           write EnregIND.
           READ FiSpectacle NEXT.
      ** add other procedures here
       END PROGRAM ReadFiSpectacles.
