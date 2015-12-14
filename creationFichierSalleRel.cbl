      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. creationFichierSallRel.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       file-control.
       select FiSalle assign "../Fichiers/SALLE.REL"
              organization is relative
              access mode is dynamic
              relative key is salleID
                  file status is fs-fiSalle.
       select FiMajSalle assign "../Fichiers/salleSeq.seq"
           organization is line sequential
           file status is fs-fiMajSalle.
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------

       FD FiSalle.
       01 EnregSalle.
           02 salleID                      pic 9(2).
           02 tabPlacesCategories          pic 9(9).
           02 REDEFINES tabPlacesCategories.
               03 nbPlaces                 pic 9(3) OCCURS 3.
       FD FiMajSalle.
       01 EnregMajSalle.
           02 numSalle                     pic 9(2).
           02 tabPlacesParSalle            pic 9(9).
           02 REDEFINES tabPlacesParSalle.
               03 nbPlacesParSalle         pic 9(3).
       WORKING-STORAGE SECTION.
      *-----------------------
       01 fs-fiSalle                          pic xx.
       01 fs-fiMajSalle                       pic xx.
           88  finFiMajSalle      VALUE "10".
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
           open output FiSalle.
           open input FiMajSalle.

           read FiMajSalle.
           perform ajouteSalle until finFiMajSalle.

           close FiSalle FiMajSalle.
            STOP RUN.
       ajouteSalle.
           move EnregMajSalle to EnregSalle
           write EnregSalle.
           read FiMajSalle.
      ** add other procedures here
       END PROGRAM creationFichierSallRel.
