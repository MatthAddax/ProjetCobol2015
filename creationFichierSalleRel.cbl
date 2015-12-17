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
       01 EnregSalle                       pic 9(9).
       FD FiMajSalle.
       01 EnregMajSalle.
           02 numSalle                     pic 9(2).
           02 reste                        pic 9(9).
       WORKING-STORAGE SECTION.
      *-----------------------
       01 salleID                             pic 9(2).
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
           move numSalle to salleID.
           write EnregSalle from reste.
           read FiMajSalle.
      ** add other procedures here
       END PROGRAM creationFichierSallRel.
