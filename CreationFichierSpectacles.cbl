       program-id. CreationFichierSpectacles as
       "CreationFichierSpectacles".

       environment division.
      *========================================
       configuration section.
      *----------------------------------------
       input-output section.
      *----------------------------------------
       file-control.
      *****************************************
           select FiSpectacle assign "../Fichiers/SPECTACLE.IND"
               organization is indexed access mode is RANDOM
               record key is codeSpect
                   alternate record key is titre
                       with duplicates
                   alternate record key is dateRepresentation
                       with duplicates
                   file status is fs-fiSpectacle.

       data division.
      *========================================
       file section.
      *----------------------------------------
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
               03 nbReservations               pic 9(3) OCCURS 3.
       working-storage section.
      *----------------------------------------
       77 fs-fiSpectacle                   pic x(2).
           88 finErreurFiSpectacle VALUES "10" THRU "99".
       77 codeGenreLu                      pic x(5) VALUE "abcde".

       procedure division.

       main.
      *----------------------------------------
           OPEN I-O FiSpectacle.
           PERFORM ajouterSpectacle.

           STOP RUN.

       ajouterSpectacle.
      *---------------------------------------
           MOVE spaces TO codeGenre.

           START FiSpectacle key is > codeGenre
                   INVALID KEY DISPLAY "Fichier vide"
                   not INVALID KEY READ FiSpectacle NEXT
           END-START.
           perform codeGenreExists until finErreurFiSpectacle
                                       OR codeGenre EQUALS codeGenreLu.
           IF codeGenre EQUALS codeGenreLu THEN
               PERFORM codePlusEleve
           ELSE
               PERFORM ajoutNouveauSpectacle
           END-IF.
       codeGenreExists.
      *----------------------------------------
           READ FiSpectacle NEXT.

       codePlusEleve.
      *----------------------------------------

       ajoutNouveauSpectacle.
      *----------------------------------------

       end program CreationFichierSpectacles.
