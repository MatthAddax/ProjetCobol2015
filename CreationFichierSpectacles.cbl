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
          select OPTIONAL FiSpectacle assign "../Fichiers/SPECTACLE.IND"
                   organization is indexed access mode is dynamic
                   record key is codeSpect
                   alternate record key is titre with duplicates
                   alternate record key is dateRepresentation
                       with duplicates
                   file status is fs-fiSpectacle.

           select FiAjoutSpectacle assign "../Fichiers/spectacleSeq.seq"
               organization is line SEQUENTIAL
               file status is fs-FiAjoutSpectacle.
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
               03 nbReservations           pic 9(3) OCCURS 3.
       FD FiAjoutSpectacle.
       01 EnregAjoutSpectacle.
           02 codeSpectAjout.
               03 codeGenreAjout           pic x(5).
               03 codeNumAjout             pic 9(2).
           02 titreAjout                   pic x(30).
           02 numSalleAjout                pic 9(2).
           02 dateRepresentationAjout      pic 9(4).
           02 tabResCategAjout             pic 9(9).
           02 REDEFINES tabResCategAjout.
               03 nbReservationsAjout      pic 9(3) OCCURS 3.

       working-storage section.
      *----------------------------------------
       77 fs-fiSpectacle                   pic x(2).
           88 finErreurFiSpectacle VALUES "10" THRU "99".
       77 fs-FiAjoutSpectacle              pic x(2).
           88 finAjoutSpectacle    VALUE "10".
       77 codeGenreLu                      pic x(5) VALUE "abcde".

       procedure division.

       main.
      *----------------------------------------
           OPEN OUTPUT FiSpectacle.
           OPEN INPUT FiAjoutSpectacle.
           PERFORM ajouterSpectacle.
           CLOSE FiAjoutSpectacle FiSpectacle.
           STOP RUN.

       ajouterSpectacle.
      *---------------------------------------
               READ FiAjoutSpectacle
           perform until finAjoutSpectacle
               MOVE EnregAjoutSpectacle to EnregSpectacle
               write EnregSpectacle
                   invalid key display "Erreur cle invalide"
               end-write
               READ FiAjoutSpectacle
           end-perform.
       end program CreationFichierSpectacles.
