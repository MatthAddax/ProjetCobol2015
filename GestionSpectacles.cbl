       program-id. GestionSpectacles as "GestionSpectacles".

       environment division.
      *========================================
       configuration section.
      *----------------------------------------
       input-output section.
      *----------------------------------------
       file-control.
      *****************************************
           select FiSpectacle assign "../Fichiers/SPECTACLE.IND"
               organization is indexed access mode is random
               record key is codeSpect
                   alternate record key is titre
                   alternate record key is dateRepresentation
                   file status is fs-fiSpectacle.

           select FiSalle assign "../Fichiers/SALLE.REL"
               organization is relative
               access mode is dynamic
               relative key is salleID
                   file status is fs-fiSalle.
           SELECT FiMaj assign "../Fichiers/MAJ.SEQ"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS fs-FiMaj.
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

       working-storage section.
      *----------------------------------------
       77 fs-fiSpectacle                   pic x(2).
           88 finErreurFiSpectacle VALUES "10" THRU "99".
       77 fs-fiSalle                       pic x(2).
           88 finErreurFiSalle     VALUES "10" THRU "99".
       77 fs-fiMaj                         pic x(2).
           88 finFiMaj                     value "10".


       procedure division.
      *========================================
       main.
           OPEN I-O FiSpectacle.
      *****************************************
      ********** LECTURE FICHIER MAJ **********
      *****************************************





      *****************************************
       ajoutSpectacle.
      *****************************************
       MOVE spaces TO codeGenre.

           START FiSpectacle key is > codeGenre
                   INVALID KEY DISPLAY "Fichier vide"
                   not INVALID KEY READ FiSpectacle NEXT
           END-START.
           perform codeGenreExists until finErreurFiSpectacle
                                      OR codeGenre EQUALS codeGenreNouv.
           IF codeGenre EQUALS codeGenreNouv THEN
               PERFORM codePlusEleve
           ELSE
               PERFORM ajoutNouveauSpectacle
           END-IF.
      *****************************************
       codeGenreExists.
      *****************************************
           READ FiSpectacle NEXT.

      *****************************************
       codePlusEleve.
      *****************************************

      *****************************************
       ajoutNouveauSpectacle.
      *****************************************
       end program GestionSpectacles.
