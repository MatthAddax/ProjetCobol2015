       program-id. GestionSpectacles as "GestionSpectacles".

       environment division.
      *========================================
       configuration section.
      *----------------------------------------
       input-output section.
      *----------------------------------------
       file-control.
      *****************************************
      *Regarder si variables assign�es au bon endroit!!! :D
      *****************************************
          select OPTIONAL FiSpectacle assign "../Fichiers/SPECTACLE.IND"
              organization is indexed access mode is dynamic
              record key is codeSpect
                  alternate record key is titre with duplicates
                  alternate record key is dateRepresentation with 
                               duplicates
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
       01 EnregDateRepresentation.
           02 codeGenreNouveau             pic x(5).
           02 numSalleNouveau              pic 99.
           02 dateRepresentationNouveau    pic 9(4).
       working-storage section.
      *----------------------------------------
       77 fs-fiSpectacle                   pic x(2).
           88 finErreurFiSpectacle VALUES "10" THRU "99".
       77 fs-fiSalle                       pic x(2).
           88 finErreurFiSalle     VALUES "10" THRU "99".
       77 fs-fiMaj                         pic x(2).
           88 finFiMaj             VALUE "10".
       77 iCategorie                       pic 9.
       77 cleLectureIndexe                 pic x(5).
       77 codeNumPrec                      pic 9(2).
       77 codeGenreSave                    pic x(5).
       procedure division.
      *========================================
       main.
           OPEN INPUT FiMaj.
           OPEN I-O FiSpectacle.
           DISPLAY fs-FiSpectacle.
      *****************************************
      ********** LECTURE FICHIER MAJ **********
      *****************************************
           READ FiMaj.
           PERFORM choixActionMaj UNTIL finFiMaj.



           CLOSE FiSpectacle FiMaj.

      *****************************************
       choixActionMaj.
      *****************************************

           EVALUATE codeMaj
               WHEN 'N'
                   PERFORM ajoutSpectacle
               WHEN 'R'
                   DISPLAY 'Reservation'
               WHEN 'A'
                   DISPLAY 'Annulation'
           END-EVALUATE.

           READ FiMaj.
      *****************************************
       ajoutSpectacle.
      *****************************************
      *----------------------------------------
      *--------------Garnir cl�----------------
      *----------------------------------------
           MOVE codeGenreNouv TO codeGenre

           START FiSpectacle key is = codeGenre
               INVALID KEY PERFORM ajoutNouveauSpectacle
               not INVALID KEY READ FiSpectacle NEXT
                               perform positionnementDernier
           END-START.

           DISPLAY fs-FiSpectacle.

           PERFORM ajoutRepresentation UNTIL FinFiMaj
                               OR codeGenre NOT EQUALS codeGenreNouveau.
       positionnementDernier.
           move codeGenreNouv to codeGenreSave.
           PERFORM codePlusEleve UNTIL finErreurFiSpectacle
                                   OR codeGenre NOT = codeGenreSave.
      *****************************************
       codeGenreExists.
      *****************************************
           READ FiSpectacle NEXT.
      *****************************************
       codePlusEleve.
      *****************************************
           move codeNum to codeNumPrec.
           READ FiSpectacle NEXT.
      *****************************************
       ajoutNouveauSpectacle.
      *****************************************
           READ FiMaj.
           MOVE 1 TO codeNum.
           MOVE codeGenreNouv TO codeGenre.
           PERFORM creationRepresentation.
      *****************************************
       ajoutRepresentation.
      *****************************************
           READ FiMaj.
           ADD 1 TO codeNumPrec.
           MOVE codeNumPrec TO codeNum.
           PERFORM creationRepresentation.
      *****************************************
       creationRepresentation.
      *****************************************
           MOVE dateRepresentationNouveau TO dateRepresentation.
           MOVE numSalleNouveau TO numSalle.
           MOVE titreNouv TO titre.
           PERFORM VARYING iCategorie FROM 1 BY 1 UNTIL iCategorie > 3
               MOVE 0 TO nbReservations(iCategorie)
           END-PERFORM.
           DISPLAY EnregSpectacle.
           WRITE EnregSpectacle.

       end program GestionSpectacles.
