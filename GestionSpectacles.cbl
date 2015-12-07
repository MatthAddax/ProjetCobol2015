       program-id. GestionSpectacles as "GestionSpectacles".

       environment division.
      *========================================
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
       configuration section.
      *----------------------------------------

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
           02 nbReservations               pic 9(3) OCCURS 3
               REDEFINES  tabReservationsCategories.
       FD FiSalle.
       01 EnregSalle.
           02 salleID                      pic 9(2).
           02 tabPlacesCategories          pic 9(9).
           02 nbPlaces                     pic 9(3) OCCURS 3
               REDEFINES tabPlacesCategories.
       working-storage section.
      *----------------------------------------
       77 fs-fiSpectacle                   pic x(2).
       77 fs-fiSalle                       pic x(2).
       77 fs-fiMaj                         pic x(2).
           88 finFiMaj                     value "10".


       procedure division.
      *========================================
       main.
      *****************************************


       end program GestionSpectacles.
