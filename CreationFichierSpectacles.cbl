       program-id. CreationFichierSpectacles as "CreationFichierSpectacles".

       environment division.
      *========================================
       input-output section.
      *----------------------------------------
       file-control.
      *****************************************
           select FiSpectacle assign "../../Fichiers/SPECTACLE.IND"
               organization is indexed access mode is random
               record key is codeSpect
                   alternate record key is titre
                   alternate record key is dateRepresentation
                   file status is fs-fiSpectacle.
           
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
       working-storage section.
      *----------------------------------------
       77 fs-fiSpectacle                   pic x(2).
       
       procedure division.

       main.
	   
           goback.

       end program CreationFichierSpectacles.
