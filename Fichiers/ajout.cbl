		ajoutSpectacle.
      ************************************
           move codeGenreNouv to codeGenre.
		   move codeGenreNouv to codeGenreSave.
           start FiSpectacle key is = codeGenre
               invalid key perform nouveauSpectacle
               not invalid key
                       perform goDernierCodeNum
           end-start.


           move titreNouv to titreSave.

           read FiMaj.
           perform ajoutRepresentation until finFiMaj
                                OR codeGenreSave not = codeGenreNouveau.

       nouveauSpectacle.
      ************************************
           move 0 to codeNum.

       goDernierCodeNum.
      ************************************
           read FiSpectacle next
           perform until finErreurFiSpectacle
                           OR codeGenre not = codeGenreNouv
               move codeNum to codeNumPrec
               read FiSpectacle next
           end-perform.

           move codeNumPrec to codeNum.

       ajoutRepresentation.
      ************************************
           move "ajoute representation" to EnregDebug.
           write EnregDebug.
           
           move titreSave to titre.
           move dateRepresentationNouveau to dateRepresentation.
           move numSalleNouveau to numSalle.
           add 1 to codeNum.


           perform varying iCategorie from 1 by 1 until iCategorie > 3
               MOVE 0 TO nbReservations(iCategorie)
           end-perform.

           move EnregSpectacle to EnregDebug.
           write EnregDebug.
           write EnregSpectacle.
           read FiMaj.