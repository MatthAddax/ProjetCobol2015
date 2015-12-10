@ECHO off
git add .
set /p commentaireCommit="Commentaire du commit : "
git commit -m "%%%commentaireCommit%%%"

git push -u origin master
pause