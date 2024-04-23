      ******************************************************************
      *                                                                *
      *   OBJECTIF  : Vérifie si une phrase est un isogramme           *
      *                                                                *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ISOGRAM.
       AUTHOR. Pierre.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *    Définit la variable pour stocker la phrase entrée
       01  INPUTPHRASE PIC X(100).      
      *    Position du caractère actuel dans la phrase
       01  CHARPOSITION PIC 9(03) COMP.
      *    Compteur de lettres
       01  LETTERCOUNT PIC 9(03) COMP VALUE 0.  
      *    Tableau pour stocker les occurrences de chaque lettre
       01  ALPHABETOCCURS PIC 9(03) OCCURS 26 VALUE 0.
      *    Indique si la phrase est un isogramme  
       01  ISISOGRAM PIC X(03) VALUE 'YES'. 
      *    Valeur ASCII du caractère actuel
       01  CHAR-VAL PIC 9(03) COMP. 
      *    Pour stocker la réponse de l'utilisateur (Oui/Non)
       01  ANS PIC X(01).   

       PROCEDURE DIVISION.

           PERFORM UNTIL ANS = 'N'
               DISPLAY "Enter a phrase to check if it's an isogram: "
               WITH NO ADVANCING
               ACCEPT INPUTPHRASE

      *    Itère à travers chaque caractère de la phrase jusqu'à ce que 
      *    la fin de la phrase soit atteinte ou jusqu'à ce qu'il soit 
      *    déterminé que la phrase n'est pas un isogramme
               PERFORM VARYING CHARPOSITION FROM 1 BY 1 UNTIL 
               CHARPOSITION > LENGTH OF INPUTPHRASE OR ISISOGRAM = 'NO'

      *    Convertit le caractère actuel en majuscule et détermine 
      *    sa valeur ASCII
                   MOVE FUNCTION ORD(FUNCTION UPPER-CASE(
                              INPUTPHRASE(CHARPOSITION:1))) TO CHAR-VAL

      *    Vérifie si le caractère est une lettre de l'alphabet
                   IF CHAR-VAL >= 65 AND CHAR-VAL <= 90 THEN
      *    Calcule l'indice dans le tableau pour le caractère
                       COMPUTE LETTERCOUNT = CHAR-VAL - 64  
      *    Vérifie si le caractère a déjà été rencontré
                       IF ALPHABETOCCURS (LETTERCOUNT) > 0 THEN
      *    Si oui, la phrase n'est pas un isogramme
                           MOVE 'NO ' TO ISISOGRAM              
                       ELSE
      *    Sinon, met à jour le tableau des occurrences
                           ADD 1 TO ALPHABETOCCURS (LETTERCOUNT)   
                       END-IF
                   END-IF
               END-PERFORM

      *    Affiche le résultat
               DISPLAY "Is the phrase an isogram? " ISISOGRAM

               DISPLAY "Do you want to check another phrase? (Y/N): "
               WITH NO ADVANCING
               ACCEPT ANS
               MOVE FUNCTION UPPER-CASE(ANS) TO ANS
           END-PERFORM

           STOP RUN.

