      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*

       PROGRAM-ID. CLIENTES.

      *----------------------------------------------------------------*
      *                                                                *
      * PROGRAMA : CLIENTES                                            *
      *     TIPO : FUNCIONAL                                           *
      *                                                                *
      * ANALISTA : VITOR MARTINS LYRA                                  *
      *     DATA : 22/04/2020                                          *
      *                                                                *
      * PROJETO  : SISTEMA DE GESTAO DE CLIENTES                       *
      *----------------------------------------------------------------*
      *                                                                *
      * MODULOS CHAMADOS:                                              *
      *                                                                *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.
      *----------------------------------------------------------------*
           SELECT CLIENTES ASSIGN TO
                                  'D:\Estudos\Alura\Cobol\CLIENTES.DAT'
             ORGANIZATION IS INDEXED
             ACCESS MODE  IS RANDOM
             FILE STATUS  IS WRK-CLIENTES-STATUS
             RECORD KEY   IS  CLIENTES-CHAVE.

      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.
      *----------------------------------------------------------------*
       FD CLIENTES.
          01 CLIENTES-REG.
             05 CLIENTES-CHAVE.
                10 CLIENTES-FONE           PIC 9(09).
             05 CLIENTES-NOME              PIC X(30).
             05 CLIENTES-EMAIL             PIC X(40).
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*

       77 WRK-OPCAO                     PIC X(001) VALUE SPACES.
       77 WRK-MODULO                    PIC X(025) VALUE SPACES.
       77 WRK-TECLA                     PIC X(001) VALUE SPACES.
       77 WRK-OPCAO-RELATO              PIC X(001) VALUE SPACES.
       77 WRK-CLIENTES-STATUS           PIC 9(002) VALUE ZEROS.

      *----------------------------------------------------------------*
       SCREEN SECTION.
      *----------------------------------------------------------------*
       01 SCR-TELA.
          05 SCR-LIMPRA-TELA.
             10 BLANK SCREEN.
             10 LINE 01 COLUMN 01 ERASE EOL
                BACKGROUND-COLOR 3.
             10 LINE 01 COLUMN 25 PIC X(20)
                BACKGROUND-COLOR 3 FOREGROUND-COLOR 0
                           FROM 'SISTEMA DE CLIENTES'.
             10 LINE 02 COLUMN 01 PIC X(25) ERASE EOL
                BACKGROUND-COLOR 1 FROM WRK-MODULO.

       01 SCR-MENU.
          05 LINE 07 COLUMN 15 VALUE '1 - INCLUIR'.
          05 LINE 08 COLUMN 15 VALUE '2 - CONSULTAR'.
          05 LINE 09 COLUMN 15 VALUE '3 - ALTERAR'.
          05 LINE 10 COLUMN 15 VALUE '4 - EXCLUIR'.
          05 LINE 11 COLUMN 15 VALUE '5 - RELATORIO'.
          05 LINE 12 COLUMN 15 VALUE 'X - SAIDA'.
          05 LINE 13 COLUMN 15 VALUE 'OPCAO......: '.
          05 LINE 13 COLUMN 28 USING WRK-OPCAO.

       01 SRC-RELATO.
          05 LINE 12 COLUMN 55 VALUE '1 - EM TELA'.
          05 LINE 13 COLUMN 55 VALUE '2 - EM DISCO'.
          05 LINE 14 COLUMN 55 VALUE 'OPCAO......: ' .
          05 LINE 14 COLUMN 68 USING WRK-OPCAO-RELATO.

       01 SRC-REGISTRO.
            05 CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE 'TELEFONE '.
               10 COLUMN PLUS 2 PIC 9(09) USING CLIENTES-FONE
                   BLANK WHEN ZEROS.
            05 SS-DADOS.
               10 LINE 11 COLUMN 10 VALUE 'NOME.... '.
               10 COLUMN PLUS 2 PIC X(30) USING CLIENTES-NOME.
               10 LINE 12 COLUMN 10 VALUE 'EMAIL... '.
               10 COLUMN PLUS 2 PIC X(40) USING CLIENTES-EMAIL.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       0000-INICIAR SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR.
           PERFORM 2000-PROCESSAR.
           PERFORM 9000-FINALIZAR.

           STOP RUN.

      *----------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR SECTION.
      *----------------------------------------------------------------*
           OPEN I-O CLIENTES.
           IF WRK-CLIENTES-STATUS EQUAL 35 THEN
              OPEN OUTPUT CLIENTES
              CLOSE CLIENTES
              OPEN I-O CLIENTES
           END-IF.

           DISPLAY SCR-TELA.
           ACCEPT SCR-MENU.

      *----------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-OPCAO
               WHEN 1
                 PERFORM 2100-INCLUIR
               WHEN 2
                 CONTINUE
               WHEN 3
                 CONTINUE
               WHEN 4
                 CONTINUE
               WHEN 5
                 ACCEPT SRC-RELATO
                 IF WRK-OPCAO-RELATO EQUAL 1
                    PERFORM 2600-RELATORIO-TELA
                 ELSE
                    PERFORM 2700-RELATORIO-DISCO
                 END-IF
               WHEN OTHER
                 IF WRK-OPCAO NOT EQUAL 'X'
                    DISPLAY 'ENTRE COM A OPCAO CORRETA'
                 END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-INCLUIR SECTION.
      *----------------------------------------------------------------*

           MOVE 'MODULO - INCLUSAO ' TO WRK-MODULO.
           DISPLAY SCR-TELA.
           ACCEPT SRC-REGISTRO.
           WRITE CLIENTES-REG.
           DISPLAY SCR-TELA.
           ACCEPT SCR-MENU.

      *----------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2600-RELATORIO-TELA SECTION.
      *----------------------------------------------------------------*

           CONTINUE.

      *----------------------------------------------------------------*
       2600-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2700-RELATORIO-DISCO SECTION.
      *----------------------------------------------------------------*

           CONTINUE.

      *----------------------------------------------------------------*
       2700-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FINALIZAR SECTION.
      *----------------------------------------------------------------*

           CLOSE CLIENTES.

      *----------------------------------------------------------------*
       9000-99-FIM. EXIT.
      *----------------------------------------------------------------*
