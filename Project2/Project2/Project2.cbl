       
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID.PROJECT2.
       AUTHOR. JON YEN.
           
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-FILE
               ASSIGN TO "W:\Project2\CH0601.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRINT-FILE
               ASSIGN TO "W:\Project2\CH0601.RPT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       file section.
       fd  cust-file
           record contains 19 characters.
       01  trans-rec-in.
           05  initial1                pic x. 
           05  inital2                 pic x.
           05  last-name               pic x(10).
           05  mon-o-transaction       pic xx.
           05  yr-o-tranaction         pic x(4).
           05  trans-amt               pic 9(6).
       fd print-file
          record contains 80 characters.
       01 print-rec                   pic x(80).

       WORKING-STORAGE SECTION.
       01  work-areas.
           05  are-there-more-records pic xxx
                   value 'yes'.
           05  ws-date.               
               10  ws-yr              pic 9(4).
               10  ws-mo              pic 99.
               10  ws-day             pic 99.
           05  ws-pg-ct
                   value zero.
       

       PROCEDURE DIVISION.