       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           PROJECT6.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL-MASTER
               ASSIGN TO "W:\CIS253PROJECT6INFO\CH1503MST.NDX"
                   ORGANIZATION IS INDEXED
                   ACCESS IS SEQUENTIAL
                   RECORD KEY IS EMPLOYEE-NO-M.
           SELECT PAYROLL-FILE
               ASSIGN TO "W:\CIS253PROJECT6INFO\CH1503TR.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRINT-FILE
               ASSIGN TO "W:\CIS253PROJECT6INFO\CH1503.RPT"
                   ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL-FILE
           RECORD CONTAINS 80 CHARACTERS.
       01  PAYROLL-REC.
           05  EMPLOYEE-NO-IN               PIC X(5).
           05  EMPLOYEE-NAME-IN             PIC X(20).
           05  TERRITORY-NO-IN              PIC 99.
           05  OFFICE-NO-IN                 PIC 99.
           05  ANNUAL-SALARY-IN             PIC 9(6).
           05  OTHER-DATA-IN                PIC X(35).
           05                               PIC X(10).
       FD  PAYROLL-MASTER
           RECORD CONTAINS 80 CHARACTERS.
       01  MASTER-REC.
           05  EMPLOYEE-NO-M                PIC X(5).
           05  EMPLOYEE-NAME-M              PIC X(20).
           05  TERRITORY-NO-M               PIC 99.
           05  OFFICE-NO-M                  PIC 99.
           05  ANNUAL-SALARY-M              PIC 9(6).
           05  OTHER-DATA-M                 PIC X(35).
           05                               PIC X(10).
       FD  PRINT-FILE
           RECORD CONTAINS 120 CHARACTERS.
       01  PRINT-REC                        PIC X(110).

       working-storage section.
       01 r-there-more-records              pic xxx            value 'yes'.
           88 no-more-recs                                     value 'no'.
       01 employee-no-ws                    pic 9(5).
       01 ws-pg-count                       pic 99             value 0.

       01 date-ws.
           05 yr-ws                         pic xxxx.
           05 mth-ws                        pic xx.
           05 day-ws                        pic xx.

       01 ws-line-count                     pic 99             value 0.

       01 heading1.
           05                               pic x(25)          value spaces.      
           05                               pic x(25)
               value "control listing of master".
           05                               pic x(17)
               value " payroll records ".
           05 month-hl                      pic xx.
           05                               pic x              value "/".
           05 day-hl                        pic xx.
           05                               pic x              value "/".
           05 yr-hl                         pic xx.
           05                               pic x              value spaces.
           05                               pic x(5)           value "page".
           05 hl-page                       pic 99.

       01 name-hdr.
           05                               pic x(40)          value spaces.
           05                               pic x(20)          
               value "author: Jon Yen".
           05                               pic x(30)          value spaces.

       01 heading2.
           05                               pic x(8)           value spaces.
           05                               pic x(26)
               value "employee no  employee name".
           05                               pic x(9)           value spaces.
           05                               pic x(9)
               value "terr no".
           05                               pic x(3)           value spaces.
           05                               pic x(9)
               value "office no".
           05                               pic x(4)           value spaces.
           05                               pic x(17)
               value "old annual salary".
           05                               pic x(4)           value spaces.
           05                               pic x(17)
               value "new annual salary".

       01 detail-line.
           05                               pic x(12)          value spaces.
           05 employee-no-out               pic 9(5).
           05                               pic x(6)           value spaces.
           05 employee-name-out             pic x(20).
           05                               pic x(3)           value spaces.
           05 territory-no-out              pic 99.
           05                               pic x(8)           value spaces.
           05 office-no-out                 pic 99.
           05                               pic x(14)          value spaces.
           05 old-a-sal-out                 pic $ZZZ,ZZZ.
           05                               pic x(10)          value spaces.
           05 new-a-sal-out                 pic $ZZZ,ZZZ.

       01 total-line.           
           05  old-a-total                  pic 9(7).
           05  new-a-total                  pic 9(7).

        01 total-line-out.
           05                               pic x(25)          value spaces.
           05                               pic x(45)
               value "annual salary totals:".
           05  old-a-total-out              pic $Z,ZZZ,ZZZ.
           05                               pic x(10)          value spaces.
           05  new-a-total-out              pic $Z,ZZZ,ZZZ.

       procedure division.
       100-main.
           open input payroll-file
               output print-file
                   payroll-master.
           move function current-date to date-ws.
           move mth-ws to month-hl.
           move day-ws to day-hl.
           move yr-ws to yr-hl.
           
           perform 200-heading-module.

           perform 
               until no-more-recs
               read PAYROLL-FILE 
                   at end move "no" to r-there-more-records
                   move old-a-total to old-a-total-out
                   move new-a-total to new-a-total-out
                   write PRINT-REC from total-line-out
                   not at end perform 300-read-move
               end-read
           end-perform.

           close PAYROLL-FILE
                 PAYROLL-MASTER
                 PRINT-FILE
           stop run.

       200-heading-module.
           add 1 to ws-pg-count
           move ws-pg-count to hl-page
           move spaces to PRINT-REC
           write print-rec after advancing page
           write print-rec from heading1 after 6
           write PRINT-REC from name-hdr
           write print-rec from heading2 after 2
           move spaces to PRINT-REC
           write PRINT-REC after 1
           move 9 to ws-line-count.

       300-read-move.
           move spaces to MASTER-REC
           move EMPLOYEE-NO-IN to
                EMPLOYEE-NO-M
                employee-no-out
                employee-no-ws.

           move employee-name-in to EMPLOYEE-NAME-M employee-name-out.
           move TERRITORY-NO-IN to TERRITORY-NO-M territory-no-out.
           move OFFICE-NO-IN to OFFICE-NO-M office-no-out.
           move ANNUAL-SALARY-IN to old-a-sal-out.
           compute old-a-total = old-a-total + annual-salary-in. 
           compute ANNUAL-SALARY-M = ANNUAL-SALARY-IN + ANNUAL-SALARY-IN * .05.
           move ANNUAL-SALARY-M to new-a-sal-out.
           compute new-a-total = new-a-total + ANNUAL-SALARY-M
           move OTHER-DATA-IN to OTHER-DATA-M

           write MASTER-REC invalid key display "INVALID KEY",
                 MASTER-REC
           end-write.

           if ws-line-count > 55
               perform 200-heading-module
           end-if.

           write PRINT-REC from detail-line after 1.
           add 1 to ws-line-count.




