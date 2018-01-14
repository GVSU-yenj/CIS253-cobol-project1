       identification division.

       program-id. project1
       *AUTHOR. JON YEN

       data division.

       working-storage section. 
       01  keyed-fields.
           05   employee-name-in       pic x(30).
           05   salary-in              pic 9(6).
       01  displayed-output.
           05   employee-name-out      pic x(30).
           05   state-tax              pic 9(5).99.
           05   federal-tax            pic 9(6).99.
       01  more-data                   pic x(3) value 'yes'.

       procedure division.
       100-main-module.
           perform until more-data = 'no'.
                   display 'enter employee name (30 character max)'
                   accept employee-name-in
                   display 'enter salary as 6 digits max'
                   accept salary-in
                   perform 200-process-and-create-output
                   display 'is there more data (yes/no)?'
                   accept more-data
           end-perform
           display 'end of job'
           stop run.

       200-process-and-create-output.
               move employee-name-in to employee-name-out
               multiply salary-in by .15 giving federal-tax
               multiply salary-in by .05 giving state-tax.
               display 'federal tax for', employee-name-out.
                           'is' federal-tax
               display 'state-tax for', employee-name-out.
                           'is' state-tax
