       identification division.
       program-id. PROJECT5.
       author. Jon Yen.

       environment division.

       input-output section.
       select registration-trans
           assign to "w:\PROJECT5\CH1204.DAT"
               organization is line sequential.

       select registration-table-input-file
           assign to "w:\PROJECT5\CH1204T.TAB"
               organization is line sequential.

       select registration-report
           assign to "w:\PROJECT5\CH1204.RPT"
               organization is line sequential.

       data division.
       file section.

       fd  registration-trans
           record contains 51 characters.
       01  registration-rec-in.
           05  class-in                    pic x(2).
           05  first-name-in               pic x(10).
           05  middle-init-in              pic x(1).
           05  last-name-in                pic x(20).
           05  weight-in                   pic 9(5).
           05  description-in              pic x(13).

       fd registration-table-input-file
          record contains 5 characters.
       01 table-rec                        pic x(5).

       fd registration-report
          data record is report-rec.
       01 report-rec                       pic x(80).

       working-storage section.

       01 r-there-more-records             pic xxx         value "yes".
           88 no-records-fam                               value "no".

       01 ws-line-ct                       pic 99          value 0.

       01 fee-totals                       pic $$$,$$$.

       01 ws-name.
           05 name-string                  pic x(35).
           05 comma1                       pic x           value ','.
           05 space1                       pic x           value spaces.
           05 period1                      pic x           value '.'.

       01 date-ws.
           05 yr-ws                        pic x(4).
           05 mth-ws                       pic xx.
           05 d-ws                         pic xx.

       01 table1.
           05 v-tab occurs 90 times indexed by x1.
               10 v-class                  pic x(2).
               10 reg-rate                 pic 9v99.

       01 hdr-1.
           05                              pic x(12)      value spaces.
           05                              pic x(20)
               value 'registration report'.
               *> Need to move corresponding
           05 date-out.
               10 mth-ws                   pic xx.
               10                          pic x          value spaces.
               10 d-ws                     pic xx.
               10                          pic x          value spaces.
               10 yr-ws                    pic x(4).
           05                              pic x(12)      value spaces.
           05                              pic x(5)       value 'page:'.
           05 pg-num-hl                    pic 99.

       01 hdr-2.
           05                              pic x(20)      value spaces.
           05                              pic x(20)
               value 'author: Jonathan Yen'.

       *>May have to detail which hdr (e.g hdr-out vs. what I have now)
       01 detail-hdr.
           05                              pic x(15)
               value 'vehicle class'.
           05                              pic x(12)      value spaces.
           05                              pic x(4)
               value 'name'.
           05                              pic x(25)      value spaces.
           05                              pic x(6)
               value 'weight'.
           05                              pic x(10)      value spaces.
           05                              pic x(3)
               value 'fee'.

       01 detail-out.
           05                              pic x(6)       value spaces.
           05  class-out                   pic x(2).
           05                              pic x(5)       value spaces.                
      * order of the name 
      *    05  m-inital-out                pic x.
      *    05  f-name-out                  pic x(10).
      *    05  l-name-out                  pic x(20).
           05  name-out                    pic x(31).
           05                              pic x(12)       value spaces.
           05  weight-out                  pic ZZ,ZZZ.
           05                              pic x(8)       value spaces.
           05  fee-out                     pic $ZZ,ZZZ.99.
      *    05  descrip-out                 pic x(13).

       01 fees.
           05 calc-fee                     pic 9(5)v99.
           05 fee-totals                   pic 9(5)v99.

       01 totals-out.
           05                              pic x(15)      value spaces.
           05                              pic x(25)      value 'total registration fees:'. 
           05 fee-totals                   pic $$$,$$$.99.
           05                              pic x(29)      value spaces.

       procedure division.

       100-main.
       open input registration-trans
            input registration-table-input-file
            output registration-report

       perform 150-date-fix.
       perform 200-hdrs-out.

      *>Reading in the table
           set x1 to 1
       perform until no-records-fam or x1>90
           read registration-table-input-file into v-tab(x1)
               at end move 'no' to r-there-more-records
           end-read
           set x1 up by 1
           end-perform.

      *> Reading in the transactions
           move 'yes' to r-there-more-records
           perform until no-records-fam
               read registration-trans
                   at end move 'no' to r-there-more-records
                   not at end perform 500-calc-rtn
               end-read
           end-perform.
          

       perform 700-total-rtn.
        close registration-trans
             registration-table-input-file
             registration-report
       stop run.
       

       150-date-fix.
           move corr date-ws to hdr-1.
           inspect date-out replacing all spaces by '/'.

       200-hdrs-out.
           add 1 to pg-num-hl.
           write report-rec from hdr-1.
           write report-rec from hdr-2.
           write report-rec from detail-hdr.

       500-calc-rtn.
       move class-in to class-out.
           if middle-init-in = ' '
           string last-name-in delimited by '  '
               ',' space first-name-in delimited by '  '
               into name-out
           else
                string last-name-in delimited by '  '
               ',' space first-name-in delimited by '  '
               space middle-init-in '. ' delimited by size
               into name-out
           end-if
*      Be sure to see the slides on data manipulation to see how to make the period disappear.
  
       move weight-in to weight-out.
       move calc-fee to fee-out.

*      searchin 
       set x1 to 1
       search v-tab 
      *      at end perform 600-error-rtn
               when class-in = v-class(x1)
               perform 650-quik-mafs.

      * 600-error-rtn.
      * move spaces to detail-out.
      * move "out of range" to name-out.
      * write report-rec from detail-out.


       650-quik-mafs.
       compute calc-fee = weight-in * reg-rate(x1).
       move fee-totals in fees to fee-totals of totals-out.
       move calc-fee to fee-out.
       compute fee-totals in fees = fee-totals in fees + calc-fee.
       write report-rec from detail-out.

       
       700-total-rtn.
           write report-rec from totals-out.


