       identification division.
       program-id. PROJECT4.

       author. Jon Yen.
       *> SEE CHP. 10 EXAMPLES, NOT CHP. 12 starts pg. 424
       environment division.
       
       input-output section.
       select student-file
           assign to "w:\Project4\CH1004.DAT"
               organization is line sequential.

       select student-report
           assign to "w:\Project4\CH1004.RPT"
               organization is line sequential.

       data division.
       file section.

       fd  student-file
           record contains 38 characters.
       01  student-rec-in.
           05  soc-sec-no-in               pic x(9).
           05  name-in                     pic x(21).
           05  class-in                    pic x.
               *> freshman
               88  fr                                  value "1".
               *> sophomore
               88  soph                                value "2".
               *> jun
               88  junior                              value "3".
               *>senior
               88  sen                                 value "4".
           05  school-in                   pic x.
               88  business                            value "1".
               88  arts                                value "2".
               88  engineering                         value "3".
           05  gpa-in                      pic 9v99.
           05  credits-in                  pic 9(3).
       fd  student-report
           record contains 80 characters.
       01  report-out                      pic x(80).

       working-storage section.
       01 skool-title
           05                              pic x(25)   value spaces.
           05                              pic x(30)   value 'Pass em State College'.
           05                              pic x(25)   value spaces.
       01 myname
           05                              pic x(25)   value spaces.
           05                              pic x(30)   value 'Jonathan Yen'.   
           05                              pic x(25)   value spaces.
       01 catagory-hdrs
           05                              pic x(10)   value 'school:'.
           05                              pic x(5)    value spaces.
           05  school-name-out             pic x(15).   
           05                              pic x(20)   value spaces.
           05                              pic x(5)    value 'page '.
           05  rpt-pg-out                  pic 9(5)    value 0.
           05                              pic x(5)    value spaces.
           05                              pic x(14)   value '10/14/2017'.
       01 real-hdrs
           05                              pic x(5)    value 'class'.
           05                              pic x(30)   value spaces.   
           05                              pic x(40)   value 'average GPA'.
       01 da-data
           05  rpt-class                   pic x(10)   value 'freshman:'.  
           05                              pic x(25)   value spaces.
           05  rpt-gpa                     pic 9.9(2).

       01 class-counts
           05  fr-count                    pic 9(3).
           05  soph-count                  pic 9(3).
           05  jun-count                   pic 9(3).
           05  sen-count                   pic 9(3).

       01 gpa-counts
           05 fr-gpa                       pic 99v99.
           05 soph-gpa                     pic 99v99.
           05 jun-gpa                      pic 99v99.
           05 sen-gpa                      pic 99v99.

       01 temp-values.
           05 ws-avg-gpa                   pic 99v99.
           05 ws-save-skool                pic 9       value 0.
           05 r-there-more-rcds            pic x(3)    value 'yes'.

       procedure division.
       000-main-module.
           open input student-file 
               output student-report
           if school-in not = ws-save-skool
               perform 100-ctrl-brk-rtn
                   perform until r-there-more-rcds = 'no'
                       read student-file
                       at end  
                           move 'no' to r-there-more-rcds
                       not at end
                           perform 200-accum-totals.
           stop-run.

       100-ctrl-brk-rtn.
           if ws-save-skool = '0'
               move 'business' to school-name-out
           else if ws-save-skool = '1'
               move 'liberal arts' to school-name-out
           else if ws-save-skool = '2'
               move 'engineering' to school-name-out
           end-if.

           if fr-count > 0
               compute ws-avg-gpa = fr-gpa/fr-count
               move ws-avg-gpa to rpt-gpa
               move "freshman" to rpt-class
               perform 300-print-rpt
               set ws-avg-gpa to zero
           else if soph-count > 0
               compute ws-avg-gpa = soph-gpa/soph-count
               move ws-avg-gpa to rpt-gpa
               move "sophomore" to rpt-class
               perform 300-print-rpt
               set ws-avg-gpa to zero
           else if jun-count > 0
               compute ws-avg-gpa = jun-gpa/jun-count
               move ws-avg-gpa to rpt-gpa
               move "junior" to rpt-class
               perform 300-print-rpt
               set ws-avg-gpa to zero
           else if sen-count > 0
               compute ws-avg-gpa = sen-gpa/sen-count
               move ws-avg-gpa to rpt-gpa
               move "senior" to rpt-class
               perform 300-print-rpt
               set ws-avg-gpa to zero
           end-if. 

       200-accum-totals.
           if class-in = '1'
               add 1 to fr-count
               add gpa-in to fr-gpa
           else if class-in = '2'
               add 1 to soph-count
               add gpa-in to soph-gpa
           else if class-in = '3'
               add 1 to jun-count
               add gpa-in to jun-gpa
           else if class-in = '4'
               add 1 to sen-count
               add gpa-in to sen-gpa.

       300-print-rpt.
           add 1 to rpt-pg-out.
           write report-out from skool-title.
           write report-out from myname.
           write report-out from catagory-hdrs.
           write report-out from real-hdrs
           perform 400-print-da-data.

       400-print-da-data.
           write report-out from da-data.
           perform 200-accum-totals.
           write report-out from da-data.
           perform 200-accum-totals.
           write report-out from da-data.
           perform 200-accum-totals.
           write report-out from da-data. 
           





       