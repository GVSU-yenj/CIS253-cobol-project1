       identification division.
       program-id. Program1 as "Project_3.Program1".
       author. jon yen.

       data division.

       working-storage section.
       01 work-areas.
           05 more-houses              pic X value 'y'.
           05 borrow-amount            pic 9(6).
           05 house-price              pic 9(6).
           05 max-borrow               pic 9(6).
           05 down-pay                 pic 9(6).
           05 down-pay-out             pic $$$$,$$9.

       screen section.
       01 intro-screen.
       *> 1 is value for Blue, 7 is value for White
           05 blank screen
              background-color is 1
              foreground-color is 7
              highlight.
              10 blank screen. 
              10 line 1 column 1       value 'Enter the amount you wish to borrow:'.
              10 pic 9(6) to borrow-amount.
              10 line 2 column 1       value 'Enter the price the house is valued at:'.
              10 pic 9(6) to house-price.
       01 greedy-screen.
       *> In case they wanna borrow 50%+
           05 background-color is 1
              foreground-color is 7
              highlight.
              10 line 7 column 1       value 'You may not borrow more than 50% of a home value'.
              10 line 9 column 1       value 'Do you wish to calculate any more homes? <y or n>'.
              10 pic X to more-houses.
       01 too-much-screen.
       *> In case they wanna borrow $500k+
           05 background-color is 1
              foreground-color is 7
              highlight.
              10 line 7 column 1       value 'The bank does not have loans for homes valued over $500k'.
              10 line 9 column 1       value 'Do you wish to calculate any more homes? <y or n>'.
              10 pic X to more-houses.
              
       01 success-screen.
            05 background-color is 1
              foreground-color is 7
              highlight.
              10 line 7 column 1       value 'The required down payment is'.
              10 line 7 column 32      pic $$$$,$$9 from down-pay-out.
              10 line 9 column 1       value 'Do you wish to calculate any more homes? <y or n>'.
              10 pic X to more-houses.

       procedure division.

       100-main.
           perform until more-houses = 'n' or 'N'
           display intro-screen
           accept  intro-screen
           perform 200-deal-or-no-deal
           end-perform.
           display 'See ya later then'.
           stop-run.

       200-deal-or-no-deal.
           if house-price > 500000
               display too-much-screen
               accept  more-houses
           else if borrow-amount > (house-price/2)
               display greedy-screen
               accept  more-houses
           else
               perform 250-quik-mafs
               display success-screen
               accept  more-houses.

       250-quik-mafs.
           if borrow-amount > 90000
               compute down-pay = ((30000 * 0.08) + (60000 * 0.04) + (borrow-amount - 90000) * 0.10) 
               move down-pay to down-pay-out
           else if
               borrow-amount > 60000
               compute down-pay = ((60000 * 0.04) + ((borrow-amount - 60000) * 0.08))
               move down-pay to down-pay-out
           else
               compute down-pay = borrow-amount * 0.04
               move down-pay to down-pay-out.
           
       end program Program1.
