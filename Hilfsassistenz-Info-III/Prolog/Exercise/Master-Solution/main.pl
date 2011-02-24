/*************************************************************************/
/*********** Stable Marriage Problem *************************************/
/*************************************************************************/
/**** Author: Claudia Ignat & Alexios Palinginis  ************************/
/**** Date: 6.12.2001 ****************************************************/
/*************************************************************************/

:- use_module(reading).
:- use_module(stable).
:- use_module(generate_table).


run:- 
      nl, write('******************* NAMES *************************'),nl,
      write('The list of names consists of names (only letters)'),nl,
      write('separated by spaces and terminated by newline'), nl,
      nl, write('Introduce the list of men names: '), 
      input_names(Men),
      length(Men, L),
      L > 0,
      write('Introduce the list of women names: '),
      input_names(Women),
      length(Women, L), !,      % the no. of men and women should be equal
                                % and be a positive no.

      nl, write('******************* CITIES ************************'),nl,
      write('The name of the city consists of one word'),nl,
      write('(only letters) followed by newline'),nl,nl,
      write('For each man input the city he lives in'), nl, 
      input_lives_in(Men), 
      write('For each woman input the city she lives in'), nl, 
      input_lives_in(Women), 

      nl, write('******************* INTERESTS *********************'),nl,
      write('The list of interests consists of a list of'), nl,
      write('at most 5 words (one word contains only letters),'),nl,
      write('the words being separated by spaces, and the list'),nl,
      write('being terminated by newline'),nl,nl,
      write('For each man input his interests'), nl,
      input_interests(Men),
      write('For each woman input her interests'), nl,
      input_interests(Women),

      nl, write('**************** LEVELS OF ATTRACTIVENESS **************'), nl,
      write('For each man indicate the level of attractiveness for the specified woman'), nl,
      write('(a number in the range 1-10, 10 = most attractive followed by .)'),nl, 
      input_preferences(Men, Women), nl,
      write('For each woman indicate the level of attractiveness for the specified man'), nl,
      write('(a number in the range 1-10, 10 = most attractive followed by .)'),nl, 
      input_preferences(Women, Men),

      stable(Men,Women,Marriages),    % run the algorithm for determining
                                      % a stable matching

      generate_table(Marriages).      % display the result in a table

run:-  nl, write('  The number of men and women should be equal'),nl,
       write('  and be a positive number'), nl,
       write('  Try again!'), nl, run.


