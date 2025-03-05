% 1.1. ------------- Project of Logica para Programacao 2022 ----------------- %

% Project of Logica para Programacao 2022
% Name: Tiago de Sousa Branquinho
% Number: 106635

% ------------------------------------------------------------------------------

% 2.1. ------------------------ Project Initialization ----------------------- %

:- set_prolog_flag(answer_write_options,[max_depth(0)]). % for complete lists.
:- ["dados.pl"], ["keywords.pl"]. % files to import.

% ------------------------------------------------------------------------------

% 3.0. -------------------------- Helper Predicates -------------------------- %

/* Helper predicates for the project to make the code more universal,
   if someone changes the database it should still work.                      */

/* getSemester/2 receives a term and returns the semester it belongs to.      */

getSemester(Term, Semester) :- 
    ((Term == p1; Term == p2; Term == p1_2), Semester = p1_2);
    ((Term == p3; Term == p4; Term == p3_4), Semester = p3_4).

/* getAllYears/1 returns a list with all the years in the database.           */

getAllYears(All_Years):- 
    setof(Year, 
          X^Y^Z^turno(X,Y,Year,Z), 
          All_Years).

/* getAllTerms/1 returns a list with all the terms in the database.           */

getAllTerms(All_Terms) :- 
    setof(Term, 
          M^N^X^Y^Z^(horario(M, N, X, Y, Z, Term), 
          Term \== p1_2, Term \== p3_4, Term \== indeterminado), 
          All_Terms).

/* getAllRoomTypes/1 returns a list with all the room types in the database.  */

getAllRoomTypes(Rooms) :-
    findall(X, 
            salas(X,_), 
            Rooms).

/* getAllDays/1 returns a list with all the days in the database.             */

getAllDays(All_Days) :-
    setof(Day, 
            M^N^X^Y^Z^horario(M, Day, N, X, Y, Z),
            All_Days).

% 3.1. -------------------------- Data Quality ------------------------------- %

/* eventosSemSalas/1 searches for all the events in the database,
   that don't have a room assigned.                                           */

eventosSemSalas(Events) :-
    findall(X, 
            evento(X,_,_,_, semSala), 
            Events).

% ------------------------------------------------------------------------------

/* eventosSemSalasDiaSemana/2 searches for all the events in the database,
   that don't have a room assigned and are in a specific day of the week.     */

eventosSemSalasDiaSemana(Day, Events) :-
    eventosSemSalas(Events_NoRoom),
    findall(X, 
            (horario(X, Day,_,_,_,_), 
            member(X, Events_NoRoom)), 
            Events).

% ------------------------------------------------------------------------------

/* eventosSemSalasPeriodo/2 searches for all the events in the database,
   that don't have a room assigned and are in specific terms or semesters,
   it does so by running through all terms in a list and calling checkTerms
   for each one of them, returning events aforementioned.                     */

eventosSemSalasPeriodo(Terms, Events) :-
    maplist(checkTerms, Terms, All_Events),
    append(All_Events, Events_Together),
    sort(Events_Together,Events).

/* checkTerms/2 is an auxiliary predicate for eventosSemSalasPeriodo.         */

checkTerms(Term, Events) :-
    getSemester(Term, Semester),
    eventosSemSalas(Events_NoRoom),
    findall(X, 
            ((horario(X,_,_,_,_, Term); horario(X,_,_,_,_, Semester)), 
            member(X, Events_NoRoom)), 
            Events).

% ------------------------------------------------------------------------------

/* organizaEventos/3 searches for all the events in the database,
   that are in a specific term or semester, and using iteration,
   it organizes them in a list, in numerical ascending order.                 */

organizaEventos(Events, Term, Result) :-
    getSemester(Term, Semester),
    organizaEventosAux(Events, Term, Semester, Result_Not_Sorted),
    sort(Result_Not_Sorted, Result).

/* organizaEventosAux/4 is an auxiliary predicate for organizaEventos.        */

organizaEventosAux([],_,_,[]) :- !.

organizaEventosAux([Single_Event|Rest], Term, Semester, [Single_Event|R]) :-
    (horario(Single_Event,_,_,_,_, Term);
    horario(Single_Event,_,_,_,_, Semester)), !,
    organizaEventosAux(Rest, Term, Semester, R).

% If the event is not in the given term or semester, it's not added to the list.
organizaEventosAux([_|Rest], Term, Semester, Rest_Events) :-
    organizaEventosAux(Rest, Term, Semester, Rest_Events).

% ------------------------------------------------------------------------------

% 3.2. ----------------------- Simple Data Analysis -------------------------- %

/* eventosMenoresQue/2 searches for all the events in the database,
   that have a duration smaller than the one given.                           */

eventosMenoresQue(Duration, Events) :-
    findall(X, 
            (horario(X,_,_,_,Y,_), Y =< Duration),
            Events).

% ------------------------------------------------------------------------------

/* eventosMenoresqueBool/2 calls eventosMenoresQue in order to get a 
   list of events with a duration smaller than the one given, 
   and returns true if the given ID is in the list of events.                 */

eventosMenoresQueBool(ID, Duration) :-
    eventosMenoresQue(Duration, Events),
    member(ID, Events).

% ------------------------------------------------------------------------------

/* procuraDisciplinas/2 searches for all the subjects in the database,
   that are lectured in a specific degree, it does so by running through
   all the events in the database from a given degree,
   calling getSubject for each one of them to get the subject adjacent to it,
   and then it sorts the list of subjects to get rid of duplicates.           */

procuraDisciplinas(Degree, Subjects) :-
    findall(X, 
            (turno(X, Degree, _, _)), 
            Degree_Events),
    maplist(getSubject, Degree_Events, Subjects_Not_Sorted),
    sort(Subjects_Not_Sorted, Subjects).

/* getSubject/2 is an auxiliary predicate for procuraDisciplinas.             */

getSubject(Event, Subject) :-
    evento(Event, Subject, _, _, _).

% ------------------------------------------------------------------------------

/* organizaDisciplinas/3 receives a list of subjects and a degree,
   and it organizes the subjects in two lists, one for the first semester
   and one for the second semester, it does so by running through all subjects
   in the list and calling getSemester to check what semester they are in,
   in order to put them in the correct list.                                  */

organizaDisciplinas([],_,[[],[]]) :- !.

% If the subject is in the first semester, it's added to the first list.
organizaDisciplinas([Subject|Rest], Degree, [[Subject|R], Semester2nd]) :-
    evento(X,Subject,_,_,_),
    horario(X,_,_,_,_,Term),
    turno(X,Degree,_,_),
    getSemester(Term, p1_2), !,
    % Using cut, it stops the search for the second semester.
    organizaDisciplinas(Rest,Degree,[R,Semester2nd]).

% If the subject is in the second semester, it's added to the second list.
organizaDisciplinas([Subject|Rest], Degree, [Semester1st, [Subject|R]]) :-
    evento(X,Subject,_,_,_),
    horario(X,_,_,_,_,Term),
    turno(X,Degree,_,_),
    getSemester(Term, p3_4),!,
    % Using cut just to be more aesthetically pleasing and efficient.
    organizaDisciplinas(Rest, Degree, [Semester1st, R]).

% ------------------------------------------------------------------------------

/* horasCurso/4 receives a term, a degree and a year
   and it returns the summed duration of all the events in those conditions.  */

horasCurso(Term, Degree, Year, Total_Hours) :-
    getSemester(Term, Semester),
    findall(X, 
            turno(X,Degree,Year,_), 
            Events),
    sort(Events, Events_Sorted),
    findall(Time, 
            ((horario(Y,_,_,_,Time, Term); horario(Y,_,_,_,Time, Semester)),
            member(Y, Events_Sorted)), 
            List_Of_Hours),
    sumlist(List_Of_Hours, Total_Hours).

% ------------------------------------------------------------------------------

/* evolucaoHorasCurso/2 receives a degree and returns a list of tuples
   each tuple containing the year, the term and the total duration of the degree
   in that year and term.                                                     */

evolucaoHorasCurso(Degree, Evolution_Time) :-
    getAllYears(All_Years),
    getEvolutionPerYear(Degree, All_Years, Evolution_Time),!.

/* getEvolutionPerYear/3 is an auxiliary predicate for evolucaoHorasCurso,
   that runs through all years in the database and calls getEvolutionPerTerm,
   to get the total duration of the degree in that year and term.             */

getEvolutionPerYear(_,[],[]) :- !.
getEvolutionPerYear(Degree, [Y|Rest], Evolution_Time) :-
    getAllTerms(All_Terms),
    getEvolutionPerTerm(Degree, Y, All_Terms, Lst),
    getEvolutionPerYear(Degree, Rest, R),
    append(Lst,R,Evolution_Time).

/* getEvolutionPerTerm/4 is an auxiliary predicate for getEvolutionPerYear,
    that runs through all terms in the database and calls horasCurso,
    to get the total duration of the degree in that year and term.            */

getEvolutionPerTerm(_,_, [], []) :- !.
getEvolutionPerTerm(Degree, Year, [P|Rest],[(Year, P, Total_Time)|R]) :-
    horasCurso(P, Degree, Year, Total_Time),
    getEvolutionPerTerm(Degree, Year, Rest, R).

% ------------------------------------------------------------------------------

% 3.3. -------------------  Rooms' Critical Occupation ----------------------- %

/* ocupaSlot/5 receives a start time, an end time, an event start time, 
   an event end time, and it returns the time that the event occupies
   in the given slot.                                                         */

ocupaSlot(Start_Time, End_Time, Event_Start_Time, Event_End_Time, Time) :-
    Overlapped_Start is max(Start_Time, Event_Start_Time),
    Overlapped_End is min(End_Time, Event_End_Time),
    Time is Overlapped_End-Overlapped_Start,
    Time >= 0.

% ------------------------------------------------------------------------------

/* numHorasOcupadas/6 receives a term, a room type, a day, 
   a start time and an end time, and it returns the total duration of all events
   in that term that occupy the given slot in the given room type;
   it does so by running through all the events in the given term,
   and checking if they are in the given room type, and calling getDuration,
   to get the summed duration of all events in the given slot.                */

numHorasOcupadas(Term, Room_Type, Day, Start_Time, End_Time, Duration) :-
    getSemester(Term, Semester),
    salas(Room_Type, Rooms),
    findall(ID, 
            (evento(ID, _, _, _, Room), 
            (horario(ID, Day,_,_,_, Term); horario(ID, Day,_,_,_, Semester)), 
            member(Room, Rooms)), 
            Events_per_Room),
    getDuration(Events_per_Room, Start_Time, End_Time, Duration).

/* getDuration/4 is an auxiliary predicate for numHorasOcupadas,
    that runs through all events in the given slot and
    gets the summed duration of all events in the given slot.                 */

getDuration([], _, _, 0) :- !.

getDuration([Event|Rest], Start_Time, End_Time, Duration) :-
    horario(Event, _, Event_Start_Time, Event_End_Time, _, _),
    ocupaSlot(Start_Time, End_Time, Event_Start_Time, Event_End_Time, Time), !,
    getDuration(Rest, Start_Time, End_Time, Rest_Duration),
    Duration is Time + Rest_Duration.

% Case for when the event doesn't occupy the given slot.
getDuration([_|Rest], Start_Time, End_Time, Duration) :-
    getDuration(Rest, Start_Time, End_Time, Duration).

% ------------------------------------------------------------------------------

/* ocupacaoMax/4 receives a room type, a start time, an end time, and it returns
   the max duration that fill the given slot by all rooms of the given type.  */

ocupacaoMax(Room_Type, Start_Time, End_Time, Max) :-
    salas(Room_Type, Rooms),
    length(Rooms, N),
    Max is (End_Time-Start_Time) * N.

% ------------------------------------------------------------------------------

/* percentagem/3 simply returns the percentage of the first argument
   in relation to the second argument (pretty self explanatory).              */

percentagem(Duration, Max, Percentage) :-
    Percentage is (Duration/Max) * 100.

% ------------------------------------------------------------------------------

/* ocupacaoCritica/4 receives a start time, an end time, a threshold, 
   and it returns a list of tuples, each tuple containing the day,
   the room type and the percentage of occupation of the given slot,
   in the given room type, in the given day, in the given term,
   that is above the given threshold.                                         */

% ocupacaoCritica/4 is a predicate that works by
% calling a bunch of predicates to get all the room types, all the days,
% all the terms, and then it finds all the tuples that satisfy the conditions,
% sorts the list of tuples and returns it.

ocupacaoCritica(Start_Time, End_Time, Threshold, Result) :-
    getAllRoomTypes(Rooms),
    getAllDays(All_Days),
    getAllTerms(All_Terms),
    findall(casosCriticos(Day, Room_Type, Percentage),
            (member(Term, All_Terms), member(Day, All_Days), 
            member(Room_Type, Rooms), 
            ocupacaoMax(Room_Type, Start_Time, End_Time, Max), 
            numHorasOcupadas(Term, Room_Type, Day, Start_Time, End_Time, Sum), 
            percentagem(Sum, Max, Per), 
            (ceiling(Per, Percentage), Percentage > Threshold)), 
            Result_Not_Sorted),
    sort(Result_Not_Sorted, Result).

% ------------------------------------------------------------------------------

% 3.4. ------------- And now for something completely different... ----------- %

/* ocupacaoMesa/3 is a predicate that solves a logic puzzle,
   based on Einstein Logic Puzzle. In this case, we have a table with 8 seats,
   and we have a list of people that we want to put in the table, we also have
   a list of restrictions that we have to respect, and we want to find a
   way to sit all of them in a way that respects all the restrictions.        */

/* ocupacaoMesa/3 is a predicate that works by
   calling a permutation predicate to get all the possible permutations of the
   list of people, and then it calls ocupacaoMesaAux to check if the
   permutation satisfies all the restrictions, and if it does, it adds it to
   a list of lists(first list: people at top of the table, 
                   second list: people at the head(left and right) of the table,
                   third list: people at the bottom of the table,
                   all when viewed from above).                               */

ocupacaoMesa(People, Restrictions, Table) :-
    permutation(People, People_Permuted),
    ocupacaoMesaAux(People_Permuted, Restrictions, Table).

/* ocupacaoMesaAux/3 is an auxiliary predicate for ocupacaoMesa,
   that receives a list of people, a list of restrictions, and a table,
   and it checks if the list of people satisfies all the restrictions,
   and if it does, it adds it to the table, as aforementioned.                */

ocupacaoMesaAux(_, [],_) :- !.

ocupacaoMesaAux(People_Permuted, [Predicate|Rest], Table) :-
    % as the restrictions are predicates, we can use call to call them
    call(Predicate, People_Permuted),
    ocupacaoMesaAux(People_Permuted, Rest, Table),
    addToList(People_Permuted, Table).

/* cab1/2 is a predicate that receives a person and a list of people, 
   and it checks if the person is the fourth person in the list
   (left of the table).                                                       */

cab1(X, List) :-
    nth1(4, List, X).

/* cab2/2 is a predicate that receives a person and a list of people, 
   and it checks if the person is the fifth person in the list
   (right of the table).                                                      */

cab2(X, List) :-
    nth1(5, List, X).

/* honra/3 is a predicate that receives two people and a list of people,
   and it checks if the first is at the head of the table(cab1/2 or cab2/2),
   and if so, checks if the second is right beside the first, on it's right.  */

honra(X, Y, List) :-
    % Case 1: X is at the head of the table on the left.
    cab1(X, List),
    nth1(Pos2, List, Y),
    Pos2 == 6.

honra(X, Y, List) :-
    % Case 2: X is at the head of the table on the right.
    cab2(X, List),
    nth1(Pos2, List, Y),
    Pos2 == 3.

/* lado/3 is a predicate that receives two people and a list of people,
   and it checks if they are side by side on the table, 
   (quicknote: if either is
   at the head of the table, it will return false, as it is considered that not
   one single person is side by side with them).                              */

lado(X,Y,List) :-
    \+cab1(X, List), \+cab2(X, List), \+cab1(Y, List), \+cab2(Y, List),
    nth1(Pos1, List, X),
    nth1(Pos2, List, Y),
    Diff is Pos2 - Pos1,
    (Diff =:= 1; Diff =:= -1).

/* naolado/3 is a predicate that receives two people and a list of people,
   and it checks if they are not side by side on the table (negating lado/3). */

naoLado(X, Y, List) :- \+lado(X ,Y ,List).

/* frente/3 is a predicate that receives two people and a list of people,
   and it checks if they are in front of each other on the table, 
   (quicknote: if either is at the head of the table, it will return false, 
   as it is considered that not one single person is in front of them).       */

frente(X,Y,List) :- 
    \+cab1(X, List), \+cab2(X, List), \+cab1(Y, List), \+cab2(Y, List),
    nth1(Pos1, List, X),
    nth1(Pos2, List, Y),
    Diff is Pos2 - Pos1,
    (Diff =:= 5; Diff =:= -5).

/* naofrente/3 is a predicate that receives two people and a list of people,
   and it checks if they are not in front of each other on the table
   (negating frente/3).                                                       */

naoFrente(X, Y, List) :- \+frente(X, Y, List).

/* addToList/2 is a predicate that receives a list of people and a table,
   and it adds the list of people to the table, as aforementioned.            */

addToList([], [[], [], []]) :- !.

addToList([P|R], [[P|Rest], Lst2, Lst3]) :-
    % Case 1: it adds 3 people to the first list to fill it up
    length([P|R], Comp),
    Comp >= 6, !,
    addToList(R, [Rest,Lst2,Lst3]).

addToList([P|R], [Lst1, [P|Rest], Lst3]) :-
    % Case 2: it adds 2 people to the second list to fill it up
    length([P|R], Comp),
    Comp >= 4, !,
    addToList(R, [Lst1, Rest, Lst3]).

addToList([P|R], [Lst1, Lst2, [P|Rest]]) :-
    % Case 3: it adds 3 people to the third list to fill it up
    addToList(R, [Lst1, Lst2,Rest]).
% ------------------------------------------------------------------------------