to_upper( Lower, Upper) :-
   atom_chars( Lower, Lowerlist),
   maplist( lower_upper, Lowerlist, Upperlist),
   atom_chars( Upper, Upperlist).

coordinates(Code, Name, Lat, Long) :-
  airport(Code, Name, degmin(LatD,LatM), degmin(LongD,LongM)),
  degreeToRad(LatD, LatM, Lat), degreeToRad(LongD, LongM, Long).

degreeToRad(Deg, Min, Rad) :-
  Dec is (Min/60),
  NewDeg is Deg + Dec,
  % write(Deg), write('.'),write(Dec), write('='), write(NewDeg), nl,
  Rad is (NewDeg * pi / 180).

distance(Src, Dst, Distance) :-
  coordinates(Src, _, SLat, SLong),
  coordinates(Dst, _, DLat, DLong),
  haversine_radians(SLat, SLong, DLat, DLong, Distance).

decToTime(Time, Hours, Mins) :-
  Hours is floor(Time), Mins is floor((Time - Hours) * 60).

timeToDec(Time, Hours, Mins) :-
  Time is Hours + (Mins / 60).

travelTime(Src, Dst, Time) :-
  distance(Src, Dst, Distance), Time is Distance / 500.

arrivalTime(Src, Dst, DepHr, DepMin, ArrivalHr, ArrivalMin) :-
  timeToDec( Dep, DepHr, DepMin),
  travelTime(Src, Dst, Time),
  Arrival is Dep + Time,
  decToTime(Arrival, ArrivalHr, ArrivalMin).

writePath([],_).
writePath([_],_).
writePath([H,N|T],Time) :- % write(H), write('->'), write(N),nl,
  showFlight(H,N,Time,Touchdown), writePath([N|T],Touchdown).

fly(Src, Src) :- write('You can\'t fly to yourself'),!,fail.
fly(Src, Dst) :- flight(Src,Dst,_), showFlight(Src,Dst,0,_),!.
fly(Src, Dst) :- flightpath(Src, Dst, Path), writePath(Path,0).
fly(Src, Dst) :-
  airport(Src, _,_,_),
  airport(Dst,_,_,_),
  format('There are no flights from %s to %s\n',[Src,Dst]),!,fail.
fly(Src,Dst) :-
  airport(Src, _,_,_),
  format('What is airport \'%s\'?\n',[Dst]),!,fail.
fly(Src,_) :- format('What is airport \'%s\'?\n',[Src]),!,fail.


showFlight(Src,Dst,Connect,Land) :-
  airport(Src, SName, _, _),
  airport(Dst, DName, _, _),
  flight(Src,Dst,time(Hrs,Mins)),
  Takeoff is Hrs + (Mins / 60),
  Takeoff > Connect + 0.5,
  to_upper(Src,S),to_upper(Dst,D),
  format('depart  %s  %s%.0f:%02.0f\n',[S,SName,Hrs,Mins]),
  arrivalTime(Src, Dst, Hrs, Mins, ArrHr, ArrMin),
  format('arrive  %s  %s%.0f:%02.0f\n',[D,DName,ArrHr,ArrMin]),
  timeToDec(Land, ArrHr, ArrMin).

not(X) :- X, !, fail.
not(_).


flightpath( Node, End, Outlist ) :-
  flightpath( Node, End, [Node], Outlist, 0 ).

flightpath( Node, Node, _, [Node], _ ).
flightpath( Node, End, Tried, [Node|List], Land ) :-
  flight( Node, Next, time(Hr,Min)), % Next is a connecting flight
  not( member( Next, Tried )), % Next has not yet been explored
  travelTime(Node,Next,AirTime),
  % make sure next flight time is >30min, .5hr after arrival.
  Takeoff is Hr + (Min / 60),
  Takeoff > Land + 0.5,
  Arr is (Takeoff + AirTime),
  flightpath( Next, End, [Next|Tried], List, Arr ).


% listpath( Node, End, Outlist ) :-
%    listpath( Node, End, [Node], Outlist ).
%
% listpath( Node, Node, _, [Node] ).
% listpath( Node, End, Tried, [Node|List] ) :-
%    % link( Node, Next ),
%    flight( Node, Next, _),
%    % flight( Node, Next, Time),
%    % write(Node), write('->'), write(Next),write(' at '), write(Time), nl,
%    not( member( Next, Tried )),
%    listpath( Next, End, [Next|Tried], List ).




% DeleteMe
