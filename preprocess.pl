:- module(preprocess, [splitIntoWords/3, kShingle/3]).

wordCharacters(english, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

isWordCharacter(Language, Character) :-
	wordCharacters(Language, ValidCharacters),
	member(Character, ValidCharacters).

parseMatchingGoal(_, [], _, _) :- fail, !.
parseMatchingGoal(Goal, [InputHead|InputTail], Parsed, Rest) :- 
	call(Goal, InputHead),

	(parseMatchingGoal(Goal, InputTail, TailParsed, Rest) ->
		append([InputHead], TailParsed, Parsed)
		;
		Parsed = [InputHead],
		Rest = InputTail
	).

splitIntoWords(_, [], []) :- !.
splitIntoWords(Language, Chars, Words) :-
	(parseMatchingGoal(isWordCharacter(Language), Chars, HeadWord, Rest) ->
		splitIntoWords(Language, Rest, TailWords),
		append([HeadWord], TailWords, Words)
		;
		tail(Chars, CharsTail),
		splitIntoWords(Language, CharsTail, Words)
	).

tail([_|Tail], Tail).

kShingle(K, List, Shingles) :-
	length(List, N),
	ShingleAmount is N-K+1,
	range(0, ShingleAmount, ShingleIndexes),
	maplist(nthKShingle(K, List), ShingleIndexes, Shingles).

nthKShingle(K, List, N, Shingle) :-
	End is N + K,
	range(N, End, ListIndexes),
	maplist(elementAtIndex(List), ListIndexes, Shingle).

elementAtIndex(List, Index, Element) :- 
	nth0(Index, List, Element).

range(End, End, []) :- !.
range(Start, End, List) :-
	End > Start,
	NextStart is Start + 1,
	range(NextStart, End, TailList),
	append([Start], TailList, List).

