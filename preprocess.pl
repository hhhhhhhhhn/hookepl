:- module(preprocess, [splitIntoWords/3, kShingle/3, removeStopWords/3, preprocess/4]).

:- use_module(library(snowball)).

preprocess(Language, K, Chars, Preprocessed) :-
	splitIntoWords(Language, Chars, WordsWithStopWords),
	removeStopWords(Language, WordsWithStopWords, Words),
	maplist(snowball(Language), Words, Stemmed),
	kShingle(K, Stemmed, Preprocessed).

wordCharacters(english, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).
stopWords(english, [[i,s],[t,h,e],[a],[t,h,e,y]]).

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

removeStopWords(Language, Words, Result) :-
	exclude(isStopWord(Language), Words, Result).

isStopWord(Language, Word) :-
	stopWords(Language, LanguageStopWords),
	member(Word, LanguageStopWords).
