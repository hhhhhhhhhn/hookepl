:- module(hooke, [cluster/3]).
:- use_module(library(clpfd)).

cluster(Distance, Points, Clusters) :-
	cluster_([], Distance, Points, Clusters).

cluster_(ClustersSoFar, _, [], ClustersSoFar) :- !. % ! Nonempty points are matching for some reason
cluster_(ClustersSoFar, Distance, [PointsHead|PointsTail], Clusters) :-
	addToClusters(Distance, PointsHead, ClustersSoFar, ClustersWithPointsHead),
	cluster_(ClustersWithPointsHead, Distance, PointsTail, Clusters).

addToClusters(Distance, Point, Clusters, Result) :- 
	partition(withinDistance(Distance, Point), Clusters, WithinDistance, NotWithinDistance),
	append(WithinDistance, [[Point]], ClustersToBeJoined),
	append(ClustersToBeJoined, NewCluster),
	append(NotWithinDistance, [NewCluster], Result).

withinDistance(Distance, Point, Cluster) :-
	distanceFromCluster(Point, Cluster, ActualDistance),
	ActualDistance #=< Distance.

distanceFromCluster(Point, [ClusterHead], Result) :-
	distance(Point, ClusterHead, Result), !. % The ! prevents the other distanceFromCluster from being tested
distanceFromCluster(Point, [ClusterHead|ClusterTail], Result) :-
	distance(Point, ClusterHead, HeadResult),
	distanceFromCluster(Point, ClusterTail, TailResult),
	Result #= min(HeadResult, TailResult).

distance(point(AX, AY), point(BX, BY), Dist) :-
	Dist #= max(abs(AX - BX), abs(AY - BY)).
