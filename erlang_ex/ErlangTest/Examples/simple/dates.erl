-module(dates).
-export([classify_day/1]).

classify_day(saturday) ->
	 weekEnd;

classify_day(sunday) ->
	weekEnd;

classify_day(_) ->
	weekDay.