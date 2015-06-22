-module(space_age).
-export([ageOn/2]).

ageOn(Planet, Seconds) ->
  earth_years(Seconds) / years_per_earth_year(Planet).

earth_years(Seconds) ->
  Seconds / 31557600.

years_per_earth_year(mercury) -> 0.2408467;
years_per_earth_year(venus) -> 0.61519726;
years_per_earth_year(earth) -> 1.0;
years_per_earth_year(mars) -> 1.8808158;
years_per_earth_year(jupiter) -> 11.862615;
years_per_earth_year(saturn) -> 29.447498;
years_per_earth_year(uranus) -> 84.016846;
years_per_earth_year(neptune) -> 164.79132.
