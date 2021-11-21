title "Napoleon's March Data";
* from Wilkinson, GOG web site (minard.txt), modified to create
  separate data sets;

/*
Cities data: longitude (deg. E), latitude (deg. N), city
NB: City spellings vary among sources (e.g., Wilna = Vilnius)
*/
data cities;
   input lonc  latc city $14;
datalines;
     24.0  55.0  Kowno
     25.3  54.7  Wilna
     26.4  54.4  Smorgoni
     26.8  54.3  Moiodexno
     27.7  55.2  Gloubokoe
     27.6  53.9  Minsk
     28.5  54.3  Studienska
     28.7  55.5  Polotzk
     29.2  54.4  Bobr
     30.2  55.3  Witebsk
     30.4  54.5  Orscha
     30.4  53.9  Mohilow
     32.0  54.8  Smolensk
     33.2  54.9  Dorogobouge
     34.3  55.2  Wixma
     34.4  55.5  Chjat
     36.0  55.5  Mojaisk
     37.6  55.8  Moscou
     36.6  55.3  Tarantino
     36.5  55.0  Malo-Jarosewii
;
/*
Temperature data: longitude, temperature (deg. F), date
*/
data temp;
 input lont   temp   month $ day date date.;
datalines; 
 37.6     0   Oct 18  18OCT1812
 36.0     0   Oct 24  24OCT1812
 33.2    -9   Nov 9   09NOV1812
 32.0   -21   Nov 14  14NOV1812
 29.2   -11   Nov 24  24NOV1812
 28.5   -20   Nov 28  28NOV1812
 27.2   -24   Dec 1   01DEC1812
 26.7   -30   Dec 6   06DEC1812
 25.3   -26   Dec 7   07DEC1812
;
/*
Army data: longitude, latitude, army-size, direction, path
  Why is the initial army size not listed here?
*/
data troops;
  input lonp  latp survivor  direction $ group;
datalines;
   24.0  54.9   340000        A     1
   24.5  55.0   340000        A     1
   25.5  54.5   340000        A     1
   26.0  54.7   320000        A     1
   27.0  54.8   300000        A     1
   28.0  54.9   280000        A     1
   28.5  55.0   240000        A     1
   29.0  55.1   210000        A     1
   30.0  55.2   180000        A     1
   30.3  55.3   175000        A     1
   32.0  54.8   145000        A     1
   33.2  54.9   140000        A     1
   34.4  55.5   127100        A     1
   35.5  55.4   100000        A     1
   36.0  55.5   100000        A     1
   37.6  55.8   100000        A     1
   37.7  55.7   100000        R     1
   37.5  55.7    98000        R     1
   37.0  55.0    97000        R     1
   36.8  55.0    96000        R     1
   35.4  55.3    87000        R     1
   34.3  55.2    55000        R     1
   33.3  54.8    37000        R     1
   32.0  54.6    24000        R     1
   30.4  54.4    20000        R     1
   29.2  54.3    20000        R     1
   28.5  54.2    20000        R     1
   28.3  54.3    20000        R     1
   27.5  54.5    20000        R     1
   26.8  54.3    12000        R     1
   26.4  54.4    14000        R     1
   25.0  54.4     8000        R     1
   24.4  54.4     4000        R     1
   24.2  54.4     4000        R     1
   24.1  54.4     4000        R     1
   24.0  55.1    60000        A     2
   24.5  55.2    60000        A     2
   25.5  54.7    60000        A     2
   26.6  55.7    40000        A     2
   27.4  55.6    33000        A     2
   28.7  55.5    33000        A     2
   28.7  55.5    33000        R     2
   29.2  54.2    30000        R     2
   28.5  54.1    30000        R     2
   28.3  54.2    28000        R     2
   24.0  55.2    22000        A     3
   24.5  55.3    22000        A     3
   24.6  55.8     6000        A     3
   24.6  55.8     6000        R     3
   24.2  54.4     6000        R     3
   24.1  54.4     6000        R     3
;
