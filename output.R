adist
agrep
agrep
match
%in%
pmatch
charmatch
character
match
%in%
stringdist
stringdistmatrix
> stringdist('foo', c('fu','bar',NA))
[1] 2 3 NA
> stringdistmatrix(c('foo','bar'), c('fu','bar',NA))
[,1] [,2] [,3]
[1,] 2 3 NA
[2,] 3 0 NA
method
> stringdist('foo', 'bar', method='lv')
NA
NA
Inf
> stringdist('fu', 'foo', method='hamming')
[1] Inf
stringdist
stringdistmatrix
stringdistmatrix
> stringdistmatrix(c('foo','bar'), c('fu','bar',NA), ncores=3)
amatch
ain
amatch(x,table)
x
table
ain(x,table)
logical
x
table
amatch
ain
match
%in%
amatch
ain
match
> amatch('fu', c('foo','bar'), maxDist=2)
[1] 1
> ain('fu', c('foo','bar'), maxDist=2)
[1] TRUE
method
fu
foo
u
o
NA
> amatch(NA, c('foo',NA))
[1] 2
> amatch(NA, c('foo',NA), matchNA=FALSE)
[1] NA
> amatch(NA, c('foo',NA), matchNA=FALSE, nomatch=0)
[1] 0
file
read.table
fileEncoding
nchar
adist
utf8
integer
useBytes=TRUE
adist
> stringdist('Motorhead', 'Motörhead')
[1] 1
> stringdist('Motorhead', enc2utf8('Motörhead'), useBytes=TRUE)
[1] 2
Motörhead
Motorhead
ö
Motöread
utf8
utf8
o
ö
utf8
ö
o
stringdist
adist
uconv
utf8
> stringdist(c('foo','fu'), 'bar', method='hamming')
[1] 3 Inf
> stringdist('leia', 'leela', method='lcs')
[1] 3
l e e l a
l e i a
l e e l a
l e i a
e
l
i
adist
> stringdist('leela', 'leia', method='lv')
[1] 2
> stringdist('leia', 'leela', method='lv', weight=c(1,0.1,1))
[1] 2
> stringdist('leia', 'leela', method='lv', weight=c(0.1,1,1))
[1] 1.1
> stringdist('leela', 'leia', method='lv', weight=c(1,0.1,1))
[1] 1.1
leela
leia
stringdist
stringdistmatrix
amatch
ain
> stringdist('ba','ab') + stringdist('ab','acb')
[1] 2
> stringdist('ba','acb')
[1] 3
b
a
c
method='dl' directive.
> stringdist('ba', 'acb', method='dl')
[1] 2
foo
fo
oo
> stringdist('leia', 'leela', method='jaccard', q=2)
[1] 0.8333333
> stringdist('leia', 'leela', method='qgram', q=1)
[1] 3
> stringdist('leia', 'leela', method='qgram', q=2)
[1] 5
> stringdist('leia', 'leela', method='qgram', q=5)
[1] Inf
leia
leela
i
leia
e
l
leela
leia
leela
stringdist returns Inf since the since one of the compared strings has less than 5 characters.
> stringdist('leia', 'leela', method='cosine', q=1)
[1] 0.1666667
qgrams
character
> qgrams(
+ x = c('foo','bar','bar'),
+ y = c('fu','bar'),
+ z = c('foobar'),
+ q = 2 )
fo oo fu ob ba ar
x 1 1 0 0 2 2
y 0 0 1 0 1 1
z 1 1 0 1 1 1
> stringdist('leia', 'leela', method='jw')
[1] 0.2166667
stringdist
method='jw'
> stringdist('leia', 'leela', method='jw', p=0.1)
[1] 0.1733333
http://CRAN.R-project.org/
package=RecordLinkage. R package version 0.4-1. [p111]
http://CRAN.R-project.
org/package=cba. R package version 0.2.12. [p111]
http://CRAN.R-project.org/package=sna
http://CRAN.R-project.
org/package=MiscPsycho. R package version 1.6. [p111]
http://CRAN.R-project.org/package=Rankcluster. R package version 0.90.3. [p112]
http://www.jstatsoft.org/v52/i06. [p111]
http://CRAN.R-
project.org/package=vwr. R package version 0.3.0. [p111]
http://CRAN.R-project.org/
package=MKmisc. R package version 0.94. [p111]
https://github.com/ugexe/Text--
Levenshtein--Damerau--XS/blob/master/damerau-int.c. Last accessed 2014-03-04. [p117]
http://CRAN.R-project.
org/package=Phangorn. R package version 1.99-1. [p112]
https://github.com/markvanderloo/deducorrect
http://www.markvanderloo.eu
mark.vanderloo@gmail.com
