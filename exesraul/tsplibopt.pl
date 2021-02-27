#!/usr/bin/perl

$instname = shift @ARGV;

#print "$instfile\n";

open(INST,"gzip -dc $instname".".tsp.gz |");

while ($_=<INST>)
{
   last if (/^\s*\d+\s+(\d+)\s+(\d+)/);
}

/\s*\d+\s+(\d+)\s+(\d+)/;
push(@inst,"($1,$2)");
while (<INST>) 
{
 last if (/EOF/);
 /\s*\d+\s+(\d+)\s+(\d+)/;
 push(@inst,"($1,$2)");
} 


#foreach $i (@inst){
#	print "$i\n";
#}


open(TOUR,"gzip -dc $instname".".opt.tour.gz |");

while ($_=<TOUR>)
{
   last if (/^\s*\d+/);
}

/\s*(\d+)/;
push(@tour,$1);
while (<TOUR>) 
{
 last if (/EOF/);
/\s*(\d+)/;
push(@tour,$1);
} 


pop @tour;

foreach $t (@tour){
	print "$inst[$t-1]\n";
}


