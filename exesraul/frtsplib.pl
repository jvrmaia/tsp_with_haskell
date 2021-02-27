#!/usr/bin/perl

while (<>)
{
   last if (/^\s*\d+\s+(\d+)\s+(\d+)/);
}

do 
{
 print "$1 $2\n" if (/\s*\d+\s+(\d+)\s+(\d+)/) ;
 exit() if (/EOF/);
} while (<>);

  
