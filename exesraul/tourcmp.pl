#!/usr/bin/perl

$inst = shift @ARGV;
$frtsplib = shift @ARGV;
$toureval = shift @ARGV;
$tourlen = shift @ARGV;
$tsplibopt = shift @ARGV;

$instrun = "gzip -dc $inst".".tsp.gz"."| $frtsplib | $toureval | $tourlen |";
open(INST,$instrun);
$len = <INST>;
chop $len;

#print "$len\n";

#print "->$tsplibopt\n";
$opttour = "$tsplibopt $inst"." | $tourlen |";

#print "$opttour\n";
open(OPTTOUR,$opttour);
$optlen = <OPTTOUR>;
chop $optlen;
$rate = $len/$optlen;

print  "$len \& $optlen \& $rate \\\\\n";


