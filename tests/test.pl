#!/usr/bin/perl

use strict ;
use YAML::PP;

{
  my $ypp = YAML::PP->new;
  my @l = <ARGV>;
  my $l = join('',@l) ;
  my @docs = $ypp->load_string($l) ;
  print $ypp->dump_string(@docs) ;
}
