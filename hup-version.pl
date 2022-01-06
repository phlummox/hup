#!/usr/bin/env perl

use strict;
use warnings;

my $stack_yaml;
if (scalar @ARGV == 1) {
  $stack_yaml=$ARGV[0];
} else {
  $stack_yaml="stack.yaml";
}

my $cmd="stack --stack-yaml=${stack_yaml} ls dependencies";
print STDERR "+running: '$cmd'\n";
my @lines=`$cmd`;
@lines = grep(/^hup/, @lines);

scalar @lines == 1 ||
  die "expected only 1 hup line";

my @bits=split(' ', $lines[0]);

scalar @bits == 2 ||
  die "expected 'hup' then version";

print "$bits[1]\n";


