use v5.32;
use warnings;

open INPUT, '<', 'inputs/2' or die;

my $valid = 0;

my $count = 0;
while (<INPUT>) {
  $count++;
  /^(\d+)-(\d+) (\w): (.*)$/ or die("Unmatched input: $_");
  my ($first, $second, $char, $password) = ($1, $2, $3, $4);
  $valid++ if substr($password, $first - 1, 1) eq $char xor substr($password, $second - 1, 1) eq $char;
}

say $valid;
