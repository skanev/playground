use v5.32;
use warnings;

open INPUT, '<', 'inputs/2' or die;

my $valid = 0;

while (<INPUT>) {
  /^(\d+)-(\d+) (\w): (.*)$/ or die("Unmatched input: $_");
  my ($low, $high, $char, $password) = ($1, $2, $3, $4);
  my $count = () = ($password =~ /$char/g);
  $valid++ if ($low <= $count && $count <= $high);
}

say $valid;
