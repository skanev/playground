use v5.32;
use warnings;
use List::Util qw( max );

open INPUT, '<', 'inputs/10';

my @numbers = ( 0 );

while (<INPUT>) {
  chomp;
  push @numbers, $_;
}

@numbers = sort { $a <=> $b } @numbers;
push @numbers, max( @numbers ) + 3;

my %diffs;

for my $i ( 0 .. $#numbers - 1 ) {
  my $diff = $numbers[$i + 1] - $numbers[$i];
  $diffs{$diff}++
}

say $diffs{1} * $diffs{3}
