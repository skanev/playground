use v5.32;
use warnings;
use List::Util qw( max reduce );

open INPUT, '<', 'inputs/10';

my @numbers;

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

my @count = (1);

for my $i ( @numbers ) {
  my $possible = reduce { ($a // 0) + ($b // 0) } @count[max($i - 3, 0) .. $i - 1];
  $count[$i] = $possible;
}

say $count[max(@numbers)];
