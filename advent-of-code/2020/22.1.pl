use v5.32;
use warnings;
use List::MoreUtils qw( zip );

open STDIN, '<', 'inputs/22';

$_ = do { local $/; <> };
m/^Player 1:\n(.*)\n\nPlayer 2:\n(.*)\n$/sm;

my @a = split "\n", $1;
my @b = split "\n", $2;

while ( @a and @b ) {
  my ( $a, $b ) = ( shift @a, shift @b );

  if ( $a > $b ) {
    push @a, $a, $b;
  } else {
    push @b, $b, $a;
  }
}

my $i = @a + @b;
my $sum = 0;

$sum += $_ * $i-- for ( @a, @b );

say $sum;
