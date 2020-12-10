use v5.32;
use warnings;

use Math::Utils qw( gcd );

open INPUT, '<', 'inputs/13';
my $contents = do { local $/; <INPUT> };

my ( $time, $schedule ) = split "\n", $contents;

my @ids = split ',', $schedule;

my $start = shift @ids;
my $offset = 0;
my $step = $start;

for my $id ( @ids ) {
  $offset += 1;
  next if $id eq 'x';
  $start += $step until ( $start + $offset ) % $id == 0 and $start >= $id ;
  $step = ($step * $id) / gcd( $step, $id );
}

say $start;
