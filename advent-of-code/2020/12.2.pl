use v5.32;
use warnings;
use experimental qw(smartmatch switch);

open INPUT, '<', 'inputs/12';

my @pos = ( 0, 0 );
my @wp = ( -1, 10 );

sub rotate {
  my ( $deg, $a, $b ) = @_;
  my @dir = $deg > 0 ? ( 1, -1 ) : ( -1, 1 );
  my $steps = abs($deg) / 90;

  for ( 1..$steps ) {
    ( $a, $b ) = ( $b * $dir[0], $a * $dir[1] );
  }

  ( $a, $b );
}

while (<INPUT>) {
  chomp;
  /^([NSEWLRF])(\d+)$/;

  my ( $cmd, $d ) = ( $1, $2 );
  my ( $x, $y, $a, $b ) = ( @pos, @wp );

  say "$_: [$x, $y] // [$a, $b]";

  given ( $cmd ) {
    when('F') { @pos = ( $x + $a * $d, $y + $b * $d ) }
    when('L') { @wp = rotate(-$d, $a, $b) }
    when('R') { @wp = rotate($d, $a, $b) }
    when('N') { @wp = ( $a - $d, $b ) }
    when('S') { @wp = ( $a + $d, $b ) }
    when('W') { @wp = ( $a, $b - $d ) }
    when('E') { @wp = ( $a, $b + $d ) }
  }

}

say abs($pos[0]) + abs($pos[1]);
