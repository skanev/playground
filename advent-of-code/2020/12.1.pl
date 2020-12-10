use v5.32;
use warnings;
use experimental qw(smartmatch switch);

open INPUT, '<', 'inputs/12';

my @pos = ( 0, 0 );
my @dir = ( 0, 1 );

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
  /^([NSEWLRF])(\d+)$/;

  my ( $cmd, $d ) = ( $1, $2 );
  my ( $x, $y, $a, $b ) = ( @pos, @dir );

  given ( $cmd ) {
    when('F') { @pos = ( $x + $a * $d, $y + $b * $d ) }
    when('L') { @dir = rotate(-$d, $a, $b) }
    when('R') { @dir = rotate($d, $a, $b) }
    when('N') { @pos = ( $x - $d, $y ) }
    when('S') { @pos = ( $x + $d, $y ) }
    when('W') { @pos = ( $x, $y - $d ) }
    when('E') { @pos = ( $x, $y + $d ) }
  }

}

say abs($pos[0]) + abs($pos[1]);

__END__
F10
N3
F7
R90
F11
