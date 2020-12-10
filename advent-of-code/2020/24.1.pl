use v5.32;
use warnings;
use experimental 'switch';

use Hash::MultiKey;

open STDIN, '<', 'inputs/24';

sub coordinates {
  my ( $x, $y ) = 0, 0;

  while ( $_[0] =~ /([ns])?([we])/g ) {
    given ( ($1 // '') . $2 ) {
      when('e') { $y += 1 }
      when('w') { $y -= 1 }
      when('ne') { $x -= 1; $y += ( $x + 1 ) % 2 }
      when('nw') { $x -= 1; $y -= $x % 2 }
      when('se') { $x += 1; $y += ( $x + 1 ) % 2 }
      when('sw') { $x += 1; $y -= $x % 2 }
      default { die "unknown $_" }
    }
  }

  ( $x, $y )
}

my %grid;
tie %grid, 'Hash::MultiKey';

while (<>) {
  chomp;
  my $c = [ coordinates( $_ ) ];
  $grid{ $c } = ! ( $grid{ $c } // 0 );
}

say scalar grep { $_ } values( %grid );
