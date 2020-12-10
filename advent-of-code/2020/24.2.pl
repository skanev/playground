use v5.32;
use warnings;
use experimental 'switch';

use List::Util qw( uniq );
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

sub neighbours {
  my ( $x, $y ) = $_[0]->@*;

  my $offset = $x % 2 || -1;

  (
    [ $x - 1, $y ], [ $x - 1, $y + $offset ],
    [ $x, $y - 1 ], [ $x, $y + 1 ],
    [ $x + 1, $y ], [ $x + 1, $y + $offset ],
  )
}

my %grid;
tie %grid, 'Hash::MultiKey';

while (<>) {
  chomp;
  my $c = [ coordinates( $_ ) ];
  $grid{ $c } = ! ( $grid{ $c } // 0 );
}

for ( 1 .. 100 ) {
  my %new;
  tie %new, 'Hash::MultiKey';

  my @check =
    map { [ split ',' ] }
    uniq
    map { join ',', @$_ }
    map { ( $_, neighbours( $_ ) ) }
    grep { $grid{ $_ } }
    keys %grid;

  for ( @check ) {
    my $black = $grid{ $_ };
    my $count = scalar grep { $grid{ $_ } } neighbours $_;

    $new{ $_ } = 1 if $count == 2 or $black and $count == 1;
  }

  %grid = %new;
}

say scalar grep { $_ } values( %grid );
