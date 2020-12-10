use v5.32;
use warnings;
use Hash::MultiKey;
use List::Util qw( sum uniq );

my %game;
tie %game, 'Hash::MultiKey';

my $x = 0;

open INPUT, '<', 'inputs/17';

while (<INPUT>) {
  my @chars = split '';

  for my $y ( 0 .. $#chars ) {
    if ( $chars[$y] eq '#' ) {
      $game{ [ $x, $y, 0] } = 1;
    }
  }

  $x++;
}

sub neighbours {
  my ( $x, $y, $z ) = $_[0]->@*;

  my @result;

  for my $dx ( -1 .. 1 ) {
    for my $dy ( -1 .. 1 ) {
      for my $dz ( -1 .. 1 ) {
        next unless $dx || $dy || $dz;
        push @result, [ $x + $dx, $y + $dy, $z + $dz ];
      }
    }
  }

  @result;
}

for ( 1 .. 6 ) {
  my %next;
  tie %next, 'Hash::MultiKey';

  my @space;

  while ( my ( $cube, $alive ) = each %game ) {
    next unless $alive;
    for my $neighbour ( neighbours( $cube ) ) {
      push @space, join(',', @$neighbour);
    }
  }

  @space = map { [ split ',' ] } uniq @space;

  for my $cube ( @space ) {
    my $count = sum( grep { defined $_ } @game{ neighbours( $cube ) } );
    my $alive = $game{ $cube } // 0;
    $next{ $cube } = 1 if  $count == 3 || ( $alive && $count == 2 ) ;
  }

  %game = %next;
}

say sum values( %game );
