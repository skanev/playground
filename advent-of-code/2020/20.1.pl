use v5.32;
use warnings;
use List::Util qw( first min max reduce );

open STDIN, '<', 'inputs/20';

my @tiles;

sub bin {
  $_ = shift;
  s/\./0/g;
  s/#/1/g;
  oct "0b$_";
}

sub rotate {
  my ( $t, $r, $b, $l ) = @_;

  return ( scalar reverse( $l ), $t, scalar reverse( $r ), $b );
}

sub hpose {
  my ( $t, $r, $b, $l ) = @_;

  return ( $b, scalar reverse( $r ), $t, scalar reverse( $l ) );
}

$/ x= 2;
while (<>) {
  m/Tile (\d+):/ or die $_;

  my $id = $1;
  my ( $first , @lines ) = split "\n";

  my ( $top, $bottom ) = ( $lines[0], $lines[-1] );
  my $left = join '', map { substr $_, 0, 1 } @lines;
  my $right = join '', map { substr $_, -1, 1 } @lines;

  my @row = ( $top, $right, $bottom, $left );

  my @variants;
  push @variants, [ @row ], [ hpose( @row ) ];
  @row = rotate( @row );
  push @variants, [ @row ], [ hpose( @row ) ];
  @row = rotate( @row );
  push @variants, [ @row ], [ hpose( @row ) ];
  @row = rotate( @row );
  push @variants, [ @row ], [ hpose( @row ) ];

  push @tiles, {
    id => $id,
    lines => [ @lines ],
    left => $left,
    right => $right,
    top => $top,
    bottom => $bottom,
    variants => [ map { [ map { bin( $_ ) } @$_ ] } @variants ]
  };
}

$tiles[0]{pos} = [0, 0];
$tiles[0]{rotation} = $tiles[0]{variants}[0];

my @unchecked = ( $tiles[0] );

while ( @unchecked ) {
  my $tile = shift @unchecked;
  my ( $top, $right, $bottom, $left ) = $tile->{rotation}->@*;
  my ( $x, $y ) = $tile->{pos}->@*;

  for my $other ( @tiles ) {
    next if exists $other->{pos};

    for my $variant ( $other->{variants}->@* ) {
      if ( $top == $variant->[2] ) {
        $other->{pos} = [ $x - 1, $y ];
        $other->{rotation} = $variant;
        push @unchecked, $other;
      } elsif ( $right == $variant->[3] ) {
        $other->{pos} = [ $x, $y + 1 ];
        $other->{rotation} = $variant;
        push @unchecked, $other;
      } elsif ( $bottom == $variant->[0] ) {
        $other->{pos} = [ $x + 1, $y ];
        $other->{rotation} = $variant;
        push @unchecked, $other;
      } elsif ( $left == $variant->[1] ) {
        $other->{pos} = [ $x, $y - 1 ];
        $other->{rotation} = $variant;
        push @unchecked, $other;
      }
    }
  }
}

my $top = min map { $_->{pos}[0] } @tiles;
my $bottom = max map { $_->{pos}[0] } @tiles;
my $left = min map { $_->{pos}[1] } @tiles;
my $right = max map { $_->{pos}[1] } @tiles;

my @corners = (
 (first { $_->{pos}[0] == $top && $_->{pos}[1] == $left } @tiles)->{id},
 (first { $_->{pos}[0] == $top && $_->{pos}[1] == $right } @tiles)->{id},
 (first { $_->{pos}[0] == $bottom && $_->{pos}[1] == $left } @tiles)->{id},
 (first { $_->{pos}[0] == $bottom && $_->{pos}[1] == $right } @tiles)->{id},
);

say reduce { $a * $b } @corners;
