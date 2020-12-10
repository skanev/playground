use v5.32;
use warnings;
use List::Util qw( first min max reduce );
use List::MoreUtils qw( all );
use Hash::MultiKey;

use experimental 'switch';

open STDIN, '>&', DATA;
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

  my @images;
  push @images, [ @row ], [ hpose( @row ) ];
  @row = rotate( @row );
  push @images, [ @row ], [ hpose( @row ) ];
  @row = rotate( @row );
  push @images, [ @row ], [ hpose( @row ) ];
  @row = rotate( @row );
  push @images, [ @row ], [ hpose( @row ) ];

  push @tiles, {
    id => $id,
    image => [ map { [split ''] } @lines ],
    left => $left,
    right => $right,
    top => $top,
    bottom => $bottom,
    variants => [ map { [ map { bin( $_ ) } @$_ ] } @images ]
  };
}

$tiles[0]{pos} = [0, 0];
$tiles[0]{rotation} = $tiles[0]{variants}[0];
$tiles[0]{orientation} = 0;

my @unchecked = ( $tiles[0] );

sub orient {
  my ( $image, $index ) = @_;

  my $s = $#{$image->[0]};

  my $pick;

  given ($index) {
    when(0) { $pick = sub { my ( $a, $b ) = @_; $image->[$a][$b] } }
    when(1) { $pick = sub { my ( $a, $b ) = @_; $image->[$s - $a][$b] } }
    when(2) { $pick = sub { my ( $a, $b ) = @_; $image->[$s - $b][$a] } }
    when(3) { $pick = sub { my ( $a, $b ) = @_; $image->[$s - $b][$s - $a] } }
    when(4) { $pick = sub { my ( $a, $b ) = @_; $image->[$s - $a][$s - $b] } }
    when(5) { $pick = sub { my ( $a, $b ) = @_; $image->[$a][$s - $b] } }
    when(6) { $pick = sub { my ( $a, $b ) = @_; $image->[$b][$s - $a] } }
    when(7) { $pick = sub { my ( $a, $b ) = @_; $image->[$b][$a] } }
  }

  my @result;
  for my $x ( 0 .. $s ) {
    my @line;
    for my $y ( 0 .. $s ) {
      push @line, $pick->( $x, $y );
    }
    push @result, [ @line ];
  }

  \@result;
}

while ( @unchecked ) {
  my $tile = shift @unchecked;
  my ( $top, $right, $bottom, $left ) = $tile->{rotation}->@*;
  my ( $x, $y ) = $tile->{pos}->@*;

  for my $other ( @tiles ) {
    next if exists $other->{pos};

    for my $i ( 0..$#{$other->{variants}} ) {
      my $variant = $other->{variants}[$i];

      use Data::Dump qw(dump);

      if ( $top == $variant->[2] ) {
        $other->{pos} = [ $x - 1, $y ];
        $other->{rotation} = $variant;
        $other->{orientation} = $i;
        push @unchecked, $other;
      } elsif ( $right == $variant->[3] ) {
        $other->{pos} = [ $x, $y + 1 ];
        $other->{rotation} = $variant;
        $other->{orientation} = $i;
        push @unchecked, $other;
      } elsif ( $bottom == $variant->[0] ) {
        $other->{pos} = [ $x + 1, $y ];
        $other->{rotation} = $variant;
        $other->{orientation} = $i;
        push @unchecked, $other;
      } elsif ( $left == $variant->[1] ) {
        $other->{pos} = [ $x, $y - 1 ];
        $other->{rotation} = $variant;
        $other->{orientation} = $i;
        push @unchecked, $other;
      }
    }
  }
}

my $top = min map { $_->{pos}[0] } @tiles;
my $bottom = max map { $_->{pos}[0] } @tiles;
my $left = min map { $_->{pos}[1] } @tiles;
my $right = max map { $_->{pos}[1] } @tiles;

my %positions;
tie %positions, 'Hash::MultiKey';

for my $tile ( @tiles ) {
  $positions{ $tile->{pos} } = $tile;
}

my @complete;

for my $x ( $top .. $bottom ) {
  my @images;

  for my $y ( $left .. $right ) {
    my $tile = $positions{ [$x, $y] };
    my $image = orient( $tile->{image}, $tile->{orientation} );
    push @images, $image;
  }

  for my $i ( 1 .. 8 ) {
    my @line;

    for my $image ( @images ) {
      push @line, @{$image->[$i]}[1..8];
    }

    push @complete, [ @line ];
  }
}

my $pattern = <<END;
                  #
#    ##    ##    ###
 #  #  #  #  #  #
END

sub patternize {
  my @dots;
  my $pattern = shift;
  my @lines = split "\n", $pattern;

  for my $x ( 0..$#lines ) {
    my $line = $lines[$x];
    my @chars = split "", $line;
    for my $y ( 0..$#chars ) {
      my $char = $chars[$y];
      if ( $char eq '#' ) {
        push @dots, [$x, $y];
      }
    }
  }

  return @dots;
}

my @dots = patternize( $pattern );

my $size = $#complete;

for my $orientation ( 0 .. 7 ) {
  my $h = max map { $_->[0] } @dots;
  my $w = max map { $_->[1] } @dots;

  my $image = orient( \@complete, $orientation );

  my $found = 0;

  for my $x ( 0 .. $size - $h ) {
    for my $y ( 0 .. $size - $w ) {
      if ( all { $image->[$x + $_->[0]][$y + $_->[1]] eq '#' } @dots ) {
        $found = 1;

        for ( @dots ) {
          $image->[$x + $_->[0]][$y + $_->[1]] = '.';
        }
      }
    }
  }

  if ( $found ) {
    say scalar grep { $_ eq '#' } map { @$_ } @$image;
    last;
  }
}
