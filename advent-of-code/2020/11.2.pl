use v5.32;
use warnings;

open INPUT, '<', 'inputs/11';

my $plan;

while ( <INPUT> ) {
  chomp;
  push @$plan, [ split '' ];
}

sub neighbours {
  my ( $x, $y, $h, $w ) = @_;

  my @result;

  for my $a ( $x - 1 .. $x + 1 ) {
    for my $b ( $y - 1 .. $y + 1 ) {
      push @result, [ $a, $b ] if ( 0 <= $a and $a < $h and 0 <= $b and $b < $w and ( $a != $x or $b != $y ) );
    }
  }

  @result;
}

sub iterate {
  my $plan = shift;
  my $height = @$plan;
  my $width = @{$plan->[0]};

  my $result = [];

  for my $i ( 0 .. $height - 1 ) {
    for my $j ( 0 .. $width - 1 ) {
      my $seat = $plan->[ $i ][ $j ];
      my $taken = visible_occupied( $plan, $i, $j );

      if ( $seat eq 'L' and $taken == 0 ) {
        $seat = '#';
      } elsif ( $seat eq '#' and $taken >= 5 ) {
        $seat = 'L';
      }

      $result->[ $i ][ $j ] = $seat;
    }
  }

  $result;
}

sub first_in_direction {
  my ( $plan, $x, $y, $direction ) = @_;
  my $height = @$plan;
  my $width = @{$plan->[0]};

  $x += $direction->[0];
  $y += $direction->[1];

  while ( 0 <= $x and $x < $height and 0 <= $y and $y < $width ) {
    my $seat = $plan->[ $x ][ $y ];
    if ( $seat ne '.' ) {
      return $seat;
    }

    $x += $direction->[0];
    $y += $direction->[1];
  }

  return '.';
}

sub visible_occupied {
  my ( $plan, $x, $y ) = @_;

  my @directions = ( [0, 1], [0, -1], [1, 0], [-1, 0], [1, 1], [-1, 1], [1, -1], [-1, -1] );
  my @seen = map { first_in_direction( $plan, $x, $y, $_ ) } @directions;
  my $count = 0;
  for ( @seen ) {
    $count++ if $_ eq '#';
  }

  $count;
}

for ( 0 .. 100000 ) {
  my $before = join "\n", map { join '', @$_ } @$plan;

  $plan = iterate( $plan );

  my $after = join "\n", map { join '', @$_ } @$plan;

  if ( $before eq $after ) {
    my $count = () = $after =~ /#/g;
    say $count;
    last;
  }
}
