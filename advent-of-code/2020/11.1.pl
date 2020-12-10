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
      my @neighbours = neighbours( $i, $j, $height, $width );
      my $seat = $plan->[ $i ][ $j ];
      my $taken = 0;

      for ( @neighbours ) {
        $taken++ if $plan->[ $_->[0] ][ $_->[1] ] eq '#';
      }

      if ( $seat eq 'L' and $taken == 0 ) {
        $seat = '#';
      } elsif ( $seat eq '#' and $taken >= 4 ) {
        $seat = 'L';
      }

      $result->[ $i ][ $j ] = $seat;
    }
  }

  $result;
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

