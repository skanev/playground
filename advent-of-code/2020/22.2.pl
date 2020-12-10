use v5.32;
use warnings;
use List::MoreUtils qw( zip );

open STDIN, '<', 'inputs/22';

$_ = do { local $/; <> };
m/^Player 1:\n(.*)\n\nPlayer 2:\n(.*)\n$/sm;

my @a = split "\n", $1;
my @b = split "\n", $2;

sub round {
  my @a = $_[0]->@*;
  my @b = $_[1]->@*;

  my %seen;

  while ( @a and @b ) {
    my $signature = join( ' ', @a ) . ' | ' . join( ' ', @b );
    return { winner => 1, a => \@a, b => \@b } if $seen{ $signature };
    $seen{ $signature } = 1;

    my ( $a, $b ) = ( shift @a, shift @b );

    my $a_wins;

    if ( $a <= @a and $b <= @b ) {
      my $result = round( [ @a[ 0 .. $a - 1] ], [ @b[ 0 .. $b - 1 ] ] );
      $a_wins = ($result->{winner} == 1);
    } elsif ( $a > $b ) {
      $a_wins = 1;
    } else {
      $a_wins = 0;
    }

    if ( $a_wins ) {
      push @a, $a, $b;
    } else {
      push @b, $b, $a;
    }
  }

  return {
    winner => ( @a ) ? 1 : 2,
    a => [ @a ],
    b => [ @b ],
  }
}

my $result = round( [ @a ], [ @b ] );

my $i = $result->{a}->@* + $result->{b}->@*;
my $sum = 0;

$sum += $_ * $i-- for ( $result->{a}->@*,$result->{b}->@* );

say $sum;
