use v5.32;
use warnings;

use List::Util qw( min max );
use Data::Dump qw(dump);

open INPUT, '<', 'inputs/9';
my $preamble = 25;

sub is_sum_of_two {
  my ( $number, $counts ) = @_;

  for my $key ( %$counts ) {
    next unless $counts->{$key};

    my $difference = $number - $key;

    return 1 if $counts->{$difference} and ($difference != $key or $counts->{$difference} >= 2);
  }

  0
}

sub weakness {
  my ( $number, $numbers ) = @_;


  for my $i ( 0 .. $#$numbers ) {
    my $sum = 0;
    my $j;

    for ( $j = $i; $j <= $#$numbers; $j++ ) {
      $sum += $numbers->[$j];
      last if $sum >= $number;
    }

    next unless $sum == $number;

    my @subrange = @{$numbers}[ $i .. $j ];
    my $min = min @subrange;
    my $max = max @subrange;
    my $weakness = $min + $max;

    return $weakness;
  }
}

my $consume = $preamble;
my @numbers;
my @window;
my %counts;

while (<INPUT>) {
  chomp;

  push @numbers, $_;

  if ( $consume ) {
    push @window, $_;
    $counts{$_}++;
    $consume--;

    next;
  }

  unless ( is_sum_of_two( $_, \%counts ) ) {
    pop @numbers;
    my $weakness = weakness( $_, \@numbers );
    say "invalid: $_";
    say "weakness: $weakness";

    last;
  }

  my $removed = shift @window;
  $counts{$removed}--;

  push @window, $_;
  $counts{$_}++;
}
