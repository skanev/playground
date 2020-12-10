use v5.32;
use warnings;

use Data::Dump qw(dump);

open INPUT, '<', 'inputs/9';

my $preamble = 25;

my $consume = $preamble;
my @window;
my %counts;

sub is_sum_of_two {
  my ( $number, $counts ) = @_;

  for my $key ( %$counts ) {
    next unless $counts->{$key};

    my $difference = $number - $key;

    return 1 if $counts->{$difference} and ($difference != $key or $counts->{$difference} >= 2);
  }

  0
}

while (<INPUT>) {
  chomp;

  if ( $consume ) {
    push @window, $_;
    $counts{$_}++;
    $consume--;

    next;
  }

  unless (is_sum_of_two( $_, \%counts )) {
    say "invalid: $_";
    last;
  }

  my $removed = shift @window;
  $counts{$removed}--;

  push @window, $_;
  $counts{$_}++;
}
