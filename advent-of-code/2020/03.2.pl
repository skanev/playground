use v5.32;
use warnings;
use List::Util qw( reduce );

open INPUT, '<', 'inputs/3' or die;

sub collisions {
  my ( $input, $directions ) = @_;
  my $width = length($input->[0]);
  my $height = @$input;

  my ($x, $y) = (0, 0);
  my $trees = 0;

  while ( $y < $height ) {
    $trees++ if substr( $input->[$y], $x, 1 ) eq '#';
    $y += $directions->[1];
    $x += $directions->[0];
    $x %= $width;
  }

  $trees;
}

my $input = [map { chomp; $_ } <INPUT>];

my $directions = [
  [1, 1],
  [3, 1],
  [5, 1],
  [7, 1],
  [1, 2],
];

my @numbers;
for my $direction (@$directions) {
  my $trees = collisions( $input, $direction );
  say "right $direction->[0], left $direction->[1] = $trees";
  push @numbers, $trees;
}

say "product = " . reduce { $a * $b } @numbers;
