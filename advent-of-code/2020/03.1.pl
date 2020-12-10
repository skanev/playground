use v5.32;
use warnings;

open INPUT, '<', 'inputs/3' or die;

my $input = [map { chomp; $_ } <INPUT>];
my $width = length($input->[0]);
my $height = @$input;

my @directions = (3, 1);

my ($x, $y) = (0, 0);
my $trees = 0;

while ($y < $height) {
  $trees++ if substr($input->[$y], $x, 1) eq '#';
  $y += $directions[1];
  $x += $directions[0];
  $x %= $width;
}

say $trees;
