use strict;
use v5.32;

open INPUT, '<', 'inputs/1.1';

my @numbers;

while (<INPUT>) {
  chomp $_;
  push @numbers, $_;
}

for my $a (@numbers) {
  for my $b (@numbers) {
    for my $c (@numbers) {
      if ($a + $b + $c == 2020) {
        say "$a + $b + $c = 2020; a * b = @{[$a * $b * $c]}";
      }
    }
  }
}
