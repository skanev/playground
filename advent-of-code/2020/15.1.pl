use v5.32;
use warnings;

my @numbers = ( 15, 12, 0, 14, 3, 1 );

my %spoken;
my $next = shift @numbers;
my $turn = 1;

for my $number ( @numbers ) {
  $spoken{$next} = $turn++;
  $next = $number;
}

while ( $turn < 2020 ) {
  my $said;

  if ( $spoken{$next} ) {
    $said = $turn - $spoken{$next};
  } else {
    $said = 0;
  }

  $spoken{$next} = $turn;
  $turn++;
  $next = $said;
}

say $next;
