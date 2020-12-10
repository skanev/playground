use v5.32;
use warnings;

open INPUT, '<', 'inputs/13';
my $contents = do { local $/; <INPUT> };

my ( $time, $schedule ) = split "\n", $contents;

my @ids = grep { $_ ne 'x' } split ',', $schedule;

my $min = 10_000_000_000;
my $answer = 0;

for my $id ( @ids ) {
  my $wait = ($id - $time % $id);
  if ( $wait < $min ) {
    $min = $wait;
    $answer = $wait * $id;
  }
}

say $answer;
__END__
939
7,13,x,x,59,x,31,19
