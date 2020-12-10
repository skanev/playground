use v5.32;
use warnings;
use List::Util qw( none );

open INPUT, '<', 'inputs/16';

my $contents = do { local $/; <INPUT> };
my @parts = split "\n\n", $contents;

my %rules;
my @ticket;
my @nearby;

for ( split "\n", $parts[0] ) {
  /^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$/ or die $_;

  my ( $name, $a, $b, $c, $d ) = ( $1, $2, $3, $4, $5 );

  $rules{$name} = sub { my $x = $_[0]; $a <= $x and $x <= $b or $c <= $x and $x <= $d };
}

@ticket = ( split ",", ( split("\n", $parts[1]) )[1] );

$parts[2] =~ s/^nearby tickets:\n//sg;

for ( split "\n", $parts[2] ) {
  push @nearby, [ split "," ];
}

my $sum;

for my $ticket ( @nearby ) {
  for my $number ( @$ticket ) {
    $sum += $number if none { $_->($number) } (values %rules);
  }
}

say $sum;
