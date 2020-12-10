use v5.32;
use warnings;
use experimental 'switch';

open INPUT, '<', 'inputs/19';
my $contents = do { local $/; <INPUT> };
my @parts = split "\n\n", $contents;

my %definitions;

for ( split "\n", $parts[0] ) {
  m/^(\d+): (.*)$/;
  $definitions{ $1 } = $2;
}

my %compiled;

sub compile {
  my $n = shift;
  return $compiled{ $n } if exists $compiled{ $n };

  my $compiled;
  given ( $definitions{ $n } ) {
    when(/^ " (\w) " $/x) {
      $compiled = $1
    }
    when(/^ (\d+) ( \s+ ( \d+ | \| ) )* $/x) {
      $compiled = '(' . ( join '|', map { s/(\d+)\s*/compile( $1 )/eg; $_ } split /\s*\|\s*/ ) . ')';
    }
    default {
      die "can't compile: $_",
    }
  }

  $compiled{ $n } = $compiled;
}

my $pattern = compile( 0 );
my $regex = qr/^$pattern$/;

my $count = 0;

for ( split "\n", $parts[1] ) {
  $count++ if $_ =~ $regex;
}

say $count;
