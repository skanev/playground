use v5.32;
use warnings;
use List::Util qw( none );
use Set::Scalar;

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

sub is_valid {
  my $ticket = shift;

  for my $number ( @$ticket ) {
    return 0 if none { $_->($number) } (values %rules);
  }

  1;
}

my @valid = grep { is_valid($_) } @nearby;

my @possible = map { Set::Scalar->new( keys(%rules) ) } ( 1 .. %rules );

for my $ticket ( @valid ) {
  my @numbers = @$ticket;
  for my $i ( 0 .. $#numbers ) {
    while ( my ( $name, $fn ) = each %rules ) {
      if ( ! $fn->( $numbers[$i] ) ) {
        $possible[$i]->delete( $name );
      }
    }
  }
}

for ( 0 .. $#possible ) {
  my $remove = Set::Scalar->new( map { $_->elements } grep { $_->size == 1 } @possible );

  for my $set ( @possible ) {
    $set -= $remove if ( $set->size > 1 );
  }
}

my %positions;

for ( 0 .. $#possible ) {
  my $set = $possible[$_];
  die "collision" if $set->size != 1;
  $positions{ ($set->members)[0] } = $_;
}

my $result = 1;

while ( my ( $name, $pos ) = each %positions ) {
  next unless $name =~ /^departure /;
  $result *= $ticket[$pos];
}

say $result;
