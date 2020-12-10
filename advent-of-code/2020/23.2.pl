use v5.32;
use warnings;

use List::Util qw( any );
use constant INPUT => '784235918';
use constant LIMIT => 1_000_000;
use constant MOVES => 10_000_000;

my ( $n, @numbers ) = split '', INPUT;
my @index;

my $first = { number => $n, next => undef };
@index[ $n ] = $first;
my $current = $first;

for ( @numbers ) {
  my $next = { number => $_, next => undef };
  @index[$_] = $next;
  $current->{next} = $next;
  $current = $next;
}


for ( my $i = 10; $i <= LIMIT; $i++ ) {
  my $next = { number => $i, next => undef };
  push @index, $next;
  $current->{next} = $next;
  $current = $next;
}

$current->{next} = $first;

$current = $first;

sub take {
  my $n = shift;
  my $first = $current->{next};
  my $last = $first;
  my @result;

  while ( $n-- ) {
    push @result, $last;
    $last = $last->{next};
  }

  $current->{next} = $last;
  $result[-1]->{next} = undef;

  @result;
}

sub destination {
  my @numbers = @_;

  my $n = $numbers[0];

  while ( any { $_ == $n } @numbers ) {
    $n--;
    $n = LIMIT if $n == 0;
  }

  $n;
}

sub insert {
  my ( $destination, @elements ) = @_;

  my $after = $destination->{next};
  $destination->{next} = $elements[0];
  $elements[-1]->{next} = $after;
}

for ( my $c = 1; $c <= MOVES; $c++ ) {
  my @pick = take 3;
  my $destination = destination $current->{number}, map { $_->{number} } @pick;
  insert $index[ $destination ], @pick;
  $current = $current->{next};
}

say $index[1]->{next}{number} * $index[1]->{next}{next}{number};
