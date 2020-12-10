use v5.32;
use warnings;
use Data::Dump qw(dump);

my %bags;

open INPUT, '<', 'inputs/7' or die;

while (<INPUT>) {
  /^(.*)? bags contain (.*)\.$/ or die;
  my $color = $1;

  if ( $2 eq 'no other bags' ) {
    $bags{ $color } = [];
    next;
  }

  my @options = split ', ', $2;

  for ( @options ) {
    /^(\d+) (.*?) bags?$/ or die;
    my ( $count, $inner_color ) = ( $1, $2 );
    push @{ $bags{$color} }, $inner_color;
  }
}

sub can_contain {
  my ( $outer, $inner ) = @_;

  my @stack = ( $outer );

  while ( @stack ) {
    my $key = shift @stack;
    return 1 if ( $key eq $inner );
    push @stack, $bags{$key}->@*;
  }

  0;
}

my $count;

for ( keys %bags ) {
  next if $_ eq 'shiny gold';
  $count++ if can_contain( $_, 'shiny gold' );
}

say $count;
