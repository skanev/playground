use v5.32;
use warnings;
use List::Util qw( reduce );
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
    push @{ $bags{$color} }, { count => $count, color => $inner_color };
  }
}


sub number_of_bags {
  my ( $color ) = @_;

  return 0 unless $bags{ $color }->@*;

  reduce { $a + $b }
    map { $_->{count} + $_->{count} * number_of_bags($_->{color}) }
    $bags{ $color }->@*;
}


say number_of_bags( 'shiny gold' );
