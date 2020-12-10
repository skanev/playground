use v5.32;
use warnings;
use Set::Scalar;
use List::Util qw( uniq );
use Array::Utils qw( array_diff );

open STDIN, '<', 'inputs/21';

my %possible;
my @foods;

while (<>) {
  m/^(.*) \(contains (.*)\)$/;

  my @ingredients = split ' ', $1;
  my @allergens = split ', ', $2;
  my $set = Set::Scalar->new( @ingredients );

  push @foods, { ingredients => [ @ingredients ], allergens => [ @allergens ] };

  for ( @allergens ) {
    $possible{ $_ } ||= $set;
    $possible{ $_ } = $possible{ $_ }->intersection( $set );
  }
}


for ( 0 .. scalar keys %possible ) {
  my @sets = values %possible;
  my $remove = Set::Scalar->new( map { $_->elements } grep { $_->size == 1 } @sets );

  for ( keys %possible ) {
    $possible{ $_ } -= $remove if ( $possible{ $_ }->size > 1 );
  }
}

my @all = uniq map { $_->{ingredients}->@* } @foods;
my @allergens = uniq map { $_->elements } values %possible;
my $safe = Set::Scalar->new( array_diff @all, @allergens );

my $count = 0;

for ( @foods ) {
  for ( $_->{ingredients}->@* ) {
    $count++ if ( $safe->has( $_ ) );
  }
}

say $count;
