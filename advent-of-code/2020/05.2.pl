use v5.32;
use warnings;
use Array::Utils qw(array_minus);
use List::Util qw(min max);

my $line = 'BFFFBBFRRR';

open INPUT, '<', 'inputs/5';

sub seat {
  $_ = shift;

  s/[BR]/1/g;
  s/[FL]/0/g;

  my ( $row, $column ) = (m/(\d{7})(\d{3})/);
  $row = oct( "0b$row" );
  $column = oct( "0b$column" );

  [ $row, $column ];
}

sub seat_id {
  my ( $row, $column ) = seat( $_[0] )->@*;

  return $row * 8 + $column;
}

my @ids;
push @ids, seat_id($_) while <INPUT>;

my @range = ( min(@ids) .. max(@ids) );

say array_minus(@range, @ids);

