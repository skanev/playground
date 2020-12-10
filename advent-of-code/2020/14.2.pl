use v5.32;
use warnings;
use experimental "switch";
use bigint;
use List::Util qw( sum );
use List::MoreUtils qw( uniq zip6 );

open INPUT, '<', 'inputs/14';

sub bin { oct "0b$_[0]" }

sub mask {
  my ( $mask, $addr ) = @_;
  my @mask = split '', $mask;
  my @addr = split '', sprintf( "%036b", $addr );

  variants( join "", map { $_->[0] == 1 ? '1' : $_->[0] eq 'X' ? 'X' : $_->[1] } zip6( @mask, @addr ) );
}

sub variants {
  my ( $mask ) = @_;
  my @result;

  return ( $mask ) unless $mask =~ m/^([01]*)(?:(X)(.*))$/;

  my ( $prefix, $suffix ) = ( $1, $3 );

  map { ( "${prefix}0$_", "${prefix}1$_" ) } variants( $suffix );
}

my $mask;
my %mem;

while (<INPUT>) {
  given ($_) {
    when(/^mask = ([X01]+)$/) {
      $mask = $1;
    }
    when(/^mem\[(\d+)\] = (\d+)$/) {
      my ( $x, $y ) = ( $1, $2 );
      my @addrs = mask( $mask, $1 );

      $mem{$_} = $y for ( @addrs );
    }
    default { say "OH NON NON ON ON O" }
  }
}

say sum values(%mem);
