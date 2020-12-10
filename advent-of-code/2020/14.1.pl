use v5.32;
use warnings;
use experimental "switch";
use bigint;
use List::Util qw( sum );

open INPUT, '<', 'inputs/14';

sub bin { oct "0b$_[0]" }

sub mask {
  my ( $mask, $num ) = @_;

  ( $num & bin( $mask =~ s/X/1/rg ) ) | bin( $mask =~ s/X/0/rg );
}


my $mask;
my %mem;

while (<INPUT>) {
  given ($_) {
    when(/^mask = ([X01]+)$/) {
      $mask = $1;
    }
    when(/^mem\[(\d+)\] = (\d+)$/) {
      $mem{$1} = mask( $mask, $2 );
    }
    default { say "OH NON NON ON ON O" }
  }
}

say sum values(%mem);
