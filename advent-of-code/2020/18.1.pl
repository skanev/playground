use v5.32;
use warnings;
use experimental 'switch';

open INPUT, '<', 'inputs/18';

sub evaluate {
  my $tokens = shift;
  my $op = sub { $_[1] };
  my $result = 0;

  while (my $next = shift( @$tokens )) {
    given ( $next ) {
      when( /(\d)/ ) { $result = $op->( $result, $1 ) }
      when( '+' ) { $op = sub { $_[0] + $_[1] } }
      when( '*' ) { $op = sub { $_[0] * $_[1] } }
      when( '(' ) { $result = $op->( $result, evaluate( $tokens ) ) }
      when( ')' ) { return $result }
      default { die "unexpected token: $next"; }
    }
  }

  $result;
}

sub parse {
  $_ = shift;
  s/ //g;
  chomp;
  my @tokens = split '';

  evaluate( \@tokens );
}

my $result = 0;

while (<INPUT>) {
  $result += parse( $_ );
}

say $result;

__DATA__
1 + (2 * 3) + (4 * (5 + 6))
2 * 3 + (4 * 5)
5 + (8 * 3 + 9 + 3 * 4 * 3)
5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2
