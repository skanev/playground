use v5.32;
use warnings;

use constant MOD => 20201227;

sub transform {
  my ( $loop, $subject ) = @_;
  my $result = 1;

  for ( 1 .. $loop ) {
    $result *= $subject;
    $result %= MOD;
  }

  $result;
}

sub find_loop_size {
  my ( $key, $subject ) = @_;

  my $n = 1;
  my $i = 0;
  while (1) {
    $i++;
    $n *= $subject;
    $n %= MOD;

    return $i if $n == $key;
  }

  return undef;
}

sub hack {
  transform( find_loop_size( $_[0], 7 ), $_[1] ) ;
}

#say hack 5764801, 17807724;
say hack 2069194, 16426071;
