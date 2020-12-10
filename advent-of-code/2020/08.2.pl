use v5.32;
use warnings;
use experimental qw( switch );

use Storable qw( dclone );

my @instructions;

open INPUT, '<', 'inputs/8';

while ( <INPUT> ) {
  my ( $op, $arg ) = split ' ';
  push @instructions, { op => $op, arg => $arg };
}

use Data::Dump qw(dump);

sub evaluate {
  my ( $instructions ) = @_;

  my @executed;
  my $infinite = 0;
  my $ip = 0;
  my $reg = 0;

  while ( $ip <= $#instructions ) {
    my ( $op, $arg ) = $instructions->[$ip]->@{'op', 'arg'};

    if ( $executed[$ip] ) {
      $infinite = 1;
      last;
    }

    $executed[$ip] = 1;
    $ip++;

    given ( $op ) {
      when('nop') { }
      when('jmp') { $ip += $arg - 1 }
      when('acc') { $reg += $arg }
    }
  }

  return { reg => $reg, infinite => $infinite };
}

use Data::Dump qw(dump);

for my $i (0..$#instructions) {
  my $modified = dclone( \@instructions );

  if ( $modified->[$i]{op} eq 'nop' ) {
    $modified->[$i]{op} = 'jmp';
  } elsif ( $modified->[$i]{op} eq 'jmp' ) {
    $modified->[$i]{op} = 'nop';
  } else {
    next
  }

  my $result = evaluate( $modified );

  if ( ! $result->{infinite} ) {
    say $result->{reg};
  }
}



__DATA__
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
