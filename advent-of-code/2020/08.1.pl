use v5.32;
use warnings;
use experimental qw( switch );

my @instructions;

open INPUT, '<', 'inputs/8';

while ( <INPUT> ) {
  my ( $op, $arg ) = split ' ';
  push @instructions, { op => $op, arg => $arg };
}

my @executed;

my $ip = 0;
my $reg = 0;

while (1) {
  my ( $op, $arg ) = $instructions[$ip]->@{'op', 'arg'};

  last if $executed[$ip];

  $executed[$ip] = 1;
  $ip++;

  given ( $op ) {
    when('nop') { }
    when('jmp') { $ip += $arg - 1 }
    when('acc') { $reg += $arg }
  }

}

say $reg;
