use v5.32;
use warnings;

my $parse = qr{
  (?&MULT) (?{ $_ = $^R->[1] })

  (?(DEFINE)
    (?<NUMBER> ( \d ) (?{ [$^R, eval $^N] }) )

    (?<ATOM> ( (?&NUMBER) | \( (?&MULT) \) ) )

    (?<MULT>
      (?&ADD) (?{ [ $^R->[0], $^R->[1] ] })
      (?: \* (?&ADD) (?{ [ $^R->[0][0], $^R->[0][1] * $^R->[1] ] }) )*
    )

    (?<ADD>
      (?&ATOM) (?{ [ $^R->[0], $^R->[1] ] })
      (?: \+ (?&ATOM) (?{ [ $^R->[0][0], $^R->[0][1] + $^R->[1] ] }) )*
    )
  )
}xms;


sub parse {
  local $_ = shift;
  local $^R;

  s/ //g;
  eval { m{\A$parse\z} } and return $_;

  die $@;
}

open INPUT, '<', 'inputs/18';

my $result;
$result += parse($_) while (<INPUT>);
say $result;
