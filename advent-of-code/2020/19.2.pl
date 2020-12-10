use v5.32;
use warnings;

open STDIN, '<', 'inputs/19';

$/ x= 2;
$_ = <>;
s/^8: .*$/8: 42 | 42 8/m;
s/^11: .*$/11: 42 31 | 42 11 31/m;
s/^(\d+): (.*)$/"(?<g$1> (" . ( $2 =~ s#(\d+)#(?&g$1)#gr ) . ") )"/gem;
s/\("(\w)"\)/$1/g;
my $pattern = qr/^(?&g0) (?(DEFINE) $_)$/x;
say scalar grep { m/$pattern/ } split "\n", <>;
