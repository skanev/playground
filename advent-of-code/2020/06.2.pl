use v5.32;
use warnings;
use Data::Dump qw( dump );
use List::Util qw( reduce );
use List::MoreUtils qw( uniq );
use Set::Scalar;

open INPUT, '<', 'inputs/6' or die;

$_ = do { local $/; <INPUT> };

my @chunks = split /\n\n/;
chomp @chunks;

dump reduce { $a + $b }
     map { scalar $_->elements }
     map { reduce { $a->intersection($b) } map { Set::Scalar->new(split '') } split "\n" }
     @chunks;
