use v5.32;
use warnings;
use Data::Dump qw( dump );
use List::Util qw( reduce );
use List::MoreUtils qw( uniq );

open INPUT, '<', 'inputs/6' or die;

$_ = do { local $/; <INPUT> };

my @chunks = split /\n\n/;
chomp @chunks;
s/\n//g for @chunks;

dump reduce { $a + $b }
     map { scalar uniq split '' }
     @chunks;

