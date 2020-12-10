use v5.32;
use warnings;
use Set::Scalar;

open INPUT, '<', 'inputs/4';

my $required = Set::Scalar->new(qw(byr iyr eyr hgt hcl ecl pid));
my $count;

local $/ = "\n\n";
while (<INPUT>) {
  chomp;

  my $record = $_;
  my %fields;

  $fields{$1} = $2 while $record =~ /(\S+):(\S+)/g;

  my $keys = Set::Scalar->new(keys(%fields));

  $count++ if $required->difference($keys)->is_empty;
}

say $count;
