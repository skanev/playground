use v5.32;
use warnings;
use Set::Scalar;
use Data::Dump qw(dump);

open INPUT, '<', 'inputs/4';

my $required = Set::Scalar->new(qw(byr iyr eyr hgt hcl ecl pid));
my $count;

local $/ = "\n\n";
while ( <INPUT> ) {
  chomp;

  my $record = $_;
  my %fields;

  $fields{$1} = $2 while $record =~ /(\S+):(\S+)/g;

  my $keys = Set::Scalar->new( keys( %fields ) );

  next unless $required->difference( $keys )->is_empty;

  my ( $height, $unit ) = ( $fields{hgt} =~ /(\d+)(cm|in)/ );

  next unless 1920 <= $fields{byr} and $fields{byr} <= 2002;
  next unless 2010 <= $fields{iyr} and $fields{iyr} <= 2020;
  next unless 2020 <= $fields{eyr} and $fields{eyr} <= 2030;
  next unless defined $unit;
  next unless ( $unit eq 'cm' and 150 <= $height and $height <= 193 ) or
              ( $unit eq 'in' and 59 <= $height and $height <= 76);
  next unless $fields{hcl} =~ /^#[0-9a-f]{6}$/;
  next unless $fields{ecl} =~ /^(amb|blu|brn|gry|grn|hzl|oth)$/;
  next unless $fields{pid} =~ /^\d{9}$/;

  $count++;
}

say $count;
