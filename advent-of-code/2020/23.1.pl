use v5.32;
use warnings;

use List::Util qw( any );
use constant INPUT => '784235916';

package Ring {
  use List::MoreUtils qw( firstidx );

  sub new {
    my ( $package, $pointer, @elements ) = @_;

    bless { pointer => $pointer, elements => [ @elements ] }, $package;
  }

  sub show {
    my $self = shift;

    use Data::Dump qw(dump);
    warn dump $self->{elements};
  }

  sub pick_three {
    my $self = shift;
    my $ring = $self->normalize;

    my @three = splice $ring->{elements}->@*, 1, 3;

    ( $ring, @three );
  }

  sub normalize {
    my $self = shift;
    my @elements = $self->{elements}->@*;
    my @prefix = splice @elements, 0, $self->{pointer};

    Ring->new( 0, @elements, @prefix );
  }

  sub current {
    my $self = shift;
    $self->{elements}[ $self->{pointer} ];
  }

  sub insert_after {
    my ( $self, $destination, @items ) = @_;
    my @elements = ( $self->{elements}->@* );
    my $i = firstidx { $_ == $destination } @{$self->{elements}};
    splice @elements, $i + 1, 0, @items;

    Ring->new( $self->{pointer}, @elements );
  }

  sub select_next {
    my $self = shift;
    my ( $pointer, @elements ) = ( $self->{pointer}, $self->{elements}->@* );

    $pointer = $pointer + 1 % scalar( @elements );

    Ring->new( $pointer, @elements );
  }

  sub cannonical {
    my $self = shift;
    my $i = firstidx { $_ == 1 } $self->{elements}->@*;
    my $ring = Ring->new( $i, $self->{elements}->@* )->normalize;

    join '', $ring->{elements}->@[1..8];
  }
}

sub destination {
  my @numbers = @_;

  my $n = $numbers[0];

  while ( any { $_ == $n } @numbers ) {
    $n--;
    $n = 9 if $n == 0;
  }

  $n;
}

my $ring = Ring->new( 0, split( '', INPUT ) );

for ( 1 .. 100 ) {
  say "-- move $_ --";
  say "cups: " . join( ' ', $ring->{elements}->@* );
  ( $ring, my @pick ) = $ring->pick_three;
  say "pick up: " . join( ' ', @pick );
  my $destination = destination $ring->current, @pick;
  say "destination: " . $destination;
  $ring = $ring->insert_after( $destination, @pick );
  $ring = $ring->select_next;
  say "";
}

say $ring->cannonical;
