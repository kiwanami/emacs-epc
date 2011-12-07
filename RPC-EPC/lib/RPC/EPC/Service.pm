package RPC::EPC::Service;

use warnings;
use strict;
use Carp;

use version; our $VERSION = qv('0.0.1');

use base 'Exporter';

our @EXPORT = qw(
   to_sexp
);

use AnyEvent;
use AnyEvent::Socket;
use AnyEvent::Handle;
use Data::SExpression;
use Data::SExpression::Symbol;
use B;
use Data::Dumper; # for debug


##################################################
# sexp encoding
# (This code is based on Mojolicious's JSON library.)

# Translate an argument object into S-expression text.
sub to_sexp {
  my $arg = shift;
  if (ref $arg eq 'HASH') {
    return _to_sexp_hash($arg);
  } elsif (ref $arg eq 'ARRAY') {
    return _to_sexp_list($arg);
  }
  my $flags = B::svref_2object(\$arg)->FLAGS;
  if ($flags & (B::SVp_IOK | B::SVp_NOK) && !($flags & B::SVp_POK)) {
    return _to_sexp_num($arg);
  } else {
    return _to_sexp_string($arg);
  }
}


my %ESCAPE = (
  '"'     => '"',
  '\\'    => '\\',
  '/'     => '/',
  'b'     => "\x07",
  'f'     => "\x0C",
  'n'     => "\x0A",
  'r'     => "\x0D",
  't'     => "\x09",
  'u2028' => "\x{2028}",
  'u2029' => "\x{2029}"
);

my %REVERSE;
for (0x00 .. 0x1F, 0x7F) { $REVERSE{pack 'C', $_} = sprintf '\u%.4X', $_ }
for my $key (keys %ESCAPE) { $REVERSE{$ESCAPE{$key}} = "\\$key" }

sub _to_sexp_string {
  my $string = shift;
  return "nil" unless $string;

  # Escape
  $string =~ s/([\x00-\x1F\x7F\x{2028}\x{2029}\\\"\/\b\f\n\r\t])/$REVERSE{$1}/gs;

  # Stringify
  return "\"$string\"";
}

sub _to_sexp_num {
  return shift;
}

sub _to_sexp_list {
  my $list = shift;
  my @out = map {to_sexp $_} @$list;
  return "(" . join(" ", @out) . ")";
}

sub _to_sexp_hash {
  my $hash = shift;
  my $out = [];
  foreach my $key ( keys %$hash ) {
    push @$out, "(".to_sexp($key)." . ".to_sexp($hash->{$key}).")";
  }
  return "(" . join(" ", @$out) . ")";
}

##################################################
# nil to undef

our $NIL = Data::SExpression::Symbol->new("nil");

sub _nil_to_undef {
  my $arg = shift;
  if (ref $arg eq 'HASH') {
    return _nil_to_undef_hash($arg);
  } elsif (ref $arg eq 'ARRAY') {
    return _nil_to_undef_list($arg);
  }
  return if ($NIL eq $arg);
  return $arg;
}

sub _nil_to_undef_list {
  my $list = shift;
  for (my $i = 0; $i < @$list; $i++) {
    $list->[$i] = _nil_to_undef($list->[$i]);
  }
  return $list;
}

sub _nil_to_undef_hash {
  my $hash = shift;
  foreach my $key ( keys %$hash ) {
    $hash->{$key} = _nil_to_undef($hash->{$key});
  }
  return $hash;
}


##################################################
# Protocol Stacks

sub new {
  my ($class, $port, $methods) = @_;
  my $cv = AnyEvent->condvar;
  return bless { 'port' => $port,
                 'count' => 0,
                 'methods' => $methods,
                 'sessions' => {},
                 'wait' => $cv }, $class;
}

sub _register_event_loop {
  my ($self,$fh) = @_;

  my $ds = Data::SExpression->new({use_symbol_class=>1,fold_alists=>1});

  my $hdl; $hdl = new AnyEvent::Handle
    (fh => $fh,
     on_error => sub {
       my ($hdl, $fatal, $msg) = @_;
       AE::log warn => "got error $msg\n";
       $hdl->destroy;
       $self->{wait}->send;
     });
  $self->{handle} = $hdl;

  my @reader; @reader =
    (line => sub {
       my ($hdl, $buf) = @_;
       my ($text, $sexp);
       # print Dumper "BUF:", $buf;
       my $len = substr($buf,0,6);
       if (!$_ =~ /[0-9a-f]{6}/i) {
         AE::log warn => "Wrong length: $buf\n";
         $hdl->destroy;
         $self->{wait}->send;
         return;
       }
       $len = hex($len)-1;
       my $body = substr($buf,6);
       if ($len > length $body) {
         AE::log warn => "Not enough input text: $buf\n";
         $hdl->destroy;
         $self->{wait}->send;
         return;
       }
       eval {
         ($sexp, $text) = $ds->read($body);
       };
       # print STDERR "SEXP:".Dumper $sexp;
       if ($sexp->[0]) {
         if ($sexp->[0]->name eq "quit") {
           $hdl->destroy;
           $self->{wait}->send;
           return;
         }
       
         my $handler = $self->{handlers}->{shift(@$sexp)->name};
         if ($handler) {
           $handler->($sexp);
         }
       } else {
         # print STDERR 'NULL:'.Dumper $sexp;
       }
       $hdl->push_read(@reader);
     });

  $hdl->push_read(@reader);
}

sub _handle_connection {
  my ($self,$fh,$host,$port) = @_;
  
  my $handlers = {
      'call' => sub { $self->_call(@_); },
      'return' => sub { $self->_return(@_); },
      'return-error' => sub { $self->_return_error(@_); },
      'epc-error' => sub { $self->_epc_error(@_); },
    };
  $self->{handlers} = $handlers;
  $self->_register_event_loop($fh);
}

sub _uid {
  my $self = shift;
  return $self->{count}++;
}

sub _send_message {
  my ($self, $message) = @_;
  my $hdl = $self->{handle};
  my $len = length($message) + 1;
  # print STDERR ">>> [$message]\n";
  $hdl->push_write(sprintf("%06x%s\n", $len, $message));
}

sub _call {
  my ($self, $sexp) = @_;
  # print STDERR "CALL:" . Dumper $sexp;
  my $id = shift(@$sexp);
  my $name = shift(@$sexp);
  my $task = $self->{methods}->{$name};
  if ($task) {
    my $args = _nil_to_undef($sexp->[0]);
    eval {
      my $ret = $task->($args);
      if ((ref $ret) eq "AnyEvent::CondVar") {
        $ret = $ret->recv;
      }
      $self->_send_message("(return $id ".to_sexp($ret).")");
    };
    if ($@) {
      $self->_send_message("(return-error $id ".to_sexp($@).")");
    }
  } else {
    $self->_send_message("(epc-error $id \"Not found the method: $name\")");
  }
}

sub _return {
  my ($self, $sexp) = @_;
  # print STDERR "RET:" . Dumper $sexp;
  my $id = shift(@$sexp);
  my $result = _nil_to_undef($sexp);
  my $cv = $self->{sessions}->{$id};
  if ($cv) {
    delete $self->{sessions}->{$id};
    $cv->send($result->[0]);
  } else {
    print STDERR "Not found ID : $id\n";
  }
}

sub _return_error {
  my ($self, $sexp) = @_;
  # print STDERR "ERR-RET:" . Dumper $sexp;
  my $id = shift(@$sexp);
  my $result = $sexp->[0];
  my $cv = $self->{sessions}->{$id};
  if ($cv) {
    delete $self->{sessions}->{$id};
    $cv->croak(['ERROR',$result]);
  } else {
    print STDERR "Not found ID : $id\n";
  }
}

sub _epc_error {
  my ($self, $sexp) = @_;
  # print STDERR "EPCERR-RET:" . Dumper $sexp;
  my $id = shift(@$sexp);
  my $result = $sexp->[0];
  my $cv = $self->{sessions}->{$id};
  if ($cv) {
    delete $self->{sessions}->{$id};
    $cv->croak(['EPC_ERROR',$result]);
  } else {
    print STDERR "Not found ID : $id\n";
  }
}

sub call_method {
  my $self = shift;
  my $name = shift;
  my $args = shift;
  my $cv = AnyEvent->condvar;
  my $id = $self->_uid;
  $self->{sessions}->{$id} = $cv;
  $self->_send_message("(call $id $name ".(to_sexp $args).")");
  return $cv;
}

sub define_method {
  my ($self, $name, $task) = @_;
  $self->{methods}->{$name} = $task;
}

sub start {
  my $self = shift;
  my $server = tcp_server undef, $self->{port}, sub {
    $self->_handle_connection(@_);
  }, sub {
    my ($fh, $thishost, $thisport) = @_;
	binmode( STDOUT, ":unix" ); # immediate flush
    print "$thisport\n"; # epc protocol
  };
  $self->{server} = $server;
  $self->{wait}->recv;
}

sub client_start {
  my $self = shift;
  my $host = "127.0.0.1";
  my $cv = AnyEvent->condvar;
  my $client = tcp_connect $host, $self->{port}, sub {
    my ($fh) = @_;
    if ($fh) {
      $self->_handle_connection($fh, $host, $self->{port});
      $cv->send;
    } else {
      $cv->croak(['Could not connect server.']);
    }
  };
  $self->{client} = $client;
  $cv->recv;
}

sub stop {
  my $self = shift;
  $self->{handle}->push_shutdown;
}



1; # Magic true value required at end of module
__END__

=head1 NAME

RPC::EPC::Service - An Asynchronous Remote Procedure Stack.


=head1 VERSION

This document describes RPC::EPC::Service version 0.0.1


=head1 SYNOPSIS

=head2 Server code

    use RPC::EPC::Service;
    use Data::Dumper;
    
    sub add_test {
        my $methods = {
            'add' => sub {
                my $args_ref = shift;
                my ($a,$b) = @$args_ref;
                return $a + $b;
            }
        };
        my $server = RPC::EPC::Service->new(8888, $methods);
        $server->start;
    }
    
    add_test();

=head2 Client code

    use RPC::EPC::Service;
    
    my $client = RPC::EPC::Service->new($port,{});
    $client->client_start;
    
    my $ret = $client->call_method('add', [1,2]);
    $ret->recv == 3;
    
    $client->stop;

=head1 DESCRIPTION

This module enables to connect the other process with the S-expression protocol, like the Swank protocol of the SLIME.

SLIME : http://common-lisp.net/project/slime/

The primary objective is for users to make some Emacs extensions with the Perl and CPAN.

=head2 Protocol

The encoding format is the S-expression.
The TCP connection is the communication layer.
Because the RPC session is written in the async manner, the programs can call the procedures asynchronously.

=head2 Object Serialization

Following types can be translated:
=over 4
=item Number
=item String
=item Array
=item Hashe
=back

The complex objects consisted of Array and Hash are supported.

=head1 INTERFACE

=head2 Server side

=head3 new
Create a server object.
If port number is 0 or undef, the number is decided by the OS.

=head3 define_method
Define a method which is called by the client.

=head3 start
Start the server and wait for the client connection.

=head3 call_method
Call the client's method.

=head2 Client side

=head3 new
Create a client object.

=head3 client_start
Establish the connection to the server.

=head3 stop
Shutdown the connection.

=head3 define_method
Define a method which is called by the server.

=head3 call_method
Call the server's method.


=head2 Utilities

=head3 to_sexp
Translate a perl object into S-expression string.

=head1 AUTHOR

Masashi Sakurai  C<< <m.sakurai@kiwanami.net> >>


=head1 LICENCE AND COPYRIGHT

Copyright (c) 2011, Masashi Sakurai C<< <m.sakurai@kiwanami.net> >>. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.


=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH
YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL,
OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE
THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.
