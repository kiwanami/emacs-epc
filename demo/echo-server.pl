#!/usr/bin/perl

# (setenv "PERL5LIB" "/home/sakurai/nwork/apps/emacs-edbc/exlib/lib/perl5/x86_64-linux:/home/sakurai/nwork/apps/emacs-edbc/exlib/lib/perl5/:/home/sakurai/nwork/apps/emacs-edbc/RPC-EPC/lib" )

use RPC::EPC::Service;

sub echo_test {
    my $methods = {
        'echo' => sub {
            my $args = shift;
            return $args;
        },
        'add' => sub {
            my $args_ref = shift;
            my ($a,$b) = @$args_ref;
            return $a + $b;
        }
    };
    my $server = RPC::EPC::Service->new(8888, $methods);
    $server->start;
}

echo_test();

