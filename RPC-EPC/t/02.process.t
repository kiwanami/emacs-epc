use Test::More tests => 1;

use IPC::Open2;
use RPC::EPC::Service;

$pid = open2(*PROC_OUT, undef, 'perl -e \'use RPC::EPC::Service; $s=RPC::EPC::Service->new(8888,{}); $s->{wait}->send; $s->start; sleep 1;\'');

$val = <PROC_OUT>;

is("8888\n", $val);

kill 1, $pid;
