use Test::More tests => 2;

use IPC::Open2;
use RPC::EPC::Service;

# start server

$pid = open2(*PROC_OUT, undef, "perl ./t/_process.pl");
$val = <PROC_OUT>;

is("8888\n", $val);

close PROC_OUT;
kill 1, $pid;
sleep 0.5;

# client start

$client = RPC::EPC::Service->new(8888,{});
eval {
  $client->client_start;
  fail("It should fail.");
};
if ($@) {
  is($@->[0], "Could not connect server.");
}
