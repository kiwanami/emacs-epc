# Emacs RPC

This program is an asynchronous RPC stack for Emacs.  Using this
RPC stack, the Emacs can communicate with the peer process.
Because the protocol is S-expression encoding and consists of
asynchronous communications, the RPC response is fairly good.

Current implementations for the EPC are followings:
- epcs.el : Emacs Lisp implementation
- RPC::EPC::Service : Perl implementation
  - One can get this module by CPAN or PPM.
- python-epc : Python implementation
  - http://python-epc.readthedocs.org/en/latest/

The current status is beta. This library needs more applications to
confirm stability of the API and robustness of the implementation.

## Applications

Projects using EPC:

- [Emacs DBI](https://github.com/kiwanami/emacs-edbi):
  Database GUI and API for Emacs
- [Emacs Jedi](https://github.com/tkf/emacs-jedi):
  Python auto-completion for Emacs

## Sample Code

Here is a client code.

```lisp
(require 'epc)

(setq epc (epc:start-epc "perl" '("echo-server.pl")))

(deferred:$
  (epc:call-deferred epc 'echo '(10))
  (deferred:nextc it 
    (lambda (x) (message "Return : %S" x))))

(deferred:$
  (epc:call-deferred epc 'add '(10 40))
  (deferred:nextc it 
    (lambda (x) (message "Return : %S" x))))

;; calling synchronously
(message "%S" (epc:call-sync epc 'echo '(10 40)))

;; Request peer's methods
(message "%S" (epc:sync epc (epc:query-methods-deferred epc)))

(epc:stop-epc epc)
```

Here is a server code in perl.

```perl
#!/usr/bin/perl

use RPC::EPC::Service;

sub echo_test {
    my $methods = {
    
        'echo' => [sub {
            my $args = shift;
            return $args;
        },"args","just echo back arguments."],
        
        'add' => sub {
            my $args_ref = shift;
            my ($a,$b) = @$args_ref;
            return $a + $b;
        }
    };
    my $server = RPC::EPC::Service->new(0, $methods);
    $server->start;
}

echo_test();
```

Here is the equivalent server code in emacs lisp.

```lisp
(require 'epcs)

(let ((connect-function
       (lambda (mngr) 
         (epc:define-method mngr 'echo (lambda (&rest x) x) "args" "just echo back arguments.")
         (epc:define-method mngr 'add '+ "args" "add argument numbers.")
         )) server-process)

  (setq server-process (epcs:server-start connect-function))
  (sleep-for 10)
  (epcs:server-stop server-process))
```

The elisp server code should be started with some arguments (batch starting and indicating load pathes) like the following code:

```lisp
(setq epc (epc:start-epc "emacs" '("-L" ".." "-L" "~/.emacs.d/elisp" "-batch" "-l" "deferred" "-l" "concurrent" "-l" "epc" "-l" "epcs" "-l" "echo-server.el")))
```

# Installation

## Package installation

If you use package.el with Marmalade (http://marmalade-repo.org/), you just select the package 'epc' and install it.

## Manual installation

This program depends on following programs:

- deferred.el, concurrent.el / https://github.com/kiwanami/emacs-deferred
- ctable.el   / https://github.com/kiwanami/emacs-ctable

Place those programs and this one (epc.el) in your load path and add following code.

```lisp
(require 'epc)
```

# API Document

TODO...
(The perl document may be helpful. http://search.cpan.org/~kiwanami/RPC-EPC-Service-v0.0.7/lib/RPC/EPC/Service.pm)

## High Level API

## Low Level API

## Management Interface

# Implementation

## Protocol Details


# License

GPL v3

----
(C) 2012 SAKURAI Masashi. m.sakurai at kiwanami.net
