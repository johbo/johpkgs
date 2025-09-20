
========
 README
========


That's my package collection based on Nix, mainly serves two purposes:

* Manage a shared set of configuration files to use on different machines, by
  installing `johbo-config`.

* Provide custom packages which are not in `nixpkgs`.


Usage
=====

The derivations are not yet provided as a flake. Installation is possible in the
following way:

.. code:: shell

   nix profile install -f default.nix johbo-config


Dependencies
============

* Needs the Nix package manager, make sure to check out http://nixos.org and
  set it up.

* Needs a recent version of `nixpkgs`, at least follow the unstable channel. If
  that's not recent enough, either wait or use the `master` branch of
  `nixpkgs`.
