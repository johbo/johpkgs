{ callPackage
, pkgs
}:

{
  d = callPackage ./d { };
  yaml = callPackage ./yaml { };
}
