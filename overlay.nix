let
    haskellOverlays = final: prev: {
        hanjiru = prev.callPackage ./hanjiru.nix {};
    };
in
    final: prev: {
        haskellPackages =
            prev.haskell.packages.ghc9101.extend haskellOverlays;
    }