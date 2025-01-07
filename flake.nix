{
    inputs = {
        nixpkgs.url = "nixpkgs/nixos-24.11";
    };

    outputs = {nixpkgs, self}:
        let
            system = "x86_64-linux";
            pkgs = import nixpkgs {
                inherit system;
            };
            haskell = pkgs.haskell.packages.ghc9101;
            hanjiru = haskell.callPackage ./hanjiru.nix {};
        in
            {
                packages.${system}.default = hanjiru;
                devShells.${system}.default = haskell.shellFor {
                    packages = _: [ hanjiru ];
                    nativeBuildInputs = [
                        haskell.cabal-install
                        haskell.haskell-language-server
                    ];
                };
            };
}