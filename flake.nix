{
    inputs = {
        nixpkgs.url = "nixpkgs/nixos-24.11";
    };

    outputs = { nixpkgs, self }:
        let
            overlay = import ./overlay.nix;

            system = "x86_64-linux";

            pkgs = import nixpkgs {
                overlays = [ overlay ];
                inherit system;
            };

            inherit (pkgs) haskellPackages;
        in
        {
            overlays.haskellPackages = overlay;

            packages.${system}.default = haskellPackages.hanjiru;

            devShells.${system}.default = haskellPackages.shellFor {
                packages = final: [ final.hanjiru ];
                    
                nativeBuildInputs = [
                    haskellPackages.cabal-install
                    haskellPackages.haskell-language-server
                ];
            };
        };
}