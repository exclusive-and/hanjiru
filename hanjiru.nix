{ mkDerivation
, stdenv
, base
, containers
, mtl
, prettyprinter
, transformers
}:

let
    hanjiru = mkDerivation {
        pname = "hanjiru";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
            base
            containers
            mtl
            prettyprinter
            transformers
        ];
        librarySystemDepends = [];
        license = stdenv.lib.licenses.bsd3;
    };
in
    hanjiru