{ mkDerivation
, lib
, base
, containers
, mtl
, prettyprinter
, primitive
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
            primitive
            transformers
        ];
        librarySystemDepends = [];
        license = lib.licenses.bsd3;
    };
in
    hanjiru