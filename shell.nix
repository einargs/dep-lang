let pinned-nixpkgs-path = import ./pinned-nixpkgs.nix;
    pinned-pkgs = import pinned-nixpkgs-path {};
in { pkgs ? pinned-pkgs }: # if I ever need to override stuff

with pkgs;

let 
  hsPkgs = haskell.packages.ghc865;
  inherit (hsPkgs) liquidhaskell ghcid liquidhaskell-cabal;
in mkShell {
  buildInputs = [
    stack ghcid
    z3 liquidhaskell
    #liquidhaskell-cabal
  ];
  PINNED_NIX_PATH = pinned-nixpkgs-path;
  NIX_PATH="nixpkgs=${pinned-nixpkgs-path}";
}
