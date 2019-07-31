let pinned-nixpkgs-path = import ./pinned-nixpkgs.nix;
    pinned-pkgs = import pinned-nixpkgs-path {};
in { pkgs ? pinned-pkgs }: # if I ever need to override stuff

with pkgs;

let 
  hsPkgs = haskell.packages.ghc865;
  inherit (hsPkgs) ghcid;
  # To get the sha256 hash for an updated revision, due to the usage of
  # submodules, it is necessary to use `nix-prefetch-git`:
  # 
  # nix-prefetch-git https://github.com/ucsd-progsys/liquidhaskell.git
  #     --rev {new-revision} --no-deepClone --fetch-submodules
  liquidhaskell-src = fetchFromGitHub {
    owner = "ucsd-progsys";
    repo = "liquidhaskell";
    rev = "f4fe82cd03fbe906379c8ebeac5ec3efae0b4cd8";
    sha256 = "1nspgp6rp24607ysavjvh25lb3sv7nh00phv93q25v93hnsyvggw";
    fetchSubmodules = true;
  };
  liquidhaskell = hsPkgs.callPackage ./liquidhaskell.nix { src = liquidhaskell-src; };
in mkShell {
  buildInputs = [
    stack ghcid
    z3 liquidhaskell
  ];
  PINNED_NIX_PATH = pinned-nixpkgs-path;
  NIX_PATH="nixpkgs=${pinned-nixpkgs-path}";
}
