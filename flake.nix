{
  description = "PokeAPI Haskell Server Elm-Land exercise";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
   let pkgs = nixpkgs.legacyPackages.x86_64-linux; 
   in  
    {
      
     devShells.x86_64-linux.default = pkgs.mkShell {
           buildInputs = [ pkgs.haskellPackages.ghc
                           pkgs.haskellPackages.cabal-install
                           pkgs.zlib
                         
                         ];
           shellHook = ''
                   ghc --version
                   cabal --version 
               '';
         };
     packages.default = pkgs.haskellPackages.callCabal2nix "pokeAPI-Haskell" ./. {};

    };
}
