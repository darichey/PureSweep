{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, easy-purescript-nix, ... }@inputs:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      easy-ps = import easy-purescript-nix { inherit pkgs; };
    in
    {
      devShells.x86_64-linux.default = pkgs.mkShell
        {
          buildInputs = [
            easy-ps.purs-0_15_2
            easy-ps.spago
            easy-ps.purty
            pkgs.nodejs-14_x
          ];
        };
    };
}
