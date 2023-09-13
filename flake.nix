{
  description = "CFEL P11 UI";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=5df4d78d54f7a34e9ea1f84a22b4fd9baebc68d0";
  inputs.elm-watch = {
    url = "github:jeslie0/elm-watch";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, elm-watch }:
    let
      system = "x86_64-linux";
    in
    rec {
      # Nixpkgs overlay providing the application
      overlay = nixpkgs.lib.composeManyExtensions [
      ];

      devShells.${system} =
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };
        in
        {
          default = pkgs.mkShell {
            buildInputs = [
              pkgs.elmPackages.elm
              pkgs.elmPackages.elm-review
              pkgs.elmPackages.elm-format
              pkgs.elmPackages.elm-language-server
              pkgs.elmPackages.elm-json
              elm-watch.packages.${system}.elm-watch
            ];
          };
        };
    };

}
