{
  description = "Input Spinner Widget for Elm";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }:
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
              pkgs.elmPackages.elm-doc-preview
              pkgs.elmPackages.elm-test
            ];
          };
        };
    };

}
