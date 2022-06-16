{
  description = "OpenAPI Test Server";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcVersion = "ghc902";

        pkgs = import nixpkgs { system = "${system}"; };
        haskellPkgs = pkgs.haskell.packages.${ghcVersion};
        ghc = haskellPkgs.ghcWithHoogle (_:
          self.packages.${system}.openapi-test-server.getBuildInputs.haskellBuildInputs);
      in {
        packages = rec {
          apikey-spec-haskell =
            (haskellPkgs.callCabal2nix "apikey-spec" ./apikey { });
          basic-spec-haskell =
            (haskellPkgs.callCabal2nix "basic-spec" ./basic { });
          bearer-spec-haskell =
            (haskellPkgs.callCabal2nix "bearer-spec" ./bearer { });
          noauth-spec-haskell =
            (haskellPkgs.callCabal2nix "noauth-spec" ./noauth { });
          custom-monad-spec-haskell =
            (haskellPkgs.callCabal2nix "custom-monad-spec" ./custom-monad { });

          openapi-test-server =
            (haskellPkgs.callCabal2nix "openapi-test-server" ./server {
              apikey-spec = apikey-spec-haskell;
              basic-spec = basic-spec-haskell;
              bearer-spec = bearer-spec-haskell;
              noauth-spec = noauth-spec-haskell;
              custom-monad-spec = custom-monad-spec-haskell;
            });

          openapi-test-client =
            (haskellPkgs.callCabal2nix "openapi-test-client" ./client {
              apikey-spec = apikey-spec-haskell;
              basic-spec = basic-spec-haskell;
              bearer-spec = bearer-spec-haskell;
              noauth-spec = noauth-spec-haskell;
            });
        };

        defaultPackage = self.packages.${system}.openapi-test-server;

        apps = {
          noauth-server = flake-utils.lib.mkApp {
            drv = self.packages.${system}.openapi-test-server;
            exePath = "/bin/noauth-server";
          };

          apikey-server = flake-utils.lib.mkApp {
            drv = self.packages.${system}.openapi-test-server;
            exePath = "/bin/apikey-server";
          };

          basic-server = flake-utils.lib.mkApp {
            drv = self.packages.${system}.openapi-test-server;
            exePath = "/bin/basic-server";
          };

          bearer-server = flake-utils.lib.mkApp {
            drv = self.packages.${system}.openapi-test-server;
            exePath = "/bin/bearer-server";
          };

          custom-monad-server = flake-utils.lib.mkApp {
            drv = self.packages.${system}.openapi-test-server;
            exePath = "/bin/custom-monad-server";
          };

          apikey-client = flake-utils.lib.mkApp {
            drv = self.packages.${system}.openapi-test-client;
          };

          update-flake = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "update-flake";
              text =
                "nix flake update"; # TODO: run tests to check if update was successful
            };
          };
        };

        devShell = pkgs.mkShell {
          buildInputs = [
            ghc
            haskellPkgs.cabal-install
            haskellPkgs.haskell-language-server
            haskellPkgs.hlint
            haskellPkgs.ormolu
            pkgs.nixfmt
          ];
        };
      });
}
