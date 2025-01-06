{
  description = "Symbolic evaluation as a library";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.grisette-nix-build-env.url = "github:lsrcz/grisette-nix-build-env/main";
  inputs.grisette.url = "github:lsrcz/grisette/nix-abstractions";

  outputs = { self, nixpkgs, flake-utils, grisette-nix-build-env, grisette }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        envLib = grisette-nix-build-env.lib.${system};

        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            envLib.overlays.z3
            envLib.overlays.hlintSrc
            envLib.overlays.cvc5
          ];
        };

        patchedHPkgs = { ghcVersion }: (envLib.patchedHaskellPackages {
          inherit pkgs;
          ghcVersion = ghcVersion;
        }).extend (hfinal: hprev: {
          grisette = pkgs.haskell.lib.dontCheck
            grisette.packages.${system}.grisette.${ghcVersion};
        });

        hPkgs = { ghcVersion, ci }:
          (patchedHPkgs { ghcVersion = ghcVersion; }).extend (hfinal: hprev: {
            grisette-synth-lib = envLib.setCIOptions {
              inherit pkgs ghcVersion hfinal ci;
              extraTestToolDepends = [ pkgs.z3 ];
              package = hfinal.callCabal2nix "grisette-synth-lib" ./. { };
              mixDirs = [
                ""
                "spec/spec-tmp"
              ];
            };
          });

        devShellWithVersion = { ghcVersion, isDevelopmentEnvironment ? false }:
          envLib.devShell {
            inherit pkgs isDevelopmentEnvironment;
            haskellPackages = patchedHPkgs { inherit ghcVersion; };
            extraBuildInputs = [
              pkgs.cairo
              pkgs.expat
              pkgs.pkg-config
              pkgs.xorg.libXdmcp
            ];
          };

        plainOutputs = envLib.defaultOutputs {
          inherit pkgs devShellWithVersion;
          haskellPackagesWithCiFlags = hPkgs;
          packageName = "grisette-synth-lib";
        };

        grisette-synth-lib =
          (hPkgs { ghcVersion = "9101"; ci = false; }).grisette-synth-lib;
      in
      pkgs.lib.recursiveUpdate plainOutputs
        {
          formatter = pkgs.nixpkgs-fmt;

          devShells = {
            default = devShellWithVersion {
              ghcVersion = "9101";
              isDevelopmentEnvironment = true;
            };
          };

          packages.grisette-synth-lib = grisette-synth-lib;
          packages.default = grisette-synth-lib;
        });
}

