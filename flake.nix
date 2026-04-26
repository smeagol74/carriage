{
  description = "Carriage-mode - build dev shell and run ERT tests";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = { self, nixpkgs }:
  let
    systems = [ "x86_64-linux" "aarch64-linux" ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
  in
  {
    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        packages = with pkgs; [
          emacs-nox
          ripgrep
          git
          coreutils
          curl
        ];
        shellHook = ''
          echo "Run tests: emacs -Q --batch -L lisp -l test/ert-runner.el"
          echo "Or via flake app: nix run .#tests"
          echo "Build (byte-compile): nix run .#build"
        '';
      };
    });

    apps = forAllSystems (pkgs:
      let
        emacs-nox = pkgs.emacs-nox;
        # Use emacs-nox for batch build/test to avoid packaging interactive ELPA overlays
        emacs-with-gptel = pkgs.emacs-nox;

        testsDrv = pkgs.writeShellApplication {
          name = "carriage-tests";
          runtimeInputs = [ pkgs.ripgrep pkgs.git pkgs.curl ];
          text = ''
            set -euo pipefail
            exec env -u NIX_LD -u NIX_LD_LIBRARY_PATH -u LD_LIBRARY_PATH -u DYLD_LIBRARY_PATH \
               ${emacs-with-gptel}/bin/emacs -Q --batch \
              --eval "(setq package-archives '((\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\")))" \
              --eval "(package-initialize)" \
              -L ${./lisp} -l ${./test}/ert-runner.el
          '';
        };

        buildDrv = pkgs.writeShellApplication {
          name = "carriage-build";
          runtimeInputs = [ pkgs.coreutils pkgs.curl ];
          text = ''
            set -euo pipefail
            tmp="$(mktemp -d)"
            cp -r ${./lisp} "$tmp/lisp"
            cd "$tmp"
            env -u NIX_LD -u NIX_LD_LIBRARY_PATH -u LD_LIBRARY_PATH -u DYLD_LIBRARY_PATH \
               ${emacs-with-gptel}/bin/emacs -Q --batch \
               -L lisp \
               -f batch-byte-compile lisp/*.el
            echo "Byte-compiled files are in: $tmp/lisp (temporary dir)"
          '';
        };
      in rec {
        tests = { type = "app"; program = "${testsDrv}/bin/carriage-tests"; };
        build = { type = "app"; program = "${buildDrv}/bin/carriage-build"; };
        default = tests;
      });

    checks = forAllSystems (pkgs: {
        ert = pkgs.runCommand "carriage-ert" { buildInputs = [ pkgs.emacs-nox pkgs.ripgrep pkgs.git pkgs.curl ]; } ''
          mkdir -p ~/.emacs.d/elpa
          cp -r ${./lisp} ./lisp
          cp -r ${./test} ./test
        env -u NIX_LD -u NIX_LD_LIBRARY_PATH -u LD_LIBRARY_PATH -u DYLD_LIBRARY_PATH \
          ${pkgs.emacs-nox}/bin/emacs -Q --batch \
          --eval "(setq package-archives '((\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\")))" \
          --eval "(package-initialize)" \
          -L lisp -l test/ert-runner.el
        touch $out
      '';

      byte-compile = pkgs.runCommand "carriage-byte-compile" { buildInputs = [ pkgs.emacs-nox pkgs.coreutils pkgs.curl ]; } ''
        cp -r ${./lisp} ./lisp
        # Compile with warnings as errors
          env -u NIX_LD -u NIX_LD_LIBRARY_PATH -u LD_LIBRARY_PATH -u DYLD_LIBRARY_PATH \
            ${pkgs.emacs-nox}/bin/emacs -Q --batch \
            -L lisp \
            -f batch-byte-compile lisp/*.el
        mkdir -p $out
        cp -r lisp $out/
      '';
    });
  };
}
