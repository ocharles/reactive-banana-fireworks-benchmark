{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, criterion, lens, linear
      , random, reactive-banana, stdenv, reflex
      }:
      mkDerivation {
        pname = "physics-2d";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base containers criterion lens linear random reactive-banana
          reflex
        ];
        homepage = "https://github.com/ocharles/reactive-banana-fireworks";
        description = "A fireworks benchmark in reactive-banana";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
