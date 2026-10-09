{
  description = "words.chreekat.net";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      lib = nixpkgs.lib;
      fs = lib.fileset;

      # The generator only rebuilds when its own code changes, not the writing.
      generator = pkgs.haskellPackages.callCabal2nix "blatherings"
        (fs.toSource {
          root = ./.;
          fileset = fs.unions [ ./blatherings.cabal ./src ./app ./test ];
        })
        { };

      site = visibility: pkgs.stdenvNoCC.mkDerivation {
        name = "words-chreekat-net";
        src = fs.toSource {
          root = ./.;
          fileset = fs.unions ([
            ./posts
            ./templates
            ./css
            ./static
            (fs.maybeMissing ./images)
          ] ++ lib.optional (visibility == "drafts") (fs.maybeMissing ./drafts));
        };
        nativeBuildInputs = [ generator ];
        LANG = "C.UTF-8";
        DRAFTS = lib.optionalString (visibility == "drafts") "1";
        buildPhase = ''
          site build
          site check --internal-links
        '';
        installPhase = "cp -r _site $out";
      };
    in
    {
      packages.${system} = {
        default = site "published";
        drafts = site "drafts";
        inherit generator;
      };

      apps.${system}.hits = {
        type = "app";
        program = "${generator}/bin/hits";
      };

      checks.${system} = {
        inherit generator;
        site = site "published";
      };

      devShells.${system} = {
        default = pkgs.haskellPackages.shellFor {
          packages = _: [ generator ];
          nativeBuildInputs = [ pkgs.cabal-install pkgs.wrangler ];
        };
        deploy = pkgs.mkShellNoCC {
          packages = [ pkgs.wrangler ];
        };
      };
    };
}
