{
  description = "OCaml bindings for Vulkan";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";
  };

  outputs = { self, nixpkgs }:
  let eachSystem = nixpkgs.lib.genAttrs ["x86_64-linux" "aarch64-linux"]; in {
    packages = eachSystem(system:
    let pkgs = nixpkgs.legacyPackages.${system}; in {
      default = pkgs.callPackage (import ./default.nix) {};
    });
  };
}
