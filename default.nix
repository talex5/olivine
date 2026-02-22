{ pkgconf, vulkan-loader, libffi, ocamlPackages }:

ocamlPackages.buildDunePackage {
  pname = "olivine";
  version = "0.1-tal";

  src = ./.;

  nativeBuildInputs = [
    pkgconf
    ocamlPackages.menhir
  ];

  propagatedBuildInputs = [
    vulkan-loader libffi
  ] ++ (with ocamlPackages; [
    dune-configurator ppxlib xmlm fmt ctypes-foreign ctypes
  ]);
}

