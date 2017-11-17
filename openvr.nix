{ stdenv, fetchurl, cmake, steam }:

with stdenv.lib;

stdenv.mkDerivation rec {
  name = "openvr-${version}";
  version = "1.0.9";

  src = fetchurl {
    url = "https://github.com/ValveSoftware/openvr/archive/v1.0.9.tar.gz";
    sha256 = "33ea807e65971eb92e6cc39946e527050572522f9747284ad6009407ad102bd4";
  };

  nativeBuildInputs = [ cmake ];

  buildInputs = [ steam ];

  outputs = [ "out" ];

  preConfigure =
    ''
      mkdir -p $out/include
      cp headers/* $out/include
    '';

  configureScript = "cmake .";

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    description = "Valve Software OpenVR bindings";
    homepage = https://github.com/ValveSoftware/openvr;
    license = licenses.bsd3;
    platforms = platforms.linux;
    maintainers = with maintainers; [ krakrjak ];
  };
}
