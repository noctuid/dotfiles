self: super: {
  font-awesome-latest = super.font-awesome.overrideAttrs (old: {
    src = super.fetchFromGitHub {
      owner = "FortAwesome";
      repo = "Font-Awesome";
      rev = "6.4.0";
      hash = "sha256-SUxXm29SBU4T2n1x2dD2ECPEJF1TX82ORq9RHT7CslY=";
    };
  });
}
