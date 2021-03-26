{
  network.description = "colorado";
  isreal-swan = {
    deployment = {
      targetHost     = "104.131.28.246";
      alwaysActivate = true;
    };
    imports = [
      ./deploy/configuration.nix
      ./module.nix
      ./service.nix
      ./base.nix
    ];
  };

}
