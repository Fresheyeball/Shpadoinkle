{
  pkgs             ? import <nixpkgs> {},
  port             ? 8080,
  imgName          ? "myImage",
  internalPort     ? 9999,
  extraNginxConfig ? "",
  extraArgs        ? "",
  client, server,
  contents         ? [],
  setup            ? "",
  command          ? "",
  env              ? []
}:


let


  inherit (pkgs.dockerTools) buildImage shadowSetup;


  nginxConf = ''
    user nginx nginx;
    daemon off;
    error_log /dev/stdout info;
    pid /dev/null;
    events {}
    http {
      access_log /dev/stdout;
      proxy_cache_path /tmp/cache-app levels=1:2 keys_zone=app_cache:10m max_size=10g inactive=60m use_temp_path=off;
      server {
        listen ${toString port};
        location ~* .(jpe?g|svn|png|gif|ico|webmanifest)$ {
          proxy_cache app_cache;
          try_files ${client}/$uri ${client}/$uri.html ${client}/$uri/ =404;
        }
        location / {
          proxy_pass http://127.0.0.1:${toString internalPort};
        }
      }
    }
    ${extraNginxConfig}
  '';


in buildImage {
  name = imgName;
  tag  = "latest";
  contents = contents ++ [ pkgs.nginx ];

  runAsRoot = ''
    #!${pkgs.stdenv.shell}
    ${shadowSetup}
    mkdir -p var/log/nginx
    mkdir -p var/cache/nginx
    mkdir -p /tmp/cache-app
    groupadd --system nginx
    useradd --system --gid nginx nginx
    cat >/conf/nginx.conf <<EOF
${nginxConf}
EOF
    ${setup}
  '';

  config = {
    Env = env;
    Cmd =  if command == "" then [ "${pkgs.bash}/bin/bash" "-c"
      "nginx -c ${nginxConf} & ${server} --port ${toString internalPort} ${extraArgs}"]
      else [ "${pkgs.writeShellScript "cmd.sh" command}" ];
    ExposedPorts = {
      "${toString port}/tcp" = {};
    };
  };
}
