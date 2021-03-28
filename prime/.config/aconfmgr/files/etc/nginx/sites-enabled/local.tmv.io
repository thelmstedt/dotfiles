server {
  listen 80 default_server;
  server_name local.tmv.io;
  server_name andy-local.tmv.io;
  proxy_set_header Host       $host;
  rewrite ^/$ http://local.tmv.io/dashboard redirect;

  # WebSocket support (nginx 1.4)
  proxy_http_version 1.1;
  proxy_set_header Upgrade $http_upgrade;
  proxy_set_header Connection "upgrade";

 location /dashboard {
    proxy_pass http://127.0.0.1:13001;
  }

  location /irc {
        proxy_pass http://localhost:11001;
        client_max_body_size 30M;
  }

  location /research {
    proxy_pass http://127.0.0.1:8082;
  }

  location /classification {
    proxy_pass http://127.0.0.1:8083;
  }

  location /tm {
    proxy_pass http://127.0.0.1:8080;
  }

  location /claims-tree {
    proxy_pass http://127.0.0.1:9091;
  }
  location /compass {
    proxy_pass http://127.0.0.1:13002;
  }
  
  location /tmassist {
    proxy_pass http://127.0.0.1:9090;
  }

  location /designs {
    proxy_pass http://127.0.0.1:15090;
  }
}
