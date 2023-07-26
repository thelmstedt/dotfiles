server {
  listen 80 default_server;
  server_name local.tmv.io;
  server_name andy-local.tmv.io;
  proxy_set_header Host       $host;

  # WebSocket support (nginx 1.4)
  proxy_http_version 1.1;
  proxy_set_header Upgrade $http_upgrade;
  proxy_set_header Connection "upgrade";


   location / {
    proxy_pass https://vpc-dplat-dev-es-hyys4oqbfrl5wlwcumbdmj3eum.us-west-2.es.amazonaws.com:443;
    proxy_redirect off;
 
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header Host $http_host;
 
    # For CORS Ajax
    add_header Access-Control-Allow-Origin *;
    add_header Access-Control-Allow-Credentials true;
    add_header 'Access-Control-Allow-Methods' 'GET, POST, OPTIONS';
    # Route all requests to feeds index
    rewrite ^(.*)/(.*)/(.*) /$1/$2/_search$3 break;
    rewrite_log on;
 
  }

}
