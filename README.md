# Nginx

Here's an nginx config fragment to make things work.

    location /triples/api/join {
        proxy_pass http://127.0.0.1:8080/api/join;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";

        # go websocket origin check
        proxy_set_header Host $host;
    }

    location /triples/ {
        proxy_pass http://127.0.0.1:8080/;
    }
