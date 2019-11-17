#! /bin/sh

docker build -t brechtserckx/secret-santa .
docker push brechtserckx/secret-santa
heroku container:push web
heroku container:release web
