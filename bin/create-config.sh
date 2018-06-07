#! /bin/sh

echo "Writing config.yml"
envsubst < ../config/settings.example.yml > ../config/settings.yml
echo "Done"
