#! /bin/sh

echo "Writing config.yml"
envsubst < /app/config/settings.example.yml > /app/config/settings.yml
echo "Done"
