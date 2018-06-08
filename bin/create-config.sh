#! /bin/sh

echo "Writing config.yml"
cat /app/config/settings.example.yml
envsubst < /app/config/settings.example.yml > /app/config/settings.yml
cat /app/config/settings.yml
echo "Done"
