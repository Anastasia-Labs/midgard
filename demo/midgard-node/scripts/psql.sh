#!/bin/bash

# Load environment variables from .env file
if [ -f .env ]; then
    export $(cat .env | grep -v '^#' | xargs)
fi

# Get container name dynamically
CONTAINER_NAME=$(docker ps --filter name=postgres --format "{{.Names}}")

if [ -z "$CONTAINER_NAME" ]; then
    echo "PostgreSQL container not found. Make sure it's running."
    exit 1
fi

echo "Connecting to PostgreSQL with user: ${POSTGRES_USER}"
docker exec -it $CONTAINER_NAME psql -U "${POSTGRES_USER}" -d "${POSTGRES_DB}"
