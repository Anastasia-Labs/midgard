# Midgard Node – Demo CLI Application

## Installing prerequisites

### To build midgard node you need:

- NVM, Node.js 18+, PNPM 9+

1. Install NVM:

```sh
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.5/install.sh | bash
```

2. Reload your shell (or open a new terminal), then install Node 18:

```sh
nvm install 18
nvm use 18
```

3. Install pnpm:

```sh
npm install -g pnpm
```

4. Verify installation

```sh
pnpm version
```

- Docker and Docker Compose for docker runs

1. Install prerequisites:

```sh
sudo apt-get install ca-certificates curl gnupg
```

2. Add Docker’s official GPG key and repository:

```sh
sudo install -m 0755 -d /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
sudo chmod a+r /etc/apt/keyrings/docker.gpg

echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
```

3. Install Docker Engine and related components:

```sh
sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
```

4. Verify that Docker is installed and running:

```sh
sudo systemctl status docker
```

- Utils for monitoring in local runs without a docker (grafana, prometheus, loki etc):

1. Run [installing script](local-monitoring/install.sh) or see [the guide](local-monitoring/INSTALL.md) for details.

## Prepare environment variables

1. Make environment variable file from example variable file:

```sh
cp .env.example .env
```

2. In `.env` file fill all necessary fields

## Start application

### With docker

1. Install SDK's dependencies:

```sh
cd midgard-sdk
pnpm install
```

2. Clean SDK if needed:

```sh
cd midgard-sdk
pnpm reset
```

3. Clean Node if needed:

```sh
cd ../midgard-node
pnpm clean
```

4. Run Docker daemon if it's not running already:

```sh
sudo dockerd
```

5. Start Services:

```sh
docker-compose up -d
```

This will start:

- Midgard Node
- PostgreSQL
- Prometheus metrics server
- OpenTelemetry collector

6. You can view your containers using `docker`:

```sh
docker ps -a
```

7. You can view logs of `midgard-node` with `docker`:

```sh
# Change container's name as needed:
sudo docker logs -f midgard-node-midgard-node-1
```

If you faced an error regarding `DATABASE_PATH`, use the following command:

```sh
# Optional: You can view your docker images to get the correct name:
docker images

# Delete the last image:
docker image rm midgard-node-midgard-node --force

# And restart the services:
docker-compose up -d
```

Now you should be able to interact with `midgard-node`.

8. If you made any changes to `midgard-node` and had an image running, restart it with the 3 steps:

```sh
docker-compose down -v
docker image rm midgard-node-midgard-node --force
docker-compose up -d
```

### Local run with monitoring

- Run [starting script](local-monitoring/start.sh) or see [the guide](local-monitoring/MANUAL-RUN.md) for details.

### Local run without monitoring

- Build and run all necessary packages:

```sh
# Optional
nix develop

cd ../midgard-sdk
pnpm install
pnpm run repack

cd ../midgard-node
pnpm install
pnpm run listen
```

## Testing

### Docker

- For running tests inside docker container:

```sh
docker-compose run --rm midgard-node-tests
```

### Local run

- For running tests locally:
```sh
cd midgard-node
pnpm test
```