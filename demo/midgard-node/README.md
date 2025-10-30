# Midgard Node – Demo CLI Application

## Running Locally Without Docker

### Prerequisites
- Node.js 18+
- pnpm 9+
- PostgreSQL 15+ running locally
- One L1 provider configured

Optional:
- Nix for dev shell (`nix develop`)

### 1. Prepare your environment:
Start by copying the .env.example file to create your personal .env file:

```sh
cp .env.example .env
```

Fill in required values:
- L1 providers keys, depending on your L1 provider. If `L1_PROVIDER=Kupmios` then `L1_OGMIOS_KEY` and `L1_KUPO_KEY`. If `L1_PROVIDER=Blockfrost` then `L1_BLOCKFROST_API_URL` and `L1_BLOCKFROST_KEY`. Note that using Blockfrost as a provider can lead to unstable application behavior.
- `L1_OPERATOR_SEED_PHRASE`
- `L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX`

Verify PostgreSQL is installed and the service is active on your system (typically accessible at localhost:5432). Create the required user and database if not done yet.
Example to create user and database:

```sh
sudo -u postgres createuser postgres
sudo -u postgres createdb midgard -O postgres
```


### 2. Enter nix shell (optional):
If you use Nix, enter the development shell to ensure dependencies are available:

```sh
nix develop
```

### 2. Build and prepare the SDK:
Navigate to the midgard-sdk directory, install dependencies, and run the repack script:

```sh
cd ../midgard-sdk
pnpm install
pnpm run repack
```

### 3. Build and start the node application:
Move to the midgard-node directory, install dependencies, and start the service:

```sh
cd ../midgard-node
pnpm install
pnpm run listen
```

Note:
If this is not your first time building the app and you see build errors, delete the `node_modules` and `dist` folders in both `midgard-sdk` and `midgard-node` folders, then run the above commands again.

## Build image

### Start docker daemon

```sh
nix develop
sudo $(which dockerd)
```

### Build & run image

```sh
SEED_PHRASE="your seed phrase" nix develop
sudo chown --recursive $(whoami):$(whoami) /var/run/docker.sock

docker run --rm --publish 3000:3000 -it -e SEED_PHRASE="$SEED_PHRASE" \
  $(docker build --build-context sdk=../midgard-sdk -q .)
```

### Test node

```sh
curl http://localhost:3000
```

### Testing

For local testing run

```sh
pnpm test
```

For testing inside docker container run

```sh
docker-compose run --rm midgard-node-tests
```
