# Midgard Node – Demo CLI Application

## Running Locally Without Docker

### Prerequisites
- Ubuntu/debian system
- Node.js 18+
- pnpm 9+
- PostgreSQL 15+ running locally
- One L1 provider configured

Optional:
- Nix for dev shell (`nix develop`)

### 1. Prerequisites Installation Guide
1. Install Node.js 18+ and npm
```sh
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt-get install -y nodejs
```
- Verify installation:
```sh
node -v
npm -v
```
Both should output version numbers, with Node.js ≥18 and npm included.

2. Install pnpm 9+
- Install pnpm globally using npm:
```sh
npm install -g pnpm
```
- Verify pnpm version:
```sh
pnpm -v
```
It should be version 9 or higher.

3. Install PostgreSQL 15+ running locally
```sh
sudo apt-get update
sudo apt-get install -y postgresql postgresql-contrib
```
- Start PostgreSQL service (if needed):
```sh
sudo systemctl start postgresql
sudo systemctl enable postgresql
```
- Verify PostgreSQL version:
```sh
psql --version
```
Should display version 15 or above.

- Create user and database (if needed):
```sh
sudo -u postgres createuser postgres
sudo -u postgres createdb midgard -O postgres
```

### 2. Prepare your environment:
Start by copying the .env.example file to create your personal .env file:

```sh
cp .env.example .env
```

Fill in required values:
- L1 providers keys, depending on your L1 provider. If `L1_PROVIDER=Kupmios` then `L1_OGMIOS_KEY` and `L1_KUPO_KEY`. If `L1_PROVIDER=Blockfrost` then `L1_BLOCKFROST_API_URL` and `L1_BLOCKFROST_KEY`. Note that using Blockfrost as a provider can lead to [unstable application behavior](https://github.com/Anastasia-Labs/midgard/issues/277).
- `L1_OPERATOR_SEED_PHRASE`
- `L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX`

### 3. Enter nix shell (optional):
If you use Nix, enter the development shell to ensure dependencies are available:

```sh
nix develop
```

### 4. Build and prepare the SDK:
Navigate to the midgard-sdk directory, install dependencies, and run the repack script:

```sh
cd ../midgard-sdk
pnpm install
pnpm run repack
```

### 5. Build and start the node application:
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
