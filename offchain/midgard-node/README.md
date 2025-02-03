# Midgard Node – Demo CLI Application

## How to Run

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

## Build image

### Start docker daemon

``` sh
nix develop
sudo $(which dockerd)
```

### Build & run image

``` sh
SEED_PHRASE="your seed phrase" nix develop
sudo chown --recursive $(whoami):$(whoami) /var/run/docker.sock

docker run --rm --publish 3000:3000 -it -e SEED_PHRASE="$SEED_PHRASE" \
  $(docker build --build-context sdk=../midgard-sdk -q .)
```

### Build & all the services

``` sh
SEED_PHRASE="your seed phrase" HYDRA_SCRIPTS_TX_ID="your_transaction" nix develop
sudo chown --recursive $(whoami):$(whoami) /var/run/docker.sock

docker compose up -d
docker compose ps
```

### Test node

``` sh
curl http://localhost:3000
```

