# Midgard Node – Demo CLI Application

## Running Locally Without Docker

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
