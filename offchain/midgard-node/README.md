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
nix develop
sudo chown --recursive $(whoami):$(whoami) /var/run/docker.sock
docker run --rm --publish 3000:3000 -it $(docker build -q .)
```

### Test node

``` sh
curl http://localhost:3000 
```
