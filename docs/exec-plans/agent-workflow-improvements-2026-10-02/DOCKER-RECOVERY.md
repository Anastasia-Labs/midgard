# Docker recovery planning

Observed 2026-10-03 at approximately 23:00 UTC. Recovery planning only: no
container stop, Docker Desktop restart, WSL shutdown, reset, prune or redeploy
was performed or authorized by this request.

## Observed failure

`/usr/bin/docker` points into Docker Desktop's WSL CLI mount. Its target was
present, but executing or reading its header failed with an I/O error. The
mounted CLI image was read-only ISO9660 on `loop0`; its backing file belonged
to Docker Desktop's cached `docker-wsl-cli.iso`. A five-second Unix-socket
`GET /_ping` to `/var/run/docker.sock` timed out without response bytes.
Kernel logs contained repeated read errors for `loop0` and `sdf`. The checkout
was on a different device, `sdd`, mounted as ext4.

This establishes an unreadable CLI image mount and an unresponsive engine
endpoint. It does not establish hardware corruption, the underlying cause,
or the health of individual containers. The unreachable daemon prevents a
trustworthy current container census; Compose declarations are not that census.

## Gates and checkpoint

Contributor builds, focused tests, emulator suites, artifact checks, receipts,
docs/spec checks, native Rust/Go compilation and full local preflight use no
Docker. Postgres-backed gates use the native shared instance on port 5433.
Docker images, actual container devnets and container-backed live acceptance
need a healthy engine. Hosted image CI uses its own engine and is unaffected
by this local endpoint failure. Repair is needed for those local Docker tickets,
not for the current contributor verification program.

The implementation has durable local Git checkpoints, including the reviewed
output-owner fix `87a831afb9f597d4ef68c4cb61e3940d589d7494`. A remote backup
exists only after the delivery branch is successfully pushed; local commits
must not be described as a remote checkpoint. Preserve each complete receipt
directory and log alongside source before disruptive recovery.

## Restart impact

| Scope                  | Expected affected work                                                                                                                                                          |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Docker Desktop restart | Desktop-managed containers and their provider/CLI consumers; in-flight container tests and live acceptance can lose their connections. Exact running inventory remains unknown. |
| Broader WSL shutdown   | Every running WSL distribution, including native Postgres, node runtimes, editor workers and current build/test processes.                                                      |

At observation, native Postgres PID 25159 listened on 5433 and used
`/home/gumbo/.midgard-pg/5433`. Several native node listeners were active on
7651, 7653–7655, 8291–8295 and 15671–15673; their complete owner attribution
was not established. An inactive system Postgres unit does not mean this
custom-managed instance is down. The shared Postgres owner reports `fsync=off`,
so a broad shutdown requires that owner's coordinated clean database shutdown.

## Prerequisites for safe recovery

Inventory and coordinate the existing service owners and their durable
deployment identities first. Checkpoint work, preserve source and evidence,
and obtain explicit restart authorization. Before a broad WSL recovery, join
running jobs, quiesce writes and have the database owner perform a clean stop.
Preserve all VHDs, volumes, journals, chain stores, pending intents and watcher
cursors. Resume the existing database directory and deployment identities;
reinitialization, pruning, resets and redeployment are separate actions.

Docker documents its WSL integration in
[Docker Desktop WSL guidance](https://docs.docker.com/desktop/features/wsl/).
Microsoft describes the all-distribution scope of
[`wsl --shutdown`](https://learn.microsoft.com/en-us/windows/wsl/basic-commands#shutdown).
