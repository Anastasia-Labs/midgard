# State Reset and Redeploy Rules

For any Midgard node environment connected to real or persistent Cardano network
state, never wipe, reset, delete, or recreate local durable state without also
performing a full, explicit on-chain redeploy/reset of the corresponding
protocol state. [review]

This includes:

- `docker compose down -v`
- Deleting or recreating Postgres volumes
- Clearing local ledger/MPT databases
- Deleting migration, mempool, block, deposit, admission, finalization, or
  confirmed-ledger state
- Using any reset script that discards local node state

For an authorized fresh deployment, discard the deprecated deployment's local
state. Do not create database dumps, state archives, or copies of its deployment
files unless the user explicitly requests a backup. Preserve active credentials,
shared Cardano/Kupo provider data, unrelated databases, and evidence from the
current run. [review]

If local DBs or durable node state are wiped, the on-chain contracts, reference
scripts, scheduler, state queue, hub oracle, operator lists, and any other
protocol UTxOs must be redeployed from a clean genesis/deployment flow before
running deposits, L2 transactions, commitments, merges, benchmarks, or readiness
checks. [review]

Do not combine a clean local database with previously deployed on-chain protocol
state. That creates unauditable split-brain state and can make deposits,
scheduler alignment, commitments, and finalization appear broken for the wrong
reason. [review]

The only exception is a clearly labeled, read-only forensic diagnostic where no
new deposits, L2 transactions, commitments, merges, or state-changing operations
are submitted.
