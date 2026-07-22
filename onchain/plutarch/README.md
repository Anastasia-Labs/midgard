# Midgard Plutarch Merkle Helpers

Status: Legacy supporting package. Aiken under `onchain/aiken` is the primary
on-chain implementation; this package supplies Plutarch Merkle Patricia
Forestry helpers and generated membership/non-membership scripts used by proof
work.

Last reviewed: 2026-07-22

## Code map

- `src/Validators/Membership.hs`: membership and non-membership validators.
- `src/Types/`: block, state-commitment, membership, and shared Plutarch types.
- `tests/Testing/`: MPF, validator, transaction-proof, crypto, and evaluation
  tests.
- `generated/`: generated Plutus JSON for the membership staking scripts.
- `app/Main.hs`: script-generation executable.

## Build and test

The package is built independently from the Aiken project. From this directory:

```sh
nix develop
cabal build all
cabal test all --test-show-details=direct
```

The Plutarch suite is not currently part of the primary Midgard node CI
workflow. A proof or release claim that depends on these helpers must record a
successful run and verify the generated script hashes against the deployment
manifest. Regenerating files under `generated/` requires review of the resulting
script bytes and hashes; do not treat generated changes as formatting output.

## Scope and safety

These helpers prove MPF membership properties. They do not establish complete
Midgard fraud-proof coverage, data availability, transition validity, or safe
challenge timing. See `../../docs/fault-proofs/` for the current coverage audit.

## License

See [LICENSE](LICENSE) for the MIT license.
