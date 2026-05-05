# T12 Migration and Assurance

## Scope

Use `lucid-midgard` in existing L2 transfer paths and prove behavior parity or
intentional improvement.

## Dependencies

- T01 through T11.
- Existing `submit-l2-transfer` tests.

## Deliverables

- Replace manual native transfer assembly with `lucid-midgard`.
- Preserve CLI/API behavior where it is still valid.
- Update tests to validate library-built transactions.
- Add examples.
- Add migration notes.

## Acceptance Criteria

- Existing L2 transfer tests pass through the new builder.
- Phase A and Phase B validation still accept generated transfers.
- Fee convergence and selected inputs remain deterministic or changes are
  documented with rationale.
- No unrelated L1/protocol builder migration is included.

## Current Implementation Notes

- `demo/midgard-node/src/commands/submit-l2-transfer.ts` now uses
  `@al-ft/lucid-midgard` for pubkey transfer body construction, deterministic
  wallet input selection, change output creation, fee convergence, and
  Midgard-native body-hash signing.
- The command still owns node UTxO fetching, local database submission, and HTTP
  `/submit` transport because those are node integration concerns.
- The static builder provider used by the command is explicitly diagnosed as a
  fallback context and supplies fee/network facts from `NodeConfig`.
- Existing transfer tests validate zero-fee fixture construction, signed-byte
  fee convergence, Phase A/B acceptance, multi-asset preservation, and mocked
  HTTP submission.

## Required Tests

- Existing `submit-l2-transfer` unit tests.
- Local submission mode test.
- HTTP submission mode test with mocked provider.
- Native tx codec fixture comparison.
- Multi-asset transfer.

## Non-Goals

- Do not refactor deposit, commit, or merge builders in this task.
- Do not add compatibility paths for prior native tx shapes.
