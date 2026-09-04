# Structural N/A claim — Q32 required-signer-set standalone family

WORKED TEMPLATE ARTIFACT. A real claim is a matrix row plus an executable
adversarial test; prose alone is insufficient (GOAL_SPEC.md §9.1). This
concrete structure example is excluded from evidence aggregation.

- Coverage row: `Q32 required signer set`
- Claim kind: `reduces-to validation-machine-v1 signer schedule`
- Normative basis: `GOAL_SPEC.md §9.1, task Q32`

## Executable adversarial evidence

- Test: `midgard/validation_machine_v1 maximum_required_signer_field_terminal_matches_typescript`
  constructs the maximum signer schedule and proves the shared validation
  machine rejects a mismatched terminal summary.
- Command: `cd onchain/aiken && node scripts/run-focused-check.mjs midgard/validation_machine_v1 maximum_required_signer_field_terminal_matches_typescript`
  — collected exactly 1/1.
- Mutation control: weakening the terminal signer-summary equality makes the
  adversarial state pass, proving the test is load-bearing.

## Removal

No standalone required-signer-set family existed. The matrix row is set to
`N/A` and cites the resulting evidence artifact.
