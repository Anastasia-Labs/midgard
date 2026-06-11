# T01 Package Scaffold

## Scope

Create the initial `lucid-midgard` package location, build configuration,
exports, and test harness. The scaffold should support TypeScript ESM output
and type declarations.

## Dependencies

- Architecture docs `00`, `01`, and `02`.
- Current package conventions in `demo/midgard-sdk` and `demo/midgard-node`.

## Deliverables

- Package directory and `package.json`.
- `tsconfig.json`.
- `tsup` or equivalent build config.
- `src/index.ts` export surface.
- Test config.
- README with minimal install/import example.

## Acceptance Criteria

- `pnpm build` succeeds for the new package.
- `pnpm test` runs an empty or minimal test suite.
- The package exports no unstable internals by default.
- Dependencies are explicit and do not import node database modules.

## Required Tests

- Import smoke test.
- Type declaration generation check.

## Non-Goals

- No transaction building logic.
- No provider implementation beyond placeholder interfaces.
