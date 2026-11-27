# Contract Changes and Deployment Parameters

Read this guidance before changing validators, applying deployment parameters,
or editing the builders and fixtures that deploy them.

## Trusted Parameters

On-chain code trusts deployment parameterization: a validator never re-checks
a `validator main(...)` parameter's width, cardinality, or domain. Assert these
facts off chain where the parameter is applied. Fault-proof parameter application
lives in `demo/midgard-sdk/src/fraud-proof/contracts/blueprint.ts`. Blind spot:
its arity and schema-shape guard covers blueprints applied through that function;
a validator parameterized by any other path is unchecked.
[runtime: applyBlueprintParams]

## Scenario Coverage

Provide lucid-evolution emulator scenarios for every contract: a successful happy
path and rejection where the validator must refuse. These scenarios verify that
the deployed, parameterized validator behaves as intended. [review]

When validator parameters are added, removed, reordered, or retyped, update every
affected off-chain builder, parameter application, deployment fixture, and
emulator scenario in the same change. Re-run the affected scenarios in both
polarities. A dedicated arity check does not replace behavior tests. [review]

Completion requires passing positive and negative scenarios using the updated
deployment path and reporting exactly which checks ran. [review]

## Builds

When compiling or debugging Aiken contracts, use
`.agents/skills/aiken-contract-build/SKILL.md` for the pinned compiler, build
environment, trace settings, and focused test commands.
