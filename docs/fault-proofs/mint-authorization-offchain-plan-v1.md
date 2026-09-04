# Mint-authorization fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `mintAuthorization` (`0000001b`). Generic Init, deployment
inspection/identity, and all five mandatory reference scripts are wired.
Typed family modules exist; family-specific CLI verbs, autonomous watcher
actuation, and live/preprod evidence remain open.

Canonical V1 includes mint and burn. This family covers the deterministic
single-party native-policy leg. Plutus policy execution remains on the
interactive validation-dispute/CEK path.

## Fault statement

An operator-accepted committed transaction contains a non-ADA mint/burn entry
for policy `H`, but authorization is unsatisfied in one of two ways:

1. no script source with versioned hash `H` exists in the transaction witnesses
   or resolved reference-input scripts; or
2. a native script whose versioned hash is `H` evaluates to false against the
   committed signer frontier and validity interval.

ADA is structurally unmintable in the canonical mint grammar. The family does
not treat malformed/guardrail-exceeded native scripts as unauthorized; those
belong to `nativeScriptDecoding`.

## On-chain chain

The five-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/mint-authorization/`
- `onchain/aiken/lib/midgard/fraud-proofs/mint-authorization/`

1. Bind an operator-accepted native transaction through the authenticated
   counted `transactions_root`.
2. Open the mint field, select the policy, and bind the prior ledger root and
   claim direction.
3. Either prove absence from script witnesses or authenticate and evaluate the
   claimed unsatisfied native script.
4. For the absence direction, scan all resolved reference-input scripts.
5. Finalize the closed verdict, burn the computation thread, and mint the
   permanent fault-proof token.

The script-source surface matches the validation machine: transaction script
witnesses plus reference scripts on resolved reference inputs. Native
evaluation uses the same signature/timelock/container semantics and bounds as
the canonical machine. Every step supports explicit prover cancellation.

## Off-chain surfaces

- SDK schema: `demo/midgard-sdk/src/fraud-proof/mint-authorization.ts`
- family implementation: `demo/midgard-fault-proofs/src/mint-authorization/`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

The package provides evidence/finding records, a proving core, Init/step/cancel
submitters, and field-carriage support. Registration is part of the canonical
deployment identity.

## Verification status

Focused Aiken selectors cover acceptance binding, policy selection, source
absence, reference-input scanning, native-script evaluation, terminal
finalization, and honest-transaction refusal. Emulator suites cover both claim
directions, adversarial polarity, direct/published carriage, permanent mint,
and faulty-block removal:

- `submit-init-emulator-mint-authorization-direction-a-lifecycle.test.ts`
- `submit-init-emulator-mint-authorization-direction-b-lifecycle.test.ts`
- `submit-init-emulator-mint-authorization-adversarial.test.ts`
- `submit-init-emulator-mint-authorization-size-forced-carriage.test.ts`

## Remaining work

- exercise the cancel submitter and the remaining large-field carriage paths;
- expose operational CLI/workflow commands;
- mount watcher detection/proving and publish preprod/live evidence;
- retain separate release evidence for the Plutus/CEK policy path;
- rerun its maximum-shape lifecycle under the shared Van Rossem emulator limits
  whenever compiler, blueprint, field bounds, or Cardano protocol limits
  change.
