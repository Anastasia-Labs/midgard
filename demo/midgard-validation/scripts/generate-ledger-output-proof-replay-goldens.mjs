#!/usr/bin/env node

/**
 * Produces the ledger output proof replay golden vectors: for every row of
 * `tests/fixtures/ledger-output-proof-replay.vectors.mjs`, the output the
 * TypeScript codec encodes, the witness list and terminal control the
 * TypeScript trace builder (`buildMidgardLedgerOutputProofTrace`) emits for
 * it, the hashes of both, and the scalar claims the step planner
 * (`@al-ft/midgard-fault-proofs`) derives on the way. They are written to two
 * places:
 *
 *   * `tests/fixtures/ledger-output-proof-replay.generated.json` — recomputed
 *     by `tests/ledger-output-proof-empty-span.test.ts` and
 *     `tests/ledger-output-proof-split-span.test.ts`, which drive the same
 *     rows through `src/` and assert equality, so a drifting builder or
 *     encoder fails on the TypeScript side; and
 *   * named constants of the hand-written
 *     `onchain/aiken/lib/midgard/ledger-output-proof-datum.test.ak` (the
 *     output, the witness list, the terminal control hash and the step
 *     planner's scalar claims), rebound in
 *     place by name, where the typed and split on-chain steps replay each
 *     witness list to the pinned terminal control.
 *
 * For every refusal row it also writes the step count and refused scan
 * control at which the TypeScript output scan refuses the output; the Aiken
 * test scans the same output and refuses it at the same step and control.
 *
 * A row whose producer no longer exhibits the row's hand-written claims (step
 * count, empty-span steps, attach shapes, output length) is refused rather
 * than emitted.
 *
 * The plumbing — the `--check` contract and the by-name rebind — is the
 * shared `@al-ft/midgard-core/scripts/golden-channel.mjs`; only what is
 * computed lives here. `--check` asserts the checked-in artifacts are exactly
 * what the twins produce today and never repairs them.
 *
 * usage: node scripts/generate-ledger-output-proof-replay-goldens.mjs [--check]
 */

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  buildMidgardLedgerOutputProofTrace,
  encodeMidgardLedgerOutputProofControl,
  encodeMidgardLedgerOutputScanControl,
  isExactMidgardLedgerOutputProofTerminal,
  MidgardLedgerOutputProofStages,
  nextMidgardCekDataTraverseSpan,
  traceMidgardLedgerOutputScan,
} from "@al-ft/midgard-core";
import {
  decodeMidgardDatum,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import {
  deriveLedgerOutputProofStepClaims,
  ledgerOutputProofDatumRoleIndex,
} from "@al-ft/midgard-fault-proofs";
import {
  bytes,
  goldenChannelEmitter,
  parseGoldenChannelArguments,
  rebindAikenConstants,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";

import {
  plutusDataToCborHex,
  validationAuxiliaryWitnessData,
} from "../dist/index.js";
// The rows live beside the fixture they produce, not here: the vitest suites
// drive the *same* rows through `src/` while this script drives them through
// `dist/`, so a builder that drifts is caught on both sides rather than only
// by `--check`.
import {
  LEDGER_OUTPUT_PROOF_REPLAY_ROWS,
  LONG_ASSET_NAME_REFUSAL_ROWS,
  ledgerOutputProofRefusalClaimProblems,
  ledgerOutputProofRefusalVector,
  ledgerOutputProofReplayClaimProblems,
  ledgerOutputProofReplayVector,
} from "../tests/fixtures/ledger-output-proof-replay.vectors.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/ledger-output-proof-replay.generated.json",
);
const aikenModule =
  "onchain/aiken/lib/midgard/ledger-output-proof-datum.test.ak";

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-ledger-output-proof-replay-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

/**
 * The public encoding of a ledger output proof step witness: the field of the
 * `ledgerOutputProofStep` auxiliary witness (`constr 32 [witness]`).
 */
const witnessData = (witness) =>
  validationAuxiliaryWitnessData({ kind: "ledgerOutputProofStep", witness })
    .fields[0];

const twins = {
  buildMidgardLedgerOutputProofTrace,
  decodeMidgardDatum,
  deriveLedgerOutputProofStepClaims,
  encodeMidgardLedgerOutputProofControl,
  encodeMidgardLedgerOutputScanControl,
  encodeMidgardTxOutput,
  isExactMidgardLedgerOutputProofTerminal,
  ledgerOutputProofDatumRoleIndex,
  MidgardLedgerOutputProofStages,
  nextMidgardCekDataTraverseSpan,
  plutusDataToCborHex,
  traceMidgardLedgerOutputScan,
  witnessData,
};

const vectors = LEDGER_OUTPUT_PROOF_REPLAY_ROWS.map(({ row, family }) => {
  const vector = ledgerOutputProofReplayVector(row, family, twins);
  const problems = ledgerOutputProofReplayClaimProblems(row, vector);
  if (problems.length > 0) {
    throw new Error(
      `the producer no longer exhibits a row's claims:\n${problems.join("\n")}`,
    );
  }
  return { family, vector };
});

const refusals = LONG_ASSET_NAME_REFUSAL_ROWS.map((row) => {
  const vector = ledgerOutputProofRefusalVector(row, twins);
  const problems = ledgerOutputProofRefusalClaimProblems(row, vector);
  if (problems.length > 0) {
    throw new Error(
      `the producer no longer exhibits a refusal row's claims:\n${problems.join("\n")}`,
    );
  }
  return vector;
});

const golden = {
  generator:
    "demo/midgard-validation/scripts/generate-ledger-output-proof-replay-goldens.mjs",
  aikenModule,
  rows: vectors.map(({ family, vector }) => ({ family, ...vector })),
  refusals,
};

/**
 * The Aiken constants of one row: its witness list, terminal control hash and
 * planner scalar claims, and for an empty-field or long-asset-name row also
 * its output (a datum-scalar row's output is built in the Aiken test from its
 * structure).
 */
const rowConstants = ({ family, vector }) => ({
  ...(family === "emptyField" || family === "longAssetNames"
    ? { [`${vector.id}_output`]: bytes(vector.outputCborHex) }
    : {}),
  [`${vector.id}_witnesses`]: bytes(vector.witnessesCborHex),
  [`${vector.id}_terminal_hash`]: bytes(vector.terminalHash),
  [`${vector.id}_planner_claims`]: bytes(vector.plannerClaimsCborHex),
});

/**
 * The Aiken constants of one refusal row: its output, the number of steps the
 * scan accepts and the hash of the control whose next step it refuses.
 */
const refusalConstants = (vector) => ({
  [`${vector.id}_output`]: bytes(vector.outputCborHex),
  [`${vector.id}_accepted_steps`]: vector.acceptedSteps,
  [`${vector.id}_refused_control_hash`]: bytes(vector.refusedControlHash),
});

writeOrCheck(generatedJsonPath, `${JSON.stringify(golden, null, 2)}\n`);
const aikenPath = join(repositoryRoot, aikenModule);
writeOrCheck(
  aikenPath,
  rebindAikenConstants({
    source: readFileSync(aikenPath, "utf8"),
    constants: Object.assign(
      {},
      ...vectors.map(rowConstants),
      ...refusals.map(refusalConstants),
    ),
  }),
);
