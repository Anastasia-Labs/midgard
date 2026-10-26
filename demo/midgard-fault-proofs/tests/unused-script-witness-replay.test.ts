/**
 * The installed unused-script-witness replay. An accepted transaction whose
 * every field-6 witness is the executed source of its script hash is left
 * alone; one carrying a witness no purpose executes, or a second copy of a
 * used witness, is detected at that witness. A transaction spending an input
 * with no predecessor descriptor gives no detection and the replay of the
 * block completes. Forced rejections are detected exactly when the recorded
 * reason contradicts the unused-witness audit.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { type LedgerEntry, singleLedgerAt } from "../src/ledger-at.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  buildMissingRedeemerBlock,
  machineForcedVerdict,
  missingRedeemerLedgerEntries,
  type MissingRedeemerSource,
  type MissingRedeemerSubjectShape,
  plutusScript,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  expectedHash,
  honestForcedShape,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import { holdingNativeScript } from "./support/unused-script-witness-emulator.subject.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/unused-script-witness-replay",
  grade: "security",
} as const;

/** One Plutus observer with its witness and redeemer: every witness used. */
const USED: MissingRedeemerSubjectShape = {
  observers: [{ script: plutusScript(1), redeemer: true }],
};

/** A second witness whose hash no purpose names. */
const EXTRA: MissingRedeemerSubjectShape = {
  ...USED,
  witnesses: [plutusScript(2)],
};

/** The observer's witness carried twice: the second copy is not the source
 * selected for the hash. */
const REPEATED: MissingRedeemerSubjectShape = {
  ...USED,
  witnesses: [plutusScript(1)],
};

/** An admitted forced shape (a native-only key spend) carrying one native
 * witness that holds and that no purpose executes. */
const FORCED_UNUSED: MissingRedeemerSubjectShape = {
  ...honestForcedShape(expectedHash()),
  observers: [],
  witnesses: [holdingNativeScript(2)],
};

const rejected = (reason: SDK.RejectionReason): SDK.OperatorVerdict => ({
  ForcedTxInvalid: { reason },
});

const forced = (verdict: SDK.OperatorVerdict): MissingRedeemerSource => ({
  kind: "forced",
  verdict,
});

/** Installed replay of the block committing `shape` from `source`. */
const replayUnder = async (
  source: MissingRedeemerSource,
  shape: MissingRedeemerSubjectShape,
  /** The predecessor ledger; every spent and referenced entry by default. */
  ledger: readonly LedgerEntry[] = [
    ...(shape.spend ?? []),
    ...(shape.reference ?? []),
  ],
) => {
  const built = await buildMissingRedeemerBlock({ shape, source, ledger });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({
      header: built.block.header,
      headerHash: built.headerHash,
    }),
    payloadEnvelopeCbor: built.block.reconstruction.payloadEnvelopeCbor,
    daProvenance,
  });
  const decision = await UNUSED_SCRIPT_WITNESS_COMPLETE_CANONICAL_REPLAY.replay(
    evidence,
    (shape.spend ?? []).length + (shape.reference ?? []).length === 0
      ? {}
      : {
          predecessor: await admitCompleteCanonicalReplayPredecessor({
            ledgerAt: singleLedgerAt(evidence.header.prevHeaderHash, ledger),
            currentEvidence: evidence,
          }),
        },
  );
  return decision.detections.map((detection) => detection.detectionId);
};

describe("unusedScriptWitness replay over accepted transactions", () => {
  it("leaves alone a transaction whose every witness is used", async () => {
    await expect(replayUnder({ kind: "normal" }, USED)).resolves.toEqual([]);
  });

  it.each([
    ["a witness no purpose executes", EXTRA],
    ["a second copy of a used witness", REPEATED],
  ] as const)("detects %s", async (_label, shape) => {
    await expect(replayUnder({ kind: "normal" }, shape)).resolves.toEqual([
      "unused-script-witness:0:0:1",
    ]);
  });
});

describe("unusedScriptWitness replay over an input with no predecessor descriptor", () => {
  // An unused witness beside an input the predecessor ledger lacks: the input
  // families convict the leaf, and no purpose walk can resolve the spend.
  it.each([
    ["spent", { spend: missingRedeemerLedgerEntries(0x51, [null]) }],
    ["referenced", { reference: missingRedeemerLedgerEntries(0x52, [null]) }],
  ] as const)(
    "declines a %s input and completes the block's replay",
    async (_label, inputs) => {
      const shape = { ...EXTRA, ...inputs };
      await expect(replayUnder({ kind: "normal" }, shape, [])).resolves.toEqual(
        [],
      );
    },
  );
});

describe("unusedScriptWitness replay over forced transactions", () => {
  it("detects an admitted forced transaction carrying an unused witness", async () => {
    await expect(
      replayUnder(forced("ForcedTxValid"), FORCED_UNUSED),
    ).resolves.toEqual(["unused-script-witness:1:0:0"]);
  });

  it("finds nothing in the node's own rejection of that transaction", async () => {
    const verdict = await machineForcedVerdict(FORCED_UNUSED);
    expect(verdict).toStrictEqual(
      rejected({ UnusedScriptWitness: { script_index: 0n } }),
    );
    await expect(replayUnder(forced(verdict), FORCED_UNUSED)).resolves.toEqual(
      [],
    );
  });

  it.each([
    [
      "an UnusedScriptWitness naming a witness field 6 does not carry",
      rejected({ UnusedScriptWitness: { script_index: 1n } }),
      ["unused-script-witness:1:0:0"],
    ],
    [
      "a reason recorded after the audit",
      rejected("ValueNotPreserved"),
      ["unused-script-witness:1:0:0"],
    ],
    ["a reason recorded before the audit", rejected("FeeBelowMinimum"), []],
  ] as const)("on %s", async (_label, verdict, ids) => {
    await expect(replayUnder(forced(verdict), FORCED_UNUSED)).resolves.toEqual(
      ids,
    );
  });
});
