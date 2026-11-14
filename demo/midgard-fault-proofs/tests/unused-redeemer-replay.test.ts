/**
 * The installed unused-redeemer replay. An accepted transaction whose every
 * redeemer a script purpose consumes is left alone; one carrying a redeemer
 * with no purpose, a repeated pointer, or a pointer at a native purpose is
 * detected at that redeemer. A forced transaction carries no redeemers (the
 * admission screen refuses them), so an admitted forced transaction and the
 * node's own forced rejection give the replay nothing, while a rejection
 * naming an UnusedRedeemer the transaction does not have is detected.
 * Rejections of a redeemer-carrying forced transaction exercise the
 * later-reason order of the audit.
 */
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { type LedgerEntry, singleLedgerAt } from "../src/ledger-at.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  buildMissingRedeemerBlock,
  buildMissingRedeemerSubject,
  honestForcedObserverShape,
  machineForcedVerdict,
  missingRedeemerLedgerEntries,
  type MissingRedeemerSource,
  type MissingRedeemerSubjectShape,
  nativeScript,
  plutusScript,
} from "./support/missing-redeemer-emulator.subject.js";
import { scriptSourceMissingVerdict } from "./support/missing-script-source-emulator.js";
import { unusedRedeemerVerdict } from "./support/unused-redeemer-emulator.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/unused-redeemer-replay",
  grade: "security",
} as const;

const DATA = Buffer.from(Data.to(0n), "hex");

/** Every redeemer is consumed: one Plutus observer with its witness. */
const CONSUMED: MissingRedeemerSubjectShape = {
  observers: [{ script: plutusScript(1), redeemer: true }],
};

/** A Reward redeemer at an index no observer occupies. */
const PURPOSELESS: MissingRedeemerSubjectShape = {
  ...CONSUMED,
  paddingRedeemers: [DATA],
};

/** A native observer's Reward pointer: a native script consumes no
 * redeemer. */
const NATIVE_POINTER: MissingRedeemerSubjectShape = {
  observers: [{ script: nativeScript(5n), redeemer: true }],
};

/** Two Spend redeemers at the same script-locked input. */
const REPEATED: MissingRedeemerSubjectShape = {
  observers: [],
  spend: missingRedeemerLedgerEntries(
    0x41,
    [null],
    scriptAddress(plutusScript(9)),
  ),
  witnesses: [plutusScript(9)],
  spendRedeemers: [0n, 0n],
};

const signed = honestForcedObserverShape(5);

/** The honest forced subject: its only observer has no source anywhere and
 * it carries no redeemer. */
const SOURCELESS: MissingRedeemerSubjectShape = {
  ...signed,
  observers: [{ script: plutusScript(5), witness: false, redeemer: false }],
  reference: [],
};

/** A forced transaction carrying a redeemer with no purpose. */
const FORCED_PURPOSELESS: MissingRedeemerSubjectShape = {
  ...signed,
  paddingRedeemers: [DATA],
};

/** A forced transaction whose observer consumes redeemer 0, followed by a
 * redeemer with no purpose at 1. */
const FORCED_CONSUMED_THEN_PURPOSELESS: MissingRedeemerSubjectShape = {
  ...signed,
  observers: [{ script: plutusScript(5), witness: false, redeemer: true }],
  paddingRedeemers: [DATA],
};

const rejected = (reason: SDK.RejectionReason): SDK.OperatorVerdict => ({
  ForcedTxInvalid: { reason },
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
  const decision = await UNUSED_REDEEMER_COMPLETE_CANONICAL_REPLAY.replay(
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

const forced = (verdict: SDK.OperatorVerdict): MissingRedeemerSource => ({
  kind: "forced",
  verdict,
});

describe("unusedRedeemer replay over accepted transactions", () => {
  it("leaves alone a transaction whose every redeemer is consumed", async () => {
    await expect(replayUnder({ kind: "normal" }, CONSUMED)).resolves.toEqual(
      [],
    );
  });

  it.each([
    ["a redeemer with no purpose", PURPOSELESS, "unused-redeemer:0:0:1"],
    ["a native purpose's pointer", NATIVE_POINTER, "unused-redeemer:0:0:0"],
    ["a repeated pointer", REPEATED, "unused-redeemer:0:0:1"],
  ] as const)("detects %s", async (_label, shape, id) => {
    await expect(replayUnder({ kind: "normal" }, shape)).resolves.toEqual([id]);
  });
});

describe("unusedRedeemer replay over an input with no predecessor descriptor", () => {
  // A redeemer with no purpose alongside an unresolved input: the input
  // families convict the leaf, and no source index can be built for it.
  it.each([
    ["spent", { spend: missingRedeemerLedgerEntries(0x51, [null]) }],
    ["referenced", { reference: missingRedeemerLedgerEntries(0x52, [null]) }],
  ] as const)("leaves alone a %s input", async (_label, inputs) => {
    const shape = { ...PURPOSELESS, ...inputs };
    await expect(replayUnder({ kind: "normal" }, shape, [])).resolves.toEqual(
      [],
    );
  });
});

describe("unusedRedeemer replay over forced transactions", () => {
  it("the admission screen refuses a forced transaction carrying a redeemer", () => {
    expect(
      screenMidgardForcedTxAdmission(
        buildMissingRedeemerSubject(FORCED_PURPOSELESS).nativeTx,
      ),
    ).not.toBeNull();
    expect(
      screenMidgardForcedTxAdmission(
        buildMissingRedeemerSubject(SOURCELESS).nativeTx,
      ),
    ).toBeNull();
  });

  it("finds nothing in the node's own rejection of an admitted forced transaction", async () => {
    await expect(machineForcedVerdict(SOURCELESS)).resolves.toStrictEqual(
      scriptSourceMissingVerdict(2n, 0n),
    );
    await expect(
      replayUnder(forced(scriptSourceMissingVerdict(2n, 0n)), SOURCELESS),
    ).resolves.toEqual([]);
  });

  it("finds nothing in the same transaction committed as admitted", async () => {
    await expect(
      replayUnder(forced("ForcedTxValid"), SOURCELESS),
    ).resolves.toEqual([]);
  });

  it("detects an UnusedRedeemer rejection naming a redeemer the transaction lacks", async () => {
    await expect(
      replayUnder(forced(unusedRedeemerVerdict(0n)), SOURCELESS),
    ).resolves.toEqual(["unused-redeemer:1:0:0"]);
  });
});

describe("unusedRedeemer replay over a redeemer-carrying forced transaction", () => {
  it.each([
    [
      "a PlutusExecutionFailed behind an unused redeemer",
      FORCED_PURPOSELESS,
      rejected({
        PlutusExecutionFailed: { purpose_kind: 0n, purpose_index: 0n },
      }),
      ["unused-redeemer:1:0:0"],
    ],
    [
      "an UnusedRedeemer naming a consumed redeemer",
      FORCED_CONSUMED_THEN_PURPOSELESS,
      unusedRedeemerVerdict(0n),
      ["unused-redeemer:1:0:0"],
    ],
    [
      "an UnusedRedeemer naming a later redeemer than the first unused one",
      {
        ...FORCED_PURPOSELESS,
        paddingRedeemers: [DATA, DATA],
      },
      unusedRedeemerVerdict(1n),
      ["unused-redeemer:1:0:0"],
    ],
  ] as const)("detects %s", async (_label, shape, verdict, ids) => {
    await expect(replayUnder(forced(verdict), shape)).resolves.toEqual(ids);
  });

  it.each([
    [
      "an UnusedRedeemer naming the first unused redeemer",
      unusedRedeemerVerdict(0n),
    ],
    [
      "a reason recorded before the audit",
      rejected("ScriptIntegrityHashMissing"),
    ],
  ] as const)("leaves alone %s", async (_label, verdict) => {
    await expect(
      replayUnder(forced(verdict), FORCED_PURPOSELESS),
    ).resolves.toEqual([]);
  });
});
