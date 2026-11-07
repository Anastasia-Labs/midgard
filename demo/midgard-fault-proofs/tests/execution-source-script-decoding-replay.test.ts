/**
 * The installed execution-source-script-decoding replay. A forced rejection
 * recording any native script decoding arm at an execution purpose is
 * detected; the node's own rejection, a forced acceptance and an accepted
 * transaction are left alone. The terminal reads the recorded reason alone,
 * so a rejection recording a decoding arm is detected even when its inputs
 * have no predecessor descriptor, alone and beside the input-existence
 * family, which still convicts an accepted transaction spending such an
 * input. A rejection whose output or mint item does not decode is still
 * detected.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { type LedgerEntry, singleLedgerAt } from "../src/ledger-at.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  type CompleteCanonicalReplay,
  createCompleteCanonicalReplayUnion,
  EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  buildMissingRedeemerBlock,
  honestForcedObserverShape,
  machineForcedVerdict,
  type MissingRedeemerSource,
  type MissingRedeemerSubjectShape,
} from "./support/missing-redeemer-emulator.subject.js";
import { unreadableItemViolations } from "./support/unreadable-item-block.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/execution-source-script-decoding-replay",
  grade: "security",
} as const;

const ESSD = EXECUTION_SOURCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY;

const SHAPE = honestForcedObserverShape(5);

const coordinate = { purpose_kind: 2n, purpose_index: 0n };

const rejected = (reason: SDK.RejectionReason): MissingRedeemerSource => ({
  kind: "forced",
  verdict: { ForcedTxInvalid: { reason } },
});

const ARMS = [
  ["ExecutionNativeScriptMalformed", "execution-native-script-malformed"],
  ["ExecutionNativeScriptNodeLimit", "execution-native-script-node-limit"],
  ["ExecutionNativeScriptDepthLimit", "execution-native-script-depth-limit"],
] as const;

const recording = (arm: (typeof ARMS)[number][0]) =>
  rejected({ [arm]: coordinate } as SDK.RejectionReason);

/** The installed replay's detections for the block committing `shape` from
 * `source`, against `ledger` as the predecessor. */
const replayUnder = async (
  replayer: CompleteCanonicalReplay,
  source: MissingRedeemerSource,
  shape: MissingRedeemerSubjectShape,
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
  const decision = await replayer.replay(evidence, {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      ledgerAt: singleLedgerAt(evidence.header.prevHeaderHash, ledger),
      currentEvidence: evidence,
    }),
  });
  return decision.detections.map((detection) =>
    replayer === ESSD ? detection.detectionId : detection.violationId,
  );
};

describe("executionSourceScriptDecoding replay over forced transactions", () => {
  it.each(ARMS)("detects a rejection recording %s", async (arm, id) => {
    await expect(replayUnder(ESSD, recording(arm), SHAPE)).resolves.toEqual([
      `${id}:1:0`,
    ]);
  });

  it("finds nothing in the node's own rejection of the subject", async () => {
    const verdict = await machineForcedVerdict(SHAPE);
    expect(verdict).toStrictEqual({
      ForcedTxInvalid: { reason: { RedeemerMissing: coordinate } },
    });
    await expect(
      replayUnder(ESSD, { kind: "forced", verdict }, SHAPE),
    ).resolves.toEqual([]);
  });

  it.each([
    [
      "an ExecutionNativeScriptFalse rejection",
      rejected({ ExecutionNativeScriptFalse: coordinate }),
    ],
    ["a forced acceptance", { kind: "forced", verdict: "ForcedTxValid" }],
    ["an accepted transaction", { kind: "normal" }],
  ] as const)("finds nothing in %s", async (_label, source) => {
    await expect(replayUnder(ESSD, source, SHAPE)).resolves.toEqual([]);
  });
});

describe("executionSourceScriptDecoding replay over inputs with no predecessor descriptor", () => {
  const UNION = () =>
    createCompleteCanonicalReplayUnion([
      NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
      ESSD,
    ]);

  it.each(ARMS)(
    "detects a rejection recording %s and the block's replay completes",
    async (arm, id) => {
      await expect(
        replayUnder(ESSD, recording(arm), SHAPE, []),
      ).resolves.toEqual([`${id}:1:0`]);
      await expect(
        replayUnder(UNION(), recording(arm), SHAPE, []),
      ).resolves.toEqual([id]);
    },
  );

  it("the input-existence family still convicts an accepted transaction spending that input", async () => {
    await expect(
      replayUnder(UNION(), { kind: "normal" }, SHAPE, []),
    ).resolves.toEqual(["non-existent-input"]);
  });
});

describe("executionSourceScriptDecoding replay beside an item no purpose walk reads", () => {
  // The terminal reads the recorded reason alone, so an output or mint item
  // that does not decode leaves the finding in place and the replay
  // completes.
  it.each([
    ["an output", { outputs: [Buffer.alloc(64)] }],
    ["a mint item", { mintPolicyItems: [Buffer.from("00", "hex")] }],
  ] as const)(
    "detects a rejection recording a decoding arm beside %s that does not decode",
    async (_label, items) => {
      await expect(
        unreadableItemViolations(
          ESSD,
          recording("ExecutionNativeScriptMalformed"),
          items,
        ),
      ).resolves.toEqual(["execution-native-script-malformed"]);
    },
  );
});
