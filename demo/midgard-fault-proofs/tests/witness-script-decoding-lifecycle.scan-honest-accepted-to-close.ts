import {
  computeMidgardNativeTxId,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import {
  deriveWitnessScriptDecodingEvidenceFromCanonicalBlock,
  detectWitnessScriptDecodingCompleteReplay,
  planWitnessScriptDecodingStep03Transition,
  prepareWitnessScriptDecodingEvidence,
  witnessScriptDecodingCheckpoint,
  type WitnessScriptDecodingEvidence,
} from "../src/witness-script-decoding/index.js";
import {
  decodingMalformedMaximumItem,
  decodingPlutusItem,
} from "./support/native-script-decoding-emulator.js";
import {
  deepCanonicalItem,
  emptyPayloadItem,
  headerMalformedAdjacentItem,
  headerMalformedItem,
  headerMalformedMaximumItem,
  nativeTxWithScriptWitnesses,
  scriptWitnessField,
  smallCanonicalItem,
  wideCanonicalMaximumItem,
  witnessSetCarriageOf,
} from "./support/witness-script-decoding-raw.js";
import {
  DEEP_DEPTH,
  type ReasonArm,
  type Shape,
} from "./witness-script-decoding-lifecycle.authentication-seams.js";
import { makeHarness } from "./witness-script-decoding-lifecycle.make-harness.js";

export const shapeOf = (
  label: string,
  items: readonly Buffer[],
  fee: bigint,
): Shape => {
  const nativeTx = nativeTxWithScriptWitnesses(items, fee);
  const fieldBytes = scriptWitnessField(items).length;
  return {
    label,
    items,
    nativeTx,
    txId: computeMidgardNativeTxId(nativeTx).toString("hex"),
    carriage: witnessSetCarriageOf(nativeTx),
    fieldBytes,
    certified: fieldBytes > 15_148,
  };
};

export const headerMaximumShape = () =>
  shapeOf(
    "32,768-byte field 6; [1, 32,762 bytes]: undecodable wrapper over nine bounded-item chunks (Certified)",
    [headerMalformedMaximumItem()],
    1_000n,
  );

export const headerAdjacentShape = () =>
  shapeOf(
    "32,769-byte field 6; [1, 32,763 bytes]: one byte past the aggregate field bound (Certified)",
    [headerMalformedAdjacentItem()],
    1_008n,
  );

export const nativeMaximumShape = () =>
  shapeOf(
    "32,768-byte field 6; tag-0 payload refused at its fourth primitive step over nine bounded-item chunks (Certified)",
    [decodingMalformedMaximumItem()],
    1_001n,
  );

export const emptyPayloadShape = () =>
  shapeOf(
    "[0, h'']: decodable wrapper, empty payload (Inline)",
    [emptyPayloadItem()],
    1_002n,
  );

export const smallCanonicalShape = (fee = 1_003n) =>
  shapeOf(
    "all[sig]: canonical, four primitive steps (Inline)",
    [smallCanonicalItem()],
    fee,
  );

export const plutusShape = (fee = 1_004n) =>
  shapeOf(
    "[3, h'01020304']: non-native language (Inline)",
    [decodingPlutusItem()],
    fee,
  );

export const headerSmallShape = (fee = 1_005n) =>
  shapeOf(
    "[1, h'0a']: undecodable wrapper (Inline)",
    [headerMalformedItem()],
    fee,
  );

export const wideMaximumShape = () =>
  shapeOf(
    "32,768-byte field 6; all[1,020 sig, 38 after] = 1,059 nodes, 2,119 primitive steps over nine bounded-item chunks (Certified)",
    [wideCanonicalMaximumItem()],
    1_006n,
  );

export const deepShape = () =>
  shapeOf(
    `${scriptWitnessField([deepCanonicalItem(DEEP_DEPTH)]).length.toString()}-byte field 6; ${DEEP_DEPTH.toString()} nested all containers over one sig, ${(2 * DEEP_DEPTH + 2).toString()} primitive steps (Certified)`,
    [deepCanonicalItem(DEEP_DEPTH)],
    1_007n,
  );

export const reasonOf = (
  arm: ReasonArm,
  scriptIndex: bigint,
): SDK.RejectionReason =>
  ({ [arm]: { script_index: scriptIndex } }) as SDK.RejectionReason;

export const acceptedEvidence = (shape: Shape, scriptIndex = 0) =>
  prepareWitnessScriptDecodingEvidence({
    finding: {
      subject: SDK.acceptedVerdictSubject(shape.txId),
      witnessSetHash: shape.carriage.witnessSetHash,
      scriptIndex,
    },
    fieldPreimage: scriptWitnessField(shape.items),
    committedFieldHashHex: shape.carriage.witnessSet.script_tx_wits_hash,
  });

export const forcedEvidence = (
  shape: Shape,
  orderKey: SDK.OutputReference,
  reason: SDK.RejectionReason,
  scriptIndex = 0,
) =>
  prepareWitnessScriptDecodingEvidence({
    finding: {
      subject: SDK.forcedVerdictSubject({
        transactionId: shape.txId,
        sourceKey: orderKey,
        rejectionReason: reason,
      }),
      witnessSetHash: shape.carriage.witnessSetHash,
      scriptIndex,
    },
    fieldPreimage: scriptWitnessField(shape.items),
    committedFieldHashHex: shape.carriage.witnessSet.script_tx_wits_hash,
  });

type Harness = Awaited<ReturnType<typeof makeHarness>>;

// ---------------------------------------------------------------------------
// Replay detection over the committed block
// ---------------------------------------------------------------------------

export const expectReplayDetection = (
  block: Awaited<ReturnType<Harness["acceptedBlock"]>>["block"],
  transactions: readonly Shape[],
  evidence: WitnessScriptDecodingEvidence,
  violationIds: readonly string[],
) => {
  const replay = {
    headerHash: block.headerHash,
    transactions: transactions.map((shape) => ({
      nodeTxId: shape.txId,
      txCbor: encodeMidgardNativeTxCanonical(shape.nativeTx).toString("hex"),
    })),
    reconstruction: block.reconstruction,
  } as never;
  expect(
    detectWitnessScriptDecodingCompleteReplay(replay).map(
      (finding) => finding.violationId,
    ),
  ).toEqual([...violationIds]);
  expect(
    deriveWitnessScriptDecodingEvidenceFromCanonicalBlock(replay)
      .itemCommitmentHex,
  ).toBe(evidence.itemCommitmentHex);
};

/**
 * Drive an accepted thread's scan with the arguments a direction-B twin of
 * the same item plans (the arguments do not depend on the direction), so an
 * honest accepted block's canonical native script reaches the exact terminal
 * on chain and closes with no fault.
 */
export const scanHonestAcceptedToClose = async (
  h: Harness,
  threadOutRef: string,
  accepted: WitnessScriptDecodingEvidence,
  twin: WitnessScriptDecodingEvidence,
) => {
  let outRef = threadOutRef;
  for (;;) {
    const state = await h.scanStateAt(outRef, 2);
    const transition = planWitnessScriptDecodingStep03Transition({
      state,
      evidence: twin,
      contracts: h.contracts,
    });
    const nextExpectedScriptHash =
      h.contracts.steps[transition.nextStepIndex].spendingScriptHash;
    const nextState: SDK.WitnessScriptDecodingScanState = {
      ...state,
      control_cbor: transition.nextState.control_cbor,
      next_expected_script_hash: nextExpectedScriptHash,
      checkpoint_hash: witnessScriptDecodingCheckpoint({
        evidence: accepted,
        controlCbor: transition.nextState.control_cbor,
        nextExpectedScriptHash,
      }),
      result_class: transition.nextState.result_class,
    };
    const submitted = await h.step03Raw(outRef, transition, { nextState });
    outRef = submitted.nextThreadOutRef;
    if (transition.closes) return outRef;
  }
};
