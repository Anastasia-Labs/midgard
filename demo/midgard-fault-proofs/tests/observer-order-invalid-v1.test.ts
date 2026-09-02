import {
  computeMidgardNativeTxIdV1,
  encodeMidgardFieldPreimageV1,
  encodeMidgardNativeTxCanonicalV1,
  materializeMidgardNativeTxFromCanonicalV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidenceV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import {
  classifyObserverOrderInvalidFindingV1,
  OBSERVER_ORDER_INVALID_CATEGORY_ID_V1,
  OBSERVER_ORDER_INVALID_CATEGORY_V1,
  observerOrderInvalidEvidenceClosesV1,
  prepareObserverOrderInvalidEvidenceV1,
  scanObserverOrderInvalidV1,
} from "../src/observer-order-invalid/family-v1.js";
import {
  admitProductionObserverOrderInvalidArtifactV1,
  buildProductionObserverOrderInvalidArtifactV1,
  productionObserverOrderInvalidArtifactDigestV1,
} from "../src/observer-order-invalid/production-artifact-v1.js";
import {
  detectObserverOrderInvalidCompleteReplayV1,
  OBSERVER_ORDER_INVALID_VIOLATION_ID_V1,
  type ObserverOrderInvalidReplayDetectionV1,
  selectCanonicalObserverOrderInvalidDetectionV1,
} from "../src/observer-order-invalid/replay-v1.js";
import {
  encodeObserverOrderWalkCheckpointV1,
  hashObserverOrderWalkCheckpointV1,
  planObserverOrderInvalidStagedWalkV1,
} from "../src/observer-order-invalid/staged-plan-v1.js";
import {
  l2TransactionSourceCborV1,
  makeNativeTx,
} from "./support/emulator/native-tx.js";

const txId = "00".repeat(31).concat("01");
const observer = (byte: number) => Buffer.alloc(28, byte);
const accepted = acceptedVerdictSubjectV1(txId);
const rejected = (observerIndex: number) =>
  forcedVerdictSubjectV1({
    transactionId: txId,
    sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
    rejectionReason: {
      ObserverOrderInvalid: { observer_index: BigInt(observerIndex) },
    },
  });
const evidence = (
  subject: typeof accepted,
  values: readonly Buffer[],
  observerIndex: number,
) => {
  const field = encodeMidgardFieldPreimageV1(values);
  return prepareObserverOrderInvalidEvidenceV1({
    finding: { subject, observerIndex },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
  });
};

describe("observerOrderInvalid V1 semantics", () => {
  it("freezes category identity", () => {
    expect(OBSERVER_ORDER_INVALID_CATEGORY_V1).toBe("observerOrderInvalid");
    expect(OBSERVER_ORDER_INVALID_CATEGORY_ID_V1).toBe("00000025");
  });
  it.each([
    ["first", [observer(2), observer(1)], 1],
    ["middle", [observer(1), observer(3), observer(2)], 2],
    ["last", [observer(1), observer(2), observer(3), observer(3)], 3],
    ["duplicate", [observer(1), observer(1)], 1],
  ] as const)("proves the %s offending position", (_label, values, index) => {
    const prepared = evidence(accepted, values, index);
    expect(prepared.violation).toBe(true);
    expect(observerOrderInvalidEvidenceClosesV1(prepared)).toBe(true);
  });
  it("proves an exact wrongful-rejection contradiction", () => {
    const prepared = evidence(
      rejected(2),
      [observer(1), observer(2), observer(3)],
      2,
    );
    expect(prepared.violation).toBe(false);
    expect(observerOrderInvalidEvidenceClosesV1(prepared)).toBe(true);
  });
  it("refuses honest polarity and reason/coordinate substitution", () => {
    expect(
      observerOrderInvalidEvidenceClosesV1(
        evidence(accepted, [observer(1), observer(2)], 1),
      ),
    ).toBe(false);
    expect(
      observerOrderInvalidEvidenceClosesV1(
        evidence(rejected(1), [observer(2), observer(1)], 1),
      ),
    ).toBe(false);
    expect(() =>
      classifyObserverOrderInvalidFindingV1({
        subject: rejected(2),
        observerIndex: 1,
      }),
    ).toThrow(/coordinate changed/u);
    expect(() =>
      classifyObserverOrderInvalidFindingV1({
        subject: forcedVerdictSubjectV1({
          transactionId: txId,
          sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
          rejectionReason: { OutputNonCanonical: { output_index: 0n } },
        }),
        observerIndex: 1,
      }),
    ).toThrow(/not ObserverOrderInvalid/u);
  });
  it("refuses committed bytes, width, range, and earlier-pair substitutions", () => {
    const field = encodeMidgardFieldPreimageV1([observer(1), observer(2)]);
    expect(() =>
      prepareObserverOrderInvalidEvidenceV1({
        finding: { subject: accepted, observerIndex: 1 },
        fieldPreimage: field,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/do not match/u);
    expect(() =>
      scanObserverOrderInvalidV1([observer(1), Buffer.alloc(27)], 1),
    ).toThrow(/28 bytes/u);
    expect(() =>
      scanObserverOrderInvalidV1([observer(1), observer(2)], 2),
    ).toThrow(/outside/u);
    expect(() =>
      scanObserverOrderInvalidV1([observer(2), observer(1), observer(3)], 2),
    ).toThrow(/earlier/u);
  });
  it("derives deterministic resumable scan checkpoints", () => {
    const values = Array.from({ length: 49 }, (_, index) =>
      observer(index + 1),
    );
    values[48] = observer(48);
    const field = encodeMidgardFieldPreimageV1(values);
    const input = {
      transactionId: txId,
      fieldPreimageCbor: field.toString("hex"),
      observerIndex: 48,
    } as const;
    const first = planObserverOrderInvalidStagedWalkV1(input);
    expect(first).toEqual(planObserverOrderInvalidStagedWalkV1(input));
    expect(first.walk).toHaveLength(3);
    expect(encodeObserverOrderWalkCheckpointV1(first.walk[0]!)[36]).toBe(3);
    expect(hashObserverOrderWalkCheckpointV1(first.walk[0]!)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
  });
  it("adapts every accepted coordinate into deterministic complete replay", () => {
    const transactions = [9n, 7n].map((fee, index) => {
      const base = makeNativeTx({ spendInputCbors: [], fee });
      const full = materializeMidgardNativeTxFromCanonicalV1({
        version: base.version,
        validity: base.validity,
        body: {
          ...base.body,
          requiredObserversPreimageCbor: encodeMidgardFieldPreimageV1([
            observer(index + 2),
            observer(index + 1),
          ]),
        },
        witnessSet: base.witnessSet,
      });
      return {
        nodeTxId: computeMidgardNativeTxIdV1(full).toString("hex"),
        txCbor: encodeMidgardNativeTxCanonicalV1(full).toString("hex"),
        l2TransactionSourceCbor: l2TransactionSourceCborV1(full),
      };
    });
    const detections = detectObserverOrderInvalidCompleteReplayV1({
      headerHash: "22".repeat(28),
      header: { transactionsRoot: "33".repeat(32), l2TransactionCount: 2n },
      payloadEnvelopeSha256: "44".repeat(32),
      payloadSha256: "55".repeat(32),
      transactions,
      reconstruction: { forcedTransactions: [] },
      inclusionRootAuthentication: {
        sourceValuePhasRoot: "66".repeat(32),
      },
    } as unknown as CanonicalBlockEvidenceV1);
    expect(detections.map(({ position }) => position)).toEqual([0n, 1n]);
    expect(
      detections.every(
        ({ violationId }) =>
          violationId === OBSERVER_ORDER_INVALID_VIOLATION_ID_V1,
      ),
    ).toBe(true);
  });
  it("selects deterministically and round-trips a durable artifact", () => {
    const prepared = evidence(accepted, [observer(2), observer(1)], 1);
    const detection = (
      position: bigint,
      detectionId: string,
    ): ObserverOrderInvalidReplayDetectionV1 => ({
      detectionId,
      headerHash: "22".repeat(28),
      violationId: OBSERVER_ORDER_INVALID_VIOLATION_ID_V1,
      position,
      transactionId: txId,
      observerIndex: 1,
      source: "accepted",
      direction: "wrongfulAcceptance",
    });
    expect(
      selectCanonicalObserverOrderInvalidDetectionV1([
        detection(9n, "z"),
        detection(2n, "b"),
        detection(2n, "a"),
      ]).detectionId,
    ).toBe("a");
    const artifact = buildProductionObserverOrderInvalidArtifactV1({
      headerHash: "22".repeat(28),
      detectionId: "observer-order-invalid:accepted:0:test:1",
      position: 0n,
      evidence: prepared,
      nativeTxCompactCbor: "80",
      witnessSetCompactCbor: "80",
      l2TransactionSourceCbor: "80",
      transactionsPhasRoot: "33".repeat(32),
      transactionMembershipCbor: "80",
    });
    expect(
      admitProductionObserverOrderInvalidArtifactV1(artifact).evidence,
    ).toEqual(prepared);
    expect(productionObserverOrderInvalidArtifactDigestV1(artifact)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(() =>
      admitProductionObserverOrderInvalidArtifactV1({
        ...artifact,
        fieldPreimageCbor: `${artifact.fieldPreimageCbor.slice(0, -2)}ff`,
      }),
    ).toThrow(/commitment changed/u);
  });
});
