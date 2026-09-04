import {
  computeMidgardNativeTxId,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  admitObserverOrderInvalidArtifact,
  buildObserverOrderInvalidArtifact,
  observerOrderInvalidArtifactDigest,
} from "../src/observer-order-invalid/artifact.js";
import {
  classifyObserverOrderInvalidFinding,
  OBSERVER_ORDER_INVALID_CATEGORY,
  OBSERVER_ORDER_INVALID_CATEGORY_ID,
  observerOrderInvalidEvidenceCloses,
  prepareObserverOrderInvalidEvidence,
  scanObserverOrderInvalid,
} from "../src/observer-order-invalid/family.js";
import {
  detectObserverOrderInvalidCompleteReplay,
  OBSERVER_ORDER_INVALID_VIOLATION_ID,
  type ObserverOrderInvalidReplayDetection,
  selectCanonicalObserverOrderInvalidDetection,
} from "../src/observer-order-invalid/replay.js";
import {
  encodeObserverOrderWalkCheckpoint,
  hashObserverOrderWalkCheckpoint,
  planObserverOrderInvalidStagedWalk,
} from "../src/observer-order-invalid/staged-plan.js";
import {
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeNativeTx,
} from "./support/emulator/native-tx.js";

const txId = "00".repeat(31).concat("01");
const observer = (byte: number) => Buffer.alloc(28, byte);
const accepted = acceptedVerdictSubject(txId);
const rejected = (observerIndex: number) =>
  forcedVerdictSubject({
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
  const field = encodeMidgardFieldPreimage(values);
  return prepareObserverOrderInvalidEvidence({
    finding: { subject, observerIndex },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
};

describe("observerOrderInvalid V1 semantics", () => {
  it("freezes category identity", () => {
    expect(OBSERVER_ORDER_INVALID_CATEGORY).toBe("observerOrderInvalid");
    expect(OBSERVER_ORDER_INVALID_CATEGORY_ID).toBe("00000025");
  });
  it.each([
    ["first", [observer(2), observer(1)], 1],
    ["middle", [observer(1), observer(3), observer(2)], 2],
    ["last", [observer(1), observer(2), observer(3), observer(3)], 3],
    ["duplicate", [observer(1), observer(1)], 1],
  ] as const)("proves the %s offending position", (_label, values, index) => {
    const prepared = evidence(accepted, values, index);
    expect(prepared.violation).toBe(true);
    expect(observerOrderInvalidEvidenceCloses(prepared)).toBe(true);
  });
  it("proves an exact wrongful-rejection contradiction", () => {
    const prepared = evidence(
      rejected(2),
      [observer(1), observer(2), observer(3)],
      2,
    );
    expect(prepared.violation).toBe(false);
    expect(observerOrderInvalidEvidenceCloses(prepared)).toBe(true);
  });
  it("refuses honest polarity and reason/coordinate substitution", () => {
    expect(
      observerOrderInvalidEvidenceCloses(
        evidence(accepted, [observer(1), observer(2)], 1),
      ),
    ).toBe(false);
    expect(
      observerOrderInvalidEvidenceCloses(
        evidence(rejected(1), [observer(2), observer(1)], 1),
      ),
    ).toBe(false);
    expect(() =>
      classifyObserverOrderInvalidFinding({
        subject: rejected(2),
        observerIndex: 1,
      }),
    ).toThrow(/coordinate changed/u);
    expect(() =>
      classifyObserverOrderInvalidFinding({
        subject: forcedVerdictSubject({
          transactionId: txId,
          sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
          rejectionReason: { OutputNonCanonical: { output_index: 0n } },
        }),
        observerIndex: 1,
      }),
    ).toThrow(/not ObserverOrderInvalid/u);
  });
  it("refuses committed bytes, width, range, and earlier-pair substitutions", () => {
    const field = encodeMidgardFieldPreimage([observer(1), observer(2)]);
    expect(() =>
      prepareObserverOrderInvalidEvidence({
        finding: { subject: accepted, observerIndex: 1 },
        fieldPreimage: field,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/do not match/u);
    expect(() =>
      scanObserverOrderInvalid([observer(1), Buffer.alloc(27)], 1),
    ).toThrow(/28 bytes/u);
    expect(() =>
      scanObserverOrderInvalid([observer(1), observer(2)], 2),
    ).toThrow(/outside/u);
    expect(() =>
      scanObserverOrderInvalid([observer(2), observer(1), observer(3)], 2),
    ).toThrow(/earlier/u);
  });
  it("derives deterministic resumable scan checkpoints", () => {
    const values = Array.from({ length: 49 }, (_, index) =>
      observer(index + 1),
    );
    values[48] = observer(48);
    const field = encodeMidgardFieldPreimage(values);
    const input = {
      transactionId: txId,
      fieldPreimageCbor: field.toString("hex"),
      observerIndex: 48,
    } as const;
    const first = planObserverOrderInvalidStagedWalk(input);
    expect(first).toEqual(planObserverOrderInvalidStagedWalk(input));
    expect(first.walk).toHaveLength(3);
    expect(encodeObserverOrderWalkCheckpoint(first.walk[0]!)[36]).toBe(3);
    expect(hashObserverOrderWalkCheckpoint(first.walk[0]!)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
  });
  it("adapts every accepted coordinate into deterministic complete replay", () => {
    const transactions = [9n, 7n].map((fee, index) => {
      const base = makeNativeTx({ spendInputCbors: [], fee });
      const full = materializeMidgardNativeTxFromCanonical({
        version: base.version,
        validity: base.validity,
        body: {
          ...base.body,
          requiredObserversPreimageCbor: encodeMidgardFieldPreimage([
            observer(index + 2),
            observer(index + 1),
          ]),
        },
        witnessSet: base.witnessSet,
      });
      return {
        nodeTxId: computeMidgardNativeTxId(full).toString("hex"),
        txCbor: encodeMidgardNativeTxCanonical(full).toString("hex"),
        l2TransactionSourceCbor: l2TransactionSourceCborV1(full),
      };
    });
    const detections = detectObserverOrderInvalidCompleteReplay({
      headerHash: "22".repeat(28),
      header: { transactionsRoot: "33".repeat(32), l2TransactionCount: 2n },
      payloadEnvelopeSha256: "44".repeat(32),
      payloadSha256: "55".repeat(32),
      transactions,
      reconstruction: { forcedTransactions: [] },
      inclusionRootAuthentication: {
        sourceValuePhasRoot: "66".repeat(32),
      },
    } as unknown as CanonicalBlockEvidence);
    expect(detections.map(({ position }) => position)).toEqual([0n, 1n]);
    expect(
      detections.every(
        ({ violationId }) =>
          violationId === OBSERVER_ORDER_INVALID_VIOLATION_ID,
      ),
    ).toBe(true);
  });
  it("selects deterministically and round-trips a durable artifact", () => {
    const prepared = evidence(accepted, [observer(2), observer(1)], 1);
    const detection = (
      position: bigint,
      detectionId: string,
    ): ObserverOrderInvalidReplayDetection => ({
      detectionId,
      headerHash: "22".repeat(28),
      violationId: OBSERVER_ORDER_INVALID_VIOLATION_ID,
      position,
      transactionId: txId,
      observerIndex: 1,
      source: "accepted",
      direction: "wrongfulAcceptance",
    });
    expect(
      selectCanonicalObserverOrderInvalidDetection([
        detection(9n, "z"),
        detection(2n, "b"),
        detection(2n, "a"),
      ]).detectionId,
    ).toBe("a");
    const artifact = buildObserverOrderInvalidArtifact({
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
    expect(admitObserverOrderInvalidArtifact(artifact).evidence).toEqual(
      prepared,
    );
    expect(observerOrderInvalidArtifactDigest(artifact)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(() =>
      admitObserverOrderInvalidArtifact({
        ...artifact,
        fieldPreimageCbor: `${artifact.fieldPreimageCbor.slice(0, -2)}ff`,
      }),
    ).toThrow(/commitment changed/u);
  });
});
