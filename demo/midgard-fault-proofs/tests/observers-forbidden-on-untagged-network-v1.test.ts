import {
  encodeMidgardFieldPreimage,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyObserversForbiddenFinding,
  MIDGARD_UNTAGGED_NETWORK_ID,
  OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY,
  OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_ID,
  observersForbiddenEvidenceCloses,
  prepareObserversForbiddenEvidence,
} from "../src/observers-forbidden-on-untagged-network/family-v1.js";
import {
  admitObserversForbiddenArtifact,
  buildObserversForbiddenArtifact,
  observersForbiddenArtifactDigest,
} from "../src/observers-forbidden-on-untagged-network/production-artifact-v1.js";
import {
  OBSERVERS_FORBIDDEN_VIOLATION_ID,
  type ObserversForbiddenReplayDetection,
  selectCanonicalObserversForbiddenDetection,
} from "../src/observers-forbidden-on-untagged-network/replay-v1.js";

const transactionId = "01".repeat(32);
const accepted = acceptedVerdictSubject(transactionId);
const forced = forcedVerdictSubject({
  transactionId,
  sourceKey: { transactionId: "02".repeat(32), outputIndex: 0n },
  rejectionReason: "ObserversForbiddenOnUntaggedNetwork",
});
const observerField = (count: number) =>
  encodeMidgardFieldPreimage(
    Array.from({ length: count }, (_, index) => Buffer.alloc(28, index + 1)),
  );
const evidence = (
  subject: typeof accepted,
  networkId: 0 | 1 | 255,
  count: number,
) => {
  const field = observerField(count);
  return prepareObserversForbiddenEvidence({
    finding: { subject, networkId },
    observerFieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
};

describe("observersForbiddenOnUntaggedNetwork V1 semantics", () => {
  it("freezes the authoritative family identity and network scalar", () => {
    expect(OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY).toBe(
      "observersForbiddenOnUntaggedNetwork",
    );
    expect(OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_ID).toBe(
      "00000024",
    );
    expect(MIDGARD_UNTAGGED_NETWORK_ID).toBe(255);
  });

  it("closes wrongful acceptance only for a non-empty observer set on the untagged scalar", () => {
    expect(observersForbiddenEvidenceCloses(evidence(accepted, 255, 1))).toBe(
      true,
    );
    expect(observersForbiddenEvidenceCloses(evidence(accepted, 255, 0))).toBe(
      false,
    );
    expect(observersForbiddenEvidenceCloses(evidence(accepted, 0, 1))).toBe(
      false,
    );
  });

  it("closes both complete forced contradiction polarities", () => {
    expect(observersForbiddenEvidenceCloses(evidence(forced, 255, 0))).toBe(
      true,
    );
    expect(observersForbiddenEvidenceCloses(evidence(forced, 1, 1))).toBe(true);
    expect(observersForbiddenEvidenceCloses(evidence(forced, 255, 1))).toBe(
      false,
    );
  });

  it("binds the exact typed reason and refuses accepted reason injection", () => {
    const wrongReason = forcedVerdictSubject({
      transactionId,
      sourceKey: { transactionId: "02".repeat(32), outputIndex: 0n },
      rejectionReason: { OutputNonCanonical: { output_index: 0n } },
    });
    expect(() =>
      classifyObserversForbiddenFinding({
        subject: wrongReason,
        networkId: 255,
      }),
    ).toThrow(/typed rejection reason changed/u);
    expect(() =>
      classifyObserversForbiddenFinding({
        subject: { ...accepted, rejection_reason: "EmptyInputs" },
        networkId: 255,
      }),
    ).toThrow(/not canonical|polarity changed/u);
  });

  it("refuses network, commitment, and observer-width substitution", () => {
    expect(() =>
      classifyObserversForbiddenFinding({
        subject: accepted,
        networkId: 2 as 0,
      }),
    ).toThrow(/network scalar changed/u);
    const field = observerField(1);
    expect(() =>
      prepareObserversForbiddenEvidence({
        finding: { subject: accepted, networkId: 255 },
        observerFieldPreimage: field,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/changed commitment/u);
    const malformed = encodeMidgardFieldPreimage([Buffer.alloc(27)]);
    expect(() =>
      prepareObserversForbiddenEvidence({
        finding: { subject: accepted, networkId: 255 },
        observerFieldPreimage: malformed,
        committedFieldHashHex:
          midgardFieldCommitment(malformed).toString("hex"),
      }),
    ).toThrow(/not a 28-byte hash/u);
  });

  it("selects all three carriage tiers deterministically", () => {
    expect(evidence(accepted, 255, 0).carriage).toBe("Inline");
    expect(evidence(accepted, 255, 478).carriage).toBe("RawUtxo");
    const maximum = evidence(accepted, 255, 505);
    expect(maximum.observerFieldPreimageCbor.length / 2).toBe(15_153);
    expect(maximum.carriage).toBe("Certified");
    expect(maximum.observerCount).toBe(505);
  });

  it("selects the earliest authenticated replay independent of traversal order", () => {
    const detection = (
      position: bigint,
      detectionId: string,
    ): ObserversForbiddenReplayDetection => ({
      detectionId,
      headerHash: "03".repeat(28),
      violationId: OBSERVERS_FORBIDDEN_VIOLATION_ID,
      position,
      transactionId,
      networkId: 255,
      observerCount: 1,
      source: "accepted",
      direction: "wrongfulAcceptance",
    });
    expect(
      selectCanonicalObserversForbiddenDetection([
        detection(9n, "z"),
        detection(2n, "b"),
        detection(2n, "a"),
      ]).detectionId,
    ).toBe("a");
  });

  it("reconstructs its artifact and refuses network/field substitution", () => {
    const prepared = evidence(accepted, 255, 1);
    const artifact = buildObserversForbiddenArtifact({
      headerHash: "03".repeat(28),
      detectionId: "accepted:0",
      position: 0n,
      evidence: prepared,
      nativeTxCompactCbor: "80",
      witnessSetCompactCbor: "80",
      l2TransactionSourceCbor: "80",
      transactionsPhasRoot: "04".repeat(32),
      transactionMembershipCbor: "80",
    });
    expect(admitObserversForbiddenArtifact(artifact).evidence).toEqual(
      prepared,
    );
    expect(observersForbiddenArtifactDigest(artifact)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(() =>
      admitObserversForbiddenArtifact({
        ...artifact,
        networkId: 1,
      }),
    ).toThrow(/source payload|artifact|field|network|closes/u);
    expect(() =>
      admitObserversForbiddenArtifact({
        ...artifact,
        fieldPreimageCbor: `${artifact.fieldPreimageCbor.slice(0, -2)}ff`,
      }),
    ).toThrow(/commitment changed/u);
  });
});
