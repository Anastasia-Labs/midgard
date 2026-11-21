import { describe, expect, it } from "vitest";

import { parsePromiseCapacityEvidence } from "../src/availability/promise-capacity-evidence.js";
import { makeRetirementFloor } from "../src/store/retirement-model.js";

/**
 * Plan §5.1: a block is final at depth k + 1 (`isFinal`: depth > k). The
 * stored certificates name a point and the block that certified it final, and
 * their parsers accept exactly the final depths: refused at depth k, accepted
 * at depth k + 1.
 */
const k = 2160;
const captured = { slot: 1_000, blockHash: "12".repeat(32), blockNo: 100 };
/** The certifying block at `depth` below which `captured` sits. */
const certifiedAtDepth = (depth: number) => ({
  slot: captured.slot + depth * 20,
  blockHash: "34".repeat(32),
  blockNo: captured.blockNo + depth - 1,
});

describe("capacity evidence certificate", () => {
  const evidence = (certifiedAt: unknown) => ({
    deploymentFingerprint: "ab".repeat(32),
    contractManifestId: "89".repeat(32),
    actorId: "cd".repeat(28),
    headerHash: "ef".repeat(28),
    commitmentDigest: "01".repeat(32),
    cutoffTimeMs: 5_000,
    recoveryDepth: k,
    retirementKind: "open_cutoff",
    point: captured,
    certifiedAt,
  });

  it("is refused at depth k", () => {
    expect(() =>
      parsePromiseCapacityEvidence(evidence(certifiedAtDepth(k))),
    ).toThrow("Capacity evidence certificate is within the recovery horizon");
  });

  it("parses at depth k + 1", () => {
    const certifiedAt = certifiedAtDepth(k + 1);
    expect(parsePromiseCapacityEvidence(evidence(certifiedAt))).toMatchObject({
      point: captured,
      certifiedAt,
    });
  });
});

describe("retirement floor certificate", () => {
  const floor = (certifiedAt: ReturnType<typeof certifiedAtDepth>) =>
    makeRetirementFloor({
      schemaVersion: 1,
      binding: {
        deploymentFingerprint: "ab".repeat(32),
        manifestSha256: "cd".repeat(32),
        contractManifestId: "89".repeat(32),
        committeeSignersHash: "ef".repeat(32),
        actorId: "01".repeat(28),
        sourceAuthoritySha256: "23".repeat(32),
        peerIds: ["peer1"],
        retentionDays: 15,
        recoveryDepth: k,
        maximumRecords: 512,
        maximumEncodedBytes: 8 * 1024 * 1024,
      },
      generation: 1,
      headerEndTimeMs: 5_000,
      point: captured,
      certifiedAt,
      nonceTimeFloorMs: 5_000,
    });

  it("is refused at depth k", () => {
    expect(() => floor(certifiedAtDepth(k))).toThrow(
      "Retirement floor lacks strict recovery depth",
    );
  });

  it("parses at depth k + 1", () => {
    const certifiedAt = certifiedAtDepth(k + 1);
    expect(floor(certifiedAt)).toMatchObject({ point: captured, certifiedAt });
  });
});
