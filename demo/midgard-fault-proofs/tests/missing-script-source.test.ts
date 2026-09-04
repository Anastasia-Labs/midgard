import {
  buildMidgardBoundedItem,
  buildMidgardValidationMerkleMembership,
  encodeMidgardSpendInputItem,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyMissingScriptSourceFinding,
  type ExecutionSourceDescriptor,
  missingScriptSourceCheckpoint,
  missingScriptSourceEvidenceCloses,
  missingScriptSourceEvidenceIdentity,
  missingScriptSourceViolationId,
  nextMissingScriptSourceAction,
  prepareMissingScriptSourceEvidence,
} from "../src/missing-script-source/family.js";
import {
  advanceMissingScriptSourceScan,
  bindMissingScriptSourceUniverse,
  initialMissingScriptSourceScan,
  missingScriptSourceScanIsComplete,
} from "../src/missing-script-source/universe-scan.js";
import {
  MISSING_SCRIPT_SOURCE_CONFIG_KEYS,
  MISSING_SCRIPT_SOURCE_STEP_DATUM_SCHEMAS,
} from "../src/missing-script-source/v1.js";

const txId = "11".repeat(32);
const requiredHash = "22".repeat(28);
const otherHash = "33".repeat(28);
const item = Buffer.from("8200581c" + "44".repeat(28), "hex");
const accepted = acceptedVerdictSubject(txId);
const forced = (purposeKind: number, purposeIndex: number) =>
  forcedVerdictSubject({
    transactionId: txId,
    sourceKey: { transactionId: "55".repeat(32), outputIndex: 0n },
    rejectionReason: {
      ScriptSourceMissing: {
        purpose_kind: BigInt(purposeKind),
        purpose_index: BigInt(purposeIndex),
      },
    },
  });

const sourceKey = (index: number) => {
  return encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x66),
    outputIndex: index,
  }).toString("hex");
};

const descriptors = (
  hashes: readonly string[],
  origins: readonly (0 | 1)[],
  purposeKind: 0 | 1 | 2 | 3,
  purposeIndex: number,
): readonly ExecutionSourceDescriptor[] => {
  const purposeLeaf = hashMidgardScriptPurposeLeaf({
    purposeKind,
    purposeIndex: BigInt(purposeIndex),
    scriptHash: Buffer.from(requiredHash, "hex"),
    subject: Buffer.from("aa", "hex"),
  });
  const sourceLeaves = hashes.map((scriptHashHex, index) => {
    const originKind = origins[index]!;
    const bounded = buildMidgardBoundedItem({
      fieldIndex: originKind === 0 ? 6 : 2,
      itemIndex: index,
      bytes: item,
    });
    return originKind === 0
      ? hashMidgardInlineScriptSourceLeaf({
          sourceIndex: BigInt(index),
          scriptLanguageTag: 0,
          scriptHash: Buffer.from(scriptHashHex, "hex"),
          scriptTotalLength: item.length,
          itemCommitment: bounded.commitment,
        })
      : hashMidgardReferenceScriptSourceLeaf({
          sourceKey: Buffer.from(sourceKey(index), "hex"),
          scriptLanguageTag: 0,
          scriptHash: Buffer.from(scriptHashHex, "hex"),
          scriptTotalLength: item.length,
          itemCommitment: bounded.commitment,
        });
  });
  return hashes.map((scriptHashHex, index) => {
    const sourceLeaf = sourceLeaves[index]!;
    const executionLeaf = hashMidgardScriptExecutionLeaf({
      languageTag: 0,
      purposeLeaf,
      sourceLeaf,
    });
    return {
      sourceIndex: index,
      originKind: origins[index]!,
      sourceKeyHex: origins[index] === 0 ? "00" : sourceKey(index),
      languageTag: 0,
      scriptHashHex,
      scriptItemHex: item.toString("hex"),
      purposeKind,
      purposeIndex,
      purposeSubjectHex: "aa",
      redeemerLeafHex: "",
      purposeMembership: buildMidgardValidationMerkleMembership(
        [purposeLeaf],
        0,
      ),
      sourceMembership: buildMidgardValidationMerkleMembership(
        sourceLeaves,
        index,
      ),
      executionMembership: buildMidgardValidationMerkleMembership(
        [executionLeaf],
        0,
      ),
    };
  });
};

const evidence = ({
  subject = accepted,
  purposeKind = 0,
  purposeIndex = 0,
  hashes = [otherHash, otherHash, otherHash],
}: {
  subject?: typeof accepted;
  purposeKind?: 0 | 1 | 2 | 3;
  purposeIndex?: number;
  hashes?: readonly string[];
} = {}) => {
  const sources = descriptors(
    hashes,
    [0, ...Array.from({ length: Math.max(0, hashes.length - 1) }, () => 1)] as (
      | 0
      | 1
    )[],
    purposeKind,
    purposeIndex,
  );
  const target = descriptors(
    [requiredHash],
    [0],
    purposeKind,
    purposeIndex,
  )[0]!;
  return prepareMissingScriptSourceEvidence({
    finding: { subject, purposeKind, purposeIndex, executionIndex: 0 },
    descriptor: target,
    sources,
  });
};

describe("missingScriptSource V1", () => {
  it("freezes ID 2d, callback-free config, and six physical datum ABIs", () => {
    expect(missingScriptSourceViolationId()).toBe("script-source-missing");
    expect(MISSING_SCRIPT_SOURCE_CONFIG_KEYS).toEqual([
      "manifest",
      "blueprintJson",
      "deploymentInfo",
      "headerHash",
      "lucid",
      "signer",
      "source",
      "decisionDigest",
      "stateQueueMutationLeaseCoordinator",
      "referenceScripts",
    ]);
    expect(MISSING_SCRIPT_SOURCE_STEP_DATUM_SCHEMAS).toHaveLength(6);
    expect(MISSING_SCRIPT_SOURCE_STEP_DATUM_SCHEMAS[0]).toBe(
      FraudProofComputationThreadStepDatum,
    );
  });

  it.each([0, 1, 2, 3] as const)(
    "proves universal absence for purpose kind %i",
    (purposeKind) => {
      const exact = evidence({ purposeKind });
      expect(exact.sourceCount).toBe(3);
      expect(exact.foundAtSourceIndex).toBeNull();
      expect(missingScriptSourceEvidenceCloses(exact)).toBe(true);
    },
  );

  it.each([0, 1] as const)(
    "finds the required hash at source location %i for forced contradiction",
    (location) => {
      const subject = forced(0, 0);
      const exact = evidence({
        subject,
        hashes: [otherHash, otherHash, otherHash].map((hash, index) =>
          index === location ? requiredHash : hash,
        ),
      });
      expect(exact.foundAtSourceIndex).toBe(location);
      expect(missingScriptSourceEvidenceCloses(exact)).toBe(true);
    },
  );

  it("refuses honest verdicts and exact purpose-coordinate substitution", () => {
    expect(
      missingScriptSourceEvidenceCloses(evidence({ hashes: [requiredHash] })),
    ).toBe(false);
    expect(
      missingScriptSourceEvidenceCloses(evidence({ subject: forced(0, 0) })),
    ).toBe(false);
    expect(() =>
      classifyMissingScriptSourceFinding({
        subject: forced(2, 7),
        purposeKind: 2,
        purposeIndex: 6,
        executionIndex: 0,
      }),
    ).toThrow(/purpose coordinate differs/u);
    expect(() =>
      classifyMissingScriptSourceFinding({
        subject: forced(0, 0),
        purposeKind: 4 as never,
        purposeIndex: 0,
        executionIndex: 0,
      }),
    ).toThrow(/purpose kind/u);
  });

  it("refuses incomplete order, membership mutation, and alternate-hash substitution", () => {
    const sources = descriptors([otherHash, otherHash], [0, 1], 0, 0);
    const target = descriptors([requiredHash], [0], 0, 0)[0]!;
    expect(() =>
      prepareMissingScriptSourceEvidence({
        finding: {
          subject: accepted,
          purposeKind: 0,
          purposeIndex: 0,
          executionIndex: 0,
        },
        descriptor: target,
        sources: [{ ...sources[0]!, sourceIndex: 1 }, sources[1]!],
      }),
    ).toThrow(/complete and consensus ordered/u);
    expect(() =>
      prepareMissingScriptSourceEvidence({
        finding: {
          subject: accepted,
          purposeKind: 0,
          purposeIndex: 0,
          executionIndex: 0,
        },
        descriptor: target,
        sources: [{ ...sources[0]!, scriptHashHex: requiredHash }, sources[1]!],
      }),
    ).toThrow(/membership was substituted/u);
    expect(evidence({ hashes: [otherHash] }).foundAtSourceIndex).toBeNull();
  });

  it("domain-separates resumable checkpoint and durable identity", () => {
    const exact = evidence();
    const checkpoint = missingScriptSourceCheckpoint({
      evidence: exact,
      controlCbor: "",
      nextExpectedScriptHash: "77".repeat(28),
    });
    expect(checkpoint).toMatch(/^[0-9a-f]{64}$/u);
    expect(
      missingScriptSourceCheckpoint({
        evidence: exact,
        controlCbor: "00",
        nextExpectedScriptHash: "77".repeat(28),
      }),
    ).not.toBe(checkpoint);
    expect(missingScriptSourceEvidenceIdentity(exact)).toContain(":0:0:");
    expect(nextMissingScriptSourceAction("scan")).toBe("submitScanOrResume");
  });

  it("scans the complete maximum frontier across resumptions", () => {
    const universe = bindMissingScriptSourceUniverse({
      purposeKind: 3,
      purposeIndex: 7,
      requiredScriptHashHex: requiredHash,
      sources: Array.from({ length: 49 }, (_, sourceIndex) => ({
        sourceIndex,
        locationKind: sourceIndex < 17 ? 0 : 1,
        scriptHashHex: sourceIndex === 47 ? requiredHash : otherHash,
        sourceKeyHex: sourceIndex < 17 ? "" : sourceKey(sourceIndex),
        itemCommitmentHex: "88".repeat(32),
      })),
    });
    let state = initialMissingScriptSourceScan(universe, "99".repeat(28));
    state = advanceMissingScriptSourceScan({
      universe,
      prior: state,
      scanScriptHashHex: "99".repeat(28),
      finalScriptHashHex: "aa".repeat(28),
    });
    expect(state.cursor).toBe(24);
    expect(state.found).toBe(false);
    state = advanceMissingScriptSourceScan({
      universe,
      prior: state,
      scanScriptHashHex: "99".repeat(28),
      finalScriptHashHex: "aa".repeat(28),
    });
    expect(state.cursor).toBe(48);
    expect(state.found).toBe(true);
    state = advanceMissingScriptSourceScan({
      universe,
      prior: state,
      scanScriptHashHex: "99".repeat(28),
      finalScriptHashHex: "aa".repeat(28),
    });
    expect(missingScriptSourceScanIsComplete(state)).toBe(true);
    expect(state.nextExpectedScriptHashHex).toBe("aa".repeat(28));
  });

  it("refuses resumed checkpoint, total, identity, and frontier-order substitution", () => {
    const universe = bindMissingScriptSourceUniverse({
      purposeKind: 0,
      purposeIndex: 0,
      requiredScriptHashHex: requiredHash,
      sources: [
        {
          sourceIndex: 0,
          locationKind: 0,
          scriptHashHex: otherHash,
          sourceKeyHex: "",
          itemCommitmentHex: "88".repeat(32),
        },
      ],
    });
    const prior = initialMissingScriptSourceScan(universe, "99".repeat(28));
    for (const mutation of [
      { ...prior, cursor: 1 },
      { ...prior, totalCount: 2 },
      { ...prior, universeIdentityHex: "ff".repeat(32) },
      { ...prior, checkpointHashHex: "ee".repeat(32) },
    ])
      expect(() =>
        advanceMissingScriptSourceScan({
          universe,
          prior: mutation,
          scanScriptHashHex: "99".repeat(28),
          finalScriptHashHex: "aa".repeat(28),
        }),
      ).toThrow(/substituted/u);
    expect(() =>
      bindMissingScriptSourceUniverse({
        ...universe,
        sources: [{ ...universe.sources[0]!, sourceIndex: 1 }],
      }),
    ).toThrow(/incomplete or reordered/u);
  });
});
