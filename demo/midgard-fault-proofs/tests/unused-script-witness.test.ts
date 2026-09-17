import {
  buildMidgardBoundedItem,
  buildMidgardValidationMerkleMembership,
  encodeMidgardVersionedScript,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptPurposeLeaf,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { initialUnusedScriptWitnessReverseScan } from "../src/unused-script-witness/checkpoint.js";
import {
  classifyUnusedScriptWitnessFinding,
  inlineSourceKeyHex,
  prepareUnusedScriptWitnessEvidence as prepareAgainstUniverse,
  type UnusedScriptPurposeOpening,
  type UnusedScriptSourceOpening,
  unusedScriptWitnessAccountabilityRoute,
  unusedScriptWitnessEvidenceCloses,
  type UnusedScriptWitnessFinding,
} from "../src/unused-script-witness/family.js";
import { detectUnusedScriptWitnessCanonicalViolations } from "../src/unused-script-witness/replay.js";
import {
  UnusedScriptAuthenticatedWitnessSchema,
  UnusedScriptBoundWitnessSchema,
  UnusedScriptDecisionSchema,
  UnusedScriptReverseScanSchema,
} from "../src/unused-script-witness/schemas.js";
import {
  createUnusedScriptWitnessWorkflowRunnerSurface,
  UNUSED_SCRIPT_WITNESS_CONFIG_KEYS,
} from "../src/unused-script-witness/v1.js";
import {
  cancelUnusedScriptWitnessWorkflow,
  runUnusedScriptWitnessWorkflow,
  type UnusedScriptWitnessCursor,
  type UnusedScriptWitnessJournalEntry,
} from "../src/unused-script-witness/workflow.js";
import { buildUnusedScriptWitnessFixture } from "./support/unused-script-witness-emulator.js";

const txId = "11".repeat(32);
const scriptA = {
  language: "PlutusV3" as const,
  scriptBytes: Buffer.from("01", "hex"),
};
const scriptB = {
  language: "PlutusV3" as const,
  scriptBytes: Buffer.from("02", "hex"),
};
const scripts = [scriptA, scriptB];
const preimage = encodeMidgardVersionedScriptListPreimage(scripts);

const sourceFixture = (): readonly UnusedScriptSourceOpening[] => {
  const leaves = scripts.map((script, sourceIndex) => {
    const bytes = encodeMidgardVersionedScript(script);
    return hashMidgardInlineScriptSourceLeaf({
      sourceIndex: BigInt(sourceIndex),
      scriptLanguageTag: 3,
      scriptHash: Buffer.from(hashMidgardVersionedScript(script), "hex"),
      scriptTotalLength: bytes.length,
      itemCommitment: buildMidgardBoundedItem({
        fieldIndex: 6,
        itemIndex: sourceIndex,
        bytes,
      }).commitment,
    });
  });
  return leaves.map((_, sourceIndex) => ({
    frontierIndex: sourceIndex,
    originKind: 0 as const,
    sourceIndex,
    sourceKeyHex: inlineSourceKeyHex(sourceIndex),
    languageTag: 3 as const,
    scriptHashHex: hashMidgardVersionedScript(scripts[sourceIndex]!),
    scriptTotalLength: encodeMidgardVersionedScript(scripts[sourceIndex]!)
      .length,
    itemCommitmentHex: buildMidgardBoundedItem({
      fieldIndex: 6,
      itemIndex: sourceIndex,
      bytes: encodeMidgardVersionedScript(scripts[sourceIndex]!),
    }).commitment.toString("hex"),
    membership: buildMidgardValidationMerkleMembership(leaves, sourceIndex),
  }));
};

const purposeFixture = (
  hashes: readonly string[],
): readonly UnusedScriptPurposeOpening[] => {
  const leaves = hashes.map((scriptHashHex, frontierIndex) =>
    hashMidgardScriptPurposeLeaf({
      purposeKind: (frontierIndex % 4) as 0 | 1 | 2 | 3,
      purposeIndex: 0n,
      scriptHash: Buffer.from(scriptHashHex, "hex"),
      subject: Buffer.from([frontierIndex]),
    }),
  );
  return leaves.map((_, frontierIndex) => ({
    frontierIndex,
    purposeKind: (frontierIndex % 4) as 0 | 1 | 2 | 3,
    purposeIndex: 0,
    scriptHashHex: hashes[frontierIndex]!,
    purposeSubjectHex: Buffer.from([frontierIndex]).toString("hex"),
    membership: buildMidgardValidationMerkleMembership(leaves, frontierIndex),
  }));
};

const acceptedFinding = {
  subject: acceptedVerdictSubject(txId),
  scriptIndex: 1,
} as const;
const forcedFinding = {
  subject: forcedVerdictSubject({
    transactionId: txId,
    sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
    rejectionReason: { UnusedScriptWitness: { script_index: 1n } },
  }),
  scriptIndex: 1,
} as const;

const prepareUnusedScriptWitnessEvidence = ({
  finding,
  fieldPreimage,
  sources,
  purposes,
}: {
  readonly finding: UnusedScriptWitnessFinding;
  readonly fieldPreimage: Uint8Array;
  readonly sources: readonly UnusedScriptSourceOpening[];
  readonly purposes: readonly UnusedScriptPurposeOpening[];
}) =>
  prepareAgainstUniverse({
    finding,
    fieldPreimage,
    universe: {
      schemaVersion: "midgard-committed-script-universe-v1",
      transactionId: finding.subject.transaction_id,
      universeDigest: "99".repeat(32),
      sources,
      purposes,
    },
  });

describe("unusedScriptWitness V1", () => {
  it("exposes only infrastructure keys and rejects another runner category", async () => {
    expect(UNUSED_SCRIPT_WITNESS_CONFIG_KEYS).toEqual([
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
    let loaded = false;
    const runner = createUnusedScriptWitnessWorkflowRunnerSurface({
      loadRuntimeConfig: async () => {
        loaded = true;
        throw new Error("must not load");
      },
    });
    await expect(
      runner.runOrResume({ category: "missingRedeemer" } as never),
    ).rejects.toThrow(/category changed/u);
    expect(loaded).toBe(false);
  });

  it("detects the first unused accepted field-6 coordinate from the machine's retained stage-11 audit", async () => {
    // Three inline scripts; the first is used, the machine's source audit
    // stops at the second, and the third is never reached. Only the second
    // coordinate has a retained audit state the thread can authenticate.
    const fixture = await buildUnusedScriptWitnessFixture({
      direction: "accepted",
      claimedVerdict: "accepted",
      accusedUnused: true,
      sourceCount: 3,
      accusedIndex: 1,
      inputByte: 0x51,
      operatorVkey: "11".repeat(28),
      startTime: 1_750_000_000_000n,
    });
    const block = fixture.canonicalBlock("aa".repeat(32));
    const detections =
      await detectUnusedScriptWitnessCanonicalViolations(block);
    expect(detections.map(({ detectionId }) => detectionId)).toEqual([
      expect.stringMatching(
        new RegExp(`:accepted:0:${fixture.nativeTxId}:1$`, "u"),
      ),
    ]);
    // Without the retained ScriptSources states nothing is provable.
    const withoutRetainedDa = {
      ...block,
      reconstruction: {
        ...block.reconstruction,
        payload: {
          ...block.reconstruction.payload,
          block_body: {
            ...block.reconstruction.payload.block_body,
            validation_trace_witnesses: [],
          },
        },
      },
    } as CanonicalBlockEvidence;
    expect(
      await detectUnusedScriptWitnessCanonicalViolations(withoutRetainedDa),
    ).toEqual([]);
  });

  it("proves universal absence across spend, mint, observe, and receive purposes", () => {
    const evidence = prepareUnusedScriptWitnessEvidence({
      finding: acceptedFinding,
      fieldPreimage: preimage,
      sources: sourceFixture(),
      purposes: purposeFixture(
        Array(4).fill(hashMidgardVersionedScript(scriptA)),
      ),
    });
    expect(evidence.unused).toBe(true);
    expect(evidence.purposes.map(({ purposeKind }) => purposeKind)).toEqual([
      0, 1, 2, 3,
    ]);
    expect(unusedScriptWitnessEvidenceCloses(evidence)).toBe(true);
  });

  it("proves reverse use and contradicts a forced unused rejection", () => {
    const evidence = prepareUnusedScriptWitnessEvidence({
      finding: forcedFinding,
      fieldPreimage: preimage,
      sources: sourceFixture(),
      purposes: purposeFixture([hashMidgardVersionedScript(scriptB)]),
    });
    expect(evidence.unused).toBe(false);
    expect(evidence.matchedPurposeIndex).toBe(0);
    expect(unusedScriptWitnessEvidenceCloses(evidence)).toBe(true);
  });

  it("applies first-source precedence to a duplicate inline witness", () => {
    const duplicatePreimage = encodeMidgardVersionedScriptListPreimage([
      scriptB,
      scriptB,
    ]);
    const bytes = encodeMidgardVersionedScript(scriptB);
    const leaves = [0, 1].map((sourceIndex) =>
      hashMidgardInlineScriptSourceLeaf({
        sourceIndex: BigInt(sourceIndex),
        scriptLanguageTag: 3,
        scriptHash: Buffer.from(hashMidgardVersionedScript(scriptB), "hex"),
        scriptTotalLength: bytes.length,
        itemCommitment: buildMidgardBoundedItem({
          fieldIndex: 6,
          itemIndex: sourceIndex,
          bytes,
        }).commitment,
      }),
    );
    const sources = leaves.map((_, sourceIndex) => ({
      frontierIndex: sourceIndex,
      originKind: 0 as const,
      sourceIndex,
      sourceKeyHex: inlineSourceKeyHex(sourceIndex),
      languageTag: 3 as const,
      scriptHashHex: hashMidgardVersionedScript(scriptB),
      scriptTotalLength: bytes.length,
      itemCommitmentHex: buildMidgardBoundedItem({
        fieldIndex: 6,
        itemIndex: sourceIndex,
        bytes,
      }).commitment.toString("hex"),
      membership: buildMidgardValidationMerkleMembership(leaves, sourceIndex),
    }));
    const evidence = prepareUnusedScriptWitnessEvidence({
      finding: acceptedFinding,
      fieldPreimage: duplicatePreimage,
      sources,
      purposes: purposeFixture([hashMidgardVersionedScript(scriptB)]),
    });
    expect(evidence.firstMatchingSourceIndex).toBe(0);
    expect(evidence.unused).toBe(true);
  });

  it("authenticates inline and reference source locations without changing inline precedence", () => {
    const inline = sourceFixture();
    const sourceKeyHex = `825820${"44".repeat(32)}190000`;
    const referenceLeaf = hashMidgardReferenceScriptSourceLeaf({
      sourceKey: Buffer.from(sourceKeyHex, "hex"),
      scriptLanguageTag: 3,
      scriptHash: Buffer.from(hashMidgardVersionedScript(scriptB), "hex"),
      scriptTotalLength: 1,
      itemCommitment: Buffer.from("55".repeat(32), "hex"),
    });
    const leaves = [
      ...inline.map(({ membership }) => Buffer.from(membership.leafHash)),
      referenceLeaf,
    ];
    const sources: readonly UnusedScriptSourceOpening[] = [
      ...inline.map((source, frontierIndex) => ({
        ...source,
        membership: buildMidgardValidationMerkleMembership(
          leaves,
          frontierIndex,
        ),
      })),
      {
        frontierIndex: 2,
        originKind: 1,
        sourceIndex: 0,
        sourceKeyHex,
        languageTag: 3,
        scriptHashHex: hashMidgardVersionedScript(scriptB),
        scriptTotalLength: 1,
        itemCommitmentHex: "55".repeat(32),
        membership: buildMidgardValidationMerkleMembership(leaves, 2),
      },
    ];
    const evidence = prepareUnusedScriptWitnessEvidence({
      finding: forcedFinding,
      fieldPreimage: preimage,
      sources,
      purposes: purposeFixture([hashMidgardVersionedScript(scriptB)]),
    });
    expect(evidence.firstMatchingSourceIndex).toBe(1);
    expect(evidence.unused).toBe(false);
  });

  it("refuses an alternate-source membership substitution", () => {
    const sources = sourceFixture();
    expect(() =>
      prepareUnusedScriptWitnessEvidence({
        finding: acceptedFinding,
        fieldPreimage: preimage,
        sources: [
          { ...sources[0]!, membership: sources[1]!.membership },
          sources[1]!,
        ],
        purposes: purposeFixture([]),
      }),
    ).toThrow(/source frontier/u);
  });

  it("refuses a partial or reordered purpose frontier", () => {
    const purposes = purposeFixture([
      hashMidgardVersionedScript(scriptA),
      hashMidgardVersionedScript(scriptB),
    ]);
    expect(() =>
      prepareUnusedScriptWitnessEvidence({
        finding: acceptedFinding,
        fieldPreimage: preimage,
        sources: sourceFixture(),
        purposes: [purposes[1]!],
      }),
    ).toThrow(/purpose frontier/u);
  });

  it("refuses another reason and mutated coordinate", () => {
    expect(() =>
      classifyUnusedScriptWitnessFinding({
        ...forcedFinding,
        scriptIndex: 0,
      }),
    ).toThrow(/coordinate/u);
    expect(() =>
      classifyUnusedScriptWitnessFinding({
        subject: forcedVerdictSubject({
          transactionId: txId,
          sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
          rejectionReason: { OutputNonCanonical: { output_index: 1n } },
        }),
        scriptIndex: 1,
      }),
    ).toThrow(/reason/u);
  });

  it("routes a malicious fabricated frontier to trace invalidity", () => {
    const evidence = prepareUnusedScriptWitnessEvidence({
      finding: acceptedFinding,
      fieldPreimage: preimage,
      sources: sourceFixture(),
      purposes: purposeFixture([hashMidgardVersionedScript(scriptB)]),
    });
    expect(
      unusedScriptWitnessAccountabilityRoute({
        evidence,
        committedUniverseDigest: "99".repeat(32),
        canonicalUniverseDigest: "98".repeat(32),
      }),
    ).toBe("traceInvalid");
  });

  it("cannot convict an honest canonical header when the witness is used", () => {
    const evidence = prepareUnusedScriptWitnessEvidence({
      finding: acceptedFinding,
      fieldPreimage: preimage,
      sources: sourceFixture(),
      purposes: purposeFixture([hashMidgardVersionedScript(scriptB)]),
    });
    expect(evidence.unused).toBe(false);
    expect(
      unusedScriptWitnessAccountabilityRoute({
        evidence,
        committedUniverseDigest: "99".repeat(32),
        canonicalUniverseDigest: "99".repeat(32),
      }),
    ).toBe("none");
  });

  it("reconciles an interrupted exact transaction without callback configuration", async () => {
    const evidence = prepareUnusedScriptWitnessEvidence({
      finding: acceptedFinding,
      fieldPreimage: preimage,
      sources: sourceFixture(),
      purposes: purposeFixture([]),
    });
    const entries: UnusedScriptWitnessJournalEntry[] = [];
    let cursor: UnusedScriptWitnessCursor = {
      stage: "none",
      threadOutRef: "",
      checkpointDigest: evidence.checkpointDigest,
    };
    const journal = {
      load: async () => entries,
      append: async (entry: UnusedScriptWitnessJournalEntry) => {
        entries.push(entry);
      },
    };
    const actuator = {
      observe: async () => cursor,
      capture: async ({ source }: { source: UnusedScriptWitnessCursor }) => {
        const target = {
          ...source,
          stage: "step01" as const,
          threadOutRef: `${"aa".repeat(32)}#0`,
        };
        return {
          txHash: "bb".repeat(32),
          target,
          submit: async () => {
            cursor = target;
            return "bb".repeat(32);
          },
        };
      },
      transactionConfirmed: async () => true,
    };
    expect(
      await runUnusedScriptWitnessWorkflow({ evidence, journal, actuator }),
    ).toBe("none");
    expect(
      await runUnusedScriptWitnessWorkflow({ evidence, journal, actuator }),
    ).toBe("step01");
    expect(entries.map(({ phase }) => phase)).toEqual([
      "intent",
      "submitted",
      "confirmed",
    ]);
  });

  it("pins the cross-language golden vectors of every carried state encoding", () => {
    // Twin of `*_golden_vector` in
    // onchain/aiken/lib/midgard/fraud-proofs/unused-script-witness/rule.test.ak.
    const txIdGolden =
      "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";
    const h28a = "11".repeat(28);
    const h32 = "33".repeat(32);
    const subject = (direction: bigint) => ({
      version: 1n,
      direction,
      source_kind: direction,
      transaction_id: txIdGolden,
      source_key: direction === 0n ? "" : "01",
      rejection_reason:
        direction === 0n ? null : { UnusedScriptWitness: { script_index: 1n } },
    });
    const bound = (direction: bigint, scriptIndex: bigint) => ({
      subject: subject(direction),
      validation_traces_root: h32,
      validation_trace_count: 1n,
      script_index: scriptIndex,
    });
    const authenticated = (direction: bigint) => ({
      bound: bound(direction, 1n),
      prior_ledger_root: h32,
      language_tag: 3n,
      script_hash: h28a,
      script_total_length: 10n,
      item_commitment: h32,
      source_count: 2n,
      source_peaks: [{ height: 1n, hash: h32 }],
      purpose_count: 1n,
      purpose_peaks: [{ height: 0n, hash: h32 }],
    });
    const subjectHex = (direction: bigint) =>
      direction === 0n
        ? `d8799f0100005820${txIdGolden}40d87a80ff`
        : `d8799f0101015820${txIdGolden}4101d8799fd9051a9f01ffffff`;
    const authenticatedHex = (direction: bigint) =>
      `d8799fd8799f${subjectHex(direction)}5820${h32}0101ff5820${h32}03581c${h28a}0a5820${h32}029fd8799f015820${h32}ffff019fd8799f005820${h32}ffffff`;
    expect(
      Data.to(bound(0n, 0n) as never, UnusedScriptBoundWitnessSchema as never),
    ).toBe(`d8799f${subjectHex(0n)}5820${h32}0100ff`);
    expect(
      Data.to(
        authenticated(0n) as never,
        UnusedScriptAuthenticatedWitnessSchema as never,
      ),
    ).toBe(authenticatedHex(0n));
    expect(
      Data.to(
        authenticated(1n) as never,
        UnusedScriptAuthenticatedWitnessSchema as never,
      ),
    ).toBe(authenticatedHex(1n));
    // The domain-separated checkpoint seed agrees with the on-chain derivation.
    expect(
      Data.to(
        initialUnusedScriptWitnessReverseScan(
          authenticated(0n) as never,
        ) as never,
        UnusedScriptReverseScanSchema as never,
      ),
    ).toBe(
      `d8799f${authenticatedHex(0n)}0000d87980d8798058200f7f6c7b0a0b8b3507c96310eb9a38807ead1a0832623262ed24aebe8aa7499cff`,
    );
    expect(
      Data.to(
        { subject: subject(1n), script_index: 1n, unused: false } as never,
        UnusedScriptDecisionSchema as never,
      ),
    ).toBe(`d8799f${subjectHex(1n)}01d87980ff`);
  });

  it("cancels from every nonterminal physical step", async () => {
    const evidence = prepareUnusedScriptWitnessEvidence({
      finding: acceptedFinding,
      fieldPreimage: preimage,
      sources: sourceFixture(),
      purposes: purposeFixture([]),
    });
    for (const stage of [
      "step01",
      "step02",
      "step03",
      "step04",
      "step05",
      "step06",
    ] as const) {
      const entries: UnusedScriptWitnessJournalEntry[] = [];
      let cursor: UnusedScriptWitnessCursor = {
        stage,
        threadOutRef: `${"aa".repeat(32)}#0`,
        checkpointDigest: evidence.checkpointDigest,
      };
      const journal = {
        load: async () => entries,
        append: async (entry: UnusedScriptWitnessJournalEntry) => {
          entries.push(entry);
        },
      };
      const actuator = {
        observe: async () => cursor,
        capture: async () => {
          const target = { ...cursor, stage: "cancelled" as const };
          return {
            txHash: "cc".repeat(32),
            target,
            submit: async () => {
              cursor = target;
              return "cc".repeat(32);
            },
          };
        },
        transactionConfirmed: async () => true,
      };
      await expect(
        cancelUnusedScriptWitnessWorkflow({ evidence, journal, actuator }),
      ).resolves.toBe("cancelled");
      expect(entries.map(({ phase }) => phase)).toEqual([
        "intent",
        "submitted",
        "confirmed",
      ]);
    }
  });
});
