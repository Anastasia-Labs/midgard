import {
  buildMidgardBoundedItemV1,
  buildMidgardValidationMerkleMembershipV1,
  encodeMidgardVersionedScript,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardReferenceScriptSourceLeafV1,
  hashMidgardScriptPurposeLeafV1,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  makeNativeTx,
  plutusV3ScriptWitness,
} from "../../midgard-validation/tests/validation-fixtures.js";
import type { CanonicalBlockEvidenceV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import {
  classifyUnusedScriptWitnessFindingV1,
  prepareUnusedScriptWitnessEvidenceV1 as prepareAgainstUniverse,
  type UnusedScriptPurposeOpeningV1,
  type UnusedScriptSourceOpeningV1,
  unusedScriptWitnessAccountabilityRouteV1,
  unusedScriptWitnessEvidenceClosesV1,
  type UnusedScriptWitnessFindingV1,
} from "../src/unused-script-witness/family-v1.js";
import { detectUnusedScriptWitnessCanonicalViolationsV1 } from "../src/unused-script-witness/production-replay-v1.js";
import {
  createUnusedScriptWitnessProductionWorkflowRunnerSurfaceV1,
  UNUSED_SCRIPT_WITNESS_PRODUCTION_CONFIG_KEYS_V1,
} from "../src/unused-script-witness/production-v1.js";
import {
  cancelUnusedScriptWitnessWorkflowV1,
  runUnusedScriptWitnessWorkflowV1,
  type UnusedScriptWitnessCursorV1,
  type UnusedScriptWitnessJournalEntryV1,
} from "../src/unused-script-witness/workflow-v1.js";

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

const sourceFixture = (): readonly UnusedScriptSourceOpeningV1[] => {
  const leaves = scripts.map((script, sourceIndex) => {
    const bytes = encodeMidgardVersionedScript(script);
    return hashMidgardInlineScriptSourceLeafV1({
      sourceIndex: BigInt(sourceIndex),
      scriptLanguageTag: 3,
      scriptHash: Buffer.from(hashMidgardVersionedScript(script), "hex"),
      scriptTotalLength: bytes.length,
      itemCommitment: buildMidgardBoundedItemV1({
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
    sourceKeyHex: "",
    languageTag: 3 as const,
    scriptHashHex: hashMidgardVersionedScript(scripts[sourceIndex]!),
    scriptTotalLength: encodeMidgardVersionedScript(scripts[sourceIndex]!)
      .length,
    itemCommitmentHex: buildMidgardBoundedItemV1({
      fieldIndex: 6,
      itemIndex: sourceIndex,
      bytes: encodeMidgardVersionedScript(scripts[sourceIndex]!),
    }).commitment.toString("hex"),
    membership: buildMidgardValidationMerkleMembershipV1(leaves, sourceIndex),
  }));
};

const purposeFixture = (
  hashes: readonly string[],
): readonly UnusedScriptPurposeOpeningV1[] => {
  const leaves = hashes.map((scriptHashHex, frontierIndex) =>
    hashMidgardScriptPurposeLeafV1({
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
    membership: buildMidgardValidationMerkleMembershipV1(leaves, frontierIndex),
  }));
};

const acceptedFinding = {
  subject: acceptedVerdictSubjectV1(txId),
  scriptIndex: 1,
} as const;
const forcedFinding = {
  subject: forcedVerdictSubjectV1({
    transactionId: txId,
    sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
    rejectionReason: { UnusedScriptWitness: { script_index: 1n } },
  }),
  scriptIndex: 1,
} as const;

const prepareUnusedScriptWitnessEvidenceV1 = ({
  finding,
  fieldPreimage,
  sources,
  purposes,
}: {
  readonly finding: UnusedScriptWitnessFindingV1;
  readonly fieldPreimage: Uint8Array;
  readonly sources: readonly UnusedScriptSourceOpeningV1[];
  readonly purposes: readonly UnusedScriptPurposeOpeningV1[];
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
    expect(UNUSED_SCRIPT_WITNESS_PRODUCTION_CONFIG_KEYS_V1).toEqual([
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
    const runner = createUnusedScriptWitnessProductionWorkflowRunnerSurfaceV1({
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

  it("detects every unselected accepted field-6 coordinate from canonical retained replay", async () => {
    const transaction = makeNativeTx({
      scriptWitnesses: [
        plutusV3ScriptWitness(Buffer.from("01", "hex")),
        plutusV3ScriptWitness(Buffer.from("02", "hex")),
      ],
      scriptLanguages: ["PlutusV3"],
    });
    const detections = await detectUnusedScriptWitnessCanonicalViolationsV1({
      headerHash: "aa".repeat(32),
      transactions: [
        {
          nodeTxId: transaction.txId.toString("hex"),
          txCbor: transaction.txCbor.toString("hex"),
        },
      ],
      reconstruction: {
        payload: { block_body: { validation_trace_witnesses: [] } },
        forcedTransactions: [],
      },
    } as unknown as CanonicalBlockEvidenceV1);
    expect(detections.map(({ detectionId }) => detectionId)).toEqual([
      expect.stringMatching(/:0$/u),
      expect.stringMatching(/:1$/u),
    ]);
  });

  it("proves universal absence across spend, mint, observe, and receive purposes", () => {
    const evidence = prepareUnusedScriptWitnessEvidenceV1({
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
    expect(unusedScriptWitnessEvidenceClosesV1(evidence)).toBe(true);
  });

  it("proves reverse use and contradicts a forced unused rejection", () => {
    const evidence = prepareUnusedScriptWitnessEvidenceV1({
      finding: forcedFinding,
      fieldPreimage: preimage,
      sources: sourceFixture(),
      purposes: purposeFixture([hashMidgardVersionedScript(scriptB)]),
    });
    expect(evidence.unused).toBe(false);
    expect(evidence.matchedPurposeIndex).toBe(0);
    expect(unusedScriptWitnessEvidenceClosesV1(evidence)).toBe(true);
  });

  it("applies first-source precedence to a duplicate inline witness", () => {
    const duplicatePreimage = encodeMidgardVersionedScriptListPreimage([
      scriptB,
      scriptB,
    ]);
    const bytes = encodeMidgardVersionedScript(scriptB);
    const leaves = [0, 1].map((sourceIndex) =>
      hashMidgardInlineScriptSourceLeafV1({
        sourceIndex: BigInt(sourceIndex),
        scriptLanguageTag: 3,
        scriptHash: Buffer.from(hashMidgardVersionedScript(scriptB), "hex"),
        scriptTotalLength: bytes.length,
        itemCommitment: buildMidgardBoundedItemV1({
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
      sourceKeyHex: "",
      languageTag: 3 as const,
      scriptHashHex: hashMidgardVersionedScript(scriptB),
      scriptTotalLength: bytes.length,
      itemCommitmentHex: buildMidgardBoundedItemV1({
        fieldIndex: 6,
        itemIndex: sourceIndex,
        bytes,
      }).commitment.toString("hex"),
      membership: buildMidgardValidationMerkleMembershipV1(leaves, sourceIndex),
    }));
    const evidence = prepareUnusedScriptWitnessEvidenceV1({
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
    const referenceLeaf = hashMidgardReferenceScriptSourceLeafV1({
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
    const sources: readonly UnusedScriptSourceOpeningV1[] = [
      ...inline.map((source, frontierIndex) => ({
        ...source,
        membership: buildMidgardValidationMerkleMembershipV1(
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
        membership: buildMidgardValidationMerkleMembershipV1(leaves, 2),
      },
    ];
    const evidence = prepareUnusedScriptWitnessEvidenceV1({
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
      prepareUnusedScriptWitnessEvidenceV1({
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
      prepareUnusedScriptWitnessEvidenceV1({
        finding: acceptedFinding,
        fieldPreimage: preimage,
        sources: sourceFixture(),
        purposes: [purposes[1]!],
      }),
    ).toThrow(/purpose frontier/u);
  });

  it("refuses another reason and mutated coordinate", () => {
    expect(() =>
      classifyUnusedScriptWitnessFindingV1({
        ...forcedFinding,
        scriptIndex: 0,
      }),
    ).toThrow(/coordinate/u);
    expect(() =>
      classifyUnusedScriptWitnessFindingV1({
        subject: forcedVerdictSubjectV1({
          transactionId: txId,
          sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
          rejectionReason: { OutputNonCanonical: { output_index: 1n } },
        }),
        scriptIndex: 1,
      }),
    ).toThrow(/reason/u);
  });

  it("routes a malicious fabricated frontier to trace invalidity", () => {
    const evidence = prepareUnusedScriptWitnessEvidenceV1({
      finding: acceptedFinding,
      fieldPreimage: preimage,
      sources: sourceFixture(),
      purposes: purposeFixture([hashMidgardVersionedScript(scriptB)]),
    });
    expect(
      unusedScriptWitnessAccountabilityRouteV1({
        evidence,
        committedUniverseDigest: "99".repeat(32),
        canonicalUniverseDigest: "98".repeat(32),
      }),
    ).toBe("traceInvalid");
  });

  it("cannot convict an honest canonical header when the witness is used", () => {
    const evidence = prepareUnusedScriptWitnessEvidenceV1({
      finding: acceptedFinding,
      fieldPreimage: preimage,
      sources: sourceFixture(),
      purposes: purposeFixture([hashMidgardVersionedScript(scriptB)]),
    });
    expect(evidence.unused).toBe(false);
    expect(
      unusedScriptWitnessAccountabilityRouteV1({
        evidence,
        committedUniverseDigest: "99".repeat(32),
        canonicalUniverseDigest: "99".repeat(32),
      }),
    ).toBe("none");
  });

  it("reconciles an interrupted exact transaction without callback configuration", async () => {
    const evidence = prepareUnusedScriptWitnessEvidenceV1({
      finding: acceptedFinding,
      fieldPreimage: preimage,
      sources: sourceFixture(),
      purposes: purposeFixture([]),
    });
    const entries: UnusedScriptWitnessJournalEntryV1[] = [];
    let cursor: UnusedScriptWitnessCursorV1 = {
      stage: "none",
      threadOutRef: "",
      checkpointDigest: evidence.checkpointDigest,
    };
    const journal = {
      load: async () => entries,
      append: async (entry: UnusedScriptWitnessJournalEntryV1) => {
        entries.push(entry);
      },
    };
    const actuator = {
      observe: async () => cursor,
      capture: async ({ source }: { source: UnusedScriptWitnessCursorV1 }) => {
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
      await runUnusedScriptWitnessWorkflowV1({ evidence, journal, actuator }),
    ).toBe("none");
    expect(
      await runUnusedScriptWitnessWorkflowV1({ evidence, journal, actuator }),
    ).toBe("step01");
    expect(entries.map(({ phase }) => phase)).toEqual([
      "intent",
      "submitted",
      "confirmed",
    ]);
  });

  it("cancels from every nonterminal physical step", async () => {
    const evidence = prepareUnusedScriptWitnessEvidenceV1({
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
      const entries: UnusedScriptWitnessJournalEntryV1[] = [];
      let cursor: UnusedScriptWitnessCursorV1 = {
        stage,
        threadOutRef: `${"aa".repeat(32)}#0`,
        checkpointDigest: evidence.checkpointDigest,
      };
      const journal = {
        load: async () => entries,
        append: async (entry: UnusedScriptWitnessJournalEntryV1) => {
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
        cancelUnusedScriptWitnessWorkflowV1({ evidence, journal, actuator }),
      ).resolves.toBe("cancelled");
      expect(entries.map(({ phase }) => phase)).toEqual([
        "intent",
        "submitted",
        "confirmed",
      ]);
    }
  });
});
