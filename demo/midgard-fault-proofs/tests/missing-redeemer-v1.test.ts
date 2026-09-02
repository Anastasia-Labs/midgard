import {
  encodeMidgardFieldPreimageV1,
  encodeMidgardRedeemerWitnessItemV1,
  encodeMidgardSpendInputItemV1,
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardReferenceScriptSourceLeafV1,
  midgardFieldCommitmentV1,
  type MidgardRedeemerPurposeV1,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { createMissingRedeemerDirectoryJournalV1 } from "../src/missing-redeemer/directory-journal-v1.js";
import {
  type AuthenticatedScriptPurposeV1,
  classifyMissingRedeemerFindingV1,
  missingRedeemerEvidenceClosesV1,
  type MissingRedeemerPurposeKindV1,
  prepareMissingRedeemerEvidenceV1,
} from "../src/missing-redeemer/family-v1.js";
import { decodeMissingRedeemerStageTenControlV1 } from "../src/missing-redeemer/retained-stage-ten-v1.js";
import {
  encodeMissingRedeemerWalkCheckpointV1,
  hashMissingRedeemerWalkCheckpointV1,
  planMissingRedeemerStagedWalkV1,
} from "../src/missing-redeemer/staged-plan-v1.js";
import {
  type MissingRedeemerDurableStateV1,
  reconcileMissingRedeemerStateV1,
  runMissingRedeemerWorkflowV1,
} from "../src/missing-redeemer/workflow-v1.js";

const txId = "00".repeat(32);
const sourceKey = { transactionId: "11".repeat(32), outputIndex: 0n };
const sourceByKind = [
  "resolved-reference",
  "witness",
  "witness",
  "resolved-reference",
] as const;
const frontier = (): readonly AuthenticatedScriptPurposeV1[] =>
  ([0, 1, 2, 3] as const).map((purposeKind) => {
    const source = sourceByKind[purposeKind];
    const sourceIndex = purposeKind;
    const sourceOriginKind = source === "witness" ? 0 : 1;
    const sourceKey =
      sourceOriginKind === 0
        ? encodeCbor(BigInt(sourceIndex))
        : encodeMidgardSpendInputItemV1({
            txId: Buffer.alloc(32, purposeKind + 20),
            outputIndex: purposeKind,
          });
    const scriptHashHex = (purposeKind + 1)
      .toString(16)
      .padStart(2, "0")
      .repeat(28);
    const sourceItemCommitmentHex = (purposeKind + 40)
      .toString(16)
      .padStart(2, "0")
      .repeat(32);
    const sourceLeaf =
      sourceOriginKind === 0
        ? hashMidgardInlineScriptSourceLeafV1({
            sourceIndex: BigInt(sourceIndex),
            scriptLanguageTag: 3,
            scriptHash: Buffer.from(scriptHashHex, "hex"),
            scriptTotalLength: 17,
            itemCommitment: Buffer.from(sourceItemCommitmentHex, "hex"),
          })
        : hashMidgardReferenceScriptSourceLeafV1({
            sourceKey,
            scriptLanguageTag: 3,
            scriptHash: Buffer.from(scriptHashHex, "hex"),
            scriptTotalLength: 17,
            itemCommitment: Buffer.from(sourceItemCommitmentHex, "hex"),
          });
    return {
      purposeKind,
      purposeIndex: 0,
      scriptHashHex,
      subjectHex: (purposeKind + 5).toString(16).padStart(2, "0").repeat(32),
      source,
      sourceIndex,
      sourceOriginKind,
      sourceKeyHex: sourceKey.toString("hex"),
      sourceLanguageTag: 3,
      sourceTotalLength: 17,
      sourceItemCommitmentHex,
      sourceLeafHashHex: sourceLeaf.toString("hex"),
      traceStateHashHex: "aa".repeat(32),
      workRootHex: "bb".repeat(32),
    };
  });
const purposeName = ["Spend", "Mint", "Reward", "Receive"] as const;
const field = (
  entries: readonly (readonly [MissingRedeemerPurposeKindV1, number])[],
) =>
  encodeMidgardFieldPreimageV1(
    entries.map(([kind, index]) =>
      encodeMidgardRedeemerWitnessItemV1({
        purpose: purposeName[kind] satisfies MidgardRedeemerPurposeV1,
        index: BigInt(index),
        redeemerCbor: Buffer.from("00", "hex"),
        executionUnits: { memory: 1n, steps: 2n },
      }),
    ),
  );
const evidence = (
  kind: MissingRedeemerPurposeKindV1,
  entries: readonly (readonly [MissingRedeemerPurposeKindV1, number])[],
  forced = false,
) => {
  const bytes = field(entries);
  return prepareMissingRedeemerEvidenceV1({
    finding: {
      subject: forced
        ? forcedVerdictSubjectV1({
            transactionId: txId,
            sourceKey,
            rejectionReason: {
              RedeemerMissing: {
                purpose_kind: BigInt(kind),
                purpose_index: 0n,
              },
            },
          })
        : acceptedVerdictSubjectV1(txId),
      purposeKind: kind,
      purposeIndex: 0,
    },
    authenticatedPurpose: frontier()[kind]!,
    redeemerFieldPreimage: bytes,
    committedFieldHashHex: midgardFieldCommitmentV1(bytes).toString("hex"),
  });
};

describe("missingRedeemer V1", () => {
  it("decodes only the exact 31-field ScriptSources stage-10 control", () => {
    const discovery = encodeCbor([
      0n,
      1n,
      0n,
      0n,
      0n,
      Buffer.alloc(28, 1),
      Buffer.from([2]),
      0n,
      3n,
      Buffer.alloc(32, 3),
      0n,
      0n,
      Buffer.alloc(0),
      0n,
      [],
    ]);
    const control = encodeCbor([
      Buffer.from([1]),
      Buffer.from([2]),
      Buffer.from([3]),
      Buffer.from([4]),
      0n,
      Buffer.alloc(32),
      0n,
      Buffer.alloc(32),
      [],
      10n,
      1n,
      [[0n, Buffer.alloc(32, 3)]],
      0n,
      [],
      0n,
      Buffer.alloc(32),
      Buffer.alloc(32),
      0n,
      1n,
      [[0n, Buffer.alloc(32, 4)]],
      0n,
      0n,
      [],
      0n,
      [0n, [], 0n, Buffer.alloc(0), Buffer.alloc(0), []],
      1n,
      0n,
      [0n, Buffer.alloc(0), 0n],
      [
        -1n,
        0n,
        Buffer.alloc(0),
        Buffer.alloc(0),
        0n,
        Buffer.alloc(0),
        0n,
        0n,
        0n,
        Buffer.alloc(0),
        0n,
        [],
      ],
      Buffer.alloc(32),
      discovery,
    ]);
    const decoded = decodeMissingRedeemerStageTenControlV1(control);
    expect(decoded.stage).toBe(10n);
    expect(decoded.discovery.matched_language_tag).toBe(3n);
    expect(() =>
      decodeMissingRedeemerStageTenControlV1(encodeCbor([10n])),
    ).toThrow(/exact stage 10/u);
  });
  it("builds canonical field-8 grammar/walk checkpoints through every batch", () => {
    const bytes = field(
      Array.from({ length: 33 }, (_, index) => [0, index + 1] as const),
    );
    const staged = planMissingRedeemerStagedWalkV1({
      transactionId: txId,
      fieldPreimageCbor: bytes.toString("hex"),
    });
    expect(staged.grammar.map(({ nextItemIndex }) => nextItemIndex)).toEqual([
      16, 32, 33,
    ]);
    expect(staged.walk.map(({ nextItemIndex }) => nextItemIndex)).toEqual([
      16, 32, 33,
    ]);
    expect(encodeMissingRedeemerWalkCheckpointV1(staged.walk[0]!)[36]).toBe(8);
    expect(hashMissingRedeemerWalkCheckpointV1(staged.walk[0]!)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
  });
  it("proves complete absence for every accepted purpose kind", () => {
    for (const kind of [0, 1, 2, 3] as const) {
      const other = ((kind + 1) % 4) as MissingRedeemerPurposeKindV1;
      const value = evidence(kind, [[other, 0]]);
      expect(value.redeemerMissing).toBe(true);
      expect(value.checkpoints.at(-1)?.cursor).toBe(value.itemCount);
      expect(missingRedeemerEvidenceClosesV1(value)).toBe(true);
    }
  });

  it("proves wrongful rejection from an exact present pointer for every kind", () => {
    for (const kind of [0, 1, 2, 3] as const) {
      const value = evidence(kind, [[kind, 0]], true);
      expect(value.redeemerMissing).toBe(false);
      expect(missingRedeemerEvidenceClosesV1(value)).toBe(true);
    }
  });

  it("scans the complete frontier and refuses alternate tag/index substitution", () => {
    const entries = Array.from(
      { length: 33 },
      (_, index) => [1, index + 1] as const,
    );
    const value = evidence(0, entries);
    expect(value.checkpoints.map((point) => point.cursor)).toEqual([
      16, 32, 33,
    ]);
    expect(value.redeemerMissing).toBe(true);
    expect(evidence(0, [[1, 0]]).redeemerMissing).toBe(true);
    expect(evidence(0, [[0, 1]]).redeemerMissing).toBe(true);
  });

  it("refuses reason, purpose-frontier, and field substitutions", () => {
    const wrong = forcedVerdictSubjectV1({
      transactionId: txId,
      sourceKey,
      rejectionReason: {
        RedeemerMissing: { purpose_kind: 1n, purpose_index: 0n },
      },
    });
    expect(() =>
      classifyMissingRedeemerFindingV1({
        subject: wrong,
        purposeKind: 0,
        purposeIndex: 0,
      }),
    ).toThrow(/coordinate/u);
    const bytes = field([]);
    expect(() =>
      prepareMissingRedeemerEvidenceV1({
        finding: {
          subject: acceptedVerdictSubjectV1(txId),
          purposeKind: 0,
          purposeIndex: 0,
        },
        authenticatedPurpose: frontier()[1]!,
        redeemerFieldPreimage: bytes,
        committedFieldHashHex: midgardFieldCommitmentV1(bytes).toString("hex"),
      }),
    ).toThrow(/differs from/u);
    expect(() =>
      prepareMissingRedeemerEvidenceV1({
        finding: {
          subject: acceptedVerdictSubjectV1(txId),
          purposeKind: 0,
          purposeIndex: 0,
        },
        authenticatedPurpose: frontier()[0]!,
        redeemerFieldPreimage: bytes,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/commitment/u);
  });

  it("rejects alternate source and native-language substitutions", () => {
    const bytes = field([]);
    const base = frontier();
    const prepare = (authenticatedPurpose: AuthenticatedScriptPurposeV1) =>
      prepareMissingRedeemerEvidenceV1({
        finding: {
          subject: acceptedVerdictSubjectV1(txId),
          purposeKind: 0,
          purposeIndex: 0,
        },
        authenticatedPurpose,
        redeemerFieldPreimage: bytes,
        committedFieldHashHex: midgardFieldCommitmentV1(bytes).toString("hex"),
      });
    expect(() =>
      prepare({
        ...base[0]!,
        sourceKeyHex: encodeCbor(7n).toString("hex"),
      }),
    ).toThrow(/source key|output reference/u);
    expect(() =>
      prepare({
        ...base[0]!,
        sourceLanguageTag: 0 as unknown as 3,
      }),
    ).toThrow(/redeemer-bearing Plutus/u);
    expect(() =>
      prepare({ ...base[0]!, sourceLeafHashHex: "cc".repeat(32) }),
    ).toThrow(/descriptor\/leaf/u);
  });
});

describe("missingRedeemer durable workflow", () => {
  it("persists compare-and-append journal state across instances", async () => {
    const directory = await mkdtemp(join(tmpdir(), "missing-redeemer-"));
    try {
      const state: MissingRedeemerDurableStateV1 = {
        stage: "step02a",
        scanCursor: 0,
        txHash: "ee".repeat(32),
        outputReference: `${"ee".repeat(32)}#0`,
      };
      const first = await createMissingRedeemerDirectoryJournalV1(directory);
      await first.append("identity", 0, state);
      const reopened = await createMissingRedeemerDirectoryJournalV1(directory);
      expect(await reopened.load("identity")).toEqual([state]);
      await expect(reopened.append("identity", 0, state)).rejects.toThrow(
        /compare-and-append conflict/u,
      );
    } finally {
      await rm(directory, { recursive: true });
    }
  });

  it("resumes from authenticated state and permanently removes after mint", async () => {
    const prepared = evidence(
      0,
      Array.from({ length: 33 }, (_, index) => [1, index] as const),
    );
    const entries: MissingRedeemerDurableStateV1[] = [];
    const stages = [
      "step01",
      "step02",
      "step02a",
      "step02b",
      "step03",
      "step04",
      "step04",
      "step04",
      "step05",
      "proven",
      "removed",
    ] as const;
    let submitted = 0;
    let observed: MissingRedeemerDurableStateV1 = {
      stage: "none",
      scanCursor: 0,
      txHash: "00".repeat(32),
      outputReference: null,
    };
    const result = await runMissingRedeemerWorkflowV1({
      evidence: prepared,
      journal: {
        load: async () => entries,
        append: async (_identity, expectedLength, state) => {
          expect(expectedLength).toBe(entries.length);
          entries.push(state);
        },
      },
      actuator: {
        observe: async () => observed,
        submit: async ({ action }) => {
          expect(action).toBe(
            [
              "init",
              "bind",
              "authenticatePurpose",
              "authenticateTrace",
              "authenticateSelection",
              "openRedeemers",
              "scan",
              "scan",
              "scan",
              "finalize",
              "remove",
            ][submitted],
          );
          observed = {
            stage: stages[submitted]!,
            scanCursor:
              stages[submitted] === "step04"
                ? [0, 16, 32][submitted - 5]!
                : stages[submitted] === "step05"
                  ? 33
                  : 0,
            txHash: submitted.toString(16).padStart(64, "0"),
            outputReference:
              stages[submitted] === "removed" ? null : `${submitted}#0`,
          };
          submitted += 1;
          return observed;
        },
      },
    });
    expect(result).toBe("removed");
    expect(entries.map(({ stage }) => stage)).toEqual(stages);
  });

  it("fails closed on journal stage or scan regression", () => {
    const recorded: MissingRedeemerDurableStateV1 = {
      stage: "step04",
      scanCursor: 16,
      txHash: "11".repeat(32),
      outputReference: `${"11".repeat(32)}#0`,
    };
    expect(() =>
      reconcileMissingRedeemerStateV1([recorded], {
        ...recorded,
        stage: "step03",
      }),
    ).toThrow(/chain regressed/u);
    expect(() =>
      reconcileMissingRedeemerStateV1([recorded], {
        ...recorded,
        scanCursor: 15,
      }),
    ).toThrow(/checkpoint regressed/u);
  });

  it("treats an observed on-chain cancellation as terminal", async () => {
    const result = await runMissingRedeemerWorkflowV1({
      evidence: evidence(0, []),
      journal: {
        load: async () => [],
        append: async () => {
          throw new Error("terminal cancellation must not append");
        },
      },
      actuator: {
        observe: async () => ({
          stage: "cancelled",
          scanCursor: 0,
          txHash: "dd".repeat(32),
          outputReference: null,
        }),
        submit: async () => {
          throw new Error("terminal cancellation must not submit");
        },
      },
    });
    expect(result).toBe("cancelled");
  });
});
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
