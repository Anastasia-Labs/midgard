import { midgardFieldCommitment } from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  type AuthenticatedScriptPurpose,
  classifyMissingRedeemerFinding,
  missingRedeemerEvidenceCloses,
  type MissingRedeemerPurposeKind,
  prepareMissingRedeemerEvidence,
} from "../src/missing-redeemer/family.js";
import { decodeMissingRedeemerStageTenControl } from "../src/missing-redeemer/retained-stage-ten.js";
import {
  createMissingRedeemerStagedPlanner,
  encodeMissingRedeemerWalkCheckpoint,
  hashMissingRedeemerWalkCheckpoint,
  planMissingRedeemerStagedWalk,
} from "../src/missing-redeemer/staged-plan.js";
import {
  evidence,
  field,
  frontier,
  sourceKey,
  txId,
} from "./missing-redeemer.frontier.js";

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
      Buffer.alloc(0),
      Buffer.alloc(0),
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
    const decoded = decodeMissingRedeemerStageTenControl(control);
    expect(decoded.stage).toBe(10n);
    expect(decoded.discovery.matched_language_tag).toBe(3n);
    expect(() =>
      decodeMissingRedeemerStageTenControl(encodeCbor([10n])),
    ).toThrow(/exact stage 10/u);
  });
  it("reuses only the exact walk inputs and isolates mutable item bytes", () => {
    const plan = createMissingRedeemerStagedPlanner();
    const input = {
      transactionId: txId,
      fieldPreimageCbor: field([
        [0, 1],
        [0, 2],
      ]).toString("hex"),
    };
    const first = plan(input);
    const second = plan({ ...input, itemBudget: 16 });
    expect(second).toEqual(first);
    expect(second.initialGrammar).toBe(first.initialGrammar);
    first.items[0]!.fill(0);
    expect(plan(input).items).toEqual(
      planMissingRedeemerStagedWalk(input).items,
    );
    expect(Object.isFrozen(first.initialGrammar)).toBe(true);
    expect(Object.isFrozen(first.grammar[0])).toBe(true);
    const newId = plan({ ...input, transactionId: "11".repeat(32) });
    expect(newId.initialGrammar.txId).toBe("11".repeat(32));
    expect(newId.initialGrammar).not.toBe(second.initialGrammar);
    const changedField = plan({
      ...input,
      fieldPreimageCbor: field([[0, 3]]).toString("hex"),
    });
    expect(changedField.items).toHaveLength(1);
    const changedBudget = plan({ ...input, itemBudget: 1 });
    expect(changedBudget.grammar).toHaveLength(2);
    expect(() => plan({ ...input, itemBudget: 0 })).toThrow("item budget");
    expect(() => plan({ ...input, fieldPreimageCbor: "ff" })).toThrow();
    expect(plan(input)).toEqual(planMissingRedeemerStagedWalk(input));
    expect(createMissingRedeemerStagedPlanner()(input).initialGrammar).not.toBe(
      plan(input).initialGrammar,
    );
  });
  it("builds canonical field-8 grammar/walk checkpoints through every batch", () => {
    const bytes = field(
      Array.from({ length: 33 }, (_, index) => [0, index + 1] as const),
    );
    const staged = planMissingRedeemerStagedWalk({
      transactionId: txId,
      fieldPreimageCbor: bytes.toString("hex"),
    });
    expect(staged.grammar.map(({ nextItemIndex }) => nextItemIndex)).toEqual([
      16, 32, 33,
    ]);
    expect(staged.walk.map(({ nextItemIndex }) => nextItemIndex)).toEqual([
      16, 32, 33,
    ]);
    expect(encodeMissingRedeemerWalkCheckpoint(staged.walk[0]!)[36]).toBe(8);
    expect(hashMissingRedeemerWalkCheckpoint(staged.walk[0]!)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
  });
  it("proves complete absence for every accepted purpose kind", () => {
    for (const kind of [0, 1, 2, 3] as const) {
      const other = ((kind + 1) % 4) as MissingRedeemerPurposeKind;
      const value = evidence(kind, [[other, 0]]);
      expect(value.redeemerMissing).toBe(true);
      expect(value.checkpoints.at(-1)?.cursor).toBe(value.itemCount);
      expect(missingRedeemerEvidenceCloses(value)).toBe(true);
    }
  });

  it("proves wrongful rejection from an exact present pointer for every kind", () => {
    for (const kind of [0, 1, 2, 3] as const) {
      const value = evidence(kind, [[kind, 0]], true);
      expect(value.redeemerMissing).toBe(false);
      expect(missingRedeemerEvidenceCloses(value)).toBe(true);
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
    const wrong = forcedVerdictSubject({
      transactionId: txId,
      sourceKey,
      rejectionReason: {
        RedeemerMissing: { purpose_kind: 1n, purpose_index: 0n },
      },
    });
    expect(() =>
      classifyMissingRedeemerFinding({
        subject: wrong,
        purposeKind: 0,
        purposeIndex: 0,
      }),
    ).toThrow(/coordinate/u);
    const bytes = field([]);
    expect(() =>
      prepareMissingRedeemerEvidence({
        finding: {
          subject: acceptedVerdictSubject(txId),
          purposeKind: 0,
          purposeIndex: 0,
        },
        authenticatedPurpose: frontier()[1]!,
        redeemerFieldPreimage: bytes,
        committedFieldHashHex: midgardFieldCommitment(bytes).toString("hex"),
      }),
    ).toThrow(/differs from/u);
    expect(() =>
      prepareMissingRedeemerEvidence({
        finding: {
          subject: acceptedVerdictSubject(txId),
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
    const prepare = (authenticatedPurpose: AuthenticatedScriptPurpose) =>
      prepareMissingRedeemerEvidence({
        finding: {
          subject: acceptedVerdictSubject(txId),
          purposeKind: 0,
          purposeIndex: 0,
        },
        authenticatedPurpose,
        redeemerFieldPreimage: bytes,
        committedFieldHashHex: midgardFieldCommitment(bytes).toString("hex"),
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
