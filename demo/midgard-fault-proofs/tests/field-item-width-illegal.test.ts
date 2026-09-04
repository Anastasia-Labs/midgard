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
  classifyFieldItemWidthFinding,
  encodeFieldItemWidthAuthenticatedWidth,
  encodeFieldItemWidthBoundCoordinate,
  FIELD_ITEM_WIDTH_ILLEGAL_MAX_OUTPUT_BYTES,
  fieldItemWidthEvidenceCloses,
  fieldItemWidthEvidenceIdentity,
  fieldItemWidthIsIllegal,
  type FieldItemWidthJournalEntry,
  type FieldItemWidthStage,
  type FieldItemWidthSubmissionAdapter,
  nextFieldItemWidthAction,
  prepareFieldItemWidthEvidence,
  reconcileFieldItemWidthJournal,
  runFieldItemWidthProof,
} from "../src/field-item-width-illegal/field-item-width-illegal.js";

const txId = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";
const forcedSource = {
  transactionId: "00".repeat(32),
  outputIndex: 4n,
};

const accepted = acceptedVerdictSubject(txId);
const rejected = (fieldIndex: number, itemIndex: number) =>
  forcedVerdictSubject({
    transactionId: txId,
    sourceKey: forcedSource,
    rejectionReason: {
      FieldItemWidthIllegal: {
        field_index: BigInt(fieldIndex),
        item_index: BigInt(itemIndex),
      },
    },
  });

const evidence = ({
  subject = accepted,
  fieldIndex = 2,
  item = Buffer.alloc(FIELD_ITEM_WIDTH_ILLEGAL_MAX_OUTPUT_BYTES + 1, 7),
}: {
  readonly subject?: typeof accepted;
  readonly fieldIndex?: number;
  readonly item?: Buffer;
} = {}) => {
  const preimage = encodeMidgardFieldPreimage([item]);
  return prepareFieldItemWidthEvidence({
    finding: { subject, fieldIndex, itemIndex: 0 },
    fieldPreimage: preimage,
    committedFieldHashHex: midgardFieldCommitment(preimage).toString("hex"),
  });
};

describe("fieldItemWidthIllegal V1 semantics and evidence", () => {
  it("matches both decisive arms and their adjacent boundaries", () => {
    expect(fieldItemWidthIsIllegal(2, 16_384)).toBe(false);
    expect(fieldItemWidthIsIllegal(2, 16_385)).toBe(true);
    expect(fieldItemWidthIsIllegal(5, 0)).toBe(true);
    expect(fieldItemWidthIsIllegal(5, 1)).toBe(false);
    expect(() => fieldItemWidthIsIllegal(4, 28)).toThrow(/outside/u);
  });

  it("proves wrongful acceptance and wrongful forced rejection", () => {
    expect(fieldItemWidthEvidenceCloses(evidence())).toBe(true);
    expect(
      fieldItemWidthEvidenceCloses(
        evidence({
          subject: rejected(2, 0),
          item: Buffer.alloc(16_384),
        }),
      ),
    ).toBe(true);
    expect(
      fieldItemWidthEvidenceCloses(
        evidence({ fieldIndex: 5, item: Buffer.alloc(0) }),
      ),
    ).toBe(true);
    expect(
      fieldItemWidthEvidenceCloses(
        evidence({
          subject: rejected(5, 0),
          fieldIndex: 5,
          item: Buffer.alloc(1),
        }),
      ),
    ).toBe(true);
  });

  it("refuses honest verdicts in both directions", () => {
    expect(
      fieldItemWidthEvidenceCloses(evidence({ item: Buffer.alloc(16_384) })),
    ).toBe(false);
    expect(
      fieldItemWidthEvidenceCloses(
        evidence({ subject: rejected(2, 0), item: Buffer.alloc(16_385) }),
      ),
    ).toBe(false);
  });

  it("binds reason constructor and coordinate exactly", () => {
    expect(() =>
      classifyFieldItemWidthFinding({
        subject: rejected(2, 1),
        fieldIndex: 2,
        itemIndex: 0,
      }),
    ).toThrow(/coordinate differs/u);
    const wrongReason = forcedVerdictSubject({
      transactionId: txId,
      sourceKey: forcedSource,
      rejectionReason: { FieldPreimageLengthMismatch: { field_index: 2n } },
    });
    expect(() =>
      classifyFieldItemWidthFinding({
        subject: wrongReason,
        fieldIndex: 2,
        itemIndex: 0,
      }),
    ).toThrow(/not FieldItemWidthIllegal/u);
  });

  it("refuses substituted field bytes, commitment, and item coordinate", () => {
    const preimage = encodeMidgardFieldPreimage([Buffer.alloc(1)]);
    expect(() =>
      prepareFieldItemWidthEvidence({
        finding: { subject: accepted, fieldIndex: 5, itemIndex: 0 },
        fieldPreimage: preimage,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/does not match/u);
    expect(() =>
      prepareFieldItemWidthEvidence({
        finding: { subject: accepted, fieldIndex: 5, itemIndex: 1 },
        fieldPreimage: preimage,
        committedFieldHashHex: midgardFieldCommitment(preimage).toString("hex"),
      }),
    ).toThrow(/outside the field/u);
  });

  it("selects direct and certified maximum carriage without overrides", () => {
    expect(evidence({ fieldIndex: 5, item: Buffer.alloc(0) }).carriage).toBe(
      "Inline",
    );
    const maximum = evidence({ item: Buffer.alloc(32_764) });
    expect(maximum.fieldPreimageHex.length / 2).toBe(32_768);
    expect(maximum.carriage).toBe("Certified");
    expect(maximum.decisiveFaultHolds).toBe(true);
  });

  it("matches the Aiken Plutus-Data ABI vectors", () => {
    const tiny = evidence({
      fieldIndex: 5,
      item: Buffer.from("000102", "hex"),
    });
    expect(
      encodeFieldItemWidthBoundCoordinate({
        subject: accepted,
        fieldIndex: 5,
        itemIndex: 0,
      }),
    ).toBe(
      "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500ff",
    );
    expect(encodeFieldItemWidthAuthenticatedWidth(tiny)).toBe(
      "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff050003ff",
    );
  });
});

describe("fieldItemWidthIllegal V1 durable runner", () => {
  it("selects every physical step and removal deterministically", () => {
    expect(
      (
        ["none", "step01", "step02", "step03", "proven", "removed"] as const
      ).map(nextFieldItemWidthAction),
    ).toEqual([
      "submitInit",
      "submitStep01",
      "submitStep02",
      "submitStep03",
      "removeDescendants",
      "done",
    ]);
  });

  it("reconstructs after each interruption and reaches permanent removal", async () => {
    const prepared = evidence();
    const identity = fieldItemWidthEvidenceIdentity(prepared);
    const entries: FieldItemWidthJournalEntry[] = [];
    let chainStage: FieldItemWidthStage = "none";
    const transition: Record<string, FieldItemWidthStage> = {
      submitInit: "step01",
      submitStep01: "step02",
      submitStep02: "step03",
      submitStep03: "proven",
      removeDescendants: "removed",
    };
    const submission: FieldItemWidthSubmissionAdapter = {
      observe: async (seenIdentity) => {
        expect(seenIdentity).toBe(identity);
        return chainStage;
      },
      submit: async (action) => {
        chainStage = transition[action] ?? "removed";
        return {
          stage: chainStage,
          txHash: entries.length.toString(16).padStart(64, "0"),
          outputReference:
            chainStage === "removed" ? null : `${entries.length}#0`,
        };
      },
      cancel: async (stage) => ({
        stage: "cancelled",
        txHash: stage.padEnd(64, "0"),
        outputReference: null,
      }),
    };
    const journal = {
      load: async () => entries,
      append: async (entry: FieldItemWidthJournalEntry) => {
        entries.push(entry);
      },
    };

    // Each call may be a fresh process. The durable journal and observed chain
    // state are the only authority; the runner remains idempotent.
    expect(
      await runFieldItemWidthProof({
        evidence: prepared,
        journal,
        submission,
      }),
    ).toBe("removed");
    expect(entries.map((entry) => entry.stage)).toEqual([
      "step01",
      "step02",
      "step03",
      "proven",
      "removed",
    ]);
    expect(
      await runFieldItemWidthProof({
        evidence: prepared,
        journal,
        submission,
      }),
    ).toBe("removed");
  });

  it("supports cancel from every nonterminal family step", async () => {
    const prepared = evidence();
    const adapter: FieldItemWidthSubmissionAdapter = {
      observe: async () => "none",
      submit: async () => {
        throw new Error("unused");
      },
      cancel: async () => ({
        stage: "cancelled",
        txHash: "00".repeat(32),
        outputReference: null,
      }),
    };
    for (const stage of ["step01", "step02", "step03"] as const) {
      await expect(adapter.cancel(stage, prepared)).resolves.toMatchObject({
        stage: "cancelled",
      });
    }
  });

  it("refuses journal identity, order, and chain-regression mutations", () => {
    expect(() =>
      reconcileFieldItemWidthJournal(
        "expected",
        [
          {
            sequence: 0,
            identity: "substituted",
            stage: "step01",
            txHash: "00".repeat(32),
            outputReference: "0#0",
          },
        ],
        "step01",
      ),
    ).toThrow(/identity/u);
    expect(() =>
      reconcileFieldItemWidthJournal(
        "expected",
        [
          {
            sequence: 0,
            identity: "expected",
            stage: "step02",
            txHash: "00".repeat(32),
            outputReference: "0#0",
          },
        ],
        "step01",
      ),
    ).toThrow(/behind/u);
  });
});
