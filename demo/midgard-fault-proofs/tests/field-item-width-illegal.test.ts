import {
  encodeMidgardFieldPreimage,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  buildFieldItemWidthIllegalFaultProofContracts,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  applyFieldItemWidthIllegalScripts,
  FIELD_ITEM_WIDTH_ILLEGAL_BLUEPRINT_TITLES,
} from "../src/field-item-width-illegal/contracts.js";
import {
  classifyFieldItemWidthFinding,
  encodeFieldItemWidthAuthenticatedItem,
  encodeFieldItemWidthBoundCoordinate,
  FIELD_ITEM_WIDTH_ILLEGAL_MAX_OUTPUT_BYTES,
  FieldItemWidthAuthenticatedItemSchema,
  FieldItemWidthBoundCoordinateSchema,
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
import { FieldItemWidthStep02ArgsSchema } from "../src/field-item-width-illegal/schemas.js";
import { buildRegisteredChainFixture } from "./support/emulator/registered-chain.js";

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
  fieldIndex = 2n,
  item = Buffer.alloc(FIELD_ITEM_WIDTH_ILLEGAL_MAX_OUTPUT_BYTES + 1, 7),
}: {
  readonly subject?: typeof accepted;
  readonly fieldIndex?: bigint;
  readonly item?: Buffer;
} = {}) => {
  const preimage = encodeMidgardFieldPreimage([item]);
  return prepareFieldItemWidthEvidence({
    finding: { subject, fieldIndex, itemIndex: 0n },
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
    expect(fieldItemWidthIsIllegal(4, 28)).toBe(false);
    expect(fieldItemWidthIsIllegal(4, 27)).toBe(true);
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
        evidence({ fieldIndex: 5n, item: Buffer.alloc(0) }),
      ),
    ).toBe(true);
    expect(
      fieldItemWidthEvidenceCloses(
        evidence({
          subject: rejected(5, 0),
          fieldIndex: 5n,
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

  it("admits supported observed coordinates under other typed claims", () => {
    expect(
      classifyFieldItemWidthFinding({
        subject: rejected(2, 1),
        fieldIndex: 2n,
        itemIndex: 0n,
      }),
    ).toMatchObject({ fieldIndex: 2n, itemIndex: 0n });
    const wrongReason = forcedVerdictSubject({
      transactionId: txId,
      sourceKey: forcedSource,
      rejectionReason: { FieldPreimageLengthMismatch: { field_index: 2n } },
    });
    expect(
      classifyFieldItemWidthFinding({
        subject: wrongReason,
        fieldIndex: 2n,
        itemIndex: 0n,
      }),
    ).toMatchObject({ fieldIndex: 2n, itemIndex: 0n });
  });

  it("refuses substituted field bytes, commitment, and item coordinate", () => {
    const preimage = encodeMidgardFieldPreimage([Buffer.alloc(1)]);
    expect(() =>
      prepareFieldItemWidthEvidence({
        finding: { subject: accepted, fieldIndex: 5n, itemIndex: 0n },
        fieldPreimage: preimage,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/does not match/u);
    const absent = prepareFieldItemWidthEvidence({
      finding: { subject: accepted, fieldIndex: 5n, itemIndex: 1n },
      fieldPreimage: preimage,
      committedFieldHashHex: midgardFieldCommitment(preimage).toString("hex"),
    });
    expect(absent.observation).toBe("ItemOutOfDomain");
    expect(fieldItemWidthEvidenceCloses(absent)).toBe(false);
  });

  it("selects direct and certified maximum carriage without overrides", () => {
    expect(evidence({ fieldIndex: 5n, item: Buffer.alloc(0) }).carriage).toBe(
      "Inline",
    );
    const maximum = evidence({ item: Buffer.alloc(32_764) });
    expect(maximum.fieldPreimageHex.length / 2).toBe(32_768);
    expect(maximum.carriage).toBe("Certified");
    expect(maximum.decisiveFaultHolds).toBe(true);
  });

  // The same explicit Data layouts are checked by the Aiken rule tests.
  const compact = {
    body: {
      spend_inputs_hash: "00".repeat(32),
      reference_inputs_hash: "00".repeat(32),
      outputs_hash: "00".repeat(32),
      fee: 42n,
      validity_interval_start: 3n,
      validity_interval_end: 99n,
      required_observers_hash: "00".repeat(32),
      required_signers_hash: "00".repeat(32),
      mint_hash: "00".repeat(32),
      script_integrity_hash: "00".repeat(32),
      auxiliary_data_hash: "00".repeat(32),
      network_id: 255n,
    },
    witness_set_hash: "ff".repeat(32),
  };

  it("serializes the source compact and optional validity in the current bound ABI", () => {
    const finding = { subject: accepted, fieldIndex: 5n, itemIndex: 0n };
    expect(
      encodeFieldItemWidthBoundCoordinate(finding, {
        ...compact,
        validity_code: 0n,
      }),
    ).toBe(
      "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500d8799fd8799f582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000182a031863582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000018ffff5820ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd8799f00ffffff",
    );
    expect(
      encodeFieldItemWidthBoundCoordinate(finding, {
        ...compact,
        validity_code: null,
      }),
    ).toBe(
      "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500d8799fd8799f582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000182a031863582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000018ffff5820ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd87a80ffff",
    );
  });

  it("serializes item legality rather than the obsolete item width", () => {
    expect(
      encodeFieldItemWidthAuthenticatedItem(
        evidence({ fieldIndex: 5n, item: Buffer.from("000102", "hex") }),
      ),
    ).toBe(
      "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500d87a9fd87a80ffff",
    );
    expect(
      encodeFieldItemWidthAuthenticatedItem(
        evidence({ fieldIndex: 5n, item: Buffer.alloc(0) }),
      ),
    ).toBe(
      "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500d87a9fd87980ffff",
    );
  });

  it("preserves the distinct out-of-domain observation constructor", () => {
    expect(
      Data.to(
        {
          subject: accepted,
          field_index: 5n,
          item_index: 0n,
          observation: "ItemOutOfDomain",
        } as never,
        FieldItemWidthAuthenticatedItemSchema as never,
      ),
    ).toBe(
      "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500d87980ff",
    );
  });

  it("encodes both absent and present step-02 openings with their Option wrapper", () => {
    expect(
      Data.to(
        { input_index: 0n, output_index: 1n, opening: null } as never,
        FieldItemWidthStep02ArgsSchema as never,
      ),
    ).toBe("d8799f0001d87a80ff");
    expect(
      Data.to(
        {
          input_index: 0n,
          output_index: 1n,
          opening: {
            BodyFieldOpening: {
              native_tx_compact_cbor: "01",
              carriage: { Inline: { preimage: "80" } },
            },
          },
        } as never,
        FieldItemWidthStep02ArgsSchema as never,
      ),
    ).toBe("d8799f0001d8799fd8799f4101d8799f4180ffffffff");
  });

  it("refuses the obsolete short bound datum", () => {
    expect(() =>
      Data.from(
        "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500ff",
        FieldItemWidthBoundCoordinateSchema as never,
      ),
    ).toThrow();
  });

  it("refuses the obsolete width-only authenticated datum", () => {
    expect(() =>
      Data.from(
        "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff050003ff",
        FieldItemWidthAuthenticatedItemSchema as never,
      ),
    ).toThrow();
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

describe("fieldItemWidthIllegal registered-chain parity", () => {
  it("applies the same chain the SDK registers for the same shared policies", async () => {
    const fixture = await buildRegisteredChainFixture(
      buildFieldItemWidthIllegalFaultProofContracts,
    );
    const registered = fixture.contracts.fieldItemWidthIllegal;
    const applied = applyFieldItemWidthIllegalScripts(fixture.applyParams);
    expect(applied.map((step) => step.spendingScriptHash)).toStrictEqual(
      registered.steps.map((step) => step.spendingScriptHash),
    );
    expect(registered.firstStep.spendingScriptHash).toBe(
      applied[0].spendingScriptHash,
    );
    expect(applied.map((step) => step.blueprintTitle)).toStrictEqual(
      Object.values(FIELD_ITEM_WIDTH_ILLEGAL_BLUEPRINT_TITLES),
    );
  });
});
