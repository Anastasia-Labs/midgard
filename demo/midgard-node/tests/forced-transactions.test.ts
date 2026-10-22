import "node:crypto";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/cek-proof";
import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/codec/cbor";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "@lucid-evolution/lucid";
import "da-committee-node/da/payload";
import "effect";
import "vitest";
import "../../midgard-validation/tests/validation-fixtures.js";
import "../../da-committee-node/tests/helpers.js";
import "../src/database/index.js";
import "../src/mpf/index.js";
import "./midgard-output-helpers.js";
import "./forced-transactions.make-signed-effectful-transaction.js";

import { createHash } from "node:crypto";

import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  RejectCodes,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { FUNDED_OUTPUT_LOVELACE } from "../../midgard-validation/tests/validation-fixtures.js";
import {
  ForcedTransactionsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import {
  buildPlaceholderValidationTraceMembers,
  classifyForcedTransactions,
} from "../src/mpf/index.js";
import {
  encodedTransaction,
  forcedEntry,
  makeOutput,
  makeSignedEffectfulTransaction,
  outputReferenceFromHash,
  TEST_ADDRESS,
} from "./forced-transactions.make-signed-effectful-transaction.js";

describe("V1 forced transaction material", () => {
  it("keeps the submitted source and identity identical across operator verdicts", async () => {
    const nativeTxCbor = encodedTransaction();
    const accepted = await Effect.runPromise(
      ForcedTransactionsDB.encodeForcedInclusionValueV1({
        nativeTxCbor,
        verdict: "ForcedTxValid",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      }),
    );
    const rejected = await Effect.runPromise(
      ForcedTransactionsDB.encodeForcedInclusionValueV1({
        nativeTxCbor,
        verdict: { ForcedTxInvalid: { reason: "FeeBelowMinimum" } },
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      }),
    );
    const decoded = Data.from(
      accepted.value.toString("hex"),
      SDK.ForcedInclusionTxV1,
    ) as SDK.ForcedInclusionTxV1;

    expect(accepted.txId).toEqual(rejected.txId);
    expect(accepted.txCompact).toEqual(rejected.txCompact);
    expect(accepted.source).toEqual(rejected.source);
    expect(accepted.transactionCommitment).toEqual(
      rejected.transactionCommitment,
    );
    expect(decodeSingleCbor(accepted.txCompact)).toHaveLength(3);
    expect(accepted.value).not.toEqual(rejected.value);
    expect(decoded).toEqual({
      tx_id: accepted.txId.toString("hex"),
      submitted_source: {
        compact_cbor: accepted.source.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          accepted.source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          accepted.source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: "ForcedTxValid",
    });
    const journalMember =
      ForcedTransactionsDB.encodeForcedTransactionJournalMember({
        sourceValueCbor: accepted.value,
        canonicalTransactionCbor: nativeTxCbor,
      });
    expect(
      ForcedTransactionsDB.decodeForcedTransactionJournalMember(journalMember),
    ).toEqual({
      sourceValueCbor: accepted.value,
      canonicalTransactionCbor: nativeTxCbor,
    });
  });

  it("encodes the sole canonical ForcedTransactionJournalMemberV1 three-field CBOR form", () => {
    expect(ForcedTransactionsDB.FORCED_TRANSACTION_JOURNAL_MEMBER_VERSION).toBe(
      1n,
    );
    expect(MIDGARD_CONSENSUS_PROFILE.forcedTransactionJournalVersion).toBe(1);
    const value = {
      sourceValueCbor: Buffer.from("aa", "hex"),
      canonicalTransactionCbor: Buffer.from("bbcc", "hex"),
    };
    const encoded =
      ForcedTransactionsDB.encodeForcedTransactionJournalMember(value);

    expect(encoded.toString("hex")).toBe("830141aa42bbcc");
    expect(
      ForcedTransactionsDB.decodeForcedTransactionJournalMember(encoded),
    ).toEqual(value);
  });

  it("rejects legacy versions, defaults, aliases, extra fields, and non-canonical journal CBOR", () => {
    const source = Buffer.from("aa", "hex");
    const transaction = Buffer.from("bbcc", "hex");
    const sidecar = Buffer.from("ddeeff", "hex");
    const canonical = ForcedTransactionsDB.encodeForcedTransactionJournalMember(
      {
        sourceValueCbor: source,
        canonicalTransactionCbor: transaction,
      },
    );
    const invalidEncodings: readonly (readonly [string, Buffer])[] = [
      ["legacy version zero", encodeCbor([0n, source, transaction])],
      ["successor version two", encodeCbor([2n, source, transaction])],
      ["source version five", encodeCbor([5n, source, transaction])],
      ["defaulted version", encodeCbor([source, transaction])],
      ["missing transaction", encodeCbor([1n, source])],
      ["retired sidecar field", encodeCbor([1n, source, transaction, sidecar])],
      [
        "extra field",
        encodeCbor([1n, source, transaction, sidecar, Buffer.from([0])]),
      ],
      [
        "named-field alias",
        encodeCbor(
          new Map<unknown, unknown>([
            ["version", 1n],
            ["source_value_cbor", source],
            ["canonical_transaction_cbor", transaction],
          ]),
        ),
      ],
      ["wrong source type", encodeCbor([1n, 1n, transaction])],
      ["wrong transaction type", encodeCbor([1n, source, 1n])],
      ["empty source", encodeCbor([1n, Buffer.alloc(0), transaction])],
      ["empty transaction", encodeCbor([1n, source, Buffer.alloc(0)])],
      ["trailing CBOR", Buffer.concat([canonical, Buffer.from([0])])],
      ["non-minimal version", Buffer.from("83180141aa42bbcc", "hex")],
      ["indefinite outer array", Buffer.from("9f0141aa42bbccff", "hex")],
    ];

    for (const [name, encoded] of invalidEncodings) {
      expect(
        () =>
          ForcedTransactionsDB.decodeForcedTransactionJournalMember(encoded),
        name,
      ).toThrow();
    }
  });

  it("refuses encoder-side defaults, aliases, extra properties, and empty fields", () => {
    const sourceValueCbor = Buffer.from("aa", "hex");
    const canonicalTransactionCbor = Buffer.from("bb", "hex");
    const encode =
      ForcedTransactionsDB.encodeForcedTransactionJournalMember as (
        value: unknown,
      ) => Buffer;
    const symbolExtra = Object.assign(
      { sourceValueCbor, canonicalTransactionCbor },
      { [Symbol("legacy")]: true },
    );
    const inheritedAlias = Object.assign(Object.create({ version: 5 }), {
      sourceValueCbor,
      canonicalTransactionCbor,
    });
    const invalidInputs: readonly (readonly [string, unknown])[] = [
      ["non-record", null],
      ["missing field", { sourceValueCbor }],
      [
        "explicit version property",
        { version: 1, sourceValueCbor, canonicalTransactionCbor },
      ],
      [
        "retired sidecar property",
        {
          sourceValueCbor,
          canonicalTransactionCbor,
          programMaterialSidecarCbor: Buffer.from([0]),
        },
      ],
      [
        "snake-case aliases",
        {
          source_value_cbor: sourceValueCbor,
          canonical_transaction_cbor: canonicalTransactionCbor,
        },
      ],
      ["symbol extra", symbolExtra],
      ["inherited legacy alias", inheritedAlias],
      [
        "empty source",
        { sourceValueCbor: Buffer.alloc(0), canonicalTransactionCbor },
      ],
      [
        "empty transaction",
        { sourceValueCbor, canonicalTransactionCbor: Buffer.alloc(0) },
      ],
    ];

    for (const [name, value] of invalidInputs) {
      expect(() => encode(value), name).toThrow();
    }
  });

  it("rejects non-V1, aliased, or digest-invalid members at the Postgres journal read boundary", async () => {
    const headerHash = Buffer.alloc(28, 0x11);
    const memberId = Buffer.alloc(34, 0x22);
    const canonicalPayload =
      ForcedTransactionsDB.encodeForcedTransactionJournalMember({
        sourceValueCbor: Buffer.from("aa", "hex"),
        canonicalTransactionCbor: Buffer.from("bb", "hex"),
      });
    const member = (
      payload: Buffer,
      overrides: Partial<PendingBlockFinalizationsDB.MemberRecord> = {},
    ): PendingBlockFinalizationsDB.MemberRecord => ({
      [PendingBlockFinalizationsDB.MemberColumns.HEADER_HASH]: headerHash,
      [PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID]: memberId,
      [PendingBlockFinalizationsDB.MemberColumns.ORDINAL]: 0,
      [PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR]: payload,
      [PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_SHA256]: createHash(
        "sha256",
      )
        .update(payload)
        .digest(),
      [PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE]:
        ForcedTransactionsDB.tableName,
      [PendingBlockFinalizationsDB.MemberColumns.SOURCE_ID]: memberId,
      [PendingBlockFinalizationsDB.MemberColumns.SOURCE_TIMESTAMP]: new Date(
        "2026-07-23T12:00:00.000Z",
      ),
      ...overrides,
    });

    await expect(
      Effect.runPromise(
        PendingBlockFinalizationsDB.validateForcedTransactionJournalMembers(
          [member(canonicalPayload)],
          headerHash,
        ),
      ),
    ).resolves.toBeUndefined();

    const invalidMembers: readonly (readonly [
      string,
      PendingBlockFinalizationsDB.MemberRecord,
    ])[] = [
      [
        "source V5 payload",
        member(
          encodeCbor([
            5n,
            Buffer.from("aa", "hex"),
            Buffer.from("bb", "hex"),
            Buffer.from("cc", "hex"),
          ]),
        ),
      ],
      [
        "named-field alias payload",
        member(
          encodeCbor(
            new Map<unknown, unknown>([
              ["version", 1n],
              ["source_value_cbor", Buffer.from("aa", "hex")],
              ["canonical_transaction_cbor", Buffer.from("bb", "hex")],
              ["program_material_sidecar_cbor", Buffer.from("cc", "hex")],
            ]),
          ),
        ),
      ],
      [
        "payload digest mismatch",
        member(canonicalPayload, {
          [PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_SHA256]:
            Buffer.alloc(32),
        }),
      ],
      [
        "source table alias",
        member(canonicalPayload, {
          [PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE]:
            "forced_transactions",
        }),
      ],
      [
        "source id mismatch",
        member(canonicalPayload, {
          [PendingBlockFinalizationsDB.MemberColumns.SOURCE_ID]: Buffer.alloc(
            memberId.length,
          ),
        }),
      ],
    ];
    for (const [name, invalid] of invalidMembers) {
      const result = await Effect.runPromise(
        Effect.either(
          PendingBlockFinalizationsDB.validateForcedTransactionJournalMembers(
            [invalid],
            headerHash,
          ),
        ),
      );
      expect(result._tag, name).toBe("Left");
    }
  });

  it("executes sequential valid forced deltas and emits accepted validation traces", async () => {
    const initialInput = outputReferenceFromHash(Buffer.alloc(32, 0x31));
    // Phase B enforces MIN-ADA-TX on every produced output, so a 10-lovelace
    // output is classified `TxIsInvalid` on `E_MIN_ADA` before the sequential
    // forced-delta semantics under test are reached. `FUNDED_OUTPUT_LOVELACE`
    // is the shared fixture amount that clears the floor with headroom, and
    // using it for both the pre-state entry and the produced output keeps this
    // two-step chain value-conserving at fee 0.
    const output = makeOutput(
      FUNDED_OUTPUT_LOVELACE,
      new Map([["ab".repeat(28), new Map([["01", 7n]])]]),
    );
    const firstTransaction = makeSignedEffectfulTransaction(
      initialInput,
      output,
    );
    const firstOutput = outputReferenceFromHash(firstTransaction.transactionId);
    const secondTransaction = makeSignedEffectfulTransaction(
      firstOutput,
      output,
    );
    const entries = [
      await forcedEntry({ label: 1, transaction: firstTransaction }),
      await forcedEntry({ label: 2, transaction: secondTransaction }),
    ];
    const classified = await Effect.runPromise(
      classifyForcedTransactions({
        entries,
        initialState: new Map([[initialInput.toString("hex"), output]]),
        blockTime: new Date("2026-07-23T12:01:00.000Z"),
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        validation: {
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          bucketConcurrency: 1,
          slotForUnixTime: () => 100n,
        },
      }),
    );

    expect(classified).toHaveLength(2);
    for (const result of classified) {
      expect(ForcedTransactionsDB.operatorValidityOfEntry(result.entry)).toBe(
        "TxIsValid",
      );
      expect(result.rejectionCode).toBeNull();
      expect(result.ledgerOps).toHaveLength(2);
      expect(result.rawLedgerOps).toHaveLength(2);
      expect(result.ledgerMutationSteps).toHaveLength(2);
      expect(result.ledgerWitnessEntries).toHaveLength(1);
    }
    const firstOutputDescriptor =
      buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: firstOutput,
        outputCbor: output,
      }).descriptorCbor;
    expect(classified[0]!.ledgerOps).toMatchObject([
      { type: "delete", key: initialInput },
      {
        type: "insert",
        key: firstOutput,
        value: firstOutputDescriptor,
      },
    ]);
    expect(classified[0]!.rawLedgerOps).toMatchObject([
      { type: "delete", key: initialInput },
      { type: "insert", key: firstOutput, value: output },
    ]);
    expect(classified[1]!.ledgerOps[0]).toMatchObject({
      type: "delete",
      key: firstOutput,
    });
    expect(classified[0]!.ledgerMutationSteps.at(-1)!.postRoot).toEqual(
      classified[1]!.ledgerMutationSteps[0]!.preRoot,
    );

    const eventKey = (label: number): SDK.EventKey => ({
      ForcedTransactionEventKey: {
        tx_order_id: {
          transactionId: Buffer.alloc(32, label).toString("hex"),
          outputIndex: 0n,
        },
      },
    });
    const members = await Effect.runPromise(
      buildPlaceholderValidationTraceMembers({
        transactions: classified.map((result, index) => ({
          eventKey: eventKey(index + 1),
          canonicalTransactionCbor:
            result.entry[ForcedTransactionsDB.Columns.NATIVE_TX_CBOR]!,
          sourceKind: "forced" as const,
          verdict: "accepted" as const,
          rejectionReason: null,
        })),
      }),
    );

    expect(members).toHaveLength(2);
    for (const member of members) {
      expect(member.value).toMatchObject({
        machine_version: 1n,
        step_count: 1n,
        verdict: "Accepted",
        rejection_code_hash: "00".repeat(32),
      });
    }
  });

  it("retains invalid forced transactions as classified no-op sources", async () => {
    const missingInput = outputReferenceFromHash(Buffer.alloc(32, 0x41));
    const transaction = makeSignedEffectfulTransaction(
      missingInput,
      makeOutput(10n),
    );
    const [classified] = await Effect.runPromise(
      classifyForcedTransactions({
        entries: [await forcedEntry({ label: 3, transaction })],
        initialState: new Map(),
        blockTime: new Date("2026-07-23T12:01:00.000Z"),
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        validation: {
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          bucketConcurrency: 1,
          slotForUnixTime: () => 100n,
        },
      }),
    );

    expect(
      ForcedTransactionsDB.operatorValidityOfEntry(classified!.entry),
    ).toBe("TxIsInvalid");
    // Polarity is derived from the committed verdict, including its reason and subject.
    expect(
      (
        Data.from(
          classified!.entry[
            ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE
          ].toString("hex"),
          SDK.ForcedInclusionTxV1,
        ) as SDK.ForcedInclusionTxV1
      ).verdict,
    ).toEqual({
      ForcedTxInvalid: {
        reason: { InputNotFound: { source_kind: 0n, input_index: 0n } },
      },
    });
    expect(classified!.rejectionCode).toBe(RejectCodes.InputNotFound);
    expect(classified!.ledgerOps).toEqual([]);
    expect(classified!.ledgerMutationSteps).toEqual([]);
    expect(classified!.ledgerWitnessEntries).toEqual([]);
  });
});

/** Every forced input resolves against its own pre-state, including script refs. */
describe("forced transaction references at their ledger position", () => {
  const scriptRefOutput = encodeMidgardTxOutput({
    address: TEST_ADDRESS,
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    script_ref: {
      language: "MidgardV1",
      scriptBytes: encodeCbor(Buffer.from("01010061", "hex")),
    },
  });
  const validation = {
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    bucketConcurrency: 1,
    slotForUnixTime: () => 100n,
  };

  /** Classifies `transaction` as the block's only event. */
  const classifyAlone = async (
    transaction: ReturnType<typeof makeSignedEffectfulTransaction>,
    initialState: ReadonlyMap<string, Buffer>,
  ) => {
    const [classified] = await Effect.runPromise(
      classifyForcedTransactions({
        entries: [await forcedEntry({ label: 9, transaction })],
        initialState: new Map(initialState),
        blockTime: new Date("2026-07-23T12:01:00.000Z"),
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        validation,
      }),
    );
    return { classified: classified! };
  };

  it("judges an absent reference input InputNotFound", async () => {
    const spent = outputReferenceFromHash(Buffer.alloc(32, 0x61));
    const absentReference = outputReferenceFromHash(Buffer.alloc(32, 0x62));
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const initialState = new Map([[spent.toString("hex"), spentOutput]]);
    const transaction = makeSignedEffectfulTransaction(spent, spentOutput, {
      referenceInputs: [absentReference],
    });

    const { classified } = await classifyAlone(transaction, initialState);

    expect(
      (
        Data.from(
          classified.entry[
            ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE
          ].toString("hex"),
          SDK.ForcedInclusionTxV1,
        ) as SDK.ForcedInclusionTxV1
      ).verdict,
    ).toEqual({
      ForcedTxInvalid: {
        reason: { InputNotFound: { source_kind: 1n, input_index: 0n } },
      },
    });
    expect(classified.rejectionCode).toBe(RejectCodes.InputNotFound);
    expect(classified.ledgerOps).toEqual([]);
    expect(classified.ledgerWitnessEntries).toEqual([
      { outRef: spent, output: spentOutput },
    ]);
  });

  it("preserves the Phase A rejection when a live script reference exists", async () => {
    const spent = outputReferenceFromHash(Buffer.alloc(32, 0x63));
    const reference = outputReferenceFromHash(Buffer.alloc(32, 0x64));
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const initialState = new Map([
      [spent.toString("hex"), spentOutput],
      [reference.toString("hex"), scriptRefOutput],
    ]);
    const transaction = makeSignedEffectfulTransaction(spent, spentOutput, {
      referenceInputs: [reference],
      networkId: 1n,
    });

    const { classified } = await classifyAlone(transaction, initialState);

    expect(ForcedTransactionsDB.operatorValidityOfEntry(classified.entry)).toBe(
      "TxIsInvalid",
    );
    expect(classified.rejectionCode).toBe(RejectCodes.NetworkIdMismatch);
  });

  it("accepts a transaction referencing the carried bytes of a live script reference", async () => {
    const spent = outputReferenceFromHash(Buffer.alloc(32, 0x65));
    const reference = outputReferenceFromHash(Buffer.alloc(32, 0x66));
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const initialState = new Map([
      [spent.toString("hex"), spentOutput],
      [reference.toString("hex"), scriptRefOutput],
    ]);
    const transaction = makeSignedEffectfulTransaction(spent, spentOutput, {
      referenceInputs: [reference],
    });

    const { classified } = await classifyAlone(transaction, initialState);

    expect(ForcedTransactionsDB.operatorValidityOfEntry(classified.entry)).toBe(
      "TxIsValid",
    );
    expect(classified.rejectionCode).toBeNull();
    expect(classified.ledgerWitnessEntries).toEqual(
      expect.arrayContaining([
        { outRef: spent, output: spentOutput },
        { outRef: reference, output: scriptRefOutput },
      ]),
    );
  });
});
