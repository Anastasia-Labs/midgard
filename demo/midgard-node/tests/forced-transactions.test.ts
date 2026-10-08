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

import { computeHash28, encodeMidgardNativeScript } from "@al-ft/midgard-core";
import {
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekTermNode,
  hashMidgardCekProgramMaterialPreimage,
  hashMidgardCekTermNode,
  mergeMidgardCekProgramMaterialSidecars,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardForcedTxFromCanonical,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  RejectCodes,
} from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { validateDaPayloadEventProgramCoverage } from "da-committee-node/da/payload";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { makePayloadFixture } from "../../da-committee-node/tests/helpers.js";
import { retainedEndpointsMatchDescriptor } from "../../da-committee-node/tests/helpers.validation-trace.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput as makeValidationOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
} from "../../midgard-validation/tests/validation-fixtures.js";
import {
  ForcedTransactionsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import { publishedProgramMaterialEntries } from "../src/forced-orders/index.js";
import {
  buildDeterministicValidationTraceMembers,
  classifyForcedTransactions,
} from "../src/mpf/index.js";
import {
  canonicalTransaction,
  encodedTransaction,
  forcedEntry,
  makeOutput,
  makeSignedEffectfulTransaction,
  outputReferenceFromHash,
  TEST_ADDRESS,
} from "./forced-transactions.make-signed-effectful-transaction.js";

describe("V1 forced transaction material", () => {
  it("accepts only exact self-authenticating material from the immutable L1 address", () => {
    const preimage = encodeMidgardCekTermNode({ kind: "error" });
    const root = hashMidgardCekProgramMaterialPreimage("term", preimage);
    const [publication] = SDK.deriveCekProgramMaterialPublications([
      { kind: "term", root, preimage },
    ]);
    const utxo = (datum: string | undefined, outputIndex: number): UTxO =>
      ({
        txHash: "12".repeat(32),
        outputIndex,
        address: "addr_test1wmaterial",
        assets: { lovelace: 2_000_000n },
        ...(datum === undefined ? {} : { datum }),
      }) as UTxO;
    const decoded = publishedProgramMaterialEntries([
      utxo(publication!.datumCbor, 0),
      utxo(
        Data.to(
          { ...publication!.datum, root: "ff".repeat(32) },
          SDK.CekProgramMaterialDatum,
        ),
        1,
      ),
      utxo(undefined, 2),
    ]);

    expect(decoded.entries).toEqual([publication!.entry]);
    expect(decoded.ignoredCount).toBe(2);
  });

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
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
      });
    expect(
      ForcedTransactionsDB.decodeForcedTransactionJournalMember(journalMember),
    ).toEqual({
      sourceValueCbor: accepted.value,
      canonicalTransactionCbor: nativeTxCbor,
      programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
    });
  });

  it("encodes the sole canonical ForcedTransactionJournalMemberV1 four-field CBOR form", () => {
    expect(ForcedTransactionsDB.FORCED_TRANSACTION_JOURNAL_MEMBER_VERSION).toBe(
      1n,
    );
    expect(MIDGARD_CONSENSUS_PROFILE.forcedTransactionJournalVersion).toBe(1);
    const value = {
      sourceValueCbor: Buffer.from("aa", "hex"),
      canonicalTransactionCbor: Buffer.from("bbcc", "hex"),
      programMaterialSidecarCbor: Buffer.from("ddeeff", "hex"),
    };
    const encoded =
      ForcedTransactionsDB.encodeForcedTransactionJournalMember(value);

    expect(encoded.toString("hex")).toBe("840141aa42bbcc43ddeeff");
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
        programMaterialSidecarCbor: sidecar,
      },
    );
    const invalidEncodings: readonly (readonly [string, Buffer])[] = [
      ["legacy version zero", encodeCbor([0n, source, transaction, sidecar])],
      ["successor version two", encodeCbor([2n, source, transaction, sidecar])],
      ["source version five", encodeCbor([5n, source, transaction, sidecar])],
      ["defaulted version", encodeCbor([source, transaction, sidecar])],
      ["missing sidecar", encodeCbor([1n, source, transaction])],
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
            ["program_material_sidecar_cbor", sidecar],
          ]),
        ),
      ],
      ["wrong source type", encodeCbor([1n, 1n, transaction, sidecar])],
      ["wrong transaction type", encodeCbor([1n, source, 1n, sidecar])],
      ["wrong sidecar type", encodeCbor([1n, source, transaction, 1n])],
      ["empty source", encodeCbor([1n, Buffer.alloc(0), transaction, sidecar])],
      ["empty transaction", encodeCbor([1n, source, Buffer.alloc(0), sidecar])],
      ["empty sidecar", encodeCbor([1n, source, transaction, Buffer.alloc(0)])],
      ["trailing CBOR", Buffer.concat([canonical, Buffer.from([0])])],
      ["non-minimal version", Buffer.from("84180141aa42bbcc43ddeeff", "hex")],
      [
        "indefinite outer array",
        Buffer.from("9f0141aa42bbcc43ddeeffff", "hex"),
      ],
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
    const programMaterialSidecarCbor = Buffer.from("cc", "hex");
    const encode =
      ForcedTransactionsDB.encodeForcedTransactionJournalMember as (
        value: unknown,
      ) => Buffer;
    const symbolExtra = Object.assign(
      {
        sourceValueCbor,
        canonicalTransactionCbor,
        programMaterialSidecarCbor,
      },
      { [Symbol("legacy")]: true },
    );
    const inheritedAlias = Object.assign(Object.create({ version: 5 }), {
      sourceValueCbor,
      canonicalTransactionCbor,
      programMaterialSidecarCbor,
    });
    const invalidInputs: readonly (readonly [string, unknown])[] = [
      ["non-record", null],
      ["missing field", { sourceValueCbor, canonicalTransactionCbor }],
      [
        "explicit version property",
        {
          version: 1,
          sourceValueCbor,
          canonicalTransactionCbor,
          programMaterialSidecarCbor,
        },
      ],
      [
        "snake-case aliases",
        {
          source_value_cbor: sourceValueCbor,
          canonical_transaction_cbor: canonicalTransactionCbor,
          program_material_sidecar_cbor: programMaterialSidecarCbor,
        },
      ],
      ["symbol extra", symbolExtra],
      ["inherited legacy alias", inheritedAlias],
      [
        "empty source",
        {
          sourceValueCbor: Buffer.alloc(0),
          canonicalTransactionCbor,
          programMaterialSidecarCbor,
        },
      ],
      [
        "empty transaction",
        {
          sourceValueCbor,
          canonicalTransactionCbor: Buffer.alloc(0),
          programMaterialSidecarCbor,
        },
      ],
      [
        "empty sidecar",
        {
          sourceValueCbor,
          canonicalTransactionCbor,
          programMaterialSidecarCbor: Buffer.alloc(0),
        },
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
        programMaterialSidecarCbor: Buffer.from("cc", "hex"),
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

  it("builds a deterministic forced rejection descriptor from the same Phase A/B replay", async () => {
    const nativeTxCbor = encodedTransaction();
    const txId = computeMidgardNativeTxId(
      materializeMidgardNativeTxFromCanonical(canonicalTransaction()),
    );
    const eventKey: SDK.EventKey = {
      ForcedTransactionEventKey: {
        tx_order_id: {
          transactionId: "44".repeat(32),
          outputIndex: 0n,
        },
      },
    };
    const members = await Effect.runPromise(
      buildDeterministicValidationTraceMembers({
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        blockEndTime: new Date("2026-07-23T12:00:00.000Z"),
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 100n,
        transactions: [
          {
            eventKey,
            transactionId: txId,
            canonicalTransactionCbor: nativeTxCbor,
            programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(
              [],
            ),
            sourceKind: "forced",
            priorUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            postUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            ledgerOps: [],
            ledgerWitnessEntries: [],
            ledgerMutationSteps: [],
            verdict: "rejected",
            rejectionCode: RejectCodes.EmptyInputs,
          },
        ],
      }),
    );

    expect(members).toHaveLength(1);
    const member = members[0]!;
    const endpoints = SDK.readRetainedValidationEndpoints({
      entries: member.witnesses,
      eventKey,
      descriptor: member.value,
    });
    expect(endpoints.initial.trace_proof.state_index).toBe(0n);
    expect(endpoints.terminal.trace_proof.state_index).toBe(
      member.value.step_count,
    );
    const records = member.witnesses.map(([key, value]) => ({
      key: SDK.decodeRetainedValidationWitnessKey(Buffer.from(key, "hex")),
      value: SDK.decodeRetainedValidationWitness(Buffer.from(value, "hex")),
    }));
    for (let index = 0n; index <= member.value.step_count; index += 1n) {
      expect(
        records.filter(
          (entry) =>
            entry.key.execution_index ===
            SDK.retainedValidationStateCoordinate(
              member.value.step_count,
              index,
            ),
        ),
      ).toHaveLength(1);
      expect(
        SDK.readRetainedValidationState({
          entries: member.witnesses,
          eventKey,
          descriptor: member.value,
          stateIndex: index,
        }).trace_proof.state_index,
      ).toBe(index);
    }
    expect(new Set(member.witnesses.map(([key]) => key)).size).toBe(
      member.witnesses.length,
    );
    const tampered = member.witnesses.map(([key, value]) => {
      const coordinate = SDK.decodeRetainedValidationWitnessKey(
        Buffer.from(key, "hex"),
      );
      return coordinate.execution_index ===
        SDK.retainedValidationEndpointCoordinate(
          member.value.step_count,
          "initial",
        )
        ? ([
            key,
            SDK.encodeRetainedValidationWitness({
              ...SDK.decodeRetainedValidationWitness(Buffer.from(value, "hex")),
              witness_cbor: "00",
            }).toString("hex"),
          ] as SDK.DaPayloadEntry)
        : ([key, value] as SDK.DaPayloadEntry);
    });
    expect(() =>
      SDK.readRetainedValidationEndpoints({
        entries: tampered,
        eventKey,
        descriptor: member.value,
      }),
    ).toThrow(/context differs/u);
    expect(() =>
      SDK.readRetainedValidationEndpoints({
        entries: member.witnesses,
        eventKey,
        descriptor: { ...member.value, trace_root: "ff".repeat(32) },
      }),
    ).toThrow(/selected operator trace/u);

    expect(members[0]?.value).toMatchObject({
      schema_version: 1n,
      machine_version: 1n,
      verdict: "Rejected",
    });
    expect(
      Data.from(
        members[0]!.valueCbor.toString("hex"),
        SDK.ValidationTraceDescriptor,
      ),
    ).toEqual(members[0]!.value);
  });

  it("retains the complete ScriptSources frontier and canonical native execution witness", async () => {
    const spent = outRefFromByte(0x7a);
    const spentOutput = makeValidationOutput(FUNDED_OUTPUT_LOVELACE);
    const nativePayload = encodeMidgardNativeScript({
      type: "all",
      scripts: [],
    });
    const policyId = computeHash28(
      Buffer.concat([Buffer.from([0]), nativePayload]),
    );
    const assetName = Buffer.from("31", "hex");
    const output = makeValidationOutput(
      FUNDED_OUTPUT_LOVELACE,
      undefined,
      new Map([
        [policyId.toString("hex"), new Map([[assetName.toString("hex"), 1n]])],
      ]),
    );
    const transaction = makeNativeTx({
      spendInputs: [spent],
      outputs: [output],
      scriptWitnesses: [nativeScriptWitness({ type: "all", scripts: [] })],
      mintPreimageCbor: makeMintPreimageCbor(
        new Map([[policyId, new Map([[assetName, 1n]])]]),
      ),
    });
    const ledgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(transaction.txId),
        outputCbor: output,
      }),
    ];
    const mutations = await buildValidationMachineLedgerMutationSteps({
      initialEntries: [{ outRef: spent, output: spentOutput }],
      operations: ledgerOps,
    });
    const eventKey: SDK.EventKey = {
      ForcedTransactionEventKey: {
        tx_order_id: {
          transactionId: "66".repeat(32),
          outputIndex: 0n,
        },
      },
    };
    const [member] = await Effect.runPromise(
      buildDeterministicValidationTraceMembers({
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        blockEndTime: new Date("2026-07-23T12:00:00.000Z"),
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 100n,
        transactions: [
          {
            eventKey,
            transactionId: transaction.txId,
            canonicalTransactionCbor: encodeMidgardForcedTxCanonical(
              materializeMidgardForcedTxFromCanonical(transaction.tx),
            ),
            programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(
              [],
            ),
            sourceKind: "forced",
            priorUtxosRoot: mutations[0]!.preRoot.toString("hex"),
            postUtxosRoot: mutations.at(-1)!.postRoot.toString("hex"),
            ledgerOps,
            ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
            ledgerMutationSteps: mutations,
            verdict: "accepted",
            rejectionCode: null,
          },
        ],
      }),
    );
    expect(member).toBeDefined();
    expect(member!.value.verdict).toBe("Accepted");
    expect(member!.value.rejection_code_hash).toBe("00".repeat(32));
    const retainedEntries = member!.witnesses.map(([keyHex, valueHex]) => ({
      key: SDK.decodeRetainedValidationWitnessKey(Buffer.from(keyHex, "hex")),
      value: SDK.decodeRetainedValidationWitness(Buffer.from(valueHex, "hex")),
    }));
    const resolvedMemberships = retainedEntries.filter(
      ({ value }) =>
        value.phase === 7n &&
        typeof value.auxiliary === "object" &&
        "ScheduledLedgerMembershipWitness" in value.auxiliary,
    );
    expect(resolvedMemberships.length).toBeGreaterThan(0);
    expect(
      resolvedMemberships.every(({ key }) => key.execution_index < 0n),
    ).toBe(true);
    for (const { value } of resolvedMemberships) {
      if (
        typeof value.auxiliary !== "object" ||
        !("ScheduledLedgerMembershipWitness" in value.auxiliary)
      )
        throw new Error("missing membership");
      expect(value.auxiliary.ScheduledLedgerMembershipWitness.key).toBe(
        spent.toString("hex"),
      );
      expect(value.auxiliary.ScheduledLedgerMembershipWitness.value).not.toBe(
        "",
      );
    }
    const scriptSources = retainedEntries.filter(
      ({ value }) => value.phase === 8n,
    );
    const nativeExecutions = retainedEntries.filter(
      ({ key, value }) => key.execution_index >= 0n && value.phase === 9n,
    );
    expect(scriptSources.length).toBeGreaterThan(0);
    expect(scriptSources.every(({ key }) => key.execution_index < 0n)).toBe(
      true,
    );
    expect(
      scriptSources.some(
        ({ value }) =>
          typeof value.auxiliary === "object" &&
          "ScriptPurposeScanWitness" in value.auxiliary,
      ),
    ).toBe(true);
    expect(
      scriptSources.some(
        ({ value }) =>
          typeof value.auxiliary === "object" &&
          "ScriptSourceScanWitness" in value.auxiliary,
      ),
    ).toBe(true);
    expect(
      scriptSources.some(
        ({ value }) => value.auxiliary === "NoAuxiliaryWitness",
      ),
    ).toBe(true);
    expect(nativeExecutions).toHaveLength(1);
    const [{ key, value: retained }] = nativeExecutions;
    expect(key).toEqual({ event_key: eventKey, execution_index: 0n });
    expect(retained).toMatchObject({
      phase: 9n,
      program_counter: retained.machine_state.program_counter,
      auxiliary: {
        NativeExecutionDescriptorWitness: {
          execution_index: 0n,
          language_tag: 0n,
        },
      },
    });
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
    const resolverCalls: string[][] = [];
    const classified = await Effect.runPromise(
      classifyForcedTransactions({
        entries,
        initialState: new Map([[initialInput.toString("hex"), output]]),
        effectiveEndTime: new Date("2026-07-23T12:01:00.000Z"),
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        validation: {
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          bucketConcurrency: 1,
          slotForUnixTime: () => 100n,
        },
        resolveProgramMaterialSidecar: (envelopes) => {
          resolverCalls.push(
            envelopes.map((envelope) =>
              Buffer.from(envelope.termRoot).toString("hex"),
            ),
          );
          return Effect.succeed(encodeMidgardCekProgramMaterialSidecar([]));
        },
      }),
    );

    expect(resolverCalls).toEqual([[], []]);
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
      buildDeterministicValidationTraceMembers({
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        blockEndTime: new Date("2026-07-23T12:01:00.000Z"),
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        blockSlot: 100n,
        transactions: classified.map((result, index) => ({
          eventKey: eventKey(index + 1),
          transactionId: result.entry[ForcedTransactionsDB.Columns.TX_ID],
          canonicalTransactionCbor:
            result.entry[ForcedTransactionsDB.Columns.NATIVE_TX_CBOR],
          programMaterialSidecarCbor: result.programMaterialSidecarCbor,
          sourceKind: "forced" as const,
          priorUtxosRoot:
            result.ledgerMutationSteps[0]!.preRoot.toString("hex"),
          postUtxosRoot: result.ledgerMutationSteps
            .at(-1)!
            .postRoot.toString("hex"),
          ledgerOps: result.ledgerOps,
          ledgerWitnessEntries: result.ledgerWitnessEntries,
          ledgerMutationSteps: result.ledgerMutationSteps,
          verdict: "accepted" as const,
          rejectionCode: null,
        })),
      }),
    );

    expect(members).toHaveLength(2);
    for (const member of members) {
      expect(member.value.machine_version).toBe(1n);
      expect(member.value.verdict).toBe("Accepted");
      expect(retainedEndpointsMatchDescriptor(member)).toBe(true);
      const retained = member.witnesses.map(([keyHex, valueHex]) => ({
        key: SDK.decodeRetainedValidationWitnessKey(Buffer.from(keyHex, "hex")),
        value: SDK.decodeRetainedValidationWitness(
          Buffer.from(valueHex, "hex"),
        ),
      }));
      const outputDescriptors = retained.filter(
        ({ value }) =>
          value.phase === 13n &&
          typeof value.auxiliary === "object" &&
          "LedgerDeltaOutputWitness" in value.auxiliary,
      );
      expect(outputDescriptors).toHaveLength(1);
      expect(outputDescriptors[0]!.key.execution_index).toBeLessThan(0n);
      const terminal = retained.find(({ value }) => {
        if (value.phase !== 10n) return false;
        const control = decodeSingleCbor(
          Buffer.from(value.witness_cbor, "hex"),
        );
        return (
          Array.isArray(control) &&
          control.length === 4 &&
          BigInt(control[1] as number) === 3n
        );
      });
      expect(terminal).toMatchObject({
        value: {
          phase: 10n,
          auxiliary: "NoAuxiliaryWitness",
        },
      });
      const control = decodeSingleCbor(
        Buffer.from(terminal!.value.witness_cbor, "hex"),
      );
      expect(Array.isArray(control) ? BigInt(control[1] as number) : null).toBe(
        3n,
      );
      expect(terminal!.key.execution_index).toBeLessThan(0n);
      const assetMutations = retained.filter(
        ({ value }) =>
          value.phase === 12n &&
          typeof value.auxiliary === "object" &&
          ("ValueInputAssetWitness" in value.auxiliary ||
            "ValueOutputAssetWitness" in value.auxiliary ||
            "ValueMintAssetWitness" in value.auxiliary),
      );
      expect(assetMutations).toHaveLength(2);
      expect(assetMutations.every(({ key }) => key.execution_index < 0n)).toBe(
        true,
      );
      expect(
        assetMutations.map(({ value }) => {
          const control = decodeSingleCbor(
            Buffer.from(value.witness_cbor, "hex"),
          );
          return {
            stage: Array.isArray(control) ? BigInt(control[1] as number) : null,
            auxiliary:
              typeof value.auxiliary === "object"
                ? Object.keys(value.auxiliary)[0]
                : value.auxiliary,
          };
        }),
      ).toEqual([
        { stage: 2n, auxiliary: "ValueInputAssetWitness" },
        { stage: 3n, auxiliary: "ValueOutputAssetWitness" },
      ]);
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
        effectiveEndTime: new Date("2026-07-23T12:01:00.000Z"),
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        validation: {
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          bucketConcurrency: 1,
          slotForUnixTime: () => 100n,
        },
        resolveProgramMaterialSidecar: () =>
          Effect.succeed(encodeMidgardCekProgramMaterialSidecar([])),
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

/**
 * Every input of a forced transaction resolves against the ledger state
 * immediately before it, as on Cardano. Its program material is the shared
 * per-event set at that position whatever its verdict, and the DA committee
 * derives the same set from the same pre-state.
 */
describe("forced transaction program material at its position", () => {
  const terminal = { kind: "error" } as const;
  const termPreimage = encodeMidgardCekTermNode(terminal);
  const termRoot = hashMidgardCekTermNode(terminal);
  const envelope = {
    uplcVersion: [1n, 1n, 0n] as const,
    termRoot,
    nodeCount: 1n,
    materialByteLength: BigInt(termPreimage.length),
  };
  const materialByRoot = new Map([
    [
      termRoot.toString("hex"),
      { kind: "term" as const, root: termRoot, preimage: termPreimage },
    ],
  ]);
  const scriptRefOutput = encodeMidgardTxOutput({
    address: TEST_ADDRESS,
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    script_ref: {
      language: "MidgardV1",
      scriptBytes: encodeMidgardCekProgramEnvelope(envelope),
    },
  });
  const validation = {
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    bucketConcurrency: 1,
    slotForUnixTime: () => 100n,
  };

  /** Classifies `transaction` as the block's only event, recording every
   * program set the node asks the material store for. */
  const classifyAlone = async (
    transaction: ReturnType<typeof makeSignedEffectfulTransaction>,
    initialState: ReadonlyMap<string, Buffer>,
  ) => {
    const resolverCalls: string[][] = [];
    const [classified] = await Effect.runPromise(
      classifyForcedTransactions({
        entries: [await forcedEntry({ label: 9, transaction })],
        initialState: new Map(initialState),
        effectiveEndTime: new Date("2026-07-23T12:01:00.000Z"),
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        validation,
        resolveProgramMaterialSidecar: (envelopes) => {
          resolverCalls.push(
            envelopes.map((requested) =>
              Buffer.from(requested.termRoot).toString("hex"),
            ),
          );
          return Effect.succeed(
            encodeMidgardCekProgramMaterialSidecar(
              envelopes.map(
                (requested) =>
                  materialByRoot.get(
                    Buffer.from(requested.termRoot).toString("hex"),
                  )!,
              ),
            ),
          );
        },
      }),
    );
    return { classified: classified!, resolverCalls };
  };

  /** The DA committee's replay of a block holding only `classified`, from
   * `initialState`, over the material the node journals for it. */
  const committeeReplay = async (
    classified: Awaited<ReturnType<typeof classifyAlone>>["classified"],
    initialState: ReadonlyMap<string, Buffer>,
  ): Promise<void> => {
    const base = await makePayloadFixture(1);
    const orderKeyHex =
      classified.entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString(
        "hex",
      );
    const txOrderId = Data.from(
      orderKeyHex,
      SDK.OutputReference,
    ) as SDK.OutputReference;
    validateDaPayloadEventProgramCoverage(
      {
        ...base.payload.block_body,
        withdrawals: [],
        transactions: [],
        transaction_preimages: [],
        forced_transactions: [
          [
            orderKeyHex,
            classified.entry[
              ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE
            ].toString("hex"),
          ],
        ],
        forced_transaction_preimages: [
          [
            orderKeyHex,
            classified.entry[
              ForcedTransactionsDB.Columns.NATIVE_TX_CBOR
            ].toString("hex"),
          ],
        ],
        event_to_step: [
          [
            Data.to(
              { ForcedTransactionEventKey: { tx_order_id: txOrderId } },
              SDK.EventKey,
            ),
            Data.to(
              { step_index: 0n, phase: "ForcedTransaction" },
              SDK.EventToStepValue,
            ),
          ],
        ],
        cek_program_material: mergeMidgardCekProgramMaterialSidecars([
          classified.programMaterialSidecarCbor,
        ]).map((entry) => [
          Buffer.from(entry.root).toString("hex"),
          encodeMidgardCekProgramMaterialDaValue(entry).toString("hex"),
        ]),
      },
      [...initialState.entries()],
    );
  };

  it("judges an absent reference input InputNotFound and commits only the attached material", async () => {
    const spent = outputReferenceFromHash(Buffer.alloc(32, 0x61));
    const absentReference = outputReferenceFromHash(Buffer.alloc(32, 0x62));
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const initialState = new Map([[spent.toString("hex"), spentOutput]]);
    const transaction = makeSignedEffectfulTransaction(spent, spentOutput, {
      referenceInputs: [absentReference],
    });

    const { classified, resolverCalls } = await classifyAlone(
      transaction,
      initialState,
    );

    expect(resolverCalls).toEqual([[]]);
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
    expect(
      decodeMidgardCekProgramMaterialSidecar(
        classified.programMaterialSidecarCbor,
      ),
    ).toEqual([]);
    await expect(
      committeeReplay(classified, initialState),
    ).resolves.toBeUndefined();
  });

  it("commits a live reference's program for a transaction Phase A rejects", async () => {
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

    const { classified, resolverCalls } = await classifyAlone(
      transaction,
      initialState,
    );

    expect(resolverCalls).toEqual([[termRoot.toString("hex")]]);
    expect(ForcedTransactionsDB.operatorValidityOfEntry(classified.entry)).toBe(
      "TxIsInvalid",
    );
    expect(classified.rejectionCode).toBe(RejectCodes.NetworkIdMismatch);
    expect(
      decodeMidgardCekProgramMaterialSidecar(
        classified.programMaterialSidecarCbor,
      ),
    ).toEqual([materialByRoot.get(termRoot.toString("hex"))]);
    await expect(
      committeeReplay(classified, initialState),
    ).resolves.toBeUndefined();
    // The committee derives the same program set: without the referenced
    // program the block's material does not cover the event at its position.
    await expect(
      committeeReplay(
        {
          ...classified,
          programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(
            [],
          ),
        },
        initialState,
      ),
    ).rejects.toMatchObject({ code: "coverage_mismatch" });
  });
});
