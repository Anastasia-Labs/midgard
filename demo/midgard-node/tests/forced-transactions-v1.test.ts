import { createHash } from "node:crypto";

import { encodeMidgardCekProgramMaterialSidecarV1 } from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardCekTermNodeV1,
  hashMidgardCekProgramMaterialPreimageV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxBodyCompactV1,
  deriveMidgardNativeTxCompactV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxBodyCanonicalV1,
  type MidgardNativeTxCanonicalV1,
  type MidgardNativeTxFullV1,
  type MidgardNativeTxWitnessSetCanonicalV1,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { RejectCodes } from "@al-ft/midgard-validation";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "@/database/index.js";
import { publishedProgramMaterialEntries } from "@/fibers/fetch-and-insert-tx-order-utxos.js";
import {
  buildDeterministicValidationTraceMembers,
  classifyForcedTransactionsV1,
} from "@/workers/utils/mpf.js";

const canonicalTransaction = (
  version: bigint = MIDGARD_NATIVE_TX_V1_VERSION,
): MidgardNativeTxCanonicalV1 => ({
  version,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: EMPTY_CBOR_LIST,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  },
});

const encodedTransaction = (version?: bigint): Buffer =>
  encodeMidgardNativeTxCanonicalV1(
    materializeMidgardNativeTxFromCanonicalV1(canonicalTransaction(version)),
  );

const TEST_PRIVATE_KEY = CML.PrivateKey.generate_ed25519();
const TEST_ADDRESS = Buffer.from(
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(TEST_PRIVATE_KEY.to_public().hash()),
  )
    .to_address()
    .to_raw_bytes(),
);

const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  encodeCbor(items.map((item) => Buffer.from(item)));

const outputReferenceFromHash = (
  transactionId: Buffer,
  outputIndex = 0n,
): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(transactionId),
      outputIndex,
    ).to_cbor_bytes(),
  );

const makeOutput = (lovelace: bigint): Buffer =>
  encodeMidgardTxOutput({
    address: TEST_ADDRESS,
    value: { lovelace, assets: new Map() },
  });

const makeSignedEffectfulTransaction = (
  spendInput: Buffer,
  output: Buffer,
): {
  readonly transaction: MidgardNativeTxFullV1;
  readonly transactionId: Buffer;
  readonly canonicalCbor: Buffer;
} => {
  const body: MidgardNativeTxBodyCanonicalV1 = {
    spendInputsPreimageCbor: encodeByteList([spendInput]),
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: encodeByteList([output]),
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  };
  const bodyHash = computeMidgardNativeTxIdV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    transactionBody: deriveMidgardNativeTxBodyCompactV1(body),
    transactionWitnessSetHash: Buffer.alloc(32),
    validity: "TxIsValid",
  });
  const witnessSet: MidgardNativeTxWitnessSetCanonicalV1 = {
    addrTxWitsPreimageCbor: encodeByteList([
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(bodyHash),
          TEST_PRIVATE_KEY,
        ).to_cbor_bytes(),
      ),
    ]),
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  };
  const transaction: MidgardNativeTxFullV1 = {
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    compact: deriveMidgardNativeTxCompactV1(
      body,
      witnessSet,
      "TxIsValid",
      MIDGARD_NATIVE_TX_V1_VERSION,
    ),
    body,
    witnessSet,
  };
  return {
    transaction,
    transactionId: computeMidgardNativeTxIdV1(transaction),
    canonicalCbor: encodeMidgardNativeTxCanonicalV1(transaction),
  };
};

const forcedEntry = async ({
  label,
  transaction,
}: {
  readonly label: number;
  readonly transaction: ReturnType<typeof makeSignedEffectfulTransaction>;
}): Promise<ForcedTransactionsDB.Entry> => {
  const encoded = await Effect.runPromise(
    ForcedTransactionsDB.encodeForcedInclusionValueV1({
      nativeTxCbor: transaction.canonicalCbor,
      operatorValidity: "UnbalancedTx",
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    }),
  );
  const txOrderId: SDK.OutputReference = {
    transactionId: Buffer.alloc(32, label).toString("hex"),
    outputIndex: 0n,
  };
  const sidecarCbor = encodeMidgardCekProgramMaterialSidecarV1([]);
  return {
    [ForcedTransactionsDB.Columns.TX_ORDER_ID]: Buffer.from(
      Data.to(txOrderId, SDK.OutputReference),
      "hex",
    ),
    [ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH]: Buffer.alloc(32, label),
    [ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX]: 0,
    [ForcedTransactionsDB.Columns.ASSET_NAME]: Buffer.from([label]),
    [ForcedTransactionsDB.Columns.RAW_DATUM]: Buffer.from([label]),
    [ForcedTransactionsDB.Columns.TX_ID]: encoded.txId,
    [ForcedTransactionsDB.Columns.TX_COMPACT]: encoded.txCompact,
    [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoded.value,
    [ForcedTransactionsDB.Columns.OPERATOR_VALIDITY]: "UnbalancedTx",
    [ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID]:
      MIDGARD_CONSENSUS_PROFILE_V1.profileId,
    [ForcedTransactionsDB.Columns.NATIVE_TX_CBOR]: transaction.canonicalCbor,
    [ForcedTransactionsDB.Columns.TRANSACTION_COMMITMENT]:
      encoded.transactionCommitment,
    [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]:
      sidecarCbor,
    [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]:
      createHash("sha256").update(sidecarCbor).digest(),
    [ForcedTransactionsDB.Columns.INCLUSION_TIME]: new Date(
      `2026-07-23T12:00:${label.toString().padStart(2, "0")}.000Z`,
    ),
    [ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH]: null,
    [ForcedTransactionsDB.Columns.STATUS]: ForcedTransactionsDB.Status.Awaiting,
  };
};

describe("V1 forced transaction material", () => {
  it("accepts only exact self-authenticating material from the immutable L1 address", () => {
    const preimage = encodeMidgardCekTermNodeV1({ kind: "error" });
    const root = hashMidgardCekProgramMaterialPreimageV1("term", preimage);
    const [publication] = SDK.deriveCekProgramMaterialPublicationsV1([
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
          SDK.CekProgramMaterialDatumV1,
        ),
        1,
      ),
      utxo(undefined, 2),
    ]);

    expect(decoded.entries).toEqual([publication!.entry]);
    expect(decoded.malformedCount).toBe(2);
  });

  it("binds the exact canonical transaction independently of its derived verdict", async () => {
    const nativeTxCbor = encodedTransaction();
    const accepted = await Effect.runPromise(
      ForcedTransactionsDB.encodeForcedInclusionValueV1({
        nativeTxCbor,
        operatorValidity: "TxIsValid",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      }),
    );
    const rejected = await Effect.runPromise(
      ForcedTransactionsDB.encodeForcedInclusionValueV1({
        nativeTxCbor,
        operatorValidity: "FeeTooLow",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      }),
    );
    const decoded = Data.from(
      accepted.value.toString("hex"),
      SDK.ForcedInclusionTxV1,
    ) as SDK.ForcedInclusionTxV1;

    expect(accepted.txId).toEqual(rejected.txId);
    expect(accepted.transactionCommitment).toEqual(
      rejected.transactionCommitment,
    );
    expect(accepted.txCompact).toEqual(rejected.txCompact);
    expect(accepted.value).not.toEqual(rejected.value);
    expect(decoded).toEqual({
      tx_id: accepted.txId.toString("hex"),
      transaction_commitment: accepted.transactionCommitment.toString("hex"),
      source: {
        compact_cbor: accepted.source.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          accepted.source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          accepted.source.fieldPreimageLengthsCbor.toString("hex"),
      },
      operator_validity: "TxIsValid",
    });
    const journalMember =
      ForcedTransactionsDB.encodeForcedTransactionJournalMemberV1({
        sourceValueCbor: accepted.value,
        canonicalTransactionCbor: nativeTxCbor,
        programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1(
          [],
        ),
      });
    expect(
      ForcedTransactionsDB.decodeForcedTransactionJournalMemberV1(
        journalMember,
      ),
    ).toEqual({
      sourceValueCbor: accepted.value,
      canonicalTransactionCbor: nativeTxCbor,
      programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([]),
    });
  });

  it("builds a deterministic forced rejection descriptor from the same Phase A/B replay", async () => {
    const nativeTxCbor = encodedTransaction();
    const txId = computeMidgardNativeTxIdV1(
      materializeMidgardNativeTxFromCanonicalV1(canonicalTransaction()),
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
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
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
            programMaterialSidecarCbor:
              encodeMidgardCekProgramMaterialSidecarV1([]),
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
    expect(members[0]?.value).toMatchObject({
      schema_version: 1n,
      machine_version: 1n,
      verdict: "Rejected",
    });
    expect(
      Data.from(
        members[0]!.valueCbor.toString("hex"),
        SDK.ValidationTraceDescriptorV1,
      ),
    ).toEqual(members[0]!.value);
  });

  it("executes sequential valid forced deltas and emits accepted validation traces", async () => {
    const initialInput = outputReferenceFromHash(Buffer.alloc(32, 0x31));
    const output = makeOutput(10n);
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
      classifyForcedTransactionsV1({
        entries,
        initialState: new Map([[initialInput.toString("hex"), output]]),
        effectiveEndTime: new Date("2026-07-23T12:01:00.000Z"),
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
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
          return Effect.succeed(encodeMidgardCekProgramMaterialSidecarV1([]));
        },
      }),
    );

    expect(resolverCalls).toEqual([[], []]);
    expect(classified).toHaveLength(2);
    for (const result of classified) {
      expect(result.entry[ForcedTransactionsDB.Columns.OPERATOR_VALIDITY]).toBe(
        "TxIsValid",
      );
      expect(result.rejectionCode).toBeNull();
      expect(result.ledgerOps).toHaveLength(2);
      expect(result.ledgerMutationSteps).toHaveLength(2);
      expect(result.ledgerWitnessEntries).toHaveLength(1);
    }
    expect(classified[0]!.ledgerOps).toMatchObject([
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
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
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
    expect(
      members.every(
        ({ value }) =>
          value.machine_version === 1n && value.verdict === "Accepted",
      ),
    ).toBe(true);
  });

  it("retains invalid forced transactions as classified no-op sources", async () => {
    const missingInput = outputReferenceFromHash(Buffer.alloc(32, 0x41));
    const transaction = makeSignedEffectfulTransaction(
      missingInput,
      makeOutput(10n),
    );
    const [classified] = await Effect.runPromise(
      classifyForcedTransactionsV1({
        entries: [await forcedEntry({ label: 3, transaction })],
        initialState: new Map(),
        effectiveEndTime: new Date("2026-07-23T12:01:00.000Z"),
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
        validation: {
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          bucketConcurrency: 1,
          slotForUnixTime: () => 100n,
        },
        resolveProgramMaterialSidecar: () =>
          Effect.succeed(encodeMidgardCekProgramMaterialSidecarV1([])),
      }),
    );

    expect(
      classified!.entry[ForcedTransactionsDB.Columns.OPERATOR_VALIDITY],
    ).toBe("NonExistentInputUtxo");
    expect(classified!.rejectionCode).toBe(RejectCodes.InputNotFound);
    expect(classified!.ledgerOps).toEqual([]);
    expect(classified!.ledgerMutationSteps).toEqual([]);
    expect(classified!.ledgerWitnessEntries).toEqual([]);
  });
});
