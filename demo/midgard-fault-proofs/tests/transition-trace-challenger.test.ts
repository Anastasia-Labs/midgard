import { Proof as MpfProof, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullV1Validity,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DaRequestResponseProtocol,
  decodeDaEventToStepByEventRequestV1Cbor,
  decodeDaPayloadByHeaderRequestV1Cbor,
  decodeDaProofBundleByHeaderRequestV1Cbor,
  decodeDaTraceStepByIndexRequestV1Cbor,
  encodeDaEventToStepByEventResponseV1Cbor,
  encodeDaMetadataByHeaderResponseV1Cbor,
  encodeDaPayloadByHeaderResponseV1Cbor,
  encodeDaProofBundleByHeaderResponseV1Cbor,
  encodeDaTraceStepByIndexResponseV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterialV1 } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  buildEventToStepMismatchFault,
  buildIndexedTraceProof,
  buildInvalidForcedTransactionNoOpWitness,
  buildL2TransactionTransitionWitness,
  buildOmittedDueL1EventFault,
  buildOutOfWindowSourceEventFault,
  buildSourceNonMembershipProof,
  buildSourcePhaseMismatchFault,
  buildTraceBoundaryFault,
  buildTransitionFaultProof,
  DaLibp2pRetainedDaSource,
  detectTransitionTraceFaults,
  encodeData,
  fetchRetainedDaPayloadByHeaderHash,
  keyValuePhasRootWithCount,
  type OmittedDueL1EventEvidence,
  type OutOfWindowSourceEventEvidence,
  reconstructDaPayloadV1,
  type RetainedDaLibp2pTransport,
  type TransitionTraceReconstruction,
} from "../src/transition-trace/index.js";

const h32 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);
const h28 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(28);

const outRef = (byte: number): SDK.OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});

const address = (byte: number): SDK.AddressData => ({
  paymentCredential: { PublicKeyCredential: [h28(byte)] },
  stakeCredential: null,
});

const depositInfo = (byte: number): SDK.DepositInfo => ({
  l2_address: address(byte),
  l2_network_id: 0n,
  l2_datum: null,
});

const withdrawalInfo = (
  byte: number,
  validity: SDK.WithdrawalValidity = "IncorrectWithdrawalSignature",
): SDK.WithdrawalInfo => ({
  body: {
    l2_outref: outRef(byte),
    l2_owner: h28(byte + 1),
    l2_value: new Map(),
    l1_address: address(byte + 2),
    l1_datum: "NoDatum",
  },
  signature: [h32(byte + 3), h32(byte + 4)],
  validity,
});

/**
 * Fixture-local index from a native proof source back to the canonical CBOR the
 * fixture built it from.
 *
 * Keyed by `native_tx_proof_commitment_v1` rather than by tx id even though #584
 * retired `transaction_commitment` from the on-chain leaves. The commitment
 * covers the compact body, the compact witness set and the validity code; the tx
 * id covers the body alone. Every fixture here happens to pin `TxIsValid` with
 * an empty witness set, so the two keys are injective over today's vectors — but
 * a later vector that varies validity or witnesses would silently collide under
 * a tx-id key and hand back the wrong preimage. Nothing outside this file sees
 * the commitment: it is derived here from the source and is deliberately not
 * re-exposed on {@link nativeMaterial}'s result.
 */
const canonicalPreimageByCommitment = new Map<string, Buffer>();

const proofSourceCommitment = (source: SDK.NativeTxProofSourceV1): string =>
  computeMidgardNativeTxProofCommitmentV1({
    compactCbor: Buffer.from(source.compact_cbor, "hex"),
    witnessSetCompactCbor: Buffer.from(source.witness_set_compact_cbor, "hex"),
    fieldPreimageLengthsCbor: Buffer.from(
      source.field_preimage_lengths_cbor,
      "hex",
    ),
  }).toString("hex");

const nativeMaterial = (
  byte: number,
  preimages: {
    readonly spendInputsPreimageCbor?: Buffer;
    readonly outputsPreimageCbor?: Buffer;
  } = {},
) => {
  const canonical: MidgardNativeTxCanonicalV1 = {
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor:
        preimages.spendInputsPreimageCbor ?? EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: preimages.outputsPreimageCbor ?? EMPTY_CBOR_LIST,
      fee: BigInt(byte),
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
  };
  const full = materializeMidgardNativeTxFromCanonicalV1(canonical);
  const canonicalCbor = encodeMidgardNativeTxCanonicalV1(full);
  const source =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(canonicalCbor);
  const txId = computeMidgardNativeTxIdV1(full).toString("hex");
  canonicalPreimageByCommitment.set(
    computeMidgardNativeTxProofCommitmentV1(source).toString("hex"),
    canonicalCbor,
  );
  return {
    txId,
    canonicalCbor,
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
};

/**
 * The #640 verdict standing in for the pre-format `FailedScript` arm: a
 * forced transaction the operator rejected for a failed Plutus execution at
 * execution index 0.
 */
const forcedTxInvalidPlutus: SDK.OperatorVerdictV1 = {
  ForcedTxInvalid: {
    reason: { PlutusExecutionFailed: { execution_index: 0n } },
  },
};

const forcedTx = (
  byte: number,
  verdict: SDK.OperatorVerdictV1 = forcedTxInvalidPlutus,
): SDK.ForcedInclusionTxV1 => {
  const material = nativeMaterial(byte);
  if (verdict === "ForcedTxValid") {
    return {
      tx_id: material.txId,
      source: material.source,
      verdict,
    };
  }
  // A rejected forced leaf commits the operator-adjudicated source
  // (§2.4.3(e)): the fixture bytes stay `TxIsValid` as submitted, while the
  // leaf's triple carries the stamped `TxIsInvalid` scalar. The DA preimage
  // registered for the leaf remains the submitted canonical bytes.
  const adjudicated = deriveMidgardNativeTxProofSourceV1(
    adjudicateMidgardNativeTxFullV1Validity(
      decodeMidgardNativeTxFullV1FromCanonicalCbor(material.canonicalCbor),
      "TxIsInvalid",
    ),
  );
  canonicalPreimageByCommitment.set(
    computeMidgardNativeTxProofCommitmentV1(adjudicated).toString("hex"),
    material.canonicalCbor,
  );
  return {
    tx_id: material.txId,
    source: {
      compact_cbor: adjudicated.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        adjudicated.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        adjudicated.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict,
  };
};

const entry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

const sorted = (entries: readonly SDK.DaPayloadEntry[]): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const LEDGER_OUTPUT_CBOR =
  "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0";
const TAG4_OUTPUT_ADDRESS = Buffer.from(`70${"aa".repeat(28)}`, "hex");
const tag4OutputRequiredFields = (lovelaceCbor = Buffer.from([0])): Buffer =>
  Buffer.concat([
    Buffer.from([0]),
    encodeCbor(TAG4_OUTPUT_ADDRESS),
    Buffer.from([1, 0x82]),
    lovelaceCbor,
    Buffer.from([0xa0]),
  ]);
const TAG4_OUTPUT_REQUIRED_FIELDS = tag4OutputRequiredFields();
const tag4OutputWithNonMinimalLovelace = (): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2]),
    tag4OutputRequiredFields(Buffer.from([0x18, 0])),
  ]);
const tag4OutputWithAssetOrder = (firstQuantityCbor: Buffer): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2, 0]),
    encodeCbor(TAG4_OUTPUT_ADDRESS),
    Buffer.from([1, 0x82, 0, 0xa2, 0x58, 28]),
    Buffer.alloc(28, 0xbb),
    Buffer.from([0xa2]),
    encodeCbor(Buffer.from([0xff])),
    firstQuantityCbor,
    encodeCbor(Buffer.from([0])),
    Buffer.from([2, 0x58, 28]),
    Buffer.alloc(28, 0xaa),
    Buffer.from([0xa1]),
    encodeCbor(Buffer.from([1])),
    Buffer.from([3]),
  ]);
const tag4OutputWithNonMinimalQuantity = (): Buffer =>
  tag4OutputWithAssetOrder(Buffer.from([0x18, 1]));
const tag4OutputWithPreservedAssetOrder = (firstQuantity = 1): Buffer =>
  tag4OutputWithAssetOrder(Buffer.from([firstQuantity]));
const tag4OutputWithOpaqueDatum = (): Buffer =>
  Buffer.concat([
    Buffer.from([0xa3]),
    TAG4_OUTPUT_REQUIRED_FIELDS,
    Buffer.from([2]),
    encodeCbor(Buffer.from([0xff])),
  ]);
const tag4OutputWithOpaqueNativeScript = (): Buffer =>
  Buffer.concat([
    Buffer.from([0xa3]),
    TAG4_OUTPUT_REQUIRED_FIELDS,
    Buffer.from([3, 0x82, 0]),
    encodeCbor(Buffer.from([0xde, 0xad, 0xff])),
  ]);

const spendInputItem = (txIdHex: string, outputIndex: number): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.from(txIdHex, "hex"),
    outputIndex,
  });

const rawLedgerEntry = (byte: number): SDK.DaPayloadEntry => [
  spendInputItem(h32(byte), 0).toString("hex"),
  LEDGER_OUTPUT_CBOR,
];

/// The exact bytes one output occupies in `utxos_root`: its
/// `LedgerOutputCommitmentV1` descriptor, keyed by the out-ref the entry is
/// filed under (spec §5.3 — "not with the full output bytes"). Fixtures that
/// feed deliberately malformed or non-canonical output bytes have no
/// descriptor at all; the challenger refuses those before it reaches MPF
/// replay, so the trie value it never reads falls back to the raw bytes rather
/// than making the fixture unbuildable.
const ledgerTrieValue = (outRef: Buffer, outputCbor: Buffer): Buffer => {
  try {
    return Buffer.from(
      buildCanonicalMidgardLedgerEntryOutputMaterialV1({
        outRef,
        outputCbor,
      }).descriptorCbor,
    );
  } catch {
    return outputCbor;
  }
};

const utxoRootWithDescriptors = (
  utxos: readonly SDK.DaPayloadEntry[],
): Promise<Awaited<ReturnType<typeof keyValuePhasRootWithCount>>> =>
  keyValuePhasRootWithCount(
    utxos.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: buildCanonicalMidgardLedgerEntryOutputMaterialV1({
        outRef: Buffer.from(key, "hex"),
        outputCbor: Buffer.from(value, "hex"),
      }).descriptorCbor,
    })),
  );

const encodedEntry = <K, V>({
  key,
  keySchema,
  value,
  valueSchema,
}: {
  readonly key: K;
  readonly keySchema: Parameters<typeof Data.Nullable>[0];
  readonly value: V;
  readonly valueSchema: Parameters<typeof Data.Nullable>[0];
}): SDK.DaPayloadEntry =>
  entry(encodeData(key, keySchema), encodeData(value, valueSchema));

const traceEntryWithKey = (
  key: bigint,
  step: SDK.TransitionStep,
): SDK.DaPayloadEntry =>
  encodedEntry({
    key,
    keySchema: Data.Integer() as never,
    value: step,
    valueSchema: SDK.TransitionStepSchema,
  });

const traceEntry = (step: SDK.TransitionStep): SDK.DaPayloadEntry =>
  traceEntryWithKey(step.step_index, step);

const eventToStepEntry = (
  key: SDK.EventKey,
  value: SDK.EventToStepValue,
): SDK.DaPayloadEntry =>
  encodedEntry({
    key,
    keySchema: SDK.EventKeySchema,
    value,
    valueSchema: SDK.EventToStepValueSchema,
  });

type PayloadFixtureInput = {
  readonly prevUtxosRoot?: string;
  readonly utxos?: readonly SDK.DaPayloadEntry[];
  readonly withdrawals?: readonly SDK.DaPayloadEntry[];
  readonly forcedTransactions?: readonly SDK.DaPayloadEntry[];
  readonly transactions?: readonly SDK.DaPayloadEntry[];
  readonly transactionPreimages?: readonly SDK.DaPayloadEntry[];
  readonly deposits?: readonly SDK.DaPayloadEntry[];
  readonly steps?: readonly SDK.TransitionStep[];
  readonly transitionTraceEntries?: readonly SDK.DaPayloadEntry[];
  readonly eventToStep?: readonly SDK.DaPayloadEntry[];
};

const buildPayloadFixture = async ({
  prevUtxosRoot = SDK.EMPTY_MERKLE_TREE_ROOT,
  utxos = [],
  withdrawals = [],
  forcedTransactions = [],
  transactions = [],
  transactionPreimages = [],
  deposits = [],
  steps = [],
  transitionTraceEntries = steps.map(traceEntry),
  eventToStep = [],
}: PayloadFixtureInput): Promise<{
  readonly payload: SDK.DaPayloadV1;
  readonly payloadEnvelopeCbor: Buffer;
  readonly header: SDK.HeaderV1;
  readonly headerHash: string;
}> => {
  const forcedTransactionPreimages = forcedTransactions.map(
    ([key, value], index): SDK.DaPayloadEntry => {
      const forced = Data.from(
        value,
        SDK.ForcedInclusionTxV1,
      ) as SDK.ForcedInclusionTxV1;
      const preimage = canonicalPreimageByCommitment.get(
        proofSourceCommitment(forced.source),
      );
      if (preimage === undefined) {
        throw new Error(
          `missing forced transaction preimage ${index.toString()}`,
        );
      }
      return [key, preimage.toString("hex")];
    },
  );
  const validationEventKeys: SDK.EventKey[] = [
    ...forcedTransactions.map(([key]) => ({
      ForcedTransactionEventKey: {
        tx_order_id: Data.from(key, SDK.OutputReference),
      },
    })),
    ...transactions.map(([key]) => ({
      L2TransactionEventKey: { tx_id: key },
    })),
  ];
  const validationTraces = validationEventKeys.map(
    (eventKey, index): SDK.DaPayloadEntry =>
      encodedEntry({
        key: eventKey,
        keySchema: SDK.EventKeySchema,
        value: {
          schema_version: 1n,
          machine_version: 1n,
          trace_root: h32(140 + index),
          step_count: 1n,
          initial_state_hash: h32(150 + index),
          terminal_state_hash: h32(160 + index),
          verdict: "Accepted",
          rejection_code_hash: h32(170 + index),
        } satisfies SDK.ValidationTraceDescriptorV1,
        valueSchema: SDK.ValidationTraceDescriptorV1Schema,
      }),
  );
  const utxoRoot = await utxoRootWithDescriptors(utxos);
  const roots = {
    withdrawals: await buildCountedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawals.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    forcedTransactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      forcedTransactions.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    transactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      transactions.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    deposits: await buildCountedRoot(
      SDK.ROOT_DOMAINS.deposits,
      deposits.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    transitionTrace: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      transitionTraceEntries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    eventToStep: await buildCountedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      eventToStep.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
    validationTraces: await buildCountedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      validationTraces.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    ),
  };
  const counts = {
    withdrawalCount: BigInt(withdrawals.length),
    forcedTransactionCount: BigInt(forcedTransactions.length),
    l2TransactionCount: BigInt(transactions.length),
    depositCount: BigInt(deposits.length),
    totalEventCount:
      BigInt(withdrawals.length) +
      BigInt(forcedTransactions.length) +
      BigInt(transactions.length) +
      BigInt(deposits.length),
    transitionStepCount: BigInt(transitionTraceEntries.length),
    validationTraceCount: BigInt(validationTraces.length),
  };
  const header: SDK.HeaderV1 = {
    prevUtxosRoot,
    utxosRoot: utxoRoot.root,
    withdrawalsRoot: roots.withdrawals.root,
    forcedTransactionsRoot: roots.forcedTransactions.root,
    transactionsRoot: roots.transactions.root,
    depositsRoot: roots.deposits.root,
    transitionTraceRoot: roots.transitionTrace.root,
    eventToStepRoot: roots.eventToStep.root,
    validationTracesRoot: roots.validationTraces.root,
    ...counts,
    startTime: 10n,
    endTime: 20n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: h28(90),
    operatorVkey: h28(91),
    protocolVersion: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  const payload: SDK.DaPayloadV1 = {
    version: SDK.DA_PAYLOAD_V1_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: sorted(utxos),
      withdrawals: sorted(withdrawals),
      forced_transactions: sorted(forcedTransactions),
      transactions: sorted(transactions),
      deposits: sorted(deposits),
      transition_trace: sorted(transitionTraceEntries),
      event_to_step: sorted(eventToStep),
      transaction_preimages: sorted(transactionPreimages),
      forced_transaction_preimages: sorted(forcedTransactionPreimages),
      cek_program_material: [],
      validation_traces: sorted(validationTraces),
      counts,
    },
  };
  return {
    payload,
    payloadEnvelopeCbor: await wrapDaPayloadV1(SDK.encodeDaPayloadV1(payload), {
      mode: "identity",
    }),
    header,
    headerHash,
  };
};

const reconstruct = async (
  fixture: Awaited<ReturnType<typeof buildPayloadFixture>>,
): Promise<TransitionTraceReconstruction> =>
  await reconstructDaPayloadV1({
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    expectedHeaderHash: fixture.headerHash,
    committedHeader: fixture.header,
  });

const forcedEventKey = (id: SDK.OutputReference): SDK.EventKey => ({
  ForcedTransactionEventKey: { tx_order_id: id },
});

const depositEventKey = (id: SDK.OutputReference): SDK.EventKey => ({
  DepositEventKey: { deposit_id: id },
});

const withdrawalEventKey = (id: SDK.OutputReference): SDK.EventKey => ({
  WithdrawalEventKey: { withdrawal_id: id },
});

const sdkProof = (proof: MpfProof): SDK.Proof =>
  Data.from(proof.toCBOR().toString("hex"), SDK.Proof) as SDK.Proof;

type L2ReplayFixture = {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly evidence: {
    readonly stepIndex: bigint;
    readonly spentUtxos: readonly SDK.LedgerDeleteWitness[];
    readonly producedUtxos: readonly SDK.LedgerInsertWitness[];
  };
  readonly replayedPostRoot: string;
};

const buildL2ReplayFixture = async ({
  matchingCommittedRoot,
  withBranchProof = false,
  spendInputCbor,
  outputCbor,
  replayedOutputCbor,
  includeProducedPayloadUtxo = true,
  producedWitnessValue = "descriptor",
}: {
  readonly matchingCommittedRoot: boolean;
  readonly withBranchProof?: boolean;
  readonly spendInputCbor?: Buffer;
  readonly outputCbor?: Buffer;
  readonly replayedOutputCbor?: Buffer;
  readonly includeProducedPayloadUtxo?: boolean;
  readonly producedWitnessValue?: "descriptor" | "fullOutputBytes";
}): Promise<L2ReplayFixture> => {
  const spentKey = spendInputCbor ?? spendInputItem(h32(81), 0);
  const spentOutputCbor = Buffer.from(LEDGER_OUTPUT_CBOR, "hex");
  const spentValue = ledgerTrieValue(spentKey, spentOutputCbor);
  const sourceOutput = outputCbor ?? Buffer.from(LEDGER_OUTPUT_CBOR, "hex");
  const producedOutput = replayedOutputCbor ?? sourceOutput;
  const material = nativeMaterial(82, {
    spendInputsPreimageCbor: encodeCbor([spentKey]),
    outputsPreimageCbor: encodeCbor([sourceOutput]),
  });
  const producedKey = spendInputItem(material.txId, 0);
  const producedValue = ledgerTrieValue(producedKey, producedOutput);
  const producedWitnessBytes =
    producedWitnessValue === "descriptor" ? producedValue : producedOutput;
  const survivors = withBranchProof
    ? Array.from({ length: 16 }, (_, index) => {
        const key = spendInputItem(h32(100 + index), index);
        return {
          key,
          outputCbor: spentOutputCbor,
          value: ledgerTrieValue(key, spentOutputCbor),
        };
      })
    : [];

  const ledger = await Trie.fromList([
    { key: spentKey, value: spentValue },
    ...survivors.map(({ key, value }) => ({ key, value })),
  ]);
  const preRoot = ledger.hash.toString("hex");
  const membershipProof = await ledger.prove(spentKey);
  const deleteProof = await ledger.prove(spentKey);
  await ledger.delete(spentKey);
  await ledger.insert(producedKey, producedValue);
  const insertProof = await ledger.prove(producedKey);
  const replayedPostRoot = ledger.hash.toString("hex");

  const source: SDK.L2TransactionSourceV1 = {
    tx_id: material.txId,
    source: material.source,
  };
  const eventKey: SDK.EventKey = {
    L2TransactionEventKey: { tx_id: material.txId },
  };
  const fixture = await buildPayloadFixture({
    prevUtxosRoot: preRoot,
    utxos: [
      ...survivors.map(
        ({ key, outputCbor: survivorOutputCbor }): SDK.DaPayloadEntry => [
          key.toString("hex"),
          survivorOutputCbor.toString("hex"),
        ],
      ),
      ...(includeProducedPayloadUtxo
        ? ([
            [producedKey.toString("hex"), producedOutput.toString("hex")],
          ] satisfies SDK.DaPayloadEntry[])
        : []),
    ],
    transactions: [
      entry(
        Buffer.from(material.txId, "hex"),
        Buffer.from(Data.to(source, SDK.L2TransactionSourceV1), "hex"),
      ),
    ],
    transactionPreimages: [
      entry(Buffer.from(material.txId, "hex"), material.canonicalCbor),
    ],
    steps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "L2Transaction",
        pre_utxos_root: preRoot,
        post_utxos_root: matchingCommittedRoot ? replayedPostRoot : h32(83),
      },
    ],
    eventToStep: [
      eventToStepEntry(eventKey, {
        step_index: 0n,
        phase: "L2Transaction",
      }),
    ],
  });
  const encodedMembershipProof = sdkProof(membershipProof);
  const encodedDeleteProof = sdkProof(deleteProof);
  const encodedInsertProof = sdkProof(insertProof);
  return {
    reconstruction: await reconstruct(fixture),
    evidence: {
      stepIndex: 0n,
      spentUtxos: [
        {
          key: spentKey.toString("hex"),
          value: spentValue.toString("hex"),
          membership_proof: encodedMembershipProof,
          delete_proof: encodedDeleteProof,
        },
      ],
      producedUtxos: [
        {
          key: producedKey.toString("hex"),
          value: producedWitnessBytes.toString("hex"),
          non_membership_proof: encodedInsertProof,
          insert_proof: encodedInsertProof,
        },
      ],
    },
    replayedPostRoot,
  };
};

describe("transition-trace challenger tooling", () => {
  const expectBuildableDetection = (
    detections: readonly unknown[],
    expected: {
      readonly kind: string;
      readonly invariant: string;
    },
  ) =>
    expect(detections).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          buildable: true,
          ...expected,
        }),
      ]),
    );

  it("reconstructs every DA payload V1 root and rejects header/root mismatches", async () => {
    const txOrderId = outRef(1);
    const forced = forcedTx(10);
    const finalUtxo = rawLedgerEntry(1);
    const finalRoot = await utxoRootWithDescriptors([finalUtxo]);
    const eventKey = forcedEventKey(txOrderId);
    const step: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 0n,
      event_key: eventKey,
      phase: "ForcedTransaction",
      pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      post_utxos_root: finalRoot.root,
    };
    const fixture = await buildPayloadFixture({
      utxos: [finalUtxo],
      forcedTransactions: [
        encodedEntry({
          key: txOrderId,
          keySchema: SDK.OutputReference as never,
          value: forced,
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      steps: [step],
      eventToStep: [
        eventToStepEntry(eventKey, {
          step_index: 0n,
          phase: "ForcedTransaction",
        }),
      ],
    });

    const result = await reconstruct(fixture);

    expect(result.roots).toEqual({
      utxosRoot: fixture.header.utxosRoot,
      withdrawalsRoot: fixture.header.withdrawalsRoot,
      forcedTransactionsRoot: fixture.header.forcedTransactionsRoot,
      transactionsRoot: fixture.header.transactionsRoot,
      depositsRoot: fixture.header.depositsRoot,
      transitionTraceRoot: fixture.header.transitionTraceRoot,
      eventToStepRoot: fixture.header.eventToStepRoot,
      validationTracesRoot: fixture.header.validationTracesRoot,
    });
    expect(result.counts.totalEventCount).toBe(1n);

    const badHeader = {
      ...fixture.header,
      utxosRoot: h32(99),
    };
    const badHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeaderV1(badHeader),
    );
    const badPayload: SDK.DaPayloadV1 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        header_hash: badHeaderHash,
        header: badHeader,
      },
    };
    await expect(
      reconstructDaPayloadV1({
        payloadEnvelopeCbor: await wrapDaPayloadV1(
          SDK.encodeDaPayloadV1(badPayload),
          { mode: "identity" },
        ),
        expectedHeaderHash: badHeaderHash,
        committedHeader: badHeader,
      }),
    ).rejects.toMatchObject({ code: "rootMismatch" });
  });

  it("rejects sparse, out-of-range, and key/value-mismatched transition traces", async () => {
    const sparseSteps: SDK.TransitionStep[] = [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: depositEventKey(outRef(2)),
        phase: "Deposit",
        pre_utxos_root: h32(2),
        post_utxos_root: h32(3),
      },
      {
        schema_version: 1n,
        step_index: 2n,
        event_key: depositEventKey(outRef(3)),
        phase: "Deposit",
        pre_utxos_root: h32(4),
        post_utxos_root: h32(5),
      },
    ];
    await expect(
      reconstruct(await buildPayloadFixture({ steps: sparseSteps })),
    ).rejects.toMatchObject({
      code: "invalidPayloadEntries",
      message: expect.stringContaining("outside"),
    });

    const mismatchedStep: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 1n,
      event_key: depositEventKey(outRef(4)),
      phase: "Deposit",
      pre_utxos_root: h32(6),
      post_utxos_root: h32(7),
    };
    await expect(
      reconstruct(
        await buildPayloadFixture({
          steps: [mismatchedStep],
          transitionTraceEntries: [traceEntryWithKey(0n, mismatchedStep)],
        }),
      ),
    ).rejects.toMatchObject({
      code: "invalidPayloadEntries",
      message: expect.stringContaining("must equal"),
    });
  });

  it("reconstructs a dense zero-based transition trace", async () => {
    const steps: SDK.TransitionStep[] = [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: depositEventKey(outRef(5)),
        phase: "Deposit",
        pre_utxos_root: h32(8),
        post_utxos_root: h32(9),
      },
      {
        schema_version: 1n,
        step_index: 1n,
        event_key: depositEventKey(outRef(6)),
        phase: "Deposit",
        pre_utxos_root: h32(10),
        post_utxos_root: h32(11),
      },
    ];
    const reconstruction = await reconstruct(
      await buildPayloadFixture({ steps }),
    );

    expect(reconstruction.transitionTrace.map(({ key }) => key)).toEqual([
      0n,
      1n,
    ]);
    expect(reconstruction.traceByStepIndex.has(0n)).toBe(true);
    expect(reconstruction.traceByStepIndex.has(1n)).toBe(true);
  });

  it("builds witness redeemers for each Task08 proof family from reconstructed DA data", async () => {
    const withdrawalId = outRef(2);
    const eventKey = withdrawalEventKey(withdrawalId);
    const withdrawalInfo: SDK.WithdrawalInfo = {
      body: {
        l2_outref: outRef(22),
        l2_owner: h28(23),
        l2_value: new Map(),
        l1_address: address(24),
        l1_datum: "NoDatum",
      },
      signature: [h32(25), h32(26)],
      validity: "IncorrectWithdrawalSignature",
    };
    const step: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 0n,
      event_key: eventKey,
      phase: "Deposit",
      pre_utxos_root: h32(30),
      post_utxos_root: h32(31),
    };
    const fixture = await buildPayloadFixture({
      prevUtxosRoot: h32(29),
      withdrawals: [
        encodedEntry({
          key: withdrawalId,
          keySchema: SDK.OutputReference as never,
          value: withdrawalInfo,
          valueSchema: SDK.WithdrawalInfoSchema,
        }),
      ],
      steps: [step],
      eventToStep: [
        eventToStepEntry(eventKey, {
          step_index: 0n,
          phase: "Deposit",
        }),
      ],
    });
    const reconstruction = await reconstruct(fixture);

    const boundary = await buildTraceBoundaryFault({
      reconstruction,
      side: "TraceStart",
      stepIndex: 0n,
    });
    const eventMismatch = await buildEventToStepMismatchFault({
      reconstruction,
      stepIndex: 0n,
    });
    const sourcePhase = await buildSourcePhaseMismatchFault({
      reconstruction,
      stepIndex: 0n,
    });
    const invalidNoOp = SDK.invalidOneStepTransitionFault(
      await buildInvalidForcedTransactionNoOpWitness({
        reconstruction: await reconstruct(
          await buildPayloadFixture({
            forcedTransactions: [
              encodedEntry({
                key: outRef(3),
                keySchema: SDK.OutputReference as never,
                value: forcedTx(40, forcedTxInvalidPlutus),
                valueSchema: SDK.ForcedInclusionTxV1Schema,
              }),
            ],
            steps: [
              {
                schema_version: 1n,
                step_index: 0n,
                event_key: forcedEventKey(outRef(3)),
                phase: "ForcedTransaction",
                pre_utxos_root: h32(41),
                post_utxos_root: h32(42),
              },
            ],
            eventToStep: [
              eventToStepEntry(forcedEventKey(outRef(3)), {
                step_index: 0n,
                phase: "ForcedTransaction",
              }),
            ],
          }),
        ),
        stepIndex: 0n,
      }),
    );
    const omittedEvidence: OmittedDueL1EventEvidence = {
      kind: "forcedTransaction",
      txOrderId: outRef(4),
      eventRefInputIndex: 0n,
      eventAssetName: "aa",
      validityOverride: forcedTxInvalidPlutus,
    };
    const omitted = await buildOmittedDueL1EventFault({
      reconstruction,
      evidence: omittedEvidence,
    });
    const outOfWindowEvidence: OutOfWindowSourceEventEvidence = {
      kind: "withdrawal",
      withdrawalId,
      eventRefInputIndex: 1n,
      eventAssetName: "bb",
      validityOverride: "IncorrectWithdrawalSignature",
    };
    const outOfWindow = await buildOutOfWindowSourceEventFault({
      reconstruction,
      evidence: outOfWindowEvidence,
    });
    const count = SDK.countFault("HeaderTotalCountMismatch");

    for (const fault of [
      boundary,
      eventMismatch,
      sourcePhase,
      invalidNoOp,
      omitted,
      outOfWindow,
      count,
    ]) {
      const proof = buildTransitionFaultProof({ reconstruction, fault });
      expect(() =>
        Data.from(
          Data.to(proof as never, SDK.TransitionFaultProof as never),
          SDK.TransitionFaultProof as never,
        ),
      ).not.toThrow();
    }
    await expect(
      buildIndexedTraceProof({ reconstruction, stepIndex: 0n }),
    ).resolves.toMatchObject({ key: 0n });
  });

  it("reconstructs authenticated L2 preimages and builds the tag-4 replay witness", async () => {
    const material = nativeMaterial(70);
    const source: SDK.L2TransactionSourceV1 = {
      tx_id: material.txId,
      source: material.source,
    };
    const eventKey: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: material.txId },
    };
    const fixture = await buildPayloadFixture({
      transactions: [
        entry(
          Buffer.from(material.txId, "hex"),
          Buffer.from(Data.to(source, SDK.L2TransactionSourceV1), "hex"),
        ),
      ],
      transactionPreimages: [
        entry(Buffer.from(material.txId, "hex"), material.canonicalCbor),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase: "L2Transaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: h32(71),
        },
      ],
      eventToStep: [
        eventToStepEntry(eventKey, {
          step_index: 0n,
          phase: "L2Transaction",
        }),
      ],
    });
    const reconstruction = await reconstruct(fixture);
    const witness = await buildL2TransactionTransitionWitness({
      reconstruction,
      stepIndex: 0n,
      evidence: { spentUtxos: [], producedUtxos: [] },
    });

    expect(reconstruction.transactions[0]).toMatchObject({
      txId: material.txId,
      validity: "TxIsValid",
      spendInputsPreimage: Buffer.from(EMPTY_CBOR_LIST),
      outputsPreimage: Buffer.from(EMPTY_CBOR_LIST),
    });
    expect(witness).toMatchObject({
      L2TransactionTransition: {
        spend_inputs_preimage: EMPTY_CBOR_LIST.toString("hex"),
        outputs_preimage: EMPTY_CBOR_LIST.toString("hex"),
        spent_utxos: [],
        produced_utxos: [],
      },
    });
    expect(
      Data.to(
        witness as never,
        SDK.InvalidOneStepTransitionWitnessSchema as never,
      ),
    ).toMatch(/^d87d/);

    const detections = await detectTransitionTraceFaults(reconstruction, {
      l2TransactionTransitions: [
        { stepIndex: 0n, spentUtxos: [], producedUtxos: [] },
      ],
    });
    expectBuildableDetection(detections, {
      kind: "invalidOneStepTransition",
      invariant: "l2_transaction_transition_matches_authenticated_replay",
    });
  });

  it("returns no tag-4 fault when verified delete/insert replay matches the committed post-root", async () => {
    const fixture = await buildL2ReplayFixture({
      matchingCommittedRoot: true,
    });
    const detections = await detectTransitionTraceFaults(
      fixture.reconstruction,
      { l2TransactionTransitions: [fixture.evidence] },
    );

    expect(
      detections.filter(
        ({ invariant }) =>
          invariant ===
          "l2_transaction_transition_matches_authenticated_replay",
      ),
    ).toEqual([]);
  });

  it("builds a tag-4 fault only after verified delete/insert replay disagrees with the committed post-root", async () => {
    const fixture = await buildL2ReplayFixture({
      matchingCommittedRoot: false,
    });
    const detections = await detectTransitionTraceFaults(
      fixture.reconstruction,
      { l2TransactionTransitions: [fixture.evidence] },
    );

    expectBuildableDetection(detections, {
      kind: "invalidOneStepTransition",
      invariant: "l2_transaction_transition_matches_authenticated_replay",
    });
  });

  // The hole this arm used to have, from the challenger side. `utxos_root` is
  // descriptor-valued everywhere it is produced, so a witness carrying the full
  // output bytes describes an insert the ledger never performed. It must be
  // refused here rather than replayed into a post-root no honest block can
  // equal — on-chain `apply_l2_outputs` binds the same value with `expect`, so
  // a witness that got past this check could not mint either.
  it("refuses a full-output-bytes insert witness against a descriptor-built ledger", async () => {
    const fixture = await buildL2ReplayFixture({
      matchingCommittedRoot: true,
      producedWitnessValue: "fullOutputBytes",
    });

    await expect(
      detectTransitionTraceFaults(fixture.reconstruction, {
        l2TransactionTransitions: [fixture.evidence],
      }),
    ).rejects.toMatchObject({
      code: "missingWitnessData",
      message: expect.stringContaining(
        "bound to its authenticated transaction output",
      ),
    });
  });

  // The honest half of the pair: byte-for-byte the same block and the same
  // fault, with the descriptor as the inserted value.
  it("accepts a descriptor insert witness against the same descriptor-built ledger", async () => {
    const honest = await buildL2ReplayFixture({
      matchingCommittedRoot: false,
    });
    const replayed = await buildL2ReplayFixture({
      matchingCommittedRoot: false,
      producedWitnessValue: "fullOutputBytes",
    });
    expect(honest.evidence.producedUtxos[0]!.value).not.toEqual(
      replayed.evidence.producedUtxos[0]!.value,
    );

    const detections = await detectTransitionTraceFaults(
      honest.reconstruction,
      {
        l2TransactionTransitions: [honest.evidence],
      },
    );

    expectBuildableDetection(detections, {
      kind: "invalidOneStepTransition",
      invariant: "l2_transaction_transition_matches_authenticated_replay",
    });
  });

  it("replays real four-neighbor MPF branch proofs from a multi-leaf ledger", async () => {
    const fixture = await buildL2ReplayFixture({
      matchingCommittedRoot: true,
      withBranchProof: true,
    });
    const spent = fixture.evidence.spentUtxos[0]!;
    const produced = fixture.evidence.producedUtxos[0]!;
    const proofSteps = [
      ...spent.membership_proof,
      ...spent.delete_proof,
      ...produced.non_membership_proof,
      ...produced.insert_proof,
    ];
    expect(
      proofSteps.some(
        (step) =>
          "Branch" in step &&
          Buffer.from(step.Branch.neighbors, "hex").length === 4 * 32,
      ),
    ).toBe(true);

    const detections = await detectTransitionTraceFaults(
      fixture.reconstruction,
      { l2TransactionTransitions: [fixture.evidence] },
    );
    expect(
      detections.filter(
        ({ invariant }) =>
          invariant ===
          "l2_transaction_transition_matches_authenticated_replay",
      ),
    ).toEqual([]);
  });

  // These output shapes survive the authenticated outputs preimage byte for
  // byte — the Aiken tag-4 encoder neither reorders nor re-canonicalises them,
  // and the field-commitment check below still passes on them. What they do
  // not have is a §5.3 ledger value: the canonical ledger-output decoder
  // refuses a datum that is not canonical Plutus data and a Value whose policy
  // or asset keys are out of order, in both languages alike. With the trie
  // valued by the descriptor rather than the full output bytes, an output with
  // no descriptor is an output `utxos_root` cannot hold, so the replay has
  // nothing to insert and the challenger must fail closed instead of inventing
  // a value. On-chain `apply_l2_outputs` binds the same derivation with
  // `expect`, so the arm aborts on exactly these inputs.
  it.each([
    {
      label: "opaque datum byte ff",
      outputCbor: tag4OutputWithOpaqueDatum(),
    },
    {
      label: "unsorted policy and asset order",
      outputCbor: tag4OutputWithPreservedAssetOrder(),
    },
  ])(
    "refuses to replay $label, which has no canonical ledger value",
    async ({ outputCbor }) => {
      const fixture = await buildL2ReplayFixture({
        matchingCommittedRoot: true,
        outputCbor,
        includeProducedPayloadUtxo: false,
      });

      await expect(
        detectTransitionTraceFaults(fixture.reconstruction, {
          l2TransactionTransitions: [fixture.evidence],
        }),
      ).rejects.toMatchObject({
        code: "missingWitnessData",
        message: expect.stringContaining("no canonical ledger value"),
      });
    },
  );

  // The one shape where the two ledger-output decoders do not yet agree: Aiken
  // `ledger_output_v1.parse_script_ref` treats a native script reference as
  // opaque bytes and builds a descriptor over them, while
  // `decodeMidgardTxOutput` parses the script structurally and rejects bytes
  // that are not a well-formed native script. The divergence predates the trie
  // value moving to the descriptor and is not this arm's to settle; what
  // matters here is the direction. The challenger declines to build a witness
  // it cannot value, which is the safe half — it can only cost a fault proof,
  // never mint one.
  it("refuses to replay opaque native-script bytes the canonical decoder rejects", async () => {
    const fixture = await buildL2ReplayFixture({
      matchingCommittedRoot: true,
      outputCbor: tag4OutputWithOpaqueNativeScript(),
      includeProducedPayloadUtxo: false,
    });

    await expect(
      detectTransitionTraceFaults(fixture.reconstruction, {
        l2TransactionTransitions: [fixture.evidence],
      }),
    ).rejects.toMatchObject({
      code: "missingWitnessData",
      message: expect.stringContaining("no canonical ledger value"),
    });
  });

  it.each([
    {
      label: "non-minimal lovelace",
      outputCbor: tag4OutputWithNonMinimalLovelace(),
      replayedOutputCbor: Buffer.from(LEDGER_OUTPUT_CBOR, "hex"),
    },
    {
      label: "non-minimal asset quantity",
      outputCbor: tag4OutputWithNonMinimalQuantity(),
      replayedOutputCbor: tag4OutputWithPreservedAssetOrder(),
    },
  ])(
    "rejects $label when canonical outputs disagree with the authenticated compact",
    async ({ outputCbor, replayedOutputCbor }) => {
      const fixture = await buildL2ReplayFixture({
        matchingCommittedRoot: false,
        outputCbor,
        replayedOutputCbor,
        includeProducedPayloadUtxo: false,
      });

      await expect(
        detectTransitionTraceFaults(fixture.reconstruction, {
          l2TransactionTransitions: [fixture.evidence],
        }),
      ).rejects.toMatchObject({
        code: "missingWitnessData",
        message: expect.stringContaining("canonical field commitment"),
      });
    },
  );

  it("rejects a malformed tag-4 mutation proof before reporting a fault", async () => {
    const fixture = await buildL2ReplayFixture({
      matchingCommittedRoot: false,
    });
    const spent = fixture.evidence.spentUtxos[0]!;
    const malformed: SDK.LedgerDeleteWitness = {
      ...spent,
      delete_proof: [
        {
          Branch: {
            skip: 0n,
            neighbors: "00",
          },
        },
      ],
    };

    await expect(
      detectTransitionTraceFaults(fixture.reconstruction, {
        l2TransactionTransitions: [
          {
            ...fixture.evidence,
            spentUtxos: [malformed],
          },
        ],
      }),
    ).rejects.toMatchObject({ code: "missingWitnessData" });
  });

  // §5.3 fields 0/1 fix the item at `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a
  // FIXED 38 bytes with a non-minimal 3-byte index head. Every other spelling
  // of the same out-ref — including the minimal-index CBOR that CML's
  // `TransactionInput` emits, and the one-byte `18 XX` form — is a distinct,
  // rejected encoding, and 65,536 is outside the admissible index domain
  // altogether so it has no canonical spelling at all.
  it.each([
    {
      label: "malformed native spend input",
      spendInputCbor: Buffer.from([0]),
    },
    {
      label: "minimal-index native spend input (CML's 36-byte spelling)",
      spendInputCbor: Buffer.concat([
        Buffer.from([0x82, 0x58, 0x20]),
        Buffer.from(h32(88), "hex"),
        Buffer.from([0x00]),
      ]),
    },
    {
      label: "one-byte-index native spend input",
      spendInputCbor: Buffer.concat([
        Buffer.from([0x82, 0x58, 0x20]),
        Buffer.from(h32(88), "hex"),
        Buffer.from([0x18, 0]),
      ]),
    },
    {
      label: "native spend index above the §5.3 uint16 domain",
      spendInputCbor: Buffer.concat([
        Buffer.from([0x82, 0x58, 0x20]),
        Buffer.from(h32(89), "hex"),
        // 65,536 needs a four-byte payload; the fixed form cannot express it.
        Buffer.from([0x1a, 0x00, 0x01, 0x00, 0x00]),
      ]),
    },
  ])("rejects $label before MPF replay", async ({ spendInputCbor }) => {
    const fixture = await buildL2ReplayFixture({
      matchingCommittedRoot: false,
      spendInputCbor,
    });

    await expect(
      detectTransitionTraceFaults(fixture.reconstruction, {
        l2TransactionTransitions: [fixture.evidence],
      }),
    ).rejects.toMatchObject({ code: "missingWitnessData" });
  });

  it.each([
    {
      label: "malformed native output",
      outputCbor: Buffer.from([0]),
    },
    {
      label: "non-canonical native output",
      outputCbor: Buffer.concat([
        Buffer.from([0xa2, 0x18, 0]),
        Buffer.from(LEDGER_OUTPUT_CBOR, "hex").subarray(2),
      ]),
    },
    {
      label: "zero asset quantity",
      outputCbor: tag4OutputWithPreservedAssetOrder(0),
    },
    {
      label: "unsupported script language",
      outputCbor: Buffer.concat([
        Buffer.from([0xa3]),
        TAG4_OUTPUT_REQUIRED_FIELDS,
        Buffer.from([3, 0x82, 1, 0x40]),
      ]),
    },
  ])("rejects $label before MPF replay", async ({ outputCbor }) => {
    const fixture = await buildL2ReplayFixture({
      matchingCommittedRoot: false,
      outputCbor,
      includeProducedPayloadUtxo: false,
    });

    await expect(
      detectTransitionTraceFaults(fixture.reconstruction, {
        l2TransactionTransitions: [fixture.evidence],
      }),
    ).rejects.toMatchObject({ code: "missingWitnessData" });
  });

  it.each([
    {
      label: "missing delete proof",
      mutate: (fixture: L2ReplayFixture) => ({
        ...fixture.evidence,
        spentUtxos: [],
      }),
    },
    {
      label: "extra delete proof",
      mutate: (fixture: L2ReplayFixture) => ({
        ...fixture.evidence,
        spentUtxos: [
          fixture.evidence.spentUtxos[0]!,
          fixture.evidence.spentUtxos[0]!,
        ],
      }),
    },
    {
      label: "missing insert proof",
      mutate: (fixture: L2ReplayFixture) => ({
        ...fixture.evidence,
        producedUtxos: [],
      }),
    },
    {
      label: "extra insert proof",
      mutate: (fixture: L2ReplayFixture) => ({
        ...fixture.evidence,
        producedUtxos: [
          fixture.evidence.producedUtxos[0]!,
          fixture.evidence.producedUtxos[0]!,
        ],
      }),
    },
  ])("rejects $label before reporting a tag-4 fault", async ({ mutate }) => {
    const fixture = await buildL2ReplayFixture({
      matchingCommittedRoot: false,
    });

    await expect(
      detectTransitionTraceFaults(fixture.reconstruction, {
        l2TransactionTransitions: [mutate(fixture)],
      }),
    ).rejects.toMatchObject({ code: "missingWitnessData" });
  });

  it.each([
    {
      label: "wrong delete key",
      mutate: (fixture: L2ReplayFixture) => ({
        ...fixture.evidence,
        spentUtxos: [
          {
            ...fixture.evidence.spentUtxos[0]!,
            key: spendInputItem(h32(84), 0).toString("hex"),
          },
        ],
      }),
    },
    {
      label: "wrong delete value",
      mutate: (fixture: L2ReplayFixture) => ({
        ...fixture.evidence,
        spentUtxos: [
          {
            ...fixture.evidence.spentUtxos[0]!,
            value: h32(85),
          },
        ],
      }),
    },
    {
      label: "wrong insert key",
      mutate: (fixture: L2ReplayFixture) => ({
        ...fixture.evidence,
        producedUtxos: [
          {
            ...fixture.evidence.producedUtxos[0]!,
            key: spendInputItem(h32(86), 0).toString("hex"),
          },
        ],
      }),
    },
    {
      label: "wrong insert value",
      mutate: (fixture: L2ReplayFixture) => ({
        ...fixture.evidence,
        producedUtxos: [
          {
            ...fixture.evidence.producedUtxos[0]!,
            value: h32(87),
          },
        ],
      }),
    },
  ])("rejects $label before reporting a tag-4 fault", async ({ mutate }) => {
    const fixture = await buildL2ReplayFixture({
      matchingCommittedRoot: false,
    });

    await expect(
      detectTransitionTraceFaults(fixture.reconstruction, {
        l2TransactionTransitions: [mutate(fixture)],
      }),
    ).rejects.toMatchObject({ code: "missingWitnessData" });
  });

  it("detects a wrong final root caused by an invalid forced no-op step", async () => {
    const txOrderId = outRef(5);
    const finalUtxo = rawLedgerEntry(5);
    const finalRoot = await utxoRootWithDescriptors([finalUtxo]);
    const fixture = await buildPayloadFixture({
      utxos: [finalUtxo],
      forcedTransactions: [
        encodedEntry({
          key: txOrderId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(50, forcedTxInvalidPlutus),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: forcedEventKey(txOrderId),
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: finalRoot.root,
        },
      ],
      eventToStep: [
        eventToStepEntry(forcedEventKey(txOrderId), {
          step_index: 0n,
          phase: "ForcedTransaction",
        }),
      ],
    });

    const detections = await detectTransitionTraceFaults(
      await reconstruct(fixture),
    );

    expect(detections).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          buildable: true,
          kind: "invalidOneStepTransition",
          invariant: "invalid_forced_transaction_is_no_op",
        }),
      ]),
    );
  });

  it("detects trace start, link, and final-root faults", async () => {
    const firstDepositId = outRef(6);
    const secondDepositId = outRef(7);
    const firstKey = depositEventKey(firstDepositId);
    const secondKey = depositEventKey(secondDepositId);
    const fixture = await buildPayloadFixture({
      prevUtxosRoot: h32(60),
      deposits: [
        encodedEntry({
          key: firstDepositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(61),
          valueSchema: SDK.DepositInfoSchema,
        }),
        encodedEntry({
          key: secondDepositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(62),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: firstKey,
          phase: "Deposit",
          pre_utxos_root: h32(63),
          post_utxos_root: h32(64),
        },
        {
          schema_version: 1n,
          step_index: 1n,
          event_key: secondKey,
          phase: "Deposit",
          pre_utxos_root: h32(65),
          post_utxos_root: h32(66),
        },
      ],
      eventToStep: [
        eventToStepEntry(firstKey, {
          step_index: 0n,
          phase: "Deposit",
        }),
        eventToStepEntry(secondKey, {
          step_index: 1n,
          phase: "Deposit",
        }),
      ],
    });

    const detections = await detectTransitionTraceFaults(
      await reconstruct(fixture),
    );

    expectBuildableDetection(detections, {
      kind: "traceBoundary",
      invariant: "trace_start_prev_utxos_root",
    });
    expectBuildableDetection(detections, {
      kind: "traceLink",
      invariant: "adjacent_trace_roots",
    });
    expectBuildableDetection(detections, {
      kind: "traceBoundary",
      invariant: "trace_end_utxos_root",
    });
  });

  it("detects header and committed-root count faults", async () => {
    const reconstruction = await reconstruct(await buildPayloadFixture({}));

    const totalMismatch = await detectTransitionTraceFaults({
      ...reconstruction,
      header: {
        ...reconstruction.header,
        totalEventCount: 1n,
      },
    });
    expectBuildableDetection(totalMismatch, {
      kind: "countFault",
      invariant: "header_total_event_count",
    });

    const stepCountMismatch = await detectTransitionTraceFaults({
      ...reconstruction,
      header: {
        ...reconstruction.header,
        transitionStepCount: 1n,
      },
    });
    expectBuildableDetection(stepCountMismatch, {
      kind: "countFault",
      invariant: "header_transition_step_count",
    });
    expectBuildableDetection(stepCountMismatch, {
      kind: "countFault",
      invariant: "transition_trace_root_count",
    });

    const committedRootMismatch = await detectTransitionTraceFaults({
      ...reconstruction,
      header: {
        ...reconstruction.header,
        depositCount: 1n,
        totalEventCount: 1n,
        transitionStepCount: 1n,
      },
    });
    expectBuildableDetection(committedRootMismatch, {
      kind: "countFault",
      invariant: "deposits_root_count",
    });
    expectBuildableDetection(committedRootMismatch, {
      kind: "countFault",
      invariant: "event_to_step_root_count",
    });
    expectBuildableDetection(committedRootMismatch, {
      kind: "countFault",
      invariant: "transition_trace_root_count",
    });
  });

  it("detects dangling trace, source, and event-to-step mappings", async () => {
    const txOrderId = outRef(8);
    const eventKey = forcedEventKey(txOrderId);
    const baseFixture = await buildPayloadFixture({
      forcedTransactions: [
        encodedEntry({
          key: txOrderId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(70, forcedTxInvalidPlutus),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(eventKey, {
          step_index: 0n,
          phase: "ForcedTransaction",
        }),
      ],
    });
    const base = await reconstruct(baseFixture);
    const empty = await reconstruct(await buildPayloadFixture({}));

    const mappedMissingSource = await detectTransitionTraceFaults({
      ...base,
      forcedTransactions: [],
      sourceEvents: [],
      sourceEventsByFingerprint: new Map(),
      rootData: {
        ...base.rootData,
        forcedTransactions: empty.rootData.forcedTransactions,
      },
    });
    expectBuildableDetection(mappedMissingSource, {
      kind: "sourceMembershipMismatch",
      invariant: "mapped_event_has_source_member",
    });

    const sourceMissingTraceMapping = await detectTransitionTraceFaults({
      ...base,
      eventToStep: [],
      eventToStepByFingerprint: new Map(),
      rootData: {
        ...base.rootData,
        eventToStep: empty.rootData.eventToStep,
      },
    });
    expectBuildableDetection(sourceMissingTraceMapping, {
      kind: "eventToStepMismatch",
      invariant: "event_to_step_matches_trace",
    });
    expectBuildableDetection(sourceMissingTraceMapping, {
      kind: "sourceMembershipMismatch",
      invariant: "source_event_has_event_to_step_member",
    });
  });

  it("detects omitted L1 events, out-of-window source events, and duplicate trace events", async () => {
    const depositId = outRef(6);
    const txOrderId = outRef(7);
    const duplicateKey = forcedEventKey(txOrderId);
    const duplicateFixture = await buildPayloadFixture({
      forcedTransactions: [
        encodedEntry({
          key: txOrderId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(60, forcedTxInvalidPlutus),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      deposits: [
        encodedEntry({
          key: depositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(61),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: duplicateKey,
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        {
          schema_version: 1n,
          step_index: 1n,
          event_key: duplicateKey,
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(duplicateKey, {
          step_index: 0n,
          phase: "ForcedTransaction",
        }),
        eventToStepEntry(depositEventKey(depositId), {
          step_index: 1n,
          phase: "Deposit",
        }),
      ],
    });
    const duplicateDetections = await detectTransitionTraceFaults(
      await reconstruct(duplicateFixture),
    );
    expect(duplicateDetections).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          buildable: true,
          kind: "duplicateTraceEvent",
          invariant: "trace_event_key_unique",
        }),
      ]),
    );

    const omittedFixture = await buildPayloadFixture({});
    const omittedDetections = await detectTransitionTraceFaults(
      await reconstruct(omittedFixture),
      {
        omittedDueL1Events: [
          {
            kind: "deposit",
            depositId: outRef(8),
            eventRefInputIndex: 0n,
            eventAssetName: "dd",
          },
          {
            kind: "withdrawal",
            withdrawalId: outRef(9),
            eventRefInputIndex: 1n,
            eventAssetName: "ee",
          },
          {
            kind: "forcedTransaction",
            txOrderId: outRef(10),
            eventRefInputIndex: 2n,
            eventAssetName: "cc",
            validityOverride: forcedTxInvalidPlutus,
          },
        ],
      },
    );
    expect(omittedDetections).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          buildable: true,
          kind: "omittedDueL1Event",
          invariant: "due_l1_event_is_in_source_root",
        }),
      ]),
    );
    expect(
      omittedDetections.filter(
        (detection) =>
          detection.buildable &&
          detection.kind === "omittedDueL1Event" &&
          detection.invariant === "due_l1_event_is_in_source_root",
      ),
    ).toHaveLength(3);

    const withdrawalId = outRef(11);
    const outOfWindowForcedId = outRef(12);
    const outOfWindowDepositKey = depositEventKey(depositId);
    const outOfWindowWithdrawalKey = withdrawalEventKey(withdrawalId);
    const outOfWindowForcedKey = forcedEventKey(outOfWindowForcedId);
    const outOfWindowFixture = await buildPayloadFixture({
      deposits: [
        encodedEntry({
          key: depositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(71),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      withdrawals: [
        encodedEntry({
          key: withdrawalId,
          keySchema: SDK.OutputReference as never,
          value: withdrawalInfo(72),
          valueSchema: SDK.WithdrawalInfoSchema,
        }),
      ],
      forcedTransactions: [
        encodedEntry({
          key: outOfWindowForcedId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(73, forcedTxInvalidPlutus),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: outOfWindowWithdrawalKey,
          phase: "Withdrawal",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        {
          schema_version: 1n,
          step_index: 1n,
          event_key: outOfWindowForcedKey,
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        {
          schema_version: 1n,
          step_index: 2n,
          event_key: outOfWindowDepositKey,
          phase: "Deposit",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(outOfWindowWithdrawalKey, {
          step_index: 0n,
          phase: "Withdrawal",
        }),
        eventToStepEntry(outOfWindowForcedKey, {
          step_index: 1n,
          phase: "ForcedTransaction",
        }),
        eventToStepEntry(outOfWindowDepositKey, {
          step_index: 2n,
          phase: "Deposit",
        }),
      ],
    });
    const outOfWindowDetections = await detectTransitionTraceFaults(
      await reconstruct(outOfWindowFixture),
      {
        outOfWindowSourceEvents: [
          {
            kind: "deposit",
            depositId,
            eventRefInputIndex: 0n,
            eventAssetName: "aa",
          },
          {
            kind: "withdrawal",
            withdrawalId,
            eventRefInputIndex: 1n,
            eventAssetName: "bb",
            validityOverride: "IncorrectWithdrawalSignature",
          },
          {
            kind: "forcedTransaction",
            txOrderId: outOfWindowForcedId,
            eventRefInputIndex: 2n,
            eventAssetName: "cc",
            validityOverride: forcedTxInvalidPlutus,
          },
        ],
      },
    );
    expect(
      outOfWindowDetections.filter(
        (detection) =>
          detection.buildable &&
          detection.kind === "outOfWindowSourceEvent" &&
          detection.invariant === "source_event_is_within_block_window",
      ),
    ).toHaveLength(3);
  });

  it("builds L2 source membership-mismatch witnesses against raw transaction roots", async () => {
    const material = nativeMaterial(12);
    const txId = material.txId;
    const source: SDK.L2TransactionSourceV1 = {
      tx_id: txId,
      source: material.source,
    };
    const eventKey: SDK.EventKey = { L2TransactionEventKey: { tx_id: txId } };
    const phaseMismatchFixture = await buildPayloadFixture({
      transactions: [
        entry(
          Buffer.from(txId, "hex"),
          Buffer.from(Data.to(source, SDK.L2TransactionSourceV1), "hex"),
        ),
      ],
      transactionPreimages: [
        entry(Buffer.from(txId, "hex"), material.canonicalCbor),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase: "Deposit",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(eventKey, {
          step_index: 0n,
          phase: "Deposit",
        }),
      ],
    });
    const phaseMismatchDetections = await detectTransitionTraceFaults(
      await reconstruct(phaseMismatchFixture),
    );

    expect(phaseMismatchDetections).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          buildable: true,
          kind: "sourceMembershipMismatch",
          invariant: "source_phase_matches_trace_phase",
        }),
      ]),
    );

    const emptyReconstruction = await reconstruct(
      await buildPayloadFixture({}),
    );
    await expect(
      buildSourceNonMembershipProof({
        reconstruction: emptyReconstruction,
        eventKey,
      }),
    ).resolves.toMatchObject({
      L2TransactionSourceNonMembership: {
        non_membership: {
          key: txId,
          domain: SDK.ROOT_DOMAINS.transactionsV1,
        },
      },
    });
  });

  it("builds both normal/forced classification fault directions", async () => {
    const forcedId = outRef(13);
    const forcedKey = forcedEventKey(forcedId);
    const forcedFixture = await buildPayloadFixture({
      forcedTransactions: [
        encodedEntry({
          key: forcedId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(13, "ForcedTxValid"),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: forcedKey,
          phase: "L2Transaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(forcedKey, {
          step_index: 0n,
          phase: "L2Transaction",
        }),
      ],
    });
    const material = nativeMaterial(14);
    const normalSource: SDK.L2TransactionSourceV1 = {
      tx_id: material.txId,
      source: material.source,
    };
    const normalKey: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: material.txId },
    };
    const normalFixture = await buildPayloadFixture({
      transactions: [
        entry(
          Buffer.from(material.txId, "hex"),
          Buffer.from(Data.to(normalSource, SDK.L2TransactionSourceV1), "hex"),
        ),
      ],
      transactionPreimages: [
        entry(Buffer.from(material.txId, "hex"), material.canonicalCbor),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: normalKey,
          phase: "ForcedTransaction",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(normalKey, {
          step_index: 0n,
          phase: "ForcedTransaction",
        }),
      ],
    });

    for (const fixture of [forcedFixture, normalFixture]) {
      const reconstruction = await reconstruct(fixture);
      const detections = await detectTransitionTraceFaults(reconstruction);
      expectBuildableDetection(detections, {
        kind: "sourceMembershipMismatch",
        invariant: "source_phase_matches_trace_phase",
      });
      const fault = await buildSourcePhaseMismatchFault({
        reconstruction,
        stepIndex: 0n,
      });
      expect(() =>
        Data.from(
          Data.to(fault as never, SDK.TransitionFault as never),
          SDK.TransitionFault as never,
        ),
      ).not.toThrow();
    }
  });

  it("fetches retained DA payloads and proof material over libp2p protocols", async () => {
    const fixture = await buildPayloadFixture({});
    const deploymentFingerprint = "11".repeat(32);
    const headerHashBytes = Buffer.from(fixture.headerHash, "hex");
    const payloadHash = computeDaSha256Hash(fixture.payloadEnvelopeCbor);
    const proofBundleBytes = Buffer.from("retained proof bundle");
    const proofBundleHash = computeDaSha256Hash(proofBundleBytes);
    const transitionStepBytes = Buffer.from("transition step");
    const traceMembershipProofBytes = Buffer.from("trace membership proof");
    const eventKeyBytes = Buffer.from("aabbcc", "hex");
    const eventToStepEntryBytes = Buffer.from("event to step entry");
    const eventProofBytes = Buffer.from("event proof");
    const calls: Array<{
      readonly peerId: string;
      readonly protocol: DaRequestResponseProtocol;
    }> = [];

    const transport: RetainedDaLibp2pTransport = {
      request: async ({ peer, protocol, payload }) => {
        calls.push({ peerId: peer.peerId, protocol });
        switch (protocol) {
          case DaRequestResponseProtocol.payloadByHeader: {
            const request = decodeDaPayloadByHeaderRequestV1Cbor(payload);
            expect(request.headerHash.equals(headerHashBytes)).toBe(true);
            return encodeDaPayloadByHeaderResponseV1Cbor({
              status: "found_inline",
              headerHash: request.headerHash,
              payloadHash,
              payloadBytes: fixture.payloadEnvelopeCbor,
              chunkManifest: null,
              reasonCode: null,
            });
          }
          case DaRequestResponseProtocol.metadataByHeader: {
            const request = decodeDaPayloadByHeaderRequestV1Cbor(payload);
            expect(request.headerHash.equals(headerHashBytes)).toBe(true);
            return encodeDaMetadataByHeaderResponseV1Cbor({
              status: "found",
              headerHash: request.headerHash,
              payloadHash,
              payloadSchemaVersion: 1,
              payloadBytes: fixture.payloadEnvelopeCbor.length,
              rootSummaryHash: computeDaSha256Hash(Buffer.from("root summary")),
              proofBundleHash,
              transitionTraceRoot: Buffer.from(
                fixture.header.transitionTraceRoot,
                "hex",
              ),
              eventToStepRoot: Buffer.from(
                fixture.header.eventToStepRoot,
                "hex",
              ),
              retainedUntilSlot: 123,
              localStatus: "verified",
            });
          }
          case DaRequestResponseProtocol.proofBundleByHeader: {
            const request = decodeDaProofBundleByHeaderRequestV1Cbor(payload);
            expect(request.headerHash.equals(headerHashBytes)).toBe(true);
            return encodeDaProofBundleByHeaderResponseV1Cbor({
              status: "found_inline",
              headerHash: request.headerHash,
              proofBundleHash,
              proofBundleBytes,
              chunkManifest: null,
              reasonCode: null,
            });
          }
          case DaRequestResponseProtocol.traceStepByIndex: {
            const request = decodeDaTraceStepByIndexRequestV1Cbor(payload);
            expect(request.headerHash.equals(headerHashBytes)).toBe(true);
            expect(request.stepIndex).toBe(0);
            return encodeDaTraceStepByIndexResponseV1Cbor({
              status: "found",
              headerHash: request.headerHash,
              stepIndex: request.stepIndex,
              transitionStepBytes,
              membershipProofBytes: traceMembershipProofBytes,
            });
          }
          case DaRequestResponseProtocol.eventToStepByEvent: {
            const request = decodeDaEventToStepByEventRequestV1Cbor(payload);
            expect(request.headerHash.equals(headerHashBytes)).toBe(true);
            expect(request.eventKey.equals(eventKeyBytes)).toBe(true);
            return encodeDaEventToStepByEventResponseV1Cbor({
              status: "found",
              headerHash: request.headerHash,
              eventKey: request.eventKey,
              eventToStepEntryBytes,
              membershipOrNonmembershipProofBytes: eventProofBytes,
            });
          }
          default:
            throw new Error(`unexpected protocol ${protocol}`);
        }
      },
    };
    const source = new DaLibp2pRetainedDaSource({
      sourceId: "committee-libp2p",
      deploymentFingerprint,
      peers: [{ peerId: "peer-a" }],
      transport,
    });

    const result = await fetchRetainedDaPayloadByHeaderHash({
      headerHash: fixture.headerHash,
      sources: [source],
      retries: 0,
    });

    expect(result.sourceId).toBe("committee-libp2p");
    expect(result.sourcePeerId).toBe("peer-a");
    expect(result.payloadEnvelopeCbor.equals(fixture.payloadEnvelopeCbor)).toBe(
      true,
    );
    expect(result.metadata).toMatchObject({
      status: "found",
      payloadBytes: fixture.payloadEnvelopeCbor.length,
      localStatus: "verified",
    });

    const proofBundle = await source.fetchProofBundleByHeaderHash(
      fixture.headerHash,
    );
    if (!proofBundle.ok) {
      throw new Error("expected proof bundle response");
    }
    expect(proofBundle.proofBundleHash.equals(proofBundleHash)).toBe(true);
    expect(proofBundle.proofBundleBytes.equals(proofBundleBytes)).toBe(true);

    const traceStep = await source.fetchTraceStepByIndex({
      headerHash: fixture.headerHash,
      stepIndex: 0,
    });
    if (!traceStep.ok) {
      throw new Error("expected trace step response");
    }
    expect(traceStep.transitionStepBytes.equals(transitionStepBytes)).toBe(
      true,
    );
    expect(
      traceStep.membershipProofBytes.equals(traceMembershipProofBytes),
    ).toBe(true);

    const eventToStep = await source.fetchEventToStepByEvent({
      headerHash: fixture.headerHash,
      eventKey: eventKeyBytes,
    });
    if (!eventToStep.ok) {
      throw new Error("expected event-to-step response");
    }
    if (
      eventToStep.eventToStepEntryBytes === null ||
      eventToStep.membershipOrNonmembershipProofBytes === null
    ) {
      throw new Error("expected event-to-step proof bytes");
    }
    expect(
      eventToStep.eventToStepEntryBytes.equals(eventToStepEntryBytes),
    ).toBe(true);
    expect(
      eventToStep.membershipOrNonmembershipProofBytes.equals(eventProofBytes),
    ).toBe(true);

    expect(calls).toEqual([
      {
        peerId: "peer-a",
        protocol: DaRequestResponseProtocol.payloadByHeader,
      },
      {
        peerId: "peer-a",
        protocol: DaRequestResponseProtocol.metadataByHeader,
      },
      {
        peerId: "peer-a",
        protocol: DaRequestResponseProtocol.proofBundleByHeader,
      },
      {
        peerId: "peer-a",
        protocol: DaRequestResponseProtocol.traceStepByIndex,
      },
      {
        peerId: "peer-a",
        protocol: DaRequestResponseProtocol.eventToStepByEvent,
      },
    ]);
  });

  it("fails closed when libp2p sources do not retain the requested payload", async () => {
    const fixture = await buildPayloadFixture({});
    const source = {
      sourceId: "empty-libp2p",
      fetchPayloadByHeaderHash: async () => ({
        ok: false as const,
        sourceId: "empty-libp2p",
        attempts: [
          {
            sourceId: "empty-libp2p",
            sourcePeerId: "peer-a",
            protocol: DaRequestResponseProtocol.payloadByHeader,
            status: "not_found" as const,
            detail: "payload not found",
          },
        ],
      }),
    };

    await expect(
      fetchRetainedDaPayloadByHeaderHash({
        headerHash: fixture.headerHash,
        sources: [source],
        retries: 0,
      }),
    ).rejects.toMatchObject({ code: "fetchFailed" });
  });
});
