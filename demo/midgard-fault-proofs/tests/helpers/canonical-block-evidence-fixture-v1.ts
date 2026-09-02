/**
 * Deterministic canonical-block fixture for the `Q03` evidence-source tests.
 *
 * By default it builds a `DaPayloadV1` exactly the way the production node
 * does: the header's `transactions_root` is the counted root over
 * `(tx_id -> Data(L2TransactionSourceV1))` leaves, which is what
 * `encodeTransactionRootValue` (demo/midgard-node/src/workers/utils/mpf.ts)
 * commits and what `reconstructDaPayloadV1` re-derives.  The native-compact
 * root mode is a test-only fixture for exercising the proof builder against
 * the on-chain inclusion convention; its root is still derived from the
 * actual compact CBOR leaves produced for each transaction.
 */
import {
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core/codec";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterialV1 } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { encodeData } from "../../src/transition-trace/reconstruct.js";

export const h32 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);
export const h28 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(28);

export const outRefCbor = (txIdByte: number, index: bigint): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.from(h32(txIdByte), "hex"),
    outputIndex: Number(index),
  });

export type FixtureTransactionInputV1 = {
  readonly spendInputs: readonly Buffer[];
  readonly referenceInputs?: readonly Buffer[];
  readonly outputs?: readonly Buffer[];
  readonly requiredObservers?: readonly Buffer[];
  readonly mintPolicyItems?: readonly Buffer[];
  readonly fee: bigint;
  readonly networkId?: bigint;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
  readonly addressWitnesses?: readonly SDK.MidgardAddressWitness[];
};

export type FixtureTransactionV1 = {
  readonly txId: string;
  readonly canonicalCbor: Buffer;
  readonly compactCbor: Buffer;
  readonly source: SDK.L2TransactionSourceV1;
  readonly sourceValueBytes: Buffer;
};

export const buildFixtureTransactionV1 = ({
  spendInputs,
  referenceInputs = [],
  outputs = [],
  requiredObservers = [],
  mintPolicyItems = [],
  fee,
  networkId = MIDGARD_NATIVE_NETWORK_ID_NONE,
  validityIntervalStart = MIDGARD_POSIX_TIME_NONE,
  validityIntervalEnd = MIDGARD_POSIX_TIME_NONE,
  addressWitnesses = [],
}: FixtureTransactionInputV1): FixtureTransactionV1 => {
  const canonical: MidgardNativeTxCanonicalV1 = {
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([...spendInputs]),
      referenceInputsPreimageCbor: encodeCbor([...referenceInputs]),
      outputsPreimageCbor: encodeCbor([...outputs]),
      fee,
      validityIntervalStart,
      validityIntervalEnd,
      requiredObserversPreimageCbor: encodeCbor([...requiredObservers]),
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: encodeCbor([...mintPolicyItems]),
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: encodeCbor(
        addressWitnesses.map((witness) =>
          SDK.encodeMidgardAddressWitnessCanonicalV1(witness),
        ),
      ),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  };
  const full = materializeMidgardNativeTxFromCanonicalV1(canonical);
  const canonicalCbor = encodeMidgardNativeTxCanonicalV1(full);
  const proofSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(canonicalCbor);
  const source: SDK.L2TransactionSourceV1 = {
    tx_id: computeMidgardNativeTxIdV1(full).toString("hex"),
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return {
    txId: source.tx_id,
    canonicalCbor,
    compactCbor: Buffer.from(proofSource.compactCbor),
    source,
    sourceValueBytes: encodeData(source, SDK.L2TransactionSourceV1Schema),
  };
};

const sortEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

export type CanonicalBlockFixtureV1 = {
  readonly payload: SDK.DaPayloadV1;
  readonly payloadEnvelopeCbor: Buffer;
  readonly header: SDK.HeaderV1;
  readonly headerHash: string;
  readonly transactionsRootMode: CanonicalTransactionsRootModeV1;
  readonly payloadSourceTransactionsRoot: string;
  readonly nativeCompactTransactionsRoot: string;
  readonly transactions: readonly FixtureTransactionV1[];
};

export type CanonicalTransactionsRootModeV1 = "payloadSource" | "nativeCompact";

export const buildCanonicalBlockFixtureV1 = async ({
  transactions,
  utxos = [],
  startTime = 10n,
  endTime = 20n,
  minFeeA = 0n,
  minFeeB = 0n,
  transactionsRootMode = "payloadSource",
  prevHeaderHash = h28(90),
  prevUtxosRoot = SDK.EMPTY_MERKLE_TREE_ROOT,
}: {
  readonly transactions: readonly FixtureTransactionV1[];
  readonly utxos?: readonly Readonly<{
    key: Uint8Array;
    value: Uint8Array;
  }>[];
  readonly startTime?: bigint;
  readonly endTime?: bigint;
  readonly minFeeA?: bigint;
  readonly minFeeB?: bigint;
  readonly transactionsRootMode?: CanonicalTransactionsRootModeV1;
  readonly prevHeaderHash?: string;
  readonly prevUtxosRoot?: string;
}): Promise<CanonicalBlockFixtureV1> => {
  const transactionEntries: SDK.DaPayloadEntry[] = transactions.map((tx) => [
    tx.txId,
    tx.sourceValueBytes.toString("hex"),
  ]);
  const nativeCompactTransactionEntries: SDK.DaPayloadEntry[] =
    transactions.map((tx) => [tx.txId, tx.compactCbor.toString("hex")]);
  const preimageEntries: SDK.DaPayloadEntry[] = transactions.map((tx) => [
    tx.txId,
    tx.canonicalCbor.toString("hex"),
  ]);
  const eventToStepEntries: SDK.DaPayloadEntry[] = transactions.map(
    (tx, index) => [
      encodeData(
        { L2TransactionEventKey: { tx_id: tx.txId } } satisfies SDK.EventKey,
        SDK.EventKeySchema,
      ).toString("hex"),
      encodeData(
        {
          step_index: BigInt(index),
          phase: "L2Transaction",
        } satisfies SDK.EventToStepValue,
        SDK.EventToStepValueSchema,
      ).toString("hex"),
    ],
  );
  const validationTraceEntries: SDK.DaPayloadEntry[] = transactions.map(
    (tx, index) => [
      encodeData(
        { L2TransactionEventKey: { tx_id: tx.txId } } satisfies SDK.EventKey,
        SDK.EventKeySchema,
      ).toString("hex"),
      encodeData(
        {
          schema_version: 1n,
          machine_version: 1n,
          trace_root: h32(140 + index),
          step_count: 1n,
          initial_state_hash: h32(150 + index),
          terminal_state_hash: h32(160 + index),
          verdict: "Accepted",
          rejection_code_hash: h32(170 + index),
        } satisfies SDK.ValidationTraceDescriptorV1,
        SDK.ValidationTraceDescriptorV1Schema,
      ).toString("hex"),
    ],
  );

  const bufferEntries = (
    entries: readonly SDK.DaPayloadEntry[],
  ): readonly { readonly key: Buffer; readonly value: Buffer }[] =>
    entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    }));

  const payloadSourceTransactionsRoot = await buildCountedRoot(
    SDK.ROOT_DOMAINS.transactionsV1,
    bufferEntries(transactionEntries),
  );
  const nativeCompactTransactionsRoot = await buildCountedRoot(
    SDK.ROOT_DOMAINS.transactionsV1,
    bufferEntries(nativeCompactTransactionEntries),
  );
  const transactionsRoot =
    transactionsRootMode === "nativeCompact"
      ? nativeCompactTransactionsRoot
      : payloadSourceTransactionsRoot;
  const emptyRoot = async (domain: SDK.RootDomain) =>
    await buildCountedRoot(domain, []);
  const withdrawalsRoot = await emptyRoot(SDK.ROOT_DOMAINS.withdrawals);
  const forcedTransactionsRoot = await emptyRoot(
    SDK.ROOT_DOMAINS.forcedTransactionsV1,
  );
  const depositsRoot = await emptyRoot(SDK.ROOT_DOMAINS.deposits);
  const transitionTraceRoot = await emptyRoot(SDK.ROOT_DOMAINS.transitionTrace);
  const eventToStepRoot = await buildCountedRoot(
    SDK.ROOT_DOMAINS.eventToStep,
    bufferEntries(eventToStepEntries),
  );
  const validationTracesRoot = await buildCountedRoot(
    SDK.ROOT_DOMAINS.validationTraces,
    bufferEntries(validationTraceEntries),
  );
  const rawUtxoEntries: SDK.DaPayloadEntry[] = utxos.map(({ key, value }) => [
    Buffer.from(key).toString("hex"),
    Buffer.from(value).toString("hex"),
  ]);
  const utxosRoot = await keyValuePhasRootWithCount(
    utxos.map(({ key, value }) => ({
      key: Buffer.from(key),
      value: buildCanonicalMidgardLedgerEntryOutputMaterialV1({
        outRef: key,
        outputCbor: value,
      }).descriptorCbor,
    })),
  );

  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: BigInt(transactions.length),
    depositCount: 0n,
    totalEventCount: BigInt(transactions.length),
    transitionStepCount: 0n,
    validationTraceCount: BigInt(transactions.length),
  };

  const header: SDK.HeaderV1 = {
    prevUtxosRoot,
    utxosRoot: utxosRoot.root,
    withdrawalsRoot: withdrawalsRoot.root,
    forcedTransactionsRoot: forcedTransactionsRoot.root,
    transactionsRoot: transactionsRoot.root,
    depositsRoot: depositsRoot.root,
    transitionTraceRoot: transitionTraceRoot.root,
    eventToStepRoot: eventToStepRoot.root,
    validationTracesRoot: validationTracesRoot.root,
    ...counts,
    startTime,
    endTime,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA,
    minFeeB,
    prevHeaderHash,
    operatorVkey: h28(91),
    protocolVersion: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  const payload: SDK.DaPayloadV1 = {
    version: SDK.DA_PAYLOAD_V1_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: sortEntries(rawUtxoEntries),
      withdrawals: [],
      forced_transactions: [],
      transactions: sortEntries(transactionEntries),
      deposits: [],
      transition_trace: [],
      event_to_step: sortEntries(eventToStepEntries),
      transaction_preimages: sortEntries(preimageEntries),
      forced_transaction_preimages: [],
      cek_program_material: [],
      validation_traces: sortEntries(validationTraceEntries),
      validation_trace_witnesses: [],
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
    transactionsRootMode,
    payloadSourceTransactionsRoot: payloadSourceTransactionsRoot.root,
    nativeCompactTransactionsRoot: nativeCompactTransactionsRoot.root,
    transactions,
  };
};

export const authenticatedHeaderObservationV1 = (
  fixture: CanonicalBlockFixtureV1,
  overrides: Partial<SDK.AuthenticatedStateQueueHeaderObservationV1> = {},
): SDK.AuthenticatedStateQueueHeaderObservationV1 => ({
  schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
  sourceMode: "local_node",
  provenance: {
    trustClass: "authenticated_cardano_l1",
    sourceId: "watcher-local-node",
    grade: "security",
  },
  chainPoint: { slot: 4242n, blockHash: h32(7) },
  confirmationDepth: 30,
  headerHash: fixture.headerHash,
  header: fixture.header,
  ...overrides,
});

/** Re-encodes the fixture payload after an arbitrary block-body mutation. */
export const reencodeFixturePayloadV1 = async (
  payload: SDK.DaPayloadV1,
): Promise<Buffer> =>
  await wrapDaPayloadV1(SDK.encodeDaPayloadV1(payload), { mode: "identity" });

export const dataToHex = <A>(
  value: A,
  schema: Parameters<typeof Data.Nullable>[0],
): string => encodeData(value, schema).toString("hex");
