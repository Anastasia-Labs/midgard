/**
 * Deterministic canonical-block fixture for the `Q03` evidence-source tests.
 *
 * By default it builds a `DaPayload` exactly the way the production node
 * does: the header's `transactions_root` is the counted root over
 * `(tx_id -> Data(L2TransactionSource))` leaves, which is what
 * `encodeTransactionRootValue` (demo/midgard-node/src/mpf/ledger-hydration.ts)
 * commits and what `reconstructDaPayloadV1` re-derives.  The native-compact
 * root mode is a test-only fixture for exercising the proof builder against
 * the on-chain inclusion convention; its root is still derived from the
 * actual compact CBOR leaves produced for each transaction.
 */
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { encodeData } from "../../src/transition-trace/reconstruct.js";

export { h28, h32 };

export const outRefCbor = (txIdByte: number, index: bigint): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(h32(txIdByte), "hex"),
    outputIndex: Number(index),
  });

export type FixtureTransactionInput = {
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

export type FixtureTransaction = {
  readonly txId: string;
  readonly canonicalCbor: Buffer;
  readonly compactCbor: Buffer;
  readonly source: SDK.L2TransactionSource;
  readonly sourceValueBytes: Buffer;
};

export const buildFixtureTransaction = ({
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
}: FixtureTransactionInput): FixtureTransaction => {
  const canonical: MidgardNativeTxCanonical = {
    version: MIDGARD_NATIVE_TX_VERSION,
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
          SDK.encodeMidgardAddressWitnessCanonical(witness),
        ),
      ),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  };
  const full = materializeMidgardNativeTxFromCanonical(canonical);
  const canonicalCbor = encodeMidgardNativeTxCanonical(full);
  const proofSource =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
  const source: SDK.L2TransactionSource = {
    tx_id: computeMidgardNativeTxId(full).toString("hex"),
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
    sourceValueBytes: encodeData(source, SDK.L2TransactionSourceSchema),
  };
};

const sortEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

export type CanonicalBlockFixture = {
  readonly payload: SDK.DaPayload;
  readonly payloadEnvelopeCbor: Buffer;
  readonly header: SDK.Header;
  readonly headerHash: string;
  readonly transactionsRootMode: CanonicalTransactionsRootMode;
  readonly payloadSourceTransactionsRoot: string;
  readonly nativeCompactTransactionsRoot: string;
  readonly transactions: readonly FixtureTransaction[];
};

export type CanonicalTransactionsRootMode = "payloadSource" | "nativeCompact";

export const buildCanonicalBlockFixture = async ({
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
  readonly transactions: readonly FixtureTransaction[];
  readonly utxos?: readonly Readonly<{
    key: Uint8Array;
    value: Uint8Array;
  }>[];
  readonly startTime?: bigint;
  readonly endTime?: bigint;
  readonly minFeeA?: bigint;
  readonly minFeeB?: bigint;
  readonly transactionsRootMode?: CanonicalTransactionsRootMode;
  readonly prevHeaderHash?: string;
  readonly prevUtxosRoot?: string;
}): Promise<CanonicalBlockFixture> => {
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
        } satisfies SDK.ValidationTraceDescriptor,
        SDK.ValidationTraceDescriptorSchema,
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
      value: buildCanonicalMidgardLedgerEntryOutputMaterial({
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

  const header: SDK.Header = {
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
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
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
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
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

export const authenticatedHeaderObservation = (
  fixture: CanonicalBlockFixture,
  overrides: Partial<SDK.AuthenticatedStateQueueHeaderObservation> = {},
): SDK.AuthenticatedStateQueueHeaderObservation => ({
  schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
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
export const reencodeFixturePayload = async (
  payload: SDK.DaPayload,
): Promise<Buffer> =>
  await wrapDaPayload(SDK.encodeDaPayload(payload), { mode: "identity" });

export const dataToHex = <A>(
  value: A,
  schema: Parameters<typeof Data.Nullable>[0],
): string => encodeData(value, schema).toString("hex");
