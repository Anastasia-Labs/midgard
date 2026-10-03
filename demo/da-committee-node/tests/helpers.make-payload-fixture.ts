import { mkdtemp } from "node:fs/promises";
import { join } from "node:path";

import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_PROTOCOL_VERSION,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
} from "@al-ft/midgard-core/consensus-profile";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { inject } from "vitest";

import { computeDaPayloadRoots } from "../src/da/payload.js";
import type { Header, ObservedStateQueueNode } from "../src/domain.js";
import { hashBlockHeader } from "../src/l1/state-queue-scanner.js";
import type {} from "./global-setup.js";

/** A fixture directory under the run's temporary root (see global-setup). */
export const tempDir = (): Promise<string> =>
  mkdtemp(join(inject("tempRoot"), "case-"));

export const fixtureHeaderBase = (): Omit<
  Header,
  | "utxosRoot"
  | "forcedTransactionsRoot"
  | "transactionsRoot"
  | "depositsRoot"
  | "withdrawalsRoot"
> => ({
  // The first block: its pre-state is the empty UTxO set.
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: SDK.GENESIS_HEADER_HASH,
  operatorVkey: "22".repeat(28),
  protocolVersion: BigInt(MIDGARD_PROTOCOL_VERSION),
});

const canonicalTransaction = (fee: bigint) => {
  const tx = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      fee,
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
  return {
    tx,
    txId: Buffer.from(computeMidgardNativeTxId(tx)).toString("hex"),
    txCbor: Buffer.from(encodeMidgardNativeTxCanonical(tx)).toString("hex"),
  };
};

const transactionSource = (
  transaction: ReturnType<typeof canonicalTransaction>,
): SDK.L2TransactionSource => {
  const source = deriveMidgardNativeTxProofSource(transaction.tx);
  return {
    tx_id: transaction.txId,
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
};

/** The committed descriptor leaf is its canonical Plutus Data encoding. */
const acceptedTraceDescriptor = (seed: number): string =>
  LucidData.to(
    SDK.validationTraceDescriptorDataFromCore({
      schemaVersion: MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
      machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
      traceRoot: Buffer.alloc(32, seed),
      stepCount: 0,
      initialStateHash: Buffer.alloc(32, seed + 1),
      terminalStateHash: Buffer.alloc(32, seed + 1),
      verdict: "accepted",
      rejectionCodeHash: Buffer.alloc(32),
    }) as never,
    SDK.ValidationTraceDescriptorSchema as never,
  );

export const makePayloadFixture = async (
  transactionCount = 3,
  headerOverrides: Partial<
    Pick<Header, "prevHeaderHash" | "prevUtxosRoot">
  > = {},
): Promise<{
  readonly payload: SDK.DaPayload;
  readonly innerPayloadCbor: Buffer;
  readonly payloadCbor: Buffer;
  readonly header: Header;
  readonly headerHash: string;
}> => {
  if (
    !Number.isSafeInteger(transactionCount) ||
    transactionCount < 1 ||
    transactionCount > MIDGARD_CONSENSUS_LIMITS.maxL2TransactionCount
  ) {
    throw new Error(
      `fixture transaction count must be in [1, ${MIDGARD_CONSENSUS_LIMITS.maxL2TransactionCount.toString()}]; got ${transactionCount.toString()}`,
    );
  }
  const transactions = Array.from({ length: transactionCount }, (_, index) =>
    canonicalTransaction(BigInt(index)),
  ).sort((left, right) => left.txId.localeCompare(right.txId));
  if (new Set(transactions.map(({ txId }) => txId)).size !== transactionCount) {
    throw new Error("fixture transaction identities must be distinct");
  }
  const sources = transactions.map(transactionSource);
  const sourceEvents: readonly SDK.EventKey[] = transactions.map(
    ({ txId }) => ({ L2TransactionEventKey: { tx_id: txId } }),
  );
  const transitionEntries = transitionTraceEntries(sourceEvents);
  const eventToStepEntries = eventToStepEntriesFor(sourceEvents);
  const validationTraceEntries = sortedEntries(
    sourceEvents.map((eventKey, index) => [
      LucidData.to(eventKey as never, SDK.EventKeySchema as never),
      acceptedTraceDescriptor(index + 1),
    ]),
  );
  const counts: SDK.DaPayloadCounts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: BigInt(transactionCount),
    depositCount: 0n,
    totalEventCount: BigInt(transactionCount),
    transitionStepCount: BigInt(transactionCount),
    validationTraceCount: BigInt(transactionCount),
  };
  const placeholderHeader: Header = {
    ...fixtureHeaderBase(),
    ...headerOverrides,
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const payloadWithoutHash: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: "00".repeat(28),
      header: placeholderHeader,
      utxos: [],
      transactions: sources.map((source) => [
        source.tx_id,
        LucidData.to(source as never, SDK.L2TransactionSourceSchema as never),
      ]),
      transaction_preimages: transactions.map(({ txId, txCbor }) => [
        txId,
        txCbor,
      ]),
      deposits: [],
      withdrawals: [],
      forced_transactions: [],
      forced_transaction_preimages: [],
      cek_program_material: [],
      transition_trace: transitionEntries,
      event_to_step: eventToStepEntries,
      validation_traces: validationTraceEntries,
      validation_trace_witnesses: [],
      counts,
    },
  };
  const roots = await computeDaPayloadRoots(payloadWithoutHash);
  const header: Header = {
    ...fixtureHeaderBase(),
    ...headerOverrides,
    utxosRoot: roots.utxosRoot,
    forcedTransactionsRoot: roots.forcedTransactionsRoot,
    transactionsRoot: roots.transactionsRoot,
    depositsRoot: roots.depositsRoot,
    withdrawalsRoot: roots.withdrawalsRoot,
    transitionTraceRoot: roots.transitionTraceRoot,
    eventToStepRoot: roots.eventToStepRoot,
    validationTracesRoot: roots.validationTracesRoot,
    withdrawalCount: counts.withdrawalCount,
    forcedTransactionCount: counts.forcedTransactionCount,
    l2TransactionCount: counts.l2TransactionCount,
    depositCount: counts.depositCount,
    totalEventCount: counts.totalEventCount,
    transitionStepCount: counts.transitionStepCount,
    validationTraceCount: counts.validationTraceCount,
  };
  const headerHash = hashBlockHeader(header);
  const payload: SDK.DaPayload = {
    ...payloadWithoutHash,
    block_body: {
      ...payloadWithoutHash.block_body,
      header_hash: headerHash,
      header,
    },
  };
  const innerPayloadCbor = SDK.encodeDaPayload(payload);
  return {
    payload,
    innerPayloadCbor,
    payloadCbor: await wrapDaPayload(innerPayloadCbor, { mode: "identity" }),
    header,
    headerHash,
  };
};

const sortedEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const eventPhase = (eventKey: SDK.EventKey): SDK.TransitionPhase => {
  if ("WithdrawalEventKey" in eventKey) {
    return "Withdrawal";
  }
  if ("ForcedTransactionEventKey" in eventKey) {
    return "ForcedTransaction";
  }
  if ("L2TransactionEventKey" in eventKey) {
    return "L2Transaction";
  }
  return "Deposit";
};

const transitionTraceEntries = (
  sourceEvents: readonly SDK.EventKey[],
): SDK.DaPayloadEntry[] =>
  sourceEvents.map((eventKey, index) => {
    const step: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: BigInt(index),
      event_key: eventKey,
      phase: eventPhase(eventKey),
      pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    };
    return [
      LucidData.to(step.step_index as never, LucidData.Integer() as never),
      LucidData.to(step as never, SDK.TransitionStepSchema as never),
    ];
  });

const eventToStepEntriesFor = (
  sourceEvents: readonly SDK.EventKey[],
): SDK.DaPayloadEntry[] =>
  sortedEntries(
    sourceEvents.map((eventKey, index) => [
      LucidData.to(eventKey as never, SDK.EventKeySchema as never),
      LucidData.to(
        {
          step_index: BigInt(index),
          phase: eventPhase(eventKey),
        } satisfies SDK.EventToStepValue as never,
        SDK.EventToStepValueSchema as never,
      ),
    ]),
  );

export const makeObservedNode = ({
  header,
  headerHash,
  daAttestation = SDK.NO_DA_ATTESTATION,
  depth = 10,
  outRef = "ab".repeat(32) + "#0",
  slot = 1,
  blockHash = "cd".repeat(32),
  assetName = `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`,
  linkedListKey = headerHash,
}: {
  readonly header: Header;
  readonly headerHash: string;
  readonly daAttestation?: SDK.DaAvailabilityStateQueueStatus;
  readonly depth?: number;
  readonly outRef?: string;
  readonly slot?: number;
  readonly blockHash?: string;
  readonly assetName?: string;
  readonly linkedListKey?: string | "Empty";
}): ObservedStateQueueNode => ({
  outRef,
  assetName,
  linkedListKey,
  header,
  daAttestation,
  chainPoint: {
    slot,
    blockHash,
    depth,
    providerSource: "fixture",
  },
});
