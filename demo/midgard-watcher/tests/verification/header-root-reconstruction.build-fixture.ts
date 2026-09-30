import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";

import type { WatcherStateQueueHeader } from "../../src/indexers/state-queue-snapshot.js";
import {
  evaluateWatcherHeaderRootReconstruction,
  makeWatcherAuthenticatedHeaderObservation,
} from "../../src/verification/header-root-reconstruction.js";
import {
  bufferEntries,
  depositInfo,
  type Fixture,
  type FixtureTransaction,
  headerHashOf,
  hex,
  outRef,
  sortEntries,
  watcherHeaderRecord,
  withdrawalInfo,
} from "./header-root-reconstruction.watcher-header-record.js";

export const buildFixture = async ({
  transactions = [],
  depositBytes = [],
  withdrawalBytes = [],
}: {
  readonly transactions?: readonly FixtureTransaction[];
  readonly depositBytes?: readonly number[];
  readonly withdrawalBytes?: readonly number[];
} = {}): Promise<Fixture> => {
  const transactionEntries: SDK.DaPayloadEntry[] = transactions.map((tx) => [
    tx.txId,
    tx.sourceValueBytes.toString("hex"),
  ]);
  const preimageEntries: SDK.DaPayloadEntry[] = transactions.map((tx) => [
    tx.txId,
    tx.canonicalCbor.toString("hex"),
  ]);
  const depositEntries: SDK.DaPayloadEntry[] = depositBytes.map((byte) => [
    hex(outRef(byte), SDK.OutputReferenceSchema),
    hex(depositInfo(byte), SDK.DepositInfoSchema),
  ]);
  const withdrawalEntries: SDK.DaPayloadEntry[] = withdrawalBytes.map(
    (byte) => [
      hex(outRef(byte), SDK.OutputReferenceSchema),
      hex(withdrawalInfo(byte), SDK.WithdrawalInfoSchema),
    ],
  );

  const eventKeyHex = (eventKey: SDK.EventKey): string =>
    hex(eventKey, SDK.EventKeySchema);
  const stepValueHex = (index: number, phase: SDK.TransitionPhase): string =>
    hex(
      { step_index: BigInt(index), phase } satisfies SDK.EventToStepValue,
      SDK.EventToStepValueSchema,
    );

  let stepIndex = 0;
  const eventToStepEntries: SDK.DaPayloadEntry[] = [];
  for (const byte of withdrawalBytes) {
    eventToStepEntries.push([
      eventKeyHex({
        WithdrawalEventKey: { withdrawal_id: outRef(byte) },
      }),
      stepValueHex(stepIndex++, "Withdrawal"),
    ]);
  }
  for (const tx of transactions) {
    eventToStepEntries.push([
      eventKeyHex({ L2TransactionEventKey: { tx_id: tx.txId } }),
      stepValueHex(stepIndex++, "L2Transaction"),
    ]);
  }
  for (const byte of depositBytes) {
    eventToStepEntries.push([
      eventKeyHex({ DepositEventKey: { deposit_id: outRef(byte) } }),
      stepValueHex(stepIndex++, "Deposit"),
    ]);
  }

  const validationTraceEntries: SDK.DaPayloadEntry[] = transactions.map(
    (tx, index) => [
      eventKeyHex({ L2TransactionEventKey: { tx_id: tx.txId } }),
      hex(
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
      ),
    ],
  );

  const countedRoot = async (
    domain: SDK.RootDomain,
    entries: readonly SDK.DaPayloadEntry[],
  ): Promise<string> =>
    (await buildCountedRoot(domain, bufferEntries(entries))).root;

  const counts = {
    withdrawalCount: BigInt(withdrawalEntries.length),
    forcedTransactionCount: 0n,
    l2TransactionCount: BigInt(transactionEntries.length),
    depositCount: BigInt(depositEntries.length),
    totalEventCount: BigInt(
      withdrawalEntries.length +
        transactionEntries.length +
        depositEntries.length,
    ),
    transitionStepCount: 0n,
    validationTraceCount: BigInt(transactionEntries.length),
  };

  const header: SDK.Header = {
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawalEntries,
    ),
    forcedTransactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      [],
    ),
    transactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      transactionEntries,
    ),
    depositsRoot: await countedRoot(SDK.ROOT_DOMAINS.deposits, depositEntries),
    transitionTraceRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      [],
    ),
    eventToStepRoot: await countedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      eventToStepEntries,
    ),
    validationTracesRoot: await countedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      validationTraceEntries,
    ),
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
  const headerHash = headerHashOf(header);
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: [],
      withdrawals: sortEntries(withdrawalEntries),
      forced_transactions: [],
      transactions: sortEntries(transactionEntries),
      deposits: sortEntries(depositEntries),
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
    header,
    headerHash,
    envelope: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    record: watcherHeaderRecord(header, headerHash),
    transactions,
  };
};

/** Re-encodes a mutated payload into a fresh envelope. */
export const reencode = async (payload: SDK.DaPayload): Promise<Buffer> =>
  await wrapDaPayload(SDK.encodeDaPayload(payload), { mode: "identity" });

export const clonePayload = (payload: SDK.DaPayload): SDK.DaPayload => ({
  version: payload.version,
  block_body: {
    ...payload.block_body,
    counts: { ...payload.block_body.counts },
    header: { ...payload.block_body.header },
    withdrawals: [...payload.block_body.withdrawals],
    forced_transactions: [...payload.block_body.forced_transactions],
    transactions: [...payload.block_body.transactions],
    deposits: [...payload.block_body.deposits],
    transition_trace: [...payload.block_body.transition_trace],
    event_to_step: [...payload.block_body.event_to_step],
    transaction_preimages: [...payload.block_body.transaction_preimages],
    forced_transaction_preimages: [
      ...payload.block_body.forced_transaction_preimages,
    ],
    cek_program_material: [...payload.block_body.cek_program_material],
    validation_traces: [...payload.block_body.validation_traces],
    utxos: [...payload.block_body.utxos],
  },
});

export const L1_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "watcher-local-node",
  grade: "security",
};

const DA_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "watcher-da-peer-1",
  grade: "security",
};

export const CHAIN_POINT = { slot: 4242n, blockHash: h32(7) } as const;

export const observationFor = async (
  fixture: Fixture,
  overrides: {
    readonly confirmationDepth?: number;
    readonly provenance?: SDK.EvidenceProvenance;
    readonly record?: WatcherStateQueueHeader;
    readonly minimumConfirmationDepth?: number;
  } = {},
): Promise<SDK.AuthenticatedStateQueueHeaderObservation> =>
  await makeWatcherAuthenticatedHeaderObservation({
    header: overrides.record ?? fixture.record,
    chainPoint: CHAIN_POINT,
    confirmationDepth: overrides.confirmationDepth ?? 12,
    sourceMode: "local_node",
    provenance: overrides.provenance ?? L1_PROVENANCE,
    ...(overrides.minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth: overrides.minimumConfirmationDepth }),
  });

/**
 * Commits a mutated header on L1 and inside the payload at once: the operator's
 * committed header and the payload's embedded copy stay byte-identical (so the
 * evaluation reaches the root/count comparison) while exactly one header field
 * diverges from what the payload body actually contains.
 */
export const commitMutatedHeader = async (
  fixture: Fixture,
  mutate: (header: SDK.Header) => SDK.Header,
): Promise<Fixture> => {
  const header = mutate(fixture.header);
  const headerHash = headerHashOf(header);
  const payload = clonePayload(fixture.payload);
  const mutated: SDK.DaPayload = {
    ...payload,
    block_body: { ...payload.block_body, header, header_hash: headerHash },
  };
  return {
    ...fixture,
    payload: mutated,
    header,
    headerHash,
    envelope: await reencode(mutated),
    record: watcherHeaderRecord(header, headerHash),
  };
};

export const evaluateFixture = async (
  fixture: Fixture,
  overrides: {
    readonly envelope?: Uint8Array;
    readonly daProvenance?: SDK.EvidenceProvenance;
    readonly observation?: SDK.AuthenticatedStateQueueHeaderObservation;
    readonly minimumConfirmationDepth?: number;
  } = {},
) =>
  await evaluateWatcherHeaderRootReconstruction({
    observation: overrides.observation ?? (await observationFor(fixture)),
    payloadEnvelopeCbor: overrides.envelope ?? fixture.envelope,
    daProvenance: overrides.daProvenance ?? DA_PROVENANCE,
    ...(overrides.minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth: overrides.minimumConfirmationDepth }),
  });
