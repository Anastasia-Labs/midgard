import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { computeDaPayloadRoots } from "../../src/workers/commit-block-header/da-payload.js";

const headerFor = (overrides: Partial<SDK.Header> = {}): SDK.Header => ({
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "11".repeat(28),
  operatorVkey: "22".repeat(28),
  protocolVersion: 1n,
  ...overrides,
});

export const oneDepositPayload = async (depositId: string) => {
  const counts: SDK.DaPayloadCounts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 0n,
  };
  const draft: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: "00".repeat(28),
      header: headerFor(),
      utxos: [],
      withdrawals: [],
      forced_transactions: [],
      transactions: [],
      deposits: [[depositId, "01"]],
      transition_trace: [["71".repeat(32), "01"]],
      event_to_step: [],
      transaction_preimages: [],
      forced_transaction_preimages: [],
      cek_program_material: [],
      validation_traces: [],
      validation_trace_witnesses: [],
      counts,
    },
  };
  const roots = await Effect.runPromise(computeDaPayloadRoots(draft));
  const header = headerFor({
    utxosRoot: roots.utxosRoot,
    withdrawalsRoot: roots.withdrawalsRoot,
    forcedTransactionsRoot: roots.forcedTransactionsRoot,
    transactionsRoot: roots.transactionsRoot,
    depositsRoot: roots.depositsRoot,
    transitionTraceRoot: roots.transitionTraceRoot,
    eventToStepRoot: roots.eventToStepRoot,
    validationTracesRoot: roots.validationTracesRoot,
    depositCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 0n,
  });
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  return {
    header,
    payload: {
      ...draft,
      block_body: {
        ...draft.block_body,
        header_hash: headerHash,
        header,
      },
    } satisfies SDK.DaPayload,
  };
};
