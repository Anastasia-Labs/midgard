import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { LucidError, type MidgardValidators } from "../common.js";
import { fetchDepositUTxOsProgram } from "../user-events/deposit.js";
import {
  EVENT_HISTORY_MAX_PROTECTION_TIME,
  EventHistoryNode,
} from "../user-events/history.js";
import {
  eventHistoryDeploymentFromContracts,
  requireEventHistoryContracts,
} from "../user-events/history-deployment.js";
import { eventHistoryKey } from "../user-events/history-query.js";
import { utxosToTxOrderUTxOs } from "../user-events/tx-order.js";
import { fetchWithdrawalUTxOsProgram } from "../user-events/withdrawal.js";
import { type NeglectedUserEventClaim } from "./strike.compute-inactivity-threshold.js";

/**
 * The claim a strike cites among `candidates`: the one with the earliest
 * `inclusion_time` after the state-queue tail's `end_time`, which gives the
 * earliest threshold. Null when no candidate is after the tail: the shift has
 * no undelivered user event, and its operator cannot be struck.
 */
export const selectNeglectedUserEvent = (
  candidates: readonly NeglectedUserEventClaim[],
  stateQueueTailEndTimeMs: bigint,
): NeglectedUserEventClaim | null =>
  candidates.reduce<NeglectedUserEventClaim | null>(
    (earliest, candidate) =>
      candidate.inclusionTimeMs > stateQueueTailEndTimeMs &&
      (earliest === null ||
        candidate.inclusionTimeMs < earliest.inclusionTimeMs)
        ? candidate
        : earliest,
    null,
  );

/** Where the scheduler authenticates each kind of cited event. */
export type NeglectedUserEventCitationSources = Readonly<{
  /** The deposit list's policy and address (`hub_datum.deposit[_addr]`). */
  deposit: Readonly<{ policyId: string; address: string }>;
  /** The withdrawal list's policy and address. */
  withdrawal: Readonly<{ policyId: string; address: string }>;
  /** The tx-order policy (`hub_datum.tx_order`). */
  txOrderPolicyId: string;
}>;

const KEY_HEX = /^[0-9a-f]{64}$/u;

/**
 * An event-history Order node as `order_facts.referenced` admits it:
 * `ordering.authenticate` (the list address, no reference script, an inline
 * `Link` datum passing `valid_link`, and exactly the policy's token named by
 * the node's key) and then `from_node` (a `Key` position, an `Order` payload,
 * and an event id whose nonce is that key). Its inclusion time must be the
 * one claimed.
 */
const citableHistoryOrder = (
  utxo: UTxO,
  list: Readonly<{ policyId: string; address: string }>,
  inclusionTimeMs: bigint,
): boolean => {
  if (
    utxo.address !== list.address ||
    utxo.scriptRef != null ||
    utxo.datum == null
  )
    return false;
  const tokens = Object.entries(utxo.assets).filter(([unit]) =>
    unit.startsWith(list.policyId),
  );
  if (tokens.length !== 1 || tokens[0]![1] !== 1n) return false;
  const key = tokens[0]![0].slice(list.policyId.length);
  if (!KEY_HEX.test(key)) return false;
  let node: EventHistoryNode;
  try {
    node = Data.from(utxo.datum, EventHistoryNode);
  } catch {
    return false;
  }
  if (
    node.position === "Root" ||
    node.position.Key[0] !== key ||
    node.protected_until < 0n ||
    node.protected_until > EVENT_HISTORY_MAX_PROTECTION_TIME ||
    (node.next !== null && !(KEY_HEX.test(node.next) && key < node.next)) ||
    node.payload === "RootContent" ||
    !("Order" in node.payload)
  )
    return false;
  const { facts } = node.payload.Order;
  const nonce = Effect.runSync(Effect.either(eventHistoryKey(facts.event_id)));
  return (
    nonce._tag === "Right" &&
    nonce.right === key &&
    facts.inclusion_time === inclusionTimeMs
  );
};

/**
 * A tx order as `tx_order.get_datum` admits it: the output's only non-ADA
 * asset is one token of the tx-order policy, and its inline datum decodes as
 * a tx-order datum, here with the inclusion time claimed.
 */
const citableTxOrder = (
  utxo: UTxO,
  policyId: string,
  inclusionTimeMs: bigint,
): boolean => {
  const [order] = Effect.runSync(utxosToTxOrderUTxOs([utxo], policyId));
  return order !== undefined && order.datum.inclusion_time === inclusionTimeMs;
};

/**
 * Whether the scheduler would accept `claim` as the event a strike cites
 * (`validate_operator_inactivity_and_get_its_link`, step 7). A reader that
 * selects evidence from its own store filters with this, so a strike never
 * cites what the chain refuses.
 */
export const citableNeglectedUserEvent = (
  claim: NeglectedUserEventClaim,
  sources: NeglectedUserEventCitationSources,
): boolean => {
  switch (claim.kind) {
    case "Deposit":
      return citableHistoryOrder(
        claim.utxo,
        sources.deposit,
        claim.inclusionTimeMs,
      );
    case "Withdrawal":
      return citableHistoryOrder(
        claim.utxo,
        sources.withdrawal,
        claim.inclusionTimeMs,
      );
    case "TxOrder":
      return citableTxOrder(
        claim.utxo,
        sources.txOrderPolicyId,
        claim.inclusionTimeMs,
      );
  }
};

/** The cited output, as the strike's reference input names it. */
export const neglectedUserEventCitationId = (
  claim: Pick<NeglectedUserEventClaim, "kind" | "utxo">,
): string =>
  `${claim.kind}:${claim.utxo.txHash}#${claim.utxo.outputIndex.toString()}`;

/**
 * Reads the deposit and withdrawal event-history Order nodes and the tx-order
 * UTxOs from a provider and selects the neglected user event a strike cites
 * (`selectNeglectedUserEvent`). A provider reader's evidence source; a node
 * that follows L1 reads the same events from its follower store instead.
 */
export const fetchNeglectedUserEventProgram = (
  provider: { utxosAt(address: string): Promise<UTxO[]> },
  contracts: Pick<MidgardValidators, "eventHistory" | "txOrder">,
  stateQueueTailEndTimeMs: bigint,
): Effect.Effect<NeglectedUserEventClaim | null, LucidError> =>
  Effect.gen(function* () {
    const history = yield* Effect.try({
      try: () => requireEventHistoryContracts(contracts),
      catch: (cause) =>
        new LucidError({
          message: `Failed to read the event-history contracts: ${String(cause)}`,
          cause,
        }),
    });
    const after = {
      inclusionTimeLowerBound: stateQueueTailEndTimeMs + 1n,
    };
    const deposits = yield* fetchDepositUTxOsProgram(provider, {
      ...eventHistoryDeploymentFromContracts(history.deposit),
      ...after,
    });
    const withdrawals = yield* fetchWithdrawalUTxOsProgram(provider, {
      ...eventHistoryDeploymentFromContracts(history.withdrawal),
      ...after,
    });
    const txOrderUtxos = yield* Effect.tryPromise({
      try: () => provider.utxosAt(contracts.txOrder.spendingScriptAddress),
      catch: (cause) =>
        new LucidError({
          message: `Failed to fetch tx-order UTxOs: ${String(cause)}`,
          cause,
        }),
    });
    const txOrders = yield* utxosToTxOrderUTxOs(
      txOrderUtxos,
      contracts.txOrder.policyId,
    );
    return selectNeglectedUserEvent(
      [
        ...deposits.map(
          (event): NeglectedUserEventClaim => ({
            kind: "Deposit",
            utxo: event.utxo,
            inclusionTimeMs: event.facts.inclusion_time,
          }),
        ),
        ...withdrawals.map(
          (event): NeglectedUserEventClaim => ({
            kind: "Withdrawal",
            utxo: event.utxo,
            inclusionTimeMs: event.facts.inclusion_time,
          }),
        ),
        ...txOrders.map(
          (order): NeglectedUserEventClaim => ({
            kind: "TxOrder",
            utxo: order.utxo,
            inclusionTimeMs: order.datum.inclusion_time,
          }),
        ),
      ],
      stateQueueTailEndTimeMs,
    );
  });
