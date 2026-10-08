import * as SDK from "@al-ft/midgard-sdk";
import { valueToAssets } from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";

import * as Ledger from "../database/utils/ledger.js";
import {
  depositDataToEntry,
  withdrawalDataToEntry,
} from "../l1-event-history-entries.js";
import {
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { landedStateQueueUTxOs } from "../services/landed-state-queue.js";
import {
  depositPayloadOf,
  outRefOf,
  policyTokens,
  withdrawalPayloadOf,
} from "./state-reconciliation.check-ledger-cache.js";
import {
  type DepositPayload,
  type JournalSummary,
  type L1EventOrder,
  type L1Payout,
  type L1QueueHeader,
  type L1Settlement,
  type L1StateView,
  type WithdrawalPayload,
} from "./state-reconciliation.compares.js";
import {
  describeError,
  toHex,
} from "./state-reconciliation.walk-merged-chain.js";

const decodeQueueHeader = (node: SDK.StateQueueUTxO) =>
  Effect.gen(function* () {
    const headerHash = (yield* SDK.headerHashFromStateQueueUTxO(
      node,
    )).toLowerCase();
    const decoded = yield* Effect.either(
      Effect.gen(function* () {
        const stateQueueNode = yield* SDK.getStateQueueNodeFromStateQueueDatum(
          node.datum,
        );
        const header = stateQueueNode.header;
        const recomputed = yield* SDK.hashBlockHeader(header);
        return {
          header,
          recomputed,
          daStatus: SDK.daAvailabilityStateQueueStatusKind(
            stateQueueNode.da_attestation,
          ),
        };
      }),
    );
    if (Either.isLeft(decoded)) {
      return {
        outRef: outRefOf(node.utxo),
        headerHash,
        recomputedHeaderHash: null,
        prevHeaderHash: null,
        endTimeMs: null,
        roots: null,
        daStatus: null,
        decodeError: describeError(decoded.left),
      } satisfies L1QueueHeader;
    }
    const { header, recomputed, daStatus } = decoded.right;
    return {
      outRef: outRefOf(node.utxo),
      headerHash,
      recomputedHeaderHash: recomputed,
      prevHeaderHash: header.prevHeaderHash,
      endTimeMs: Number(header.endTime),
      roots: {
        utxos: header.utxosRoot,
        deposits: header.depositsRoot,
        withdrawals: header.withdrawalsRoot,
        forcedTransactions: header.forcedTransactionsRoot,
        transactions: header.transactionsRoot,
      },
      daStatus,
      decodeError: null,
    } satisfies L1QueueHeader;
  });

const lucidPromise = <A>(message: string, run: () => Promise<A>) =>
  Effect.tryPromise({
    try: run,
    catch: (cause) => new SDK.LucidError({ message, cause }),
  });

/** One read of every L1 collection the checks compare. */
export const collectL1StateView: Effect.Effect<
  L1StateView,
  unknown,
  Lucid | MidgardContracts | NodeConfig | Database
> = Effect.gen(function* () {
  const { api: lucid } = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const nodeConfig = yield* NodeConfig;
  // The queue as the node's follower landed it (P1).
  const queue = yield* landedStateQueueUTxOs(
    contracts.stateQueue,
    "state reconciliation",
  );
  const rootNode = queue[0];
  if (rootNode === undefined || rootNode.datum.key !== "Empty") {
    return yield* Effect.fail(
      new Error("L1 state queue has no confirmed-state root node"),
    );
  }
  const confirmed = yield* SDK.getConfirmedStateFromStateQueueDatum(
    rootNode.datum,
  );
  const unmerged = yield* Effect.forEach(queue.slice(1), decodeQueueHeader, {
    concurrency: 1,
  });
  const eventHistory = SDK.requireEventHistoryContracts(contracts);
  const depositUtxos = yield* SDK.fetchDepositUTxOsProgram(lucid, {
    ...SDK.eventHistoryDeploymentFromContracts(eventHistory.deposit),
  });
  const deposits = yield* Effect.forEach(depositUtxos, (utxo) =>
    Effect.either(
      depositDataToEntry({ ...utxo, location: utxo.utxo }, nodeConfig.NETWORK),
    ).pipe(
      Effect.map(
        (result): L1EventOrder<DepositPayload> => ({
          outRef: outRefOf(utxo.utxo),
          payload: Either.isRight(result)
            ? depositPayloadOf(result.right)
            : null,
          decodeError: Either.isLeft(result)
            ? describeError(result.left)
            : null,
        }),
      ),
    ),
  );
  const withdrawalUtxos = yield* SDK.fetchWithdrawalUTxOsProgram(lucid, {
    ...SDK.eventHistoryDeploymentFromContracts(eventHistory.withdrawal),
  });
  const withdrawals = yield* Effect.forEach(withdrawalUtxos, (utxo) =>
    Effect.either(
      withdrawalDataToEntry({
        ...utxo,
        location: utxo.utxo,
        payloadCbor: utxo.history.payloadCbor,
      }),
    ).pipe(
      Effect.map(
        (result): L1EventOrder<WithdrawalPayload> => ({
          outRef: outRefOf(utxo.utxo),
          payload: Either.isRight(result)
            ? withdrawalPayloadOf(result.right)
            : null,
          decodeError: Either.isLeft(result)
            ? describeError(result.left)
            : null,
        }),
      ),
    ),
  );
  const payoutUtxos = yield* lucidPromise("Failed to fetch payout UTxOs", () =>
    lucid.utxosAt(contracts.payout.spendingScriptAddress),
  );
  const payouts = payoutUtxos
    .map((utxo): L1Payout | null => {
      const tokens = policyTokens(utxo, contracts.payout.policyId);
      if (tokens.length === 0) return null;
      try {
        if (utxo.datum == null)
          throw new Error("payout UTxO has no inline datum");
        const datum = LucidData.from(
          utxo.datum,
          SDK.PayoutDatum,
        ) as SDK.PayoutDatum;
        return {
          outRef: outRefOf(utxo),
          tokens,
          l2Value: valueToAssets(datum.l2_value),
          l1AddressCbor: LucidData.to(datum.l1_address, SDK.AddressData),
          l1DatumCbor: LucidData.to(datum.l1_datum, SDK.CardanoDatum),
          decodeError: null,
        };
      } catch (error) {
        return {
          outRef: outRefOf(utxo),
          tokens,
          l2Value: null,
          l1AddressCbor: null,
          l1DatumCbor: null,
          decodeError: describeError(error),
        };
      }
    })
    .filter((payout): payout is L1Payout => payout !== null);
  const settlementUtxos = yield* lucidPromise(
    "Failed to fetch settlement UTxOs",
    () => lucid.utxosAt(contracts.settlement.spendingScriptAddress),
  );
  const settlements = settlementUtxos
    .map((utxo): L1Settlement | null => {
      const tokens = policyTokens(utxo, contracts.settlement.policyId);
      if (tokens.length === 0) return null;
      try {
        if (utxo.datum == null)
          throw new Error("settlement UTxO has no inline datum");
        const datum = LucidData.from(
          utxo.datum,
          SDK.SettlementDatum,
        ) as SDK.SettlementDatum;
        return {
          outRef: outRefOf(utxo),
          tokens,
          roots: {
            deposits: datum.deposits_root,
            withdrawals: datum.withdrawals_root,
            forcedTransactions: datum.forced_transactions_root,
            transactions: datum.transactions_root,
          },
          decodeError: null,
        };
      } catch (error) {
        return {
          outRef: outRefOf(utxo),
          tokens,
          roots: null,
          decodeError: describeError(error),
        };
      }
    })
    .filter((settlement): settlement is L1Settlement => settlement !== null);
  return {
    confirmed: {
      outRef: outRefOf(rootNode.utxo),
      headerHash: confirmed.data.headerHash,
      utxoRoot: confirmed.data.utxoRoot,
      endTimeMs: Number(confirmed.data.endTime),
    },
    unmerged,
    deposits,
    withdrawals,
    payouts,
    settlements,
  };
});

/**
 * Identity of an L1 read: every UTxO position plus the confirmed header. Two
 * reads with equal fingerprints observed the same L1 state for the checks.
 */
export const l1Fingerprint = (view: L1StateView): string =>
  JSON.stringify([
    view.confirmed.outRef,
    view.confirmed.headerHash,
    view.unmerged.map((h) => h.outRef),
    view.deposits.map((d) => d.outRef).sort(),
    view.withdrawals.map((w) => w.outRef).sort(),
    view.payouts.map((p) => p.outRef).sort(),
    view.settlements.map((s) => s.outRef).sort(),
  ]);

// ---------------------------------------------------------------------------
// SQL snapshot collection
// ---------------------------------------------------------------------------

export type JournalRow = {
  readonly header_hash: Buffer;
  readonly status: string;
  readonly base_tail_header_hash: Buffer;
  readonly base_utxos_root: string;
  readonly expected_utxos_root: string;
  readonly expected_deposits_root: string;
  readonly expected_withdrawals_root: string;
  readonly expected_forced_transactions_root: string;
  readonly expected_transactions_root: string;
  readonly correction_transition_digest: string | null;
  readonly submitted_tx_hash: Buffer | string | null;
  readonly header_cbor: Buffer | null;
};

export const headerEndTimeMs = (cbor: Buffer | null): number | null => {
  if (cbor === null || cbor.length === 0) return null;
  try {
    const header = LucidData.from(toHex(cbor), SDK.Header) as SDK.Header;
    return Number(header.endTime);
  } catch {
    return null;
  }
};

export const entriesMap = (
  entries: readonly Ledger.Entry[],
): Map<string, string> =>
  new Map(
    entries.map((entry) => [
      toHex(entry[Ledger.Columns.OUTREF]),
      toHex(entry[Ledger.Columns.OUTPUT]),
    ]),
  );

export const chainFrom = (
  headerHash: string,
  confirmedRoot: string,
  journals: ReadonlyMap<string, JournalSummary>,
): string[] => {
  const chain: string[] = [];
  let current = journals.get(headerHash);
  while (current !== undefined && chain.length <= journals.size) {
    chain.push(current.headerHash);
    if (current.baseUtxosRoot === confirmedRoot) break;
    current = journals.get(current.baseTailHeaderHash);
  }
  return chain.reverse();
};
