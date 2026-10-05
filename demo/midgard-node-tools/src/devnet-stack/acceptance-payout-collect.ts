import * as SDK from "@al-ft/midgard-sdk";
import { Data, datumToHash } from "@lucid-evolution/lucid";
import {
  fetchKupoAncestorPoint,
  fetchKupoCreationPoint,
  fetchKupoMatch,
  fetchKupoSpend,
} from "midgard-node/l1-tx-order-carriage.fetch-kupo-spend";
import type {
  FetchLike,
  L1ChainPoint,
} from "midgard-node/l1-tx-order-carriage.l1-chain-point";

import type { AcceptanceNativePayoutScope } from "./acceptance-native-boundary.js";
import { verifyAcceptancePayoutLineage } from "./acceptance-payout-lineage.js";
import { acceptanceRemainingMs } from "./acceptance-payout-sources.js";
import type { AcceptanceSettlementSnapshot } from "./acceptance-payout-sql.js";
import { decodeAcceptanceCanonicalTransaction } from "./acceptance-payout-transaction.js";
import {
  type AcceptanceCanonicalTransaction,
  type AcceptanceOutRef,
  type AcceptancePayoutConfig,
  type AcceptanceSettlementTransaction,
  requireAcceptance,
} from "./acceptance-payout-types.js";
import type { WithdrawalRecord } from "./journey-values.js";

/** Source locators may be stale. Only exact selected-chain bytes and depth feed the pure verifier. */
export const collectAcceptancePayoutLineages = async (input: {
  scope: AcceptanceNativePayoutScope;
  records: readonly WithdrawalRecord[];
  snapshot: AcceptanceSettlementSnapshot;
  config: AcceptancePayoutConfig;
  kupoUrl: string;
  fetchImpl: FetchLike;
  blockScanLimit: number;
  maxReferenceInputs: number;
}) => {
  const { scope, records, snapshot, config, kupoUrl, fetchImpl } = input;
  requireAcceptance(
    records.length === 4 &&
      new Set(records.map((row) => row.withdrawalEventId)).size === 4,
    "expected four distinct completed withdrawal records",
  );
  for (const limit of [input.blockScanLimit, input.maxReferenceInputs])
    requireAcceptance(
      Number.isSafeInteger(limit) && limit > 0,
      "invalid canonical source work bound",
    );
  const hints = () => ({
    kupoUrl,
    fetchImpl,
    timeoutMs: acceptanceRemainingMs(scope),
  });
  const canonical = async (
    txHash: string,
    point: L1ChainPoint,
  ): Promise<AcceptanceCanonicalTransaction> => {
    scope.assertCurrent();
    const intersection = await fetchKupoAncestorPoint({
      ...hints(),
      slot: point.slot,
    });
    scope.assertCurrent();
    const observed = await scope.readExactTransaction({
      txHash,
      blockPoint: point,
      intersection,
      blockScanLimit: input.blockScanLimit,
    });
    scope.assertCurrent();
    requireAcceptance(
      observed.txHash === txHash &&
        observed.blockPoint.headerHash === point.headerHash &&
        observed.blockPoint.slot === point.slot &&
        Number.isSafeInteger(observed.blockPoint.blockNo) &&
        BigInt(observed.blockPoint.blockNo) <= BigInt(scope.point.blockNo),
      "canonical transaction point differs from locator/boundary",
    );
    const canonicalDepth = await scope.canonicalBlockDepth({
      blockHash: point.headerHash,
      slot: point.slot,
      blockNo: BigInt(observed.blockPoint.blockNo),
    });
    scope.assertCurrent();
    requireAcceptance(
      canonicalDepth !== null,
      "transaction is not on current selected chain",
    );
    const evidence = { observed, canonicalDepth };
    decodeAcceptanceCanonicalTransaction(evidence, config);
    return evidence;
  };
  const fromRef = async (ref: AcceptanceOutRef) => {
    const point = await fetchKupoCreationPoint({ ...hints(), outRef: ref });
    scope.assertCurrent();
    return canonical(ref.txHash, point);
  };
  const proofs = [];
  for (const record of records) {
    scope.assertCurrent();
    const order = await fromRef({ txHash: record.txHash, outputIndex: 0 });
    const decoded = decodeAcceptanceCanonicalTransaction(order, config);
    const eventKey = datumToHash(
      Data.to(
        Data.from(record.withdrawalEventId, SDK.OutputReference),
        SDK.OutputReference,
      ),
    );
    const unit = config.withdrawalPolicyId + eventKey;
    const indexes = decoded.outputs.flatMap((output, index) =>
      output.assets[unit] === undefined ? [] : [index],
    );
    requireAcceptance(
      indexes.length === 1,
      "original Order NFT output is missing or ambiguous",
    );
    let current = { txHash: record.txHash, outputIndex: indexes[0]! };
    const node = Data.from(
      decoded.outputs[current.outputIndex]!.datum!,
      SDK.EventHistoryNode,
    );
    requireAcceptance(
      typeof node.payload === "object" && "Order" in node.payload,
      "original Order payload missing",
    );
    let externalDatum: string | undefined;
    if ("External" in node.payload.Order.facts.location) {
      const expected =
        node.payload.Order.facts.location.External.storage_datum_hash;
      requireAcceptance(
        order.observed.referenceInputs.length <= input.maxReferenceInputs,
        "external payload reference inputs exceed explicit bound",
      );
      for (const ref of order.observed.referenceInputs) {
        const match = await fetchKupoMatch({ ...hints(), outRef: ref });
        scope.assertCurrent();
        if (
          match.datum_type === "inline" &&
          typeof match.datum === "string" &&
          match.datum.length / 2 <= config.maxTransactionBytes &&
          datumToHash(match.datum) === expected
        ) {
          externalDatum = match.datum;
          break;
        }
      }
      requireAcceptance(
        externalDatum !== undefined,
        "committed external Order payload unavailable",
      );
    }
    const rows = snapshot.attempts.filter(
      (row) => row.event_id === record.withdrawalEventId,
    );
    requireAcceptance(
      rows.length >= 2 && rows.length + 1 <= config.maxLineageTransactions,
      "settlement lineage missing or exceeds explicit bound",
    );
    const initializers = rows.filter((row) => row.phase === "initialize");
    requireAcceptance(
      initializers.length === 1,
      "initializer receipt missing or ambiguous",
    );
    const successors: AcceptanceCanonicalTransaction[] = [];
    let initialize: AcceptanceCanonicalTransaction | undefined;
    const visited = new Set([record.txHash]);
    for (;;) {
      requireAcceptance(
        1 + successors.length + rows.length <= config.maxLineageTransactions,
        "Order and settlement lineage exceeds explicit bound",
      );
      const spend = await fetchKupoSpend({ ...hints(), outRef: current });
      scope.assertCurrent();
      requireAcceptance(
        spend !== null && !visited.has(spend.transactionId),
        "Order spend missing or cyclic",
      );
      visited.add(spend.transactionId);
      if (spend.transactionId !== initializers[0]!.tx_hash)
        requireAcceptance(
          2 + successors.length + rows.length <= config.maxLineageTransactions,
          "Order and settlement lineage exceeds explicit bound",
        );
      const tx = await canonical(spend.transactionId, spend.point);
      if (spend.transactionId === initializers[0]!.tx_hash) {
        initialize = tx;
        break;
      }
      const moved = decodeAcceptanceCanonicalTransaction(tx, config);
      const next = moved.outputs.flatMap((output, index) =>
        output.assets[unit] === undefined ? [] : [index],
      );
      requireAcceptance(
        next.length === 1,
        "Order spend neither preserves NFT nor matches initializer receipt",
      );
      successors.push(tx);
      current = { txHash: tx.observed.txHash, outputIndex: next[0]! };
    }
    const settlements: AcceptanceSettlementTransaction[] = [];
    for (const row of rows) {
      const tx =
        row.tx_hash === initialize.observed.txHash
          ? initialize
          : await fromRef({
              txHash: row.tx_hash,
              outputIndex: row.required_outputs[0]!,
            });
      settlements.push({
        ...tx,
        phase: row.phase,
        signedCbor: row.signed_cbor,
        requiredOutputs: row.required_outputs,
      });
    }
    proofs.push(
      verifyAcceptancePayoutLineage(
        {
          record,
          order,
          orderSuccessors: successors,
          settlements,
          ...(externalDatum === undefined ? {} : { externalDatum }),
        },
        config,
      ),
    );
  }
  scope.assertCurrent();
  return proofs;
};
