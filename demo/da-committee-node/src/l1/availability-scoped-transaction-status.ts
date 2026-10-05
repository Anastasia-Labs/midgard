import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { CommitteeSourceReadLimits } from "../availability/scoped-transports.js";
import { committeeScopedKupoRows } from "./availability-scoped-utxos.js";
import {
  getRecord,
  safeBlockHash,
  safeSlot,
} from "./provider.parse-persisted-chain-sync-state.js";

/** Matches the pinned Kupmios transaction status response, with streamed
 * byte/count bounds and actual fetch cancellation. An inclusion still needs
 * the existing exact selected-chain depth proof; Kupo alone is not ancestry. */
export const committeeScopedTransactionStatus =
  (input: {
    kupoUrl: string;
    limits: CommitteeSourceReadLimits;
    fetchImpl?: typeof fetch;
  }) =>
  async (
    txHash: string,
    scope: DaAvailabilityReadScope,
  ): ReturnType<LucidEvolution["transactionStatus"]> => {
    safeBlockHash(txHash, "Status transaction hash");
    const rows = await committeeScopedKupoRows(input, `*@${txHash}`, "", scope);
    if (rows.length === 0) return { status: "not_found", txHash };
    let inclusion: Readonly<{ slot: number; blockHash: string }> | undefined;
    const seen = new Set<number>();
    for (const item of rows) {
      const row = getRecord(item, "Kupo transaction output");
      const index = safeSlot(row.output_index, "Kupo status output index");
      if (row.transaction_id !== txHash || seen.has(index))
        throw new Error(
          "Kupo status query contains a foreign or duplicate output",
        );
      seen.add(index);
      const created = getRecord(row.created_at, "Kupo transaction inclusion");
      const point = {
        slot: safeSlot(created.slot_no, "Kupo inclusion slot"),
        blockHash: safeBlockHash(
          created.header_hash,
          "Kupo inclusion block hash",
        ),
      };
      if (
        inclusion &&
        (inclusion.slot !== point.slot ||
          inclusion.blockHash !== point.blockHash)
      )
        throw new Error(
          "Kupo status outputs disagree about transaction inclusion",
        );
      inclusion = point;
    }
    scope.assertCurrent();
    return {
      status: "confirmed",
      txHash,
      confirmation: { txHash, ...inclusion! },
    };
  };
