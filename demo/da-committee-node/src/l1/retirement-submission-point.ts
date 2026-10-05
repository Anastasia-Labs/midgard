import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import type { PromiseCapacityPoint } from "../availability/promise-capacity-evidence.js";
import {
  committeeScopedFetch,
  committeeScopedWebSocketFactory,
  type CommitteeSourceReadLimits,
} from "../availability/scoped-transports.js";
import type { RetirementPointProof } from "../store/retirement-source.js";
import { committeeScopedTransactionStatus } from "./availability-scoped-transaction-status.js";
import {
  safeBlockHash,
  safeSlot,
} from "./provider.parse-persisted-chain-sync-state.js";
import { fetchAncestor } from "./state-queue-replay-provider.open-rpc.js";
import { readTransaction } from "./state-queue-replay-provider.parse-transaction.js";

/** Kupo proposes an inclusion; the existing native replay reader verifies the
 * exact transaction in the raw block, with its same-response selected tip. */
export const readCommitteeRetirementSubmission = async (args: {
  kupoUrl: string;
  ogmiosUrl: string;
  txHash: string;
  boundary: PromiseCapacityPoint;
  scope: DaAvailabilityReadScope;
  limits: CommitteeSourceReadLimits;
}): Promise<RetirementPointProof | null> => {
  const status = await committeeScopedTransactionStatus(args)(
    args.txHash,
    args.scope,
  );
  if (status.status !== "confirmed") return null;
  const point = {
    slot: safeSlot(status.confirmation.slot, "Retirement submission slot"),
    blockHash: safeBlockHash(
      status.confirmation.blockHash,
      "Retirement submission block hash",
    ),
  };
  const ancestor = await fetchAncestor(
    args.kupoUrl,
    point.slot,
    committeeScopedFetch(args.scope, args.limits),
  );
  const transaction = await readTransaction(
    args.ogmiosUrl,
    ancestor,
    { transactionHash: args.txHash, point },
    committeeScopedWebSocketFactory(args.scope, args.limits),
  );
  const tip = transaction.selectedChainTip;
  if (
    transaction.transactionHash !== args.txHash ||
    tip.id !== args.boundary.blockHash ||
    tip.slot !== args.boundary.slot ||
    tip.height !== args.boundary.blockNo
  )
    throw new Error("Retirement submission differs from the selected boundary");
  if (transaction.cbor !== undefined) {
    const raw = CML.Transaction.from_cbor_hex(transaction.cbor);
    try {
      if (CML.hash_transaction(raw.body()).to_hex() !== args.txHash)
        throw new Error("Retirement submission CBOR has a different hash");
    } finally {
      raw.free();
    }
  }
  args.scope.assertCurrent();
  return {
    point: {
      slot: transaction.slot,
      blockHash: transaction.blockHash,
      blockNo: transaction.blockNo,
    },
    tip: args.boundary,
  };
};
