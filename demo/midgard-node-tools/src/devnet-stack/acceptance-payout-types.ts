import type { Assets, Network } from "@lucid-evolution/lucid";

import type { ObservedL1TransactionAtPoint } from "../harness-kupmios.l1-chain-point.js";
import type { WithdrawalRecord } from "./journey-values.js";

export type AcceptanceOutRef = Readonly<{
  txHash: string;
  outputIndex: number;
}>;
export type AcceptanceTransaction = Readonly<{
  txHash: string;
  inputs: readonly AcceptanceOutRef[];
  outputs: readonly ReturnType<
    typeof import("@lucid-evolution/lucid").coreToTxOutput
  >[];
  mint: Readonly<Assets>;
  policies: readonly string[];
  redeemers: ReadonlyMap<string, string>;
}>;
export type AcceptanceCanonicalTransaction = Readonly<{
  observed: ObservedL1TransactionAtPoint;
  canonicalDepth: bigint;
}>;
export type AcceptanceSettlementTransaction = AcceptanceCanonicalTransaction &
  Readonly<{
    phase: "initialize" | "fund" | "conclude";
    signedCbor: string;
    requiredOutputs: readonly number[];
  }>;
export type AcceptancePayoutInput = Readonly<{
  record: WithdrawalRecord;
  order: AcceptanceCanonicalTransaction;
  settlements: readonly AcceptanceSettlementTransaction[];
  orderSuccessors?: readonly AcceptanceCanonicalTransaction[];
  externalDatum?: string;
}>;
export type AcceptancePayoutConfig = Readonly<{
  network: Network;
  withdrawalPolicyId: string;
  withdrawalAddress: string;
  payoutPolicyId: string;
  payoutAddress: string;
  confirmationDepth: bigint;
  maxTransactionBytes: number;
  maxLineageTransactions: number;
}>;
export type AcceptancePayoutProof = Readonly<{
  eventId: string;
  eventKey: string;
  order: AcceptanceOutRef;
  currentOrder: AcceptanceOutRef;
  payout: AcceptanceOutRef;
  beneficiary: AcceptanceOutRef;
  address: string;
  assets: Readonly<Assets>;
  datum?: string;
  datumHash?: string;
  lineage: readonly Readonly<{ phase: string; txHash: string }>[];
}>;

export const acceptanceOutRefKey = (ref: AcceptanceOutRef) =>
  `${ref.txHash}#${ref.outputIndex}`;
export function requireAcceptance(
  condition: unknown,
  message: string,
): asserts condition {
  if (!condition) throw new Error(`exact payout: ${message}`);
}
