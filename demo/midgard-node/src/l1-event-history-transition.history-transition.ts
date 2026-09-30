import type { OutRefLike } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";

import type { LedgerSnapshotOutput } from "./l1-ledger-snapshot.js";

export type Kind = "deposit" | "withdrawal";

export type Node = SDK.AuthenticatedHistoryNode;

/** Stable, pointer-independent facts for durable provenance. Output location
 * is separate so a continuation cannot replace the admission's identity. */
export type HistoryTransitionEvent = Readonly<{
  key: string;
  idCbor: string;
  inclusionTime: bigint;
  factsCbor: string;
  payloadCbor: string;
  originalAssetsCbor: string;
  outRef: OutRefLike;
}>;

export type HistoryTransition = Readonly<{
  kind: Kind;
  operation:
    | "Initialize"
    | "InsertOrder"
    | "InsertFiller"
    | "PromoteFiller"
    | "ReclaimFiller"
    | "RetireOrder";
  transactionHash: string;
  consumed: readonly OutRefLike[];
  produced: readonly LedgerSnapshotOutput[];
  admission?: HistoryTransitionEvent;
  continuations: readonly Readonly<{
    key: string;
    before: OutRefLike;
    after: OutRefLike;
  }>[];
  retirement?: Readonly<{
    event: HistoryTransitionEvent;
    reason: "absorbed" | "payout_initialized" | "refunded";
    observerRedeemerIndex: number;
    witnessCbor: string;
  }>;
}>;

export const fail = (message: string): never => {
  throw new Error(`Invalid history transition: ${message}`);
};

export const label = (ref: OutRefLike) => `${ref.txHash}#${ref.outputIndex}`;

export const refOf = (output: OutRefLike): OutRefLike =>
  Object.freeze({ txHash: output.txHash, outputIndex: output.outputIndex });

export const at = <T>(
  items: readonly T[],
  index: bigint,
  description: string,
): T => {
  if (index < 0n || index >= BigInt(items.length))
    return fail(`${description} index is outside the transaction roster`);
  return items[Number(index)]!;
};

export const order = (node: Node) => {
  if (node.node.payload === "RootContent" || !("Order" in node.node.payload))
    return fail("expected an Order node");
  return node.node.payload.Order.facts;
};

export const sameAssets = (
  left: LedgerSnapshotOutput["assets"],
  right: LedgerSnapshotOutput["assets"],
) =>
  Object.keys(left).length === Object.keys(right).length &&
  Object.entries(left).every(([unit, amount]) => right[unit] === amount);

export const authenticate = (
  outputs: readonly LedgerSnapshotOutput[],
  deployment: SDK.EventHistoryDeployment,
) => {
  const selected = outputs.filter((output) =>
    Object.keys(output.assets).some((unit) =>
      unit.startsWith(deployment.policyId),
    ),
  );
  if (selected.some((output) => output.hasReferenceScript))
    return fail("authenticated output carries a reference script");
  return SDK.authenticateHistoryNodes(
    selected.map((output) => ({ ...output, assets: { ...output.assets } })),
    deployment,
  );
};
