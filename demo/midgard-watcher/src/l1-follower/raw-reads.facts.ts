import type { FraudProofRawL1Point } from "@al-ft/midgard-fault-proofs";
import type {
  FactStore,
  OutRef,
  StoredBlock,
} from "@al-ft/midgard-l1-follower";
import { CML } from "@lucid-evolution/lucid";

import { ok, rawPointOf, type RawRead, refused } from "./raw-reads.types.js";
import { createdOutputOf } from "./reads.js";

/**
 * The fact-store lookups the raw reads share: whether a point is a
 * canonical stored block, and why the follower holds no row for an output.
 */

export const samePoint = (
  left: FraudProofRawL1Point,
  right: FraudProofRawL1Point,
): boolean =>
  left.slot === right.slot &&
  left.blockHash === right.blockHash &&
  left.blockNo === right.blockNo &&
  left.pointId === right.pointId;

/** The point as a canonical stored block, or why it is not one. */
export const canonicalBlock = async (
  store: FactStore,
  point: FraudProofRawL1Point,
): Promise<RawRead<StoredBlock>> => {
  const hash = Buffer.from(point.blockHash, "hex");
  const status = await store.pointStatus({ slot: Number(point.slot), hash });
  if (status.kind !== "canonical") return refused(status.kind, status.detail);
  const block = await store.blockByHash(hash);
  if (block === null || !samePoint(rawPointOf(block), point))
    return refused(
      "point_not_canonical",
      "the point's height or id differs from the stored block",
    );
  return ok(block);
};

type Tracked = ReturnType<FactStore["trackedSet"]>;

export const isTrackedAddress = (
  tracked: Tracked,
  address: CML.Address,
): boolean => {
  const payment = address.payment_cred();
  const credential =
    payment?.as_script()?.to_hex() ?? payment?.as_pub_key()?.to_hex();
  return (
    tracked.addresses.has(
      Buffer.from(address.to_raw_bytes()).toString("hex"),
    ) ||
    (credential !== undefined && tracked.paymentCredentials.has(credential))
  );
};

/** Whether the tracked set covers an output (address, credential or policy). */
export const isTrackedOutput = (
  tracked: Tracked,
  output: CML.TransactionOutput,
): boolean => {
  if (isTrackedAddress(tracked, output.address())) return true;
  const policies = output.amount().multi_asset().keys();
  for (let i = 0; i < policies.len(); i += 1)
    if (tracked.policies.has(policies.get(i).to_hex())) return true;
  return false;
};

/** The pruned-through slot, or null while nothing was pruned. */
export const prunedSinceOrigin = async (
  store: FactStore,
): Promise<number | null> => {
  const cursor = await store.cursor();
  if (cursor === null || cursor.prunedThroughSlot <= cursor.origin.slot)
    return null;
  return cursor.prunedThroughSlot;
};

/**
 * Why the follower holds no row for `outRef`: `unknown` when it provably
 * never held a tracked row for it, else `beyond_retention`.
 */
export const missingRowReason = async (
  store: FactStore,
  outRef: OutRef,
): Promise<"unknown" | "beyond_retention"> => {
  if ((await prunedSinceOrigin(store)) === null) return "unknown";
  const creating = await store.txByHash(outRef.txHash);
  if (creating === null) return "beyond_retention";
  const body = CML.TransactionBody.from_cbor_bytes(creating.bodyCbor);
  try {
    const output = createdOutputOf(body, creating.isValid, outRef.index);
    if (output === undefined) return "unknown";
    return isTrackedOutput(store.trackedSet(), output)
      ? "beyond_retention"
      : "unknown";
  } finally {
    body.free();
  }
};
