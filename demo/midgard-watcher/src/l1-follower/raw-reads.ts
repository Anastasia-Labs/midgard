import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Point,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1Utxo,
} from "@al-ft/midgard-fault-proofs";
import {
  depth,
  type FactStore,
  type OutRef,
  type SqlTx,
  type StoredBlock,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import { outRefLabel, parseOutRefLabel, resolveRawUtxoIn } from "./reads.js";
import { WATCHER_QUEUE_UNIT_HISTORY_TABLE } from "./tables.js";

/**
 * The fraud-proof raw L1 reads the watcher makes through the local Kupmios
 * raw source (`readAdmittedLocalKupmios*`), answered from the follower's
 * facts and the watcher projection (ticket W1). Each read returns the value
 * in the Kupmios source's shape, or a refusal saying why the follower cannot
 * answer it: an untracked address, a unit no projection records, a point
 * off the stored chain, or an output whose creating body is not stored
 * (plan §12.3 resolves those in phase B, ruling 2).
 *
 * Pure reads: no clock, no network, no write.
 */

export type RawRead<T> =
  | Readonly<{ kind: "ok"; value: T }>
  | Readonly<{ kind: "refused"; reason: string }>;

const ok = <T>(value: T): RawRead<T> => ({ kind: "ok", value });
const refused = <T>(reason: string): RawRead<T> => ({
  kind: "refused",
  reason,
});

/** A stored block as the raw source's point. */
export const rawPointOf = (
  block: Readonly<{ slot: number; hash: Buffer; height: number }>,
): FraudProofRawL1Point => {
  const point = {
    slot: block.slot.toString(),
    blockHash: block.hash.toString("hex"),
    blockNo: block.height.toString(),
  };
  return { ...point, pointId: computeFraudProofRawL1PointId(point) };
};

export type FollowerVerifiedSpend = Readonly<{
  outRef: string;
  spendingTxHash: string;
  spendPoint: FraudProofRawL1Point;
}>;

export type FollowerOutRefsAtPoint = Readonly<{
  /** The requested outrefs unspent at the point. */
  outputs: readonly FraudProofRawL1Utxo[];
  /** The requested outrefs spent at or below the point. */
  spends: readonly FollowerVerifiedSpend[];
  /** Requested outrefs the follower holds no row for (untracked or pruned). */
  unknown: readonly string[];
}>;

export type FollowerUnitHistory = Readonly<{
  checkpoint: FraudProofRawL1Point;
  transactions: readonly Readonly<{
    txHash: string;
    inclusionPoint: FraudProofRawL1Point;
  }>[];
}>;

export type FollowerRawTransaction = Readonly<{
  transaction: FraudProofRawL1Transaction;
  /** Inputs whose creating body the follower does not store (ruling 2). */
  unresolvedInputs: readonly string[];
  unresolvedReferenceInputs: readonly string[];
}>;

export type FollowerRawReads = Readonly<{
  addressUtxosAtPoint: (
    address: string,
    point: FraudProofRawL1Point,
  ) => Promise<RawRead<readonly FraudProofRawL1Utxo[]>>;
  utxosByOutRefAtPoint: (
    outRefs: readonly string[],
    point: FraudProofRawL1Point,
  ) => Promise<RawRead<FollowerOutRefsAtPoint>>;
  unitHistoryAtPoint: (
    unit: string,
    point: FraudProofRawL1Point,
  ) => Promise<RawRead<FollowerUnitHistory>>;
  transactionInclusion: (
    txHash: string,
  ) => Promise<RawRead<FraudProofRawL1Point | null>>;
  rawTransaction: (
    txHash: string,
    expectedInclusionPoint: FraudProofRawL1Point,
  ) => Promise<RawRead<FollowerRawTransaction>>;
  predecessorPoint: (
    point: FraudProofRawL1Point,
  ) => Promise<RawRead<FraudProofRawL1Point>>;
}>;

const samePoint = (
  left: FraudProofRawL1Point,
  right: FraudProofRawL1Point,
): boolean =>
  left.slot === right.slot &&
  left.blockHash === right.blockHash &&
  left.blockNo === right.blockNo &&
  left.pointId === right.pointId;

/** The point as a canonical stored block, or why it is not one. */
const canonicalBlock = async (
  store: FactStore,
  point: FraudProofRawL1Point,
): Promise<RawRead<StoredBlock>> => {
  const hash = Buffer.from(point.blockHash, "hex");
  const status = await store.pointStatus({ slot: Number(point.slot), hash });
  if (status.kind !== "canonical")
    return refused(`${status.kind}: ${status.detail}`);
  const block = await store.blockByHash(hash);
  if (block === null || !samePoint(rawPointOf(block), point))
    return refused("the point's height or id differs from the stored block");
  return ok(block);
};

const isTrackedAddress = (store: FactStore, address: CML.Address): boolean => {
  const tracked = store.trackedSet();
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

/** A tracked live row's exact bytes; a seed row has no stored creating body. */
const exactRows = async (
  tx: SqlTx,
  outRefs: readonly OutRef[],
): Promise<RawRead<FraudProofRawL1Utxo[]>> => {
  const utxos: FraudProofRawL1Utxo[] = [];
  for (const outRef of outRefs) {
    const utxo = await resolveRawUtxoIn(tx, outRef);
    if (utxo === null)
      return refused(
        `${outRefLabel(outRef)} has no stored creating body (a seed row or a pruned tx)`,
      );
    utxos.push(utxo);
  }
  return ok(utxos);
};

const STATE_QUEUE_NODE_UNIT = (policyId: string): RegExp =>
  new RegExp(
    `^${policyId}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}([0-9a-f]{56})$`,
    "u",
  );

const hex = (value: unknown): string =>
  Buffer.from(value as Uint8Array).toString("hex");

/**
 * The follower-backed raw reads for one store. `stateQueuePolicyId` names
 * the node units whose history the watcher projection records.
 */
export const createFollowerRawReads = (
  store: FactStore,
  options: Readonly<{ stateQueuePolicyId: string }>,
): FollowerRawReads => {
  const nodeUnit = STATE_QUEUE_NODE_UNIT(options.stateQueuePolicyId);

  const addressUtxosAtPoint: FollowerRawReads["addressUtxosAtPoint"] = async (
    address,
    point,
  ) => {
    const parsed = CML.Address.from_bech32(address);
    try {
      if (!isTrackedAddress(store, parsed))
        return refused(`${address} is not in the tracked set`);
      const block = await canonicalBlock(store, point);
      if (block.kind !== "ok") return block;
      const live = await store.liveUtxos(
        {
          by: "address",
          address: Buffer.from(parsed.to_raw_bytes()),
        },
        { slot: block.value.slot, hash: block.value.hash },
      );
      if (live.kind !== "ok") return refused(`${live.kind}: ${live.detail}`);
      return await store.transaction("read", (tx) =>
        exactRows(
          tx,
          live.utxos.map(({ outRef }) => outRef),
        ),
      );
    } finally {
      parsed.free();
    }
  };

  const utxosByOutRefAtPoint: FollowerRawReads["utxosByOutRefAtPoint"] = async (
    outRefs,
    point,
  ) => {
    const block = await canonicalBlock(store, point);
    if (block.kind !== "ok") return block;
    const at = block.value.slot;
    const outputs: FraudProofRawL1Utxo[] = [];
    const spends: FollowerVerifiedSpend[] = [];
    const unknown: string[] = [];
    for (const label of outRefs) {
      const outRef = parseOutRefLabel(label);
      const stored = await store.output(outRef);
      if (stored === null) {
        unknown.push(label);
        continue;
      }
      if (stored.created !== null && stored.created.slot > at) continue;
      if (stored.spent !== null && stored.spent.slot <= at) {
        const spender = await store.blockAtOrBeforeSlot(stored.spent.slot);
        if (spender === null || spender.slot !== stored.spent.slot)
          return refused(`the block spending ${label} is not stored`);
        spends.push({
          outRef: label,
          spendingTxHash: stored.spent.txHash.toString("hex"),
          spendPoint: rawPointOf(spender),
        });
        continue;
      }
      const exact = await store.transaction("read", (tx) =>
        exactRows(tx, [outRef]),
      );
      if (exact.kind !== "ok") return exact;
      outputs.push(...exact.value);
    }
    return ok({ outputs, spends, unknown });
  };

  const unitHistoryAtPoint: FollowerRawReads["unitHistoryAtPoint"] = async (
    unit,
    point,
  ) => {
    const match = nodeUnit.exec(unit);
    if (match === null)
      return refused(`no projection records the history of unit ${unit}`);
    const block = await canonicalBlock(store, point);
    if (block.kind !== "ok") return block;
    const rows = await store.transaction("read", (tx) =>
      tx.query(
        `SELECT tx_hash, block_hash, block_height, from_slot FROM ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} WHERE header_hash = ? AND from_slot <= ? ORDER BY tx_hash`,
        [Buffer.from(match[1]!, "hex"), block.value.slot],
      ),
    );
    return ok({
      checkpoint: point,
      transactions: rows.map((row) => ({
        txHash: hex(row.tx_hash),
        inclusionPoint: rawPointOf({
          slot: Number(row.from_slot),
          hash: Buffer.from(row.block_hash as Uint8Array),
          height: Number(row.block_height),
        }),
      })),
    });
  };

  const transactionInclusion: FollowerRawReads["transactionInclusion"] = async (
    txHash,
  ) => {
    const stored = await store.txByHash(Buffer.from(txHash, "hex"));
    if (stored === null) return ok(null);
    const block = await store.blockAtOrBeforeSlot(stored.blockSlot);
    if (block === null || block.slot !== stored.blockSlot)
      return refused(`the block holding ${txHash} is not stored`);
    return ok(rawPointOf(block));
  };

  const rawTransaction: FollowerRawReads["rawTransaction"] = async (
    txHash,
    expectedInclusionPoint,
  ) => {
    const stored = await store.txByHash(Buffer.from(txHash, "hex"));
    if (stored === null) return refused(`${txHash} is not stored`);
    const block = await store.blockAtOrBeforeSlot(stored.blockSlot);
    if (block === null || !samePoint(rawPointOf(block), expectedInclusionPoint))
      return refused(`${txHash} is not stored at the expected point`);
    if (!stored.isValid)
      return refused(`transaction ${txHash} is phase-2 invalid`);
    const cursor = await store.cursor();
    if (cursor === null) return refused("the store has no cursor");
    const witness = CML.TransactionWitnessSet.from_cbor_bytes(
      stored.witnessCbor,
    );
    const redeemersCbor = witness.redeemers()?.to_canonical_cbor_hex() ?? null;
    witness.free();
    const resolveAll = (outRefs: readonly OutRef[]) =>
      store.transaction("read", async (tx) => {
        const resolved: FraudProofRawL1Utxo[] = [];
        const unresolved: string[] = [];
        for (const outRef of outRefs) {
          const utxo = await resolveRawUtxoIn(tx, outRef);
          if (utxo === null) unresolved.push(outRefLabel(outRef));
          else resolved.push(utxo);
        }
        return { resolved, unresolved };
      });
    const inputs = await resolveAll(stored.inputs);
    const references = await resolveAll(stored.referenceInputs);
    return ok({
      transaction: {
        txHash,
        bodyCbor: stored.bodyCbor.toString("hex"),
        witnessSetCbor: stored.witnessCbor.toString("hex"),
        redeemersCbor,
        isValid: true,
        inclusionPoint: expectedInclusionPoint,
        confirmationDepth: depth(cursor.height, block.height),
        resolvedInputs: inputs.resolved,
        resolvedReferenceInputs: references.resolved,
      },
      unresolvedInputs: inputs.unresolved,
      unresolvedReferenceInputs: references.unresolved,
    });
  };

  const predecessorPoint: FollowerRawReads["predecessorPoint"] = async (
    point,
  ) => {
    const block = await canonicalBlock(store, point);
    if (block.kind !== "ok") return block;
    if (block.value.parentHash === null)
      return refused("the block has no stored predecessor");
    const parent = await store.blockByHash(block.value.parentHash);
    if (parent === null) return refused("the predecessor block is not stored");
    return ok(rawPointOf(parent));
  };

  return {
    addressUtxosAtPoint,
    utxosByOutRefAtPoint,
    unitHistoryAtPoint,
    transactionInclusion,
    rawTransaction,
    predecessorPoint,
  };
};
