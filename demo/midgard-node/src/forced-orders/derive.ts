/**
 * The forced-order projection's S3 derivation (plan §12.3 steps 0 and 1).
 *
 * Per qualifying tx, in block order:
 * - a spend of a stored order closes its row (`spent_slot`);
 * - an output at the order address holding a tx-order token this valid tx
 *   mints, and authenticating as an order, opens a row. Its carriage is
 *   read from the order's own mint redeemer and from outputs earlier txs
 *   of the same block created. All of it found: `resolved`, with the nine
 *   field preimages. Some created before the block: `carriage_pending`,
 *   for the driver hook to resolve by outref. Unopenable: `malformed`.
 *
 * A pure function of the block and the config: no clock, no network. The
 * rewind of everything written here is the registry's.
 */
import {
  createdOutputs,
  type DerivationContext,
  type DerivationHook,
  encodeOutRef,
  type OutputSummary,
  type OutRef,
  type TxSummary,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  carriageFieldPreimages,
  carriageOutRefs,
  carriageVector,
  encodeFieldPreimages,
  txOrderMintRedeemer,
} from "./carriage.js";
import type { ForcedOrderConfig } from "./config.js";
import { FORCED_ORDERS_TABLE } from "./schema.js";

const hex = (bytes: Buffer): string => bytes.toString("hex");

/** An outref as a JSON key: `<tx hash hex>#<index>`. */
export const outRefLabel = (outRef: OutRef): string =>
  `${hex(outRef.txHash)}#${outRef.index.toString()}`;

/** A follower output as the SDK's UTxO. */
export const lucidUtxo = (outRef: OutRef, output: OutputSummary): UTxO => {
  const assets: Record<string, bigint> = { lovelace: output.lovelace };
  for (const [policy, names] of output.assets)
    for (const [name, quantity] of names) assets[policy + name] = quantity;
  return {
    txHash: hex(outRef.txHash),
    outputIndex: outRef.index,
    address: hex(output.address),
    assets,
    ...(output.datum === null ? {} : { datum: hex(output.datum) }),
    ...(output.datumHash === null ? {} : { datumHash: hex(output.datumHash) }),
  };
};

/** The order an output authenticates as, or null when it is not one. */
export const authenticOrder = (
  utxo: UTxO,
  policyId: string,
): SDK.TxOrderUTxOV1 | null =>
  Effect.runSync(SDK.utxosToTxOrderUTxOs([utxo], policyId))[0] ?? null;

/** The reference inputs as stored: their 34-byte outrefs, concatenated in ledger order. */
export const encodeReferenceInputs = (outRefs: readonly OutRef[]): Buffer =>
  Buffer.concat(outRefs.map(encodeOutRef));

/** Datums of outputs earlier txs of the block created, by outref label. */
const blockDatums = (
  block: readonly TxSummary[],
  before: number,
  wanted: readonly OutRef[],
): Map<string, string | null> => {
  const labels = new Set(wanted.map(outRefLabel));
  const found = new Map<string, string | null>();
  for (const tx of block) {
    if (tx.index >= before) break;
    for (const { outRef, output } of createdOutputs(tx)) {
      const label = outRefLabel(outRef);
      if (labels.has(label))
        found.set(label, output.datum === null ? null : hex(output.datum));
    }
  }
  return found;
};

type Row = Readonly<{
  status: "resolved" | "carriage_pending" | "malformed";
  mintRedeemer: Buffer | null;
  blockDatums: Readonly<Record<string, string | null>>;
  fieldPreimages: Buffer | null;
  detail: string | null;
}>;

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/** The row of an authenticated order created by `tx` (steps 0 and 1). */
export const resolveInBlock = (
  context: Pick<DerivationContext, "block">,
  tx: TxSummary,
  order: SDK.TxOrderUTxOV1,
  policyId: string,
): Row => {
  const redeemer = txOrderMintRedeemer(tx, policyId);
  const malformed = (detail: string, mintRedeemer: Buffer | null): Row => ({
    status: "malformed",
    mintRedeemer,
    blockDatums: {},
    fieldPreimages: null,
    detail: detail.slice(0, 500),
  });
  if (redeemer === null)
    return malformed("the order tx carries no tx-order mint redeemer", null);
  try {
    const carriage = carriageVector(redeemer.data);
    const wanted = carriageOutRefs(carriage, tx.referenceInputs);
    const found = blockDatums(context.block.txs, tx.index, wanted);
    const datums = Object.fromEntries(found);
    if (found.size < wanted.length)
      return {
        status: "carriage_pending",
        mintRedeemer: redeemer.data,
        blockDatums: datums,
        fieldPreimages: null,
        detail: null,
      };
    const preimages = carriageFieldPreimages({
      payload: order.datum.event.tx,
      carriage,
      referenceInputs: tx.referenceInputs,
      datumOf: (outRef) => {
        const datum = found.get(outRefLabel(outRef));
        return datum == null ? null : Buffer.from(datum, "hex");
      },
    });
    return {
      status: "resolved",
      mintRedeemer: redeemer.data,
      blockDatums: datums,
      fieldPreimages: encodeFieldPreimages(preimages),
      detail: null,
    };
  } catch (error) {
    return malformed(message(error), redeemer.data);
  }
};

const mintsName = (tx: TxSummary, policyId: string, name: string): boolean =>
  (tx.mint.get(policyId)?.get(name) ?? 0n) > 0n;

/** The S3 derivation of the node's forced orders. */
export const forcedOrderDerivation = (
  config: ForcedOrderConfig,
): DerivationHook => ({
  name: "node_l1_forced_orders",
  writes: [FORCED_ORDERS_TABLE],
  apply: async (context) => {
    const { block, previous } = context;
    for (const { tx, created, spent } of context.qualified) {
      for (const outRef of spent)
        await context.tx.query(
          `UPDATE ${FORCED_ORDERS_TABLE} SET spent_slot = ? WHERE order_tx_hash = ? AND order_output_index = ? AND spent_slot IS NULL`,
          [block.point.slot, outRef.txHash, outRef.index],
        );
      if (!tx.isValid || !tx.mint.has(config.policyId)) continue;
      for (const entry of created) {
        if (hex(entry.output.address) !== config.orderAddress) continue;
        const names = entry.output.assets.get(config.policyId);
        if (names === undefined) continue;
        if (![...names.keys()].some((n) => mintsName(tx, config.policyId, n)))
          continue;
        const order = authenticOrder(
          lucidUtxo(entry.outRef, entry.output),
          config.policyId,
        );
        if (order === null) continue;
        const row = resolveInBlock(context, tx, order, config.policyId);
        await context.tx.query(
          `INSERT INTO ${FORCED_ORDERS_TABLE} (order_tx_hash, order_output_index, order_tx_index, block_hash, height, order_slot, spent_slot, parent_slot, parent_hash, inclusion_time, status, reference_inputs, mint_redeemer, block_datums, field_preimages, detail) VALUES (?, ?, ?, ?, ?, ?, NULL, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
          [
            entry.outRef.txHash,
            entry.outRef.index,
            tx.index,
            block.point.hash,
            block.height,
            block.point.slot,
            previous.point.slot,
            previous.point.hash,
            order.datum.inclusion_time.toString(),
            row.status,
            encodeReferenceInputs(tx.referenceInputs),
            row.mintRedeemer,
            JSON.stringify(row.blockDatums),
            row.fieldPreimages,
            row.detail,
          ],
        );
      }
    }
  },
});
