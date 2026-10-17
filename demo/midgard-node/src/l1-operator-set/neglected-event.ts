/**
 * The undelivered user event a strike for inactivity cites, read from the
 * follower's projections at its tip (NC14): the earliest live deposit and
 * withdrawal (`node_l1_events`) and tx order (`node_l1_forced_order_fields`)
 * whose inclusion time is after the state-queue tail's end time, by index
 * reads in inclusion order, never by walking a list. The SDK's
 * `selectNeglectedUserEvent` picks among the three. Null means no due user
 * event: the shift's operator cannot be struck.
 *
 * Only what the chain would admit is cited: each candidate's live output must
 * pass the scheduler's own authentication (`SDK.citableNeglectedUserEvent`),
 * and a candidate that does not, or that the caller excludes (a citation a
 * strike already failed on), is passed over for the next one. The scan reads
 * at most `NEGLECTED_EVENT_SCAN_LIMIT` candidates of each kind, so one bad
 * candidate never hides a valid later one and no read is unbounded.
 */
import {
  type Dialect,
  liveUtxosIn,
  type SqlTx,
} from "@al-ft/midgard-l1-follower";
import {
  type EventProjectionConfig,
  EVENTS_TABLE,
} from "@al-ft/midgard-l1-follower/events";
import { addressText, toLucidUtxo } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";

import type { ForcedOrderConfig } from "../forced-orders/config.js";
import { FORCED_ORDERS_TABLE } from "../forced-orders/schema.js";

/**
 * The candidates of each kind one read considers, in inclusion order. It
 * exceeds the watchdog's per-state exclusions (`MAX_EXCLUDED_CITATIONS`), so
 * a full exclusion set still leaves candidates to read.
 */
export const NEGLECTED_EVENT_SCAN_LIMIT = 64;

/**
 * Where the scheduler authenticates each kind of cited event: the event
 * lists' policies (whose tokens, named by event key, the Order nodes hold)
 * and addresses, and the tx-order policy.
 */
export type NeglectedEventSources = SDK.NeglectedUserEventCitationSources;

/** The sources of the follower's event and forced-order projections. */
export const neglectedEventSourcesOf = (
  projection: EventProjectionConfig,
  forcedOrders: Pick<ForcedOrderConfig, "policyId">,
): NeglectedEventSources => {
  const listOf = (kind: "deposit" | "withdrawal") => {
    const list = projection.lists.find((candidate) => candidate.kind === kind);
    if (list === undefined) throw new Error(`no ${kind} event list`);
    return {
      policyId: list.policyId,
      address: addressText(Buffer.from(list.listAddress, "hex")),
    };
  };
  return {
    deposit: listOf("deposit"),
    withdrawal: listOf("withdrawal"),
    txOrderPolicyId: forcedOrders.policyId,
  };
};

const bytes = (value: unknown): Buffer => Buffer.from(value as Uint8Array);
const int = (value: unknown): bigint =>
  BigInt(value as string | number | bigint);

/** The one live output holding `policyId.assetName`, or null. */
const soleHolder = async (
  tx: SqlTx,
  dialect: Dialect,
  policyId: string,
  assetName: Buffer,
) => {
  const read = await liveUtxosIn(tx, dialect, {
    by: "unit",
    policyId: Buffer.from(policyId, "hex"),
    assetName,
  });
  if (read.kind !== "ok") throw new Error(`event token read: ${read.detail}`);
  return read.utxos.length === 1 ? read.utxos[0]! : null;
};

type Candidate = Readonly<{
  sources: NeglectedEventSources;
  excluded: ReadonlySet<string>;
}>;

/** The first candidate the chain admits and the caller does not exclude. */
const firstCitable = async (
  rows: readonly Record<string, unknown>[],
  claimOf: (
    row: Record<string, unknown>,
  ) => Promise<SDK.NeglectedUserEventClaim | null>,
  { sources, excluded }: Candidate,
): Promise<SDK.NeglectedUserEventClaim | null> => {
  for (const row of rows) {
    const claim = await claimOf(row);
    if (
      claim !== null &&
      !excluded.has(SDK.neglectedUserEventCitationId(claim)) &&
      SDK.citableNeglectedUserEvent(claim, sources)
    )
      return claim;
  }
  return null;
};

const earliestHistoryEvent = async (
  tx: SqlTx,
  dialect: Dialect,
  kind: "deposit" | "withdrawal",
  tailEndMs: bigint,
  candidate: Candidate,
): Promise<SDK.NeglectedUserEventClaim | null> => {
  const rows = await tx.query(
    `SELECT event_key, inclusion_time FROM ${EVENTS_TABLE}
      WHERE kind = ? AND retired_slot IS NULL AND inclusion_time > ?
      ORDER BY inclusion_time, event_key LIMIT ?`,
    [kind, tailEndMs, NEGLECTED_EVENT_SCAN_LIMIT],
  );
  const { policyId } = candidate.sources[kind];
  return firstCitable(
    rows,
    async (row) => {
      // The Order node holds the list token named by the event key.
      const order = await soleHolder(
        tx,
        dialect,
        policyId,
        bytes(row.event_key),
      );
      return order === null
        ? null
        : {
            kind: kind === "deposit" ? "Deposit" : "Withdrawal",
            utxo: toLucidUtxo(order.outRef, order.output),
            inclusionTimeMs: int(row.inclusion_time),
          };
    },
    candidate,
  );
};

const earliestTxOrder = async (
  tx: SqlTx,
  dialect: Dialect,
  tailEndMs: bigint,
  candidate: Candidate,
): Promise<SDK.NeglectedUserEventClaim | null> => {
  const rows = await tx.query(
    `SELECT order_tx_hash, order_output_index, inclusion_time FROM ${FORCED_ORDERS_TABLE}
      WHERE spent_slot IS NULL AND inclusion_time > ?
      ORDER BY inclusion_time, order_tx_hash, order_output_index LIMIT ?`,
    [tailEndMs, NEGLECTED_EVENT_SCAN_LIMIT],
  );
  return firstCitable(
    rows,
    async (row) => {
      const outRef = {
        txHash: bytes(row.order_tx_hash),
        index: Number(row.order_output_index as number | string),
      };
      const read = await liveUtxosIn(tx, dialect, {
        by: "outref",
        outRefs: [outRef],
      });
      if (read.kind !== "ok") throw new Error(`tx-order read: ${read.detail}`);
      const order = read.utxos[0];
      return order === undefined
        ? null
        : {
            kind: "TxOrder",
            utxo: toLucidUtxo(order.outRef, order.output),
            inclusionTimeMs: int(row.inclusion_time),
          };
    },
    candidate,
  );
};

/**
 * The neglected user event a strike cites against a tail ending at
 * `tailEndMs`, at the follower's tip in the caller's transaction, or null
 * when no live user event after it is citable. `excluded` names citations
 * (`SDK.neglectedUserEventCitationId`) to pass over.
 */
export const neglectedUserEventIn = async (
  tx: SqlTx,
  dialect: Dialect,
  sources: NeglectedEventSources,
  tailEndMs: bigint,
  excluded: ReadonlySet<string> = new Set(),
): Promise<SDK.NeglectedUserEventClaim | null> => {
  const candidate = { sources, excluded };
  return SDK.selectNeglectedUserEvent(
    [
      await earliestHistoryEvent(tx, dialect, "deposit", tailEndMs, candidate),
      await earliestHistoryEvent(
        tx,
        dialect,
        "withdrawal",
        tailEndMs,
        candidate,
      ),
      await earliestTxOrder(tx, dialect, tailEndMs, candidate),
    ].filter((claim) => claim !== null),
    tailEndMs,
  );
};
