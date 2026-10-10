/**
 * The node row a projected event decodes into, with its follower admission
 * identity. Decoding throws for an event the node cannot represent; the
 * ingestion refuses that event by name (`l1_event_undecodable`).
 */
import type { ProjectedEvent } from "@al-ft/midgard-l1-follower/events";
import type { Statement } from "@effect/sql";
import type { Network } from "@lucid-evolution/lucid";

import { userEventEntry } from "../l1-events/entries.js";
import * as Deposits from "./deposits.js";
import * as Withdrawals from "./withdrawals.js";

/** The 34-byte admission outref: tx hash || u16 big-endian output index. */
export const admissionOutRefBytes = (
  outRef: ProjectedEvent["admission"]["outRef"],
): Buffer => {
  const index = Buffer.alloc(2);
  index.writeUInt16BE(outRef.index);
  return Buffer.concat([Buffer.from(outRef.txHash), index]);
};

/** The follower admission identity a projected event's row carries. */
export const identityOf = (event: ProjectedEvent) => ({
  l1_event_key: Buffer.from(event.key, "hex"),
  l1_origin_outref: admissionOutRefBytes(event.admission.outRef),
});

/** The node row of a projected event (ruling 2: a deposit's L1 tx hash is its admission tx). */
export const rowOf = (
  event: ProjectedEvent,
  network: Network,
): Readonly<Record<string, Statement.Argument>> => {
  const decoded = userEventEntry(event, network);
  const identity = identityOf(event);
  if (decoded.kind === "deposit") {
    const entry = decoded.entry;
    return {
      [Deposits.Columns.ID]: Buffer.from(entry.idCbor, "hex"),
      [Deposits.Columns.INFO]: Buffer.from(entry.infoCbor, "hex"),
      [Deposits.Columns.INCLUSION_TIME]: new Date(entry.inclusionTimeMs),
      [Deposits.Columns.DEPOSIT_L1_TX_HASH]: Buffer.from(
        event.admission.outRef.txHash,
      ),
      [Deposits.Columns.LEDGER_TX_ID]: Buffer.from(entry.ledgerTxId, "hex"),
      [Deposits.Columns.LEDGER_OUTPUT]: Buffer.from(entry.ledgerOutput, "hex"),
      [Deposits.Columns.LEDGER_ADDRESS]: entry.ledgerAddress,
      [Deposits.Columns.PROJECTED_HEADER_HASH]: null,
      [Deposits.Columns.STATUS]: Deposits.Status.Awaiting,
      ...identity,
    };
  }
  const entry = decoded.entry;
  return {
    [Withdrawals.Columns.ID]: Buffer.from(entry.idCbor, "hex"),
    [Withdrawals.Columns.RAW_EVENT_INFO]: Buffer.from(
      entry.rawEventInfo,
      "hex",
    ),
    [Withdrawals.Columns.SETTLEMENT_EVENT_INFO]: null,
    [Withdrawals.Columns.INCLUSION_TIME]: new Date(entry.inclusionTimeMs),
    [Withdrawals.Columns.WITHDRAWAL_L1_TX_HASH]: Buffer.from(
      entry.l1TxHash,
      "hex",
    ),
    [Withdrawals.Columns.WITHDRAWAL_L1_OUTPUT_INDEX]: entry.l1OutputIndex,
    [Withdrawals.Columns.ASSET_NAME]: Buffer.from(entry.assetName, "hex"),
    [Withdrawals.Columns.L2_OUTREF]: Buffer.from(entry.l2Outref, "hex"),
    [Withdrawals.Columns.L2_OWNER]: Buffer.from(entry.l2Owner, "hex"),
    [Withdrawals.Columns.L2_VALUE]: Buffer.from(entry.l2Value, "hex"),
    [Withdrawals.Columns.L1_ADDRESS]: Buffer.from(entry.l1Address, "hex"),
    [Withdrawals.Columns.L1_DATUM]: Buffer.from(entry.l1Datum, "hex"),
    [Withdrawals.Columns.REFUND_ADDRESS]: Buffer.from(
      entry.refundAddress,
      "hex",
    ),
    [Withdrawals.Columns.REFUND_DATUM]: Buffer.from(entry.refundDatum, "hex"),
    [Withdrawals.Columns.VALIDITY]: null,
    [Withdrawals.Columns.CLASSIFICATION_REVISION]: 0,
    [Withdrawals.Columns.REOPENED_FROM_HEADER_HASH]: null,
    [Withdrawals.Columns.PROJECTED_HEADER_HASH]: null,
    [Withdrawals.Columns.STATUS]: Withdrawals.Status.Awaiting,
    ...identity,
  };
};
