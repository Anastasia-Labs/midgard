import { FraudProofL1UnavailableError } from "@al-ft/midgard-fault-proofs";
import type { FactStore } from "@al-ft/midgard-l1-follower";
import { eventKeyOfId, EVENTS_TABLE } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import type { WatcherStateQueueHeaderObservation } from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherUserEventKind } from "../verification/user-event.js";
import { WATCHER_UNIT_HISTORY_TABLE } from "./tables.js";
import type { WatcherUserEventScripts } from "./user-events.js";

/** Enumerate all due admissions, including retired orders, through the commit. */
export const replayEventIds = async (
  input: Readonly<{
    store: FactStore;
    scripts: WatcherUserEventScripts;
    header: WatcherStateQueueHeaderObservation;
    payload: SDK.DaPayload;
  }>,
) => {
  const commit = await input.store.txByHash(
    Buffer.from(input.header.observedTransactionHash, "hex"),
  );
  if (commit === null)
    throw new FraudProofL1UnavailableError(
      "replay commit transaction was pruned",
    );
  const { startTime, endTime } = input.payload.block_body.header;
  const slot = Number(input.header.observedSlot);
  const due = new Map<WatcherUserEventKind, Set<string>>([
    ["deposit", new Set()],
    ["withdrawal", new Set()],
    ["forced_order", new Set()],
  ]);
  const rows = await input.store.transaction("read", (tx) =>
    tx.query(
      `SELECT kind, event_id FROM ${EVENTS_TABLE} WHERE inclusion_time > ? AND inclusion_time <= ? AND (admitted_slot < ? OR (admitted_slot = ? AND admission_tx_index <= ?))`,
      [
        startTime.toString(),
        endTime.toString(),
        slot,
        slot,
        commit.blockTxIndex,
      ],
    ),
  );
  for (const row of rows)
    due
      .get(row.kind as "deposit" | "withdrawal")!
      .add(Buffer.from(row.event_id as Uint8Array).toString("hex"));
  const policy = Buffer.from(input.scripts.forcedOrderPolicyId, "hex");
  const txs = await input.store.transaction("read", (tx) =>
    tx.query(
      `SELECT DISTINCT tx_hash FROM ${WATCHER_UNIT_HISTORY_TABLE} WHERE from_slot <= ? AND substr(unit, 1, 28) = ?`,
      [slot, policy],
    ),
  );
  for (const row of txs) {
    const tx = await input.store.txByHash(
      Buffer.from(row.tx_hash as Uint8Array),
    );
    if (tx === null)
      throw new FraudProofL1UnavailableError(
        "forced replay admission transaction was pruned",
      );
    if (
      !tx.isValid ||
      (tx.blockSlot === slot && tx.blockTxIndex > commit.blockTxIndex)
    )
      continue;
    const outputs = CML.TransactionBody.from_cbor_bytes(tx.bodyCbor).outputs();
    for (let index = 0; index < outputs.len(); index += 1) {
      const output = outputs.get(index);
      if (output.address().to_hex() !== input.scripts.forcedOrderAddressHex)
        continue;
      const names = output
        .amount()
        .multi_asset()
        .get_assets(CML.ScriptHash.from_hex(input.scripts.forcedOrderPolicyId));
      if (names === undefined) continue;
      const datumCbor = output.datum()?.as_datum()?.to_cbor_hex();
      if (datumCbor === undefined)
        throw new Error("authenticated forced order has no inline datum");
      const datum = Data.from(datumCbor, SDK.TxOrderDatum);
      const id = Data.to(datum.event.id, SDK.OutputReference);
      const name = eventKeyOfId(Buffer.from(id, "hex")).toString("hex");
      if (
        names.get(CML.AssetName.from_hex(name)) !== 1n ||
        (tx.mint.get(input.scripts.forcedOrderPolicyId)?.get(name) ?? 0n) !== 1n
      )
        continue;
      if (datum.inclusion_time > startTime && datum.inclusion_time <= endTime)
        due.get("forced_order")!.add(id);
    }
  }
  const body = input.payload.block_body;
  for (const [kind, named] of [
    ["deposit", body.deposits],
    ["withdrawal", body.withdrawals],
    ["forced_order", body.forced_transactions],
  ] as const) {
    const ids = named.map(([id]) => id);
    const expected = due.get(kind)!;
    if (
      new Set(ids).size !== ids.length ||
      ids.length !== expected.size ||
      ids.some((id) => !expected.has(id))
    )
      throw new Error(
        `${kind} replay sources differ from the complete authenticated admission set`,
      );
  }
  return due;
};
