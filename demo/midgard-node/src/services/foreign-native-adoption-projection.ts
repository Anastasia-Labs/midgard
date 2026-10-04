import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";

import type * as Ledger from "../database/utils/ledger.js";
import type { PersistedNativeMpfReplay } from "./mpf-native-owner/protocol.js";
import {
  digest,
  EVENT_LOG_DIGEST_DOMAIN,
  EVENT_LOG_HEADER_BYTES,
} from "./mpf-native-owner/service.normalize-owner-options.js";

export type AdoptionLedgerRow = Readonly<{
  tx_id: string;
  outref: string;
  output: string;
  address: string;
  source_event_id?: string;
}>;
export const foreignAdoptionRowsForKeys = (
  keys: readonly Buffer[],
  entries: readonly Ledger.MinimalEntry[],
) => {
  const canonical = new Map(
    entries.map((entry) => [entry.outref.toString("hex"), entry.output]),
  );
  return keys.flatMap((key): AdoptionLedgerRow[] => {
    const output = canonical.get(key.toString("hex"));
    if (output === undefined) return [];
    return [
      {
        tx_id: Buffer.from(decodeMidgardSpendInputItem(key).txId).toString(
          "hex",
        ),
        outref: key.toString("hex"),
        output: output.toString("hex"),
        address: encodeMidgardAddressText(
          decodeMidgardTxOutput(output).address,
        ),
      },
    ];
  });
};

/** Decode the keys of the exact owner-validated replay, retaining event order.
 * Canonical output bytes come from the independently verified final ledger.
 */
export const foreignAdoptionProjection = (
  replay: PersistedNativeMpfReplay,
  entries: readonly Ledger.MinimalEntry[],
): { keys: readonly Buffer[]; rows: readonly AdoptionLedgerRow[] } => {
  const log = Buffer.from(replay.eventLog);
  if (
    log.length < EVENT_LOG_HEADER_BYTES ||
    digest(EVENT_LOG_DIGEST_DOMAIN, log).toString("hex") !==
      replay.eventLogDigest ||
    log.readUInt32LE(8) !== replay.eventCount ||
    log.subarray(28, 60).toString("hex") !== replay.baseRoot
  )
    throw new Error("Foreign adoption replay identity is invalid");
  let offset = EVENT_LOG_HEADER_BYTES;
  let operations = 0;
  const changed = new Map<string, Buffer | null>();
  const requireBytes = (size: number) => {
    if (size > log.length - offset)
      throw new Error("Foreign adoption replay is truncated");
  };
  for (let event = 0; event < replay.eventCount; event++) {
    requireBytes(4);
    const count = log.readUInt32LE(offset);
    offset += 4;
    for (let op = 0; op < count; op++) {
      requireBytes(7);
      const kind = log.readUInt8(offset++);
      const keySize = log.readUInt16LE(offset);
      offset += 2;
      const valueSize = log.readUInt32LE(offset);
      offset += 4;
      requireBytes(keySize + valueSize);
      const key = log.subarray(offset, offset + keySize);
      offset += keySize;
      const value = log.subarray(offset, offset + valueSize);
      offset += valueSize;
      decodeMidgardSpendInputItem(key);
      if ((kind !== 1 && kind !== 2) || (kind === 2 && valueSize !== 0))
        throw new Error(
          "Foreign adoption replay contains an invalid operation",
        );
      changed.set(key.toString("hex"), kind === 1 ? Buffer.from(value) : null);
      operations++;
    }
  }
  if (offset !== log.length || operations !== log.readUInt32LE(12))
    throw new Error("Foreign adoption replay operation count differs");
  const canonical = new Map(
    entries.map((entry) => [entry.outref.toString("hex"), entry.output]),
  );
  const rows: AdoptionLedgerRow[] = [];
  for (const [key, output] of changed) {
    const final = canonical.get(key);
    if (
      output === null
        ? final !== undefined
        : final === undefined || !final.equals(output)
    )
      throw new Error(
        "Foreign adoption projection differs from the verified ledger",
      );
    if (output === null) continue;
    const input = decodeMidgardSpendInputItem(Buffer.from(key, "hex"));
    rows.push({
      tx_id: Buffer.from(input.txId).toString("hex"),
      outref: key,
      output: output.toString("hex"),
      address: encodeMidgardAddressText(decodeMidgardTxOutput(output).address),
    });
  }
  return {
    keys: [...changed.keys()].map((key) => Buffer.from(key, "hex")),
    rows,
  };
};
