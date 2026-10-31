import * as SDK from "@al-ft/midgard-sdk";

import type {
  DaStoredPayloadRecord,
  StateQueueHeaderRecord,
} from "../domain.js";
import { stateQueueNodeOf } from "../l1/follower/queue-derivation.js";

/** A known header uses its stored L1 node; only an unobserved payload uses receipt time. */
export const retentionBlockTimeMs = (
  payload: Pick<DaStoredPayloadRecord, "fetchedAt">,
  header: StateQueueHeaderRecord | undefined,
): number => {
  if (header !== undefined) {
    return storedHeaderBlockTimeMs(header);
  }
  const fetchedAtMs = Date.parse(payload.fetchedAt);
  return Number.isSafeInteger(fetchedAtMs) && fetchedAtMs >= 0
    ? fetchedAtMs
    : 0;
};

/** The commit clock carried by this record's exact authenticated L1 node. */
export const storedHeaderBlockTimeMs = (
  header: StateQueueHeaderRecord,
): number => {
  const node =
    header.rawStateQueueDatumCbor === undefined
      ? null
      : stateQueueNodeOf(header.rawStateQueueDatumCbor);
  if (
    node === null ||
    !SDK.encodeHeaderCbor(node.header).equals(
      SDK.encodeHeaderCbor(header.header),
    )
  )
    throw new Error("Retained header has no authenticated block_time");
  const blockTimeMs = Number(node.block_time);
  if (!Number.isSafeInteger(blockTimeMs) || blockTimeMs < 0)
    throw new Error("Retained header block_time is not a safe timestamp");
  return blockTimeMs;
};
