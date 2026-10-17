import {
  type CheckedLedger,
  checkLedgerAgainstRoot,
  type LedgerEntry,
} from "@al-ft/midgard-fault-proofs";
import type * as SDK from "@al-ft/midgard-sdk";

import type { JourneyBlock } from "./fixture.js";

/** An input-only retained block paired with its separately checked replay state. */
export type JourneyLedgerPredecessor = {
  header: SDK.Header;
  headerHash: string;
  payload: SDK.DaPayload;
  payloadEnvelopeCbor: Uint8Array;
  ledger: CheckedLedger;
};

/** String pairs for fixture builders; these are never serialized into retained DA. */
export const journeyLedgerPairs = (
  ledger: CheckedLedger,
): SDK.DaPayloadEntry[] =>
  ledger.entries.map(({ outRef, output }) => [
    outRef.toString("hex"),
    output.toString("hex"),
  ]);

/** Admit a fixture's explicitly computed claimed state before composing another step. */
export const checkedJourneyConstruction = async <
  T extends { header: SDK.Header },
>(
  block: T,
  entries: readonly LedgerEntry[],
): Promise<T & { ledger: CheckedLedger }> => ({
  ...block,
  ledger: await checkLedgerAgainstRoot({
    entries,
    utxosRoot: block.header.utxosRoot,
    label: "journey constructed ledger",
  }),
});

/** Checkpoints retain consensus bytes; checked replay state belongs to the keeper. */
export const persistableJourneyBlock = (block: JourneyBlock): JourneyBlock => ({
  header: block.header,
  headerHash: block.headerHash,
  payloadEnvelopeCbor: Buffer.from(block.payloadEnvelopeCbor),
});

export const journeyBlockBuilder =
  <T extends unknown[]>(
    build: (...args: T) => Promise<JourneyBlock>,
  ): ((...args: T) => Promise<JourneyBlock>) =>
  async (...args) =>
    persistableJourneyBlock(await build(...args));
