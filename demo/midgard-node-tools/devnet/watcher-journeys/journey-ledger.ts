import type { LedgerEntry } from "@al-ft/midgard-fault-proofs/ledger-at";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKDa from "@al-ft/midgard-sdk/da";
import { Effect, Either } from "effect";

import type { JourneyBlock } from "./fixture.js";

/** A block's ledger as a journey holds it: the store ledger the fault-proof
 * readers take, and its entries for the fixture builders that compose the
 * next block. */
export type JourneyLedger = {
  ledger: Ledger;
  ledgerEntries: readonly LedgerEntry[];
};

/** An input-only retained block paired with its separately checked replay state. */
export type JourneyLedgerPredecessor = JourneyLedger & {
  header: SDK.Header;
  headerHash: string;
  payload: SDKDa.DaPayload;
  payloadEnvelopeCbor: Uint8Array;
};

/** `ledger` with the entries it lists. */
export const journeyLedger = async (ledger: Ledger): Promise<JourneyLedger> => {
  const ledgerEntries: LedgerEntry[] = [];
  for await (const entry of ledger.entries()) ledgerEntries.push(entry);
  return { ledger, ledgerEntries };
};

/** String pairs for fixture builders; these are never serialized into retained DA. */
export const journeyLedgerPairs = (
  entries: readonly LedgerEntry[],
): SDKDa.DaPayloadEntry[] =>
  entries.map(({ outRef, output }) => [
    outRef.toString("hex"),
    output.toString("hex"),
  ]);

/**
 * Admit a fixture's header commitments, as the node checks them before it
 * commits a header, and its explicitly computed claimed state before composing
 * another step.
 */
export const checkedJourneyConstruction = async <
  T extends { header: SDK.Header },
>(
  block: T,
  entries: readonly LedgerEntry[],
): Promise<T & JourneyLedger> => {
  const commitments = Effect.runSync(
    Effect.either(SDK.validateHeaderTransitionCommitmentsProgram(block.header)),
  );
  if (Either.isLeft(commitments))
    throw new Error(
      `journey constructed header: ${commitments.left.message} (${String(commitments.left.cause)})`,
    );
  return {
    ...block,
    ledger: await createLedgerStore(createMemoryLedgerStorage()).admit(
      entries,
      block.header.utxosRoot,
    ),
    ledgerEntries: entries,
  };
};

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
