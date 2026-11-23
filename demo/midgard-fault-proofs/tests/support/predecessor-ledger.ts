/**
 * Test sources for the ledger after a fixture block, read from the fixture
 * ledger its builder recorded and admitted to a memory store against the
 * root the test names; the production code under test binds what it gets to
 * the authenticated header's root.
 */
import type { CanonicalBlockEvidence } from "../../src/evidence/canonical-block-evidence.js";
import { type LedgerAt, type LedgerEntry } from "../../src/ledger-at.js";
import type { Ledger } from "../../src/ledger-store/index.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.reconstruct-da-payload.js";
import type { TransitionTraceReconstruction } from "../../src/transition-trace/reconstruct.transition-trace-reconstruction.js";
import { fixtureLedgerEntriesByRoot, memoryLedger } from "./fixture-ledger.js";

type Block = CanonicalBlockEvidence | TransitionTraceReconstruction;

export const ledgerEntriesAfter = (block: Block): LedgerEntry[] =>
  fixtureLedgerEntriesByRoot(block.header.utxosRoot);

/** A `LedgerAt` holding exactly one block's ledger: `entries` admitted
 * against `root`, once. */
export const singleLedgerAt = (
  headerHash: string,
  entries: readonly LedgerEntry[],
  root: string,
): LedgerAt => {
  let ledger: Promise<Ledger> | undefined;
  return async (requested) =>
    requested === headerHash
      ? await (ledger ??= memoryLedger(entries, root))
      : undefined;
};

/** A `LedgerAt` that holds only the ledger after `block`. */
export const ledgerAtAfter = (block: Block): LedgerAt =>
  singleLedgerAt(
    block.headerHash,
    ledgerEntriesAfter(block),
    block.header.utxosRoot,
  );

/**
 * A `LedgerAt` holding the ledger after each of `blocks`: a replay context
 * reads both the predecessor's and the challenged block's.
 */
export const ledgerAtAcross = (...blocks: readonly Block[]): LedgerAt => {
  const sources = new Map(
    blocks.map((block) => [block.headerHash, ledgerAtAfter(block)]),
  );
  return async (headerHash) => await sources.get(headerHash)?.(headerHash);
};

/** The ledger after `block` in a memory store of its own, admitted against
 * its `utxos_root`. */
export const ledgerAfter = async (block: Block): Promise<Ledger> =>
  await memoryLedger(ledgerEntriesAfter(block), block.header.utxosRoot);

/** A `LedgerAt` that holds only the ledger after the block in `payloadEnvelopeCbor`. */
export const payloadLedgerAt =
  (payloadEnvelopeCbor: Uint8Array): LedgerAt =>
  async (headerHash) => {
    const block = await reconstructDaPayload({ payloadEnvelopeCbor });
    return block.headerHash === headerHash
      ? await ledgerAfter(block)
      : undefined;
  };
