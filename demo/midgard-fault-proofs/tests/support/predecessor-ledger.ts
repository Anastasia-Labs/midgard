/**
 * Test sources for the ledger after a fixture block, read from the fixture
 * ledger its builder recorded; the production code under test admits it only
 * through the root check.
 */
import type { CanonicalBlockEvidence } from "../../src/evidence/canonical-block-evidence.js";
import {
  type CheckedLedger,
  checkLedgerAgainstRoot,
  type LedgerAt,
  type LedgerEntry,
  singleLedgerAt,
} from "../../src/ledger-at.js";
import {
  reconstructDaPayload,
  type TransitionTraceReconstruction,
} from "../../src/transition-trace/reconstruct.js";
import { fixtureLedgerEntriesByRoot } from "./fixture-ledger.js";

type Block = CanonicalBlockEvidence | TransitionTraceReconstruction;

export const ledgerEntriesAfter = (block: Block): LedgerEntry[] =>
  fixtureLedgerEntriesByRoot(block.header.utxosRoot);

/** A `LedgerAt` that holds only the ledger after `block`. */
export const ledgerAtAfter = (block: Block): LedgerAt =>
  singleLedgerAt(block.headerHash, ledgerEntriesAfter(block));

/**
 * A `LedgerAt` holding the ledger after each of `blocks`: a replay context
 * reads both the predecessor's and the challenged block's.
 */
export const ledgerAtAcross = (...blocks: readonly Block[]): LedgerAt => {
  const ledgers = new Map(
    blocks.map((block) => [block.headerHash, ledgerEntriesAfter(block)]),
  );
  return async (headerHash) => ledgers.get(headerHash);
};

/** The ledger after `block`, checked against its `utxos_root`. */
export const checkedLedgerAfter = async (
  block: Block,
): Promise<CheckedLedger> =>
  await checkLedgerAgainstRoot({
    entries: ledgerEntriesAfter(block),
    utxosRoot: block.header.utxosRoot,
    label: "test ledger",
  });

/** A `LedgerAt` that holds only the ledger after the block in `payloadEnvelopeCbor`. */
export const payloadLedgerAt =
  (payloadEnvelopeCbor: Uint8Array): LedgerAt =>
  async (headerHash) => {
    const block = await reconstructDaPayload({ payloadEnvelopeCbor });
    return block.headerHash === headerHash
      ? ledgerEntriesAfter(block)
      : undefined;
  };
