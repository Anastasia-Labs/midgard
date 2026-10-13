/**
 * Test sources for the ledger after a fixture block. Fixture payloads still
 * carry their post-state UTxO set, so a test reads the ledger from there; the
 * production code under test admits it only through the root check.
 */
import type { CanonicalBlockEvidence } from "../../src/evidence/canonical-block-evidence.js";
import {
  type CheckedLedger,
  checkLedgerAgainstRoot,
  type LedgerAt,
  type LedgerEntry,
  singleLedgerAt,
} from "../../src/ledger-at.js";
import { payloadLedgerEntries } from "../../src/retained-payload-ledger-at.js";
import {
  reconstructDaPayload,
  type TransitionTraceReconstruction,
} from "../../src/transition-trace/reconstruct.js";

type Block = CanonicalBlockEvidence | TransitionTraceReconstruction;

const reconstructionOf = (block: Block): TransitionTraceReconstruction =>
  "reconstruction" in block ? block.reconstruction : block;

export const ledgerEntriesAfter = (block: Block): LedgerEntry[] =>
  payloadLedgerEntries(reconstructionOf(block));

/** A `LedgerAt` that holds only the ledger after `block`. */
export const ledgerAtAfter = (block: Block): LedgerAt =>
  singleLedgerAt(block.headerHash, ledgerEntriesAfter(block));

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
