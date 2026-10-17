import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type CheckedLedger, requireLedgerOpening } from "../ledger-at.js";

/**
 * The previous ledger a predecessor-relative family replays the challenged
 * block against: the admitted predecessor ledger, or the empty genesis ledger
 * when the challenged header commits it.
 *
 * The challenged header alone decides which ledger applies. A header that
 * commits a non-empty `prev_utxos_root` is refused without a ledger, and a
 * ledger checked against any other root is refused, so neither an omitted nor
 * a substituted ledger can change the verdict.
 */
export const requireReplayPredecessorLedger = ({
  block,
  predecessor,
  label,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor: CheckedLedger | undefined;
  readonly label: string;
}): CheckedLedger =>
  requireLedgerOpening({
    ledger: predecessor,
    utxosRoot: block.header.prevUtxosRoot,
    label: `${label} predecessor ledger`,
  });

/** The previous ledger's outputs keyed by canonical out-ref bytes (hex). */
export const replayPredecessorOutputs = (
  ledger: CheckedLedger,
): ReadonlyMap<string, Uint8Array> => ledger.outputs;
