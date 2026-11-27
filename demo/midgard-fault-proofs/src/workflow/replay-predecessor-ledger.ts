import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { requireLedgerOpening } from "../ledger-at.js";
import type { Ledger } from "../ledger-store/index.js";

/**
 * The previous ledger a predecessor-relative family replays the challenged
 * block against: the admitted predecessor ledger, or the empty genesis ledger
 * when the challenged header commits it.
 *
 * The challenged header alone decides which ledger applies. A header that
 * commits a non-empty `prev_utxos_root` is refused without a ledger, and a
 * ledger with any other root is refused, so neither an omitted nor
 * a substituted ledger can change the verdict.
 */
export const requireReplayPredecessorLedger = ({
  block,
  predecessor,
  label,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor: Ledger | undefined;
  readonly label: string;
}): Ledger =>
  requireLedgerOpening({
    ledger: predecessor,
    utxosRoot: block.header.prevUtxosRoot,
    label: `${label} predecessor ledger`,
  });
