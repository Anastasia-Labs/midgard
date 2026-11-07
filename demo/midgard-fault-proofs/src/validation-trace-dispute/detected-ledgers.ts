/** The pre-state ledgers the validation-trace detector walked to for the
 * events it found failing, by the evidence object it replayed. Claim
 * preparation and artifact validation of that same evidence read the ledger
 * before their event here instead of walking the block's steps from the
 * predecessor again. For evidence the detector did not replay in this
 * process, or an event it did not find failing, they walk as before. Each
 * ledger is the walk's frozen ledger before exactly that event, so the event
 * position it serves is the one it was built for. */
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CheckedLedger } from "../ledger-at.js";

/** Ledgers kept per block: the earliest failing events found. */
const KEPT_PER_BLOCK = 4;

type Kept = {
  /** The predecessor root the ledgers were walked from. */
  readonly predecessorRoot: string | undefined;
  /** By event fingerprint. */
  readonly ledgers: Map<string, CheckedLedger>;
};

/** Held weakly: released with the evidence object. */
const kept = new WeakMap<CanonicalBlockEvidence, Kept>();

export const keepDetectedLedger = ({
  evidence,
  predecessor,
  fingerprint,
  ledger,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly predecessor: CheckedLedger | undefined;
  readonly fingerprint: string;
  readonly ledger: CheckedLedger;
}): void => {
  let block = kept.get(evidence);
  if (block === undefined || block.predecessorRoot !== predecessor?.utxosRoot) {
    block = { predecessorRoot: predecessor?.utxosRoot, ledgers: new Map() };
    kept.set(evidence, block);
  }
  if (block.ledgers.has(fingerprint) || block.ledgers.size < KEPT_PER_BLOCK)
    block.ledgers.set(fingerprint, ledger);
};

/** The ledger the detector walked to before `fingerprint`'s event of
 * `evidence` from `predecessor`, if it kept one. */
export const detectedLedger = ({
  evidence,
  predecessor,
  fingerprint,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly predecessor: CheckedLedger | undefined;
  readonly fingerprint: string;
}): CheckedLedger | undefined => {
  const block = kept.get(evidence);
  return block?.predecessorRoot === predecessor?.utxosRoot
    ? block?.ledgers.get(fingerprint)
    : undefined;
};
