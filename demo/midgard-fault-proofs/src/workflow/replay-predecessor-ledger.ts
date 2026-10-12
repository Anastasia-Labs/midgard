import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";

/**
 * The previous ledger a predecessor-relative family replays the challenged
 * block against: the classifier-admitted predecessor's reconstruction, or
 * `undefined` when the challenged header commits the empty genesis ledger.
 *
 * The challenged header alone decides which case applies. A header that
 * commits a non-empty `prev_utxos_root` is refused without its predecessor,
 * and a predecessor whose header hash or UTxO root differs from the
 * challenged header's `prev_header_hash` / `prev_utxos_root` is refused, so
 * neither an omitted nor a substituted predecessor can change the verdict.
 */
export const requireReplayPredecessorLedger = ({
  block,
  predecessor,
  label,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor: CanonicalBlockEvidence | undefined;
  readonly label: string;
}): TransitionTraceReconstruction | undefined => {
  if (predecessor === undefined) {
    if (block.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT)
      throw new Error(
        `${label} requires the authenticated predecessor of a header committing a non-empty previous ledger`,
      );
    return undefined;
  }
  if (
    predecessor.headerHash !== block.header.prevHeaderHash ||
    predecessor.header.utxosRoot !== block.header.prevUtxosRoot
  )
    throw new Error(
      `${label} predecessor differs from the challenged prev_header_hash or prev_utxos_root`,
    );
  return predecessor.reconstruction;
};

/** The previous ledger's outputs keyed by canonical out-ref bytes (hex). */
export const replayPredecessorOutputs = (
  ledger: TransitionTraceReconstruction | undefined,
): ReadonlyMap<string, Uint8Array> =>
  new Map(
    (ledger?.utxos ?? []).map(({ key, value }) => [
      Buffer.from(key).toString("hex"),
      Buffer.from(value),
    ]),
  );
