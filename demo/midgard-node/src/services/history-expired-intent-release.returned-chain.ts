import type * as Pending from "../database/pendingBlockFinalizations.js";
import type {
  QueueNode,
  QueueView,
} from "./history-expired-intent-release.signed-commit-node.js";
import { C } from "./history-expired-intent-release.table.js";
import { signedTxSpends } from "./state-queue-correction-rewind.prove-unlanded.js";

export type LandedReplacement = Readonly<{
  record: Pending.Record;
  node: QueueNode | undefined;
  queued: boolean;
  evidence: string;
}>;

/** Multiple replacement journals may have returned together: ancestor first,
 * then its child. Only an exact authenticated checkpoint chain proves they
 * coexist; DB ancestry or historical landing hints alone do not. Revive just
 * its head. The existing active-journal gate serializes local finalization
 * before the next abandoned descendant can be revived. */
export const returnedReplacementChain = (
  found: readonly LandedReplacement[],
  queue: QueueView,
  manifestId: string,
): readonly LandedReplacement[] | undefined => {
  const byHeader = new Map(
    found.map((candidate) => [
      candidate.record[C.HEADER_HASH].toString("hex"),
      candidate,
    ]),
  );
  if (byHeader.size !== found.length) return undefined;
  const heads = found.filter(
    ({ record }) =>
      !byHeader.has(record[C.BASE_TAIL_HEADER_HASH].toString("hex")),
  );
  if (heads.length !== 1) return undefined;
  const ordered: LandedReplacement[] = [];
  let candidate: LandedReplacement | undefined = heads[0];
  while (candidate !== undefined) {
    const record: Pending.Record = candidate.record;
    const node: QueueNode | undefined = candidate.node;
    const header = record[C.HEADER_HASH].toString("hex");
    const base = record[C.BASE_TAIL_HEADER_HASH].toString("hex");
    const signed = record[C.SIGNED_TX_CBOR];
    const intended = record[C.INTENDED_TX_HASH];
    const replay = record.nativeMpfReplay;
    const parent = queue.nodes.find((entry) => entry.headerHash === base);
    const prior = ordered.at(-1)?.record;
    if (
      ordered.includes(candidate) ||
      node === undefined ||
      !candidate.queued ||
      !queue.nodes.includes(node) ||
      node === queue.root ||
      node.headerHash !== header ||
      node.prevHeaderHash !== base ||
      parent === undefined ||
      parent.node.datum.next === "Empty" ||
      parent.node.datum.next.Key.key !== header ||
      record[C.DEPLOYMENT_MANIFEST_ID] !== manifestId ||
      signed == null ||
      intended == null ||
      !signedTxSpends(signed, intended, record[C.BASE_TAIL_OUT_REF]) ||
      replay === undefined ||
      replay.baseRoot.toString("hex") !== record[C.BASE_UTXOS_ROOT] ||
      replay.candidateRoot.toString("hex") !== record[C.EXPECTED_UTXOS_ROOT] ||
      (prior !== undefined &&
        prior[C.EXPECTED_UTXOS_ROOT] !== record[C.BASE_UTXOS_ROOT])
    )
      return undefined;
    ordered.push(candidate);
    const children: readonly LandedReplacement[] = found.filter(
      ({ record: child }) =>
        child[C.BASE_TAIL_HEADER_HASH].toString("hex") === header,
    );
    if (children.length > 1) return undefined;
    candidate = children[0];
  }
  return ordered.length === found.length ? ordered : undefined;
};
