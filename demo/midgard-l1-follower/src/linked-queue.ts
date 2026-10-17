/**
 * The landed linked queue (plan §5.5 P1), shared by every role that reads a
 * state queue from follower facts. A role decodes its live queue outputs
 * into entries (a root, a node keyed by its linked-list key, or an output
 * under the queue policy it cannot read) and this walk turns them into one
 * list from the root, or names why the outputs are not one list.
 *
 * It is a pure function of the entries: the same live outputs give the same
 * walk on any backend, so the projection equals a fresh replay whenever the
 * facts do.
 */

/** One live queue output as the walk sees it. */
export type LinkedQueueEntry = Readonly<{
  /** A stable label for the output, such as `<tx hash hex>#<index>`. */
  id: string;
  /**
   * `invalid`: an output under the queue policy that is not a root or a
   * node (a malformed NFT or datum). It makes the queue unhealthy; it is
   * never dropped.
   */
  kind: "root" | "node" | "invalid";
  /** A node's linked-list key; null for the root and for invalid outputs. */
  key: string | null;
  /** The key this entry links to; null at the tail. */
  next: string | null;
}>;

/** Why the live queue outputs are not one list from one root. */
export type LinkedQueueUnhealthyReason =
  | "no_root"
  | "multiple_roots"
  /** An output under the queue policy that is not a root or a node. */
  | "invalid_output"
  /** A link names a key no live node has. */
  | "broken_link"
  /** Two live nodes share a key. */
  | "duplicate_key"
  | "cycle"
  /** A live node the walk from the root never reaches. */
  | "orphan_node"
  /** More live nodes than the reader's cap. */
  | "over_cap";

export type LinkedQueueWalk<E extends LinkedQueueEntry> = Readonly<{
  healthy: boolean;
  /** Null when healthy. */
  reason: LinkedQueueUnhealthyReason | null;
  detail: string | null;
  root: E | null;
  /** The walk from the root, in list order (cut where the walk failed). */
  nodes: readonly E[];
  /** Ids of the entries the walk did not reach. */
  strays: readonly string[];
}>;

export type LinkedQueueWalkOptions = Readonly<{
  /** The most nodes (root excluded) a healthy queue may hold. */
  maxNodes?: number;
}>;

/**
 * Walks `entries` from the single root along the links. Healthy means one
 * root, a walk that reaches every node exactly once and ends at a tail, no
 * invalid entry and, with `maxNodes`, no more nodes than the cap. Anything
 * else is unhealthy with a named reason, and the walk so far is kept so a
 * reader can still serve it.
 */
export const walkLinkedQueue = <E extends LinkedQueueEntry>(
  entries: readonly E[],
  options: LinkedQueueWalkOptions = {},
): LinkedQueueWalk<E> => {
  const roots = entries.filter((entry) => entry.kind === "root");
  const nodes = entries.filter((entry) => entry.kind === "node");
  const invalid = entries.filter((entry) => entry.kind === "invalid");
  const unhealthy = (
    reason: LinkedQueueUnhealthyReason,
    detail: string,
    root: E | null,
    walked: readonly E[],
  ): LinkedQueueWalk<E> => {
    const reached = new Set(walked.map((entry) => entry.id));
    if (root !== null) reached.add(root.id);
    return {
      healthy: false,
      reason,
      detail,
      root,
      nodes: walked,
      strays: entries.map((entry) => entry.id).filter((id) => !reached.has(id)),
    };
  };
  const root = roots[0];
  if (root === undefined) return unhealthy("no_root", "no live root", null, []);
  if (roots.length > 1)
    return unhealthy(
      "multiple_roots",
      roots.map((entry) => entry.id).join(" "),
      root,
      [],
    );
  if (options.maxNodes !== undefined && nodes.length > options.maxNodes)
    return unhealthy(
      "over_cap",
      `${nodes.length.toString()} live nodes; the cap is ${options.maxNodes.toString()}`,
      root,
      [],
    );
  const byKey = new Map<string, E[]>();
  for (const entry of nodes) {
    const key = entry.key ?? "";
    byKey.set(key, [...(byKey.get(key) ?? []), entry]);
  }
  const walked: E[] = [];
  const seen = new Set<string>();
  let key = root.next;
  while (key !== null) {
    if (seen.has(key)) return unhealthy("cycle", key, root, walked);
    seen.add(key);
    const matches = byKey.get(key) ?? [];
    const only = matches[0];
    if (only === undefined) return unhealthy("broken_link", key, root, walked);
    if (matches.length > 1)
      return unhealthy(
        "duplicate_key",
        matches.map((entry) => entry.id).join(" "),
        root,
        walked,
      );
    walked.push(only);
    key = only.next;
  }
  if (invalid.length > 0)
    return unhealthy(
      "invalid_output",
      invalid.map((entry) => entry.id).join(" "),
      root,
      walked,
    );
  if (walked.length < nodes.length) {
    const reached = new Set(walked.map((entry) => entry.id));
    return unhealthy(
      "orphan_node",
      nodes
        .filter((entry) => !reached.has(entry.id))
        .map((entry) => entry.id)
        .join(" "),
      root,
      walked,
    );
  }
  return {
    healthy: true,
    reason: null,
    detail: null,
    root,
    nodes: walked,
    strays: [],
  };
};
