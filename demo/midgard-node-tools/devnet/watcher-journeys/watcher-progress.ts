/** The largest `/v1/diagnostics` page the watcher serves. */
const PAGE_LIMIT = 100;

type L1SourceRecord = Readonly<{
  sequence: string;
  blockHash: string;
  blockNo: string;
}>;

type L1SourcePage = Readonly<{
  records: readonly L1SourceRecord[];
  nextCursor: string | null;
}>;

const l1SourcePage = (cursor: string, limit: number) =>
  `/v1/diagnostics?kind=l1_source&cursor=${cursor}&limit=${limit.toString()}`;

/**
 * A watcher's progress as a count that rises only when the watcher did work,
 * read through `read` (a `/v1/...` path to its parsed JSON answer). A wait
 * budgeted by progress restarts its stall bound only when the count rises.
 *
 * It counts three of the watcher's own fields:
 * - each startup stage `/v1/status` names (`startup.stage`) for the first
 *   time since the watcher (re)started: while it starts, the watcher serves
 *   nothing else;
 * - each rise of its L1 follower's cursor height while startup holds at
 *   `l1_follower_ready`. A restarted watcher catches up there, from its
 *   store's cursor to the node's tip, for as long as that takes, and the
 *   cursor height inside the stage's report (the follower's
 *   `l1_follower_catching_up` detail, "cursor height N, node tip height M")
 *   is the only progress it answers then. The node's tip height is never
 *   counted;
 * - each rise of the highest block the watcher has decided at: the
 *   `blockNo` of its `l1_source` diagnostics, one record per completed
 *   decision pass at its follower's cursor. It rises only when the follower
 *   applied new blocks and the watcher decided them, so a follower stuck on
 *   a block, an L1 node that fell behind, or a decision pass that keeps
 *   failing leaves it where it is.
 *
 * Reason and detail text are not counted: their detail (seconds behind, a
 * retry count, the node's tip height, the last error) changes every read
 * while nothing moves, save the cursor height above. A read that fails
 * counts nothing. A status naming `startup` after one that did not is a
 * restarted watcher: its stages count again, and its first cursor height
 * does not. A watcher that serves and then restarts again and again counts
 * its stages again on each restart. The diagnostics
 * sequence restarts with the watcher process; a last-read record that the
 * watcher no longer answers with the same block makes the next read start
 * again from the first record it holds, and only blocks above the highest
 * already seen count.
 */
export const createWatcherProgress = (
  read: (path: string) => Promise<unknown>,
): (() => Promise<number>) => {
  const stages = new Set<string>();
  // Whether the last status read was a serving watcher's (no `startup`).
  let serving = false;
  let cursorHeight: bigint | undefined;
  let count = 0;
  let highest: bigint | undefined;
  let cursor = "0";
  let last: L1SourceRecord | undefined;

  const countStartup = async () => {
    const status = (await read("/v1/status")) as {
      startup?: { stage?: unknown; error?: unknown } | null;
    } | null;
    if (status === null || typeof status !== "object") return;
    if (!("startup" in status)) {
      serving = true;
      return;
    }
    if (serving) {
      serving = false;
      stages.clear();
      cursorHeight = undefined;
    }
    const { stage, error } = status.startup ?? {};
    if (typeof stage === "string" && !stages.has(stage)) {
      stages.add(stage);
      count += 1;
    }
    const height =
      typeof error === "string"
        ? /\bcursor height (\d+)\b/u.exec(error)?.[1]
        : undefined;
    if (height === undefined) return;
    if (cursorHeight !== undefined && BigInt(height) > cursorHeight) count += 1;
    cursorHeight = BigInt(height);
  };

  // Whether the watcher still answers the last-read record at its sequence.
  const sameProcess = async (seen: L1SourceRecord) => {
    const after = (BigInt(seen.sequence) - 1n).toString();
    const page = (await read(l1SourcePage(after, 1))) as L1SourcePage;
    const [record] = page.records;
    return (
      record?.sequence === seen.sequence && record.blockHash === seen.blockHash
    );
  };

  const countDecidedBlocks = async () => {
    let fresh = 0;
    for (;;) {
      const page = (await read(
        l1SourcePage(cursor, PAGE_LIMIT),
      )) as L1SourcePage;
      for (const record of page.records) {
        const blockNo = BigInt(record.blockNo);
        if (highest === undefined || blockNo > highest) {
          highest = blockNo;
          count += 1;
        }
        last = record;
        cursor = record.sequence;
        fresh += 1;
      }
      if (page.nextCursor === null) break;
    }
    if (fresh > 0 || last === undefined || (await sameProcess(last))) return;
    // A restarted watcher numbers its records from the start again.
    cursor = "0";
    last = undefined;
    await countDecidedBlocks();
  };

  return async () => {
    await countStartup().catch(() => undefined);
    await countDecidedBlocks().catch(() => undefined);
    return count;
  };
};
