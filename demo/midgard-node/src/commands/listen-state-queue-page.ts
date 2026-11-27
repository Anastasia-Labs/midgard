import type { ReadPage } from "./listen-read-page.js";

/** The linked queue's order is stable only within one authenticated follower view. */
export const stateQueueHeaderPage = (
  headers: readonly string[],
  page: ReadPage,
  viewDigest: Buffer,
) => {
  let offset = 0;
  if (page.cursor !== undefined) {
    if (page.cursor.length !== 60) return { kind: "invalid" } as const;
    if (!page.cursor.subarray(0, 32).equals(viewDigest))
      return { kind: "stale" } as const;
    const index = headers.indexOf(page.cursor.subarray(32).toString("hex"));
    if (index === -1) return { kind: "stale" } as const;
    offset = index + 1;
  }
  const entries = headers.slice(offset, offset + page.limit);
  const nextCursor =
    offset + entries.length < headers.length
      ? Buffer.concat([
          viewDigest,
          Buffer.from(entries.at(-1)!, "hex"),
        ]).toString("hex")
      : null;
  return { kind: "page", entries, nextCursor } as const;
};
