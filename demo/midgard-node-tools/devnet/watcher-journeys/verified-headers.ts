import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { existsSync } from "node:fs";
import { join } from "node:path";

import type {
  WatcherOperationsPage,
  WatcherVerificationDiagnostic,
} from "midgard-watcher";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneyBlock, JourneySuccessor } from "./fixture.js";

/** Healthy-block evidence the journey keeps next to its result. */
export const JOURNEY_VERIFIED_HEADERS_ARTIFACT = "verified-headers.json";

const DECIDED = new Set<WatcherVerificationDiagnostic["outcome"]>([
  "verified",
  "fault_detected",
  "unprovable_gap",
]);

/**
 * Headers the watcher saw released on L1 before verifying them. Each is a
 * decided outcome that fails the awaited header: it was never shown healthy.
 */
const RELEASED_UNVERIFIED: ReadonlyMap<string, string> = new Map([
  ["unverified_merged", "merged"],
  ["unverified_removed", "removed"],
]);

/**
 * The watcher's verified record for `block`: the header was classified
 * healthy from exactly the payload envelope the journey committed.
 */
export const assertJourneyVerifiedHeader = (
  record: WatcherVerificationDiagnostic | undefined,
  block: JourneyBlock,
): WatcherVerificationDiagnostic => {
  assert(
    record !== undefined,
    "Healthy predecessor/successor decision is missing",
  );
  assert.equal(record.kind, "verification");
  assert.equal(record.headerHash, block.headerHash);
  const released = RELEASED_UNVERIFIED.get(record.outcome);
  assert(
    released === undefined,
    `Header ${block.headerHash} was ${released} on L1 before the watcher verified it (${record.outcome})`,
  );
  assert.equal(
    record.outcome,
    "verified",
    `Header ${block.headerHash} was not verified healthy`,
  );
  assert.equal(
    record.payloadEnvelopeSha256,
    createHash("sha256").update(block.payloadEnvelopeCbor).digest("hex"),
    `Header ${block.headerHash} was verified from another payload envelope`,
  );
  return record;
};

/** The successor's parent: a resumed proof may have adopted a later head. */
export const readJourneySuccessorPredecessor = async (
  directory: string,
  predecessor: JourneyBlock,
): Promise<JourneyBlock> => {
  const path = join(directory, "successor-predecessor.json");
  return existsSync(path)
    ? await readJourneyArtifact<JourneySuccessor>(path)
    : predecessor;
};

/**
 * Only fault decisions are journaled. A healthy classification appears in the
 * watcher's bounded, in-memory verification diagnostics, so the journey reads
 * them as it goes and keeps each header's latest decided record for the whole
 * run. A restarted watcher numbers its diagnostics from one again, so a new
 * process id restarts the cursor.
 */
export const createJourneyVerifiedHeaders = (
  watcher: {
    operations(path: string): Promise<unknown>;
    observe(): { pid: number | null };
  },
  directory: string,
) => {
  const decided = new Map<string, WatcherVerificationDiagnostic>();
  let cursor = "0";
  let processId: number | null | undefined;
  let healthy: WatcherVerificationDiagnostic[] | undefined;
  /** Verified records of `blocks` once all are seen; any other decided outcome fails. */
  const collect = async (blocks: readonly JourneyBlock[]) => {
    const current = watcher.observe().pid;
    if (current !== processId) {
      processId = current;
      cursor = "0";
    }
    for (;;) {
      const page = (await watcher.operations(
        `/v1/diagnostics?kind=verification&limit=100&cursor=${cursor}`,
      )) as WatcherOperationsPage;
      for (const record of page.records)
        if (
          record.kind === "verification" &&
          record.headerHash !== undefined &&
          (DECIDED.has(record.outcome) ||
            // A restarted watcher replaying a stale observation reports a
            // header L1 released since; it does not undo an earlier decision.
            (RELEASED_UNVERIFIED.has(record.outcome) &&
              !decided.has(record.headerHash)))
        )
          decided.set(record.headerHash, record);
      cursor = page.records.at(-1)?.sequence ?? cursor;
      if (page.nextCursor === null) break;
    }
    if (blocks.some(({ headerHash }) => !decided.has(headerHash)))
      return undefined;
    return blocks.map((block) =>
      assertJourneyVerifiedHeader(decided.get(block.headerHash), block),
    );
  };
  return {
    collect,
    /** The staged predecessor, the successor's parent and the successor. */
    collectHealthy: async (
      predecessor: JourneyBlock,
      successor: JourneyBlock,
    ) =>
      (healthy = await collect([
        predecessor,
        await readJourneySuccessorPredecessor(directory, predecessor),
        successor,
      ])),
    /** Persist the healthy records the evidence verifier re-checks. */
    retain: async () => {
      assert(healthy !== undefined, "Healthy blocks were not verified");
      await writeJourneyArtifact(
        join(directory, JOURNEY_VERIFIED_HEADERS_ARTIFACT),
        healthy,
      );
    },
  };
};

/** Each healthy block needs a retained verified record bound to its payload. */
export const verifyJourneyVerifiedHeaders = async (
  directory: string,
  blocks: readonly JourneyBlock[],
) => {
  const path = join(directory, JOURNEY_VERIFIED_HEADERS_ARTIFACT);
  assert(existsSync(path), "Healthy predecessor/successor decision is missing");
  const records =
    await readJourneyArtifact<WatcherVerificationDiagnostic[]>(path);
  assert(Array.isArray(records), "Verified header evidence is malformed");
  for (const block of blocks)
    assertJourneyVerifiedHeader(
      records.find((record) => record.headerHash === block.headerHash),
      block,
    );
};
