import type {
  WatcherProofRetention,
  WatcherProofRetentionTarget,
} from "../../src/l1-follower/proof-retention.js";

/**
 * A retention with no follower store behind it: supervisor tests that read
 * no L1 history. Its k is the mainnet and preprod security parameter. It
 * holds no block, so an included correction is always observed again.
 */
export const storelessProofRetention: WatcherProofRetention = Object.freeze({
  securityParameter: 2_160,
  pin: async () => ({ kind: "pinned" }) as const,
  release: async () => undefined,
  holdUnits: async () => ({ kind: "held" }) as const,
  holdEvents: async () => ({ kind: "held" }) as const,
  passAtTip: async () => false,
  lifted: () => false,
  releaseUnowned: async () => [],
  pinned: async () => [],
  blockDepth: async () => null,
  degradations: () => [],
  readiness: () => [],
});

/**
 * A storeless retention that records which objectives' L1 history it holds,
 * as `<category>/<headerHash>`. With `atTip`, every admission completes a
 * pass at tip, and the pass releases a hold no objective owns once its
 * header is out of the finalized queue: the ownership and queue halves of
 * the follower's rule, whose depth halves run over a real store in
 * proof-retention-unowned-holds.test.ts.
 */
export const recordingProofRetention = ({ atTip = false } = {}) => {
  const pinned = new Set<string>();
  const targets = new Map<string, WatcherProofRetentionTarget>();
  const name = (target: WatcherProofRetentionTarget) =>
    `${target.category}/${target.headerHash}`;
  const retention: WatcherProofRetention = {
    ...storelessProofRetention,
    pin: async (target) => (
      pinned.add(name(target)),
      targets.set(name(target), target),
      { kind: "pinned" }
    ),
    release: async (target) => void pinned.delete(name(target)),
    ...(atTip && {
      passAtTip: async () => true,
      releaseUnowned: async ({ owned, queued }) => {
        const released = [...pinned]
          .map((held) => targets.get(held)!)
          .filter((target) => !owned(target) && !queued.has(target.headerHash));
        for (const target of released) pinned.delete(name(target));
        return released;
      },
    }),
  };
  return { pinned, retention };
};
