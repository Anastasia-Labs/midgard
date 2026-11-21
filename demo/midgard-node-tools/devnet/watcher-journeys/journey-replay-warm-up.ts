import {
  fillJourneyReplayStore,
  type JourneyReplayStoreIdentity,
} from "./journey-replay-store.js";
import type { JourneyReplayWarmth } from "./replayed-ledger.read-when-replayed.js";

/** A replayed ledger the warm-up can drive to the node's tip. */
export type WarmableReplayedLedger = Readonly<{
  warm(): Promise<JourneyReplayWarmth>;
  close(): Promise<void>;
}>;

/** A filled replay store and where its follower stopped. */
export type JourneyReplayWarmed = JourneyReplayWarmth &
  Readonly<{ directory: string }>;

/**
 * Fills a run's replay store before its drill: takes the store as the
 * drill would, records its deployment and origin, opens the replayed ledger
 * in it (`openLedger`) and drives that ledger's follower to the node's tip.
 * Its only bound is the replay's progress rule; it has no total limit.
 */
export const warmJourneyReplayStore = async (
  input: Readonly<{
    runDirectory: string;
    identity: JourneyReplayStoreIdentity;
    openLedger: (directory: string) => Promise<WarmableReplayedLedger>;
  }>,
): Promise<JourneyReplayWarmed> => {
  const store = await fillJourneyReplayStore(
    input.runDirectory,
    input.identity,
  );
  try {
    const ledger = await input.openLedger(store.directory);
    try {
      return { directory: store.directory, ...(await ledger.warm()) };
    } finally {
      await ledger.close();
    }
  } finally {
    store.release();
  }
};

const height = (value: number | null) =>
  value === null ? "unknown" : value.toString();

/** The warm-up's one-line report: the blocks walked and the gap to the tip. */
export const describeJourneyReplayWarmth = (warmed: JourneyReplayWarmed) => {
  const { fromHeight, cursorHeight, tipHeight } = warmed;
  const walked =
    fromHeight === null || cursorHeight === null
      ? "an unknown number of"
      : (cursorHeight - fromHeight).toString();
  const behind =
    cursorHeight === null || tipHeight === null
      ? "an unknown number of"
      : (tipHeight - cursorHeight).toString();
  return `Journey replay store ${warmed.directory}: walked ${walked} blocks from height ${height(fromHeight)} to ${height(cursorHeight)}; ${behind} blocks behind the node's tip at ${height(tipHeight)}`;
};
