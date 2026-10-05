import {
  isLocalKupmiosPointBehindKupoHead,
  LocalKupmiosCheckpointChangedError,
} from "@al-ft/midgard-fault-proofs";

const KUPO_LAG_POLL_MS = 2_000;

/**
 * Reads the exact native block from the local Kupo/Ogmios source. A moving
 * provider head forces a fresh source (its pinned head belongs to the
 * interrupted capture) for at most three attempts. Kupo lagging behind the
 * native chain-sync is not a divergence: the read waits, within a bounded
 * budget, for Kupo's checkpoint to reach the requested slot, and every wait
 * starts a fresh source so its pinned head can move forward. A checkpoint that
 * reached the slot and still differs fails closed at once.
 */
export const captureExactBlockWithKupoLag = async <T>({
  read,
  recreateSource,
  isClosed,
  lagBudgetMs,
  pollMs = KUPO_LAG_POLL_MS,
  sleep = (ms) => new Promise<void>((resolve) => setTimeout(resolve, ms)),
  now = () => Date.now(),
}: {
  read: () => Promise<T>;
  recreateSource: () => void;
  isClosed: () => boolean;
  lagBudgetMs: number;
  pollMs?: number;
  sleep?: (ms: number) => Promise<void>;
  now?: () => number;
}): Promise<T> => {
  const startedAt = now();
  for (let headMoves = 0; ; ) {
    if (isClosed()) throw new Error("local Kupo/Ogmios runtime is closed");
    // One source serves every observation so immutable checkpoints and
    // blocks stay cached.
    try {
      return await read();
    } catch (error) {
      if (error instanceof LocalKupmiosCheckpointChangedError) {
        if (headMoves >= 2) throw error;
        headMoves += 1;
      } else if (isLocalKupmiosPointBehindKupoHead(error)) {
        if (now() - startedAt + pollMs > lagBudgetMs) throw error;
        await sleep(pollMs);
      } else {
        throw error;
      }
      recreateSource();
    }
  }
};
