import type {
  BackgroundJob,
  ImportCheckLane,
  WorkerSlot,
} from "./validation-pool.bundled-worker-exec-argv.js";

/**
 * The number of import-check jobs in `lane` (any lane when undefined) that
 * hold a worker: running, or waiting for their fresh worker to start.
 */
export const backgroundHeld = (
  slots: readonly WorkerSlot[],
  lane?: ImportCheckLane,
): number =>
  slots.filter((slot) => {
    const held = slot.reservedFor?.lane ?? slot.lane;
    return held !== null && (lane === undefined || held === lane);
  }).length;

/** Whether `slot` has no job, is not restarting and is not reserved. */
export const isIdle = (slot: WorkerSlot): boolean =>
  slot.inFlight === null && !slot.restarting && slot.reservedFor === null;

/** An idle worker, a started one first. */
export const idleSlot = (
  slots: readonly WorkerSlot[],
): WorkerSlot | undefined => {
  const idle = slots.filter(isIdle);
  return idle.find((candidate) => candidate.started) ?? idle.at(0);
};

/**
 * Takes the next import-check job to start on an idle worker from
 * `pool.backgroundQueue`:
 * - a backlog job, while the backlog lane holds fewer than its reserved
 *   share of workers;
 * - else, when `open` (no other job waits), the oldest job of either lane,
 *   while import-check jobs hold fewer than `size - 1` workers (one on a
 *   single-worker pool).
 */
export const takeBackgroundJob = (
  pool: Readonly<{
    slots: readonly WorkerSlot[];
    backgroundQueue: BackgroundJob[];
    backlogShare: number;
    size: number;
  }>,
  open: boolean,
): BackgroundJob | undefined => {
  if (backgroundHeld(pool.slots, "backlog") < pool.backlogShare) {
    const index = pool.backgroundQueue.findIndex(
      (job) => job.lane === "backlog",
    );
    if (index >= 0) return pool.backgroundQueue.splice(index, 1)[0];
  }
  if (!open || backgroundHeld(pool.slots) >= Math.max(1, pool.size - 1))
    return undefined;
  return pool.backgroundQueue.shift();
};
