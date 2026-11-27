import {
  type BackgroundJob,
  type ImportCheckLane,
  ValidationWorkerError,
  type WorkerSlot,
} from "./validation-pool.bundled-worker-exec-argv.js";

/**
 * The workers reserved for the backlog lane on a pool of `size`: none on a
 * single-worker pool, else `requested` within 1 to `size - 1`, so admission
 * always keeps a worker.
 */
export const clampBacklogShare = (size: number, requested: number): number =>
  size <= 1 ? 0 : Math.max(1, Math.min(size - 1, requested));

/**
 * The number of import-check jobs in `lane` (any lane when undefined) that
 * hold a worker: running, waiting for their fresh worker to start, or
 * killed and waiting for the replacement.
 */
export const backgroundHeld = (
  slots: readonly WorkerSlot[],
  lane?: ImportCheckLane,
): number =>
  slots.filter((slot) => {
    const held = slot.reservedFor?.lane ?? slot.lane ?? slot.heldLane;
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

type Lanes = Readonly<{
  slots: readonly WorkerSlot[];
  backgroundQueue: BackgroundJob[];
  backlogShare: number;
  size: number;
}>;

/**
 * Takes the next import-check job to start on an idle worker from
 * `pool.backgroundQueue`. Both lanes together hold at most `size - 1`
 * workers (one on a single-worker pool, and only when `open`):
 * - a backlog job, while the backlog lane holds fewer than its reserved
 *   share of workers;
 * - else, when `open` (no other job waits), the oldest job of either lane.
 * A taken job's cancel timer is stopped.
 */
export const takeBackgroundJob = (
  pool: Lanes,
  open: boolean,
): BackgroundJob | undefined => {
  const held = backgroundHeld(pool.slots);
  let job: BackgroundJob | undefined;
  if (
    held < pool.size - 1 &&
    backgroundHeld(pool.slots, "backlog") < pool.backlogShare
  ) {
    const index = pool.backgroundQueue.findIndex(
      (queued) => queued.lane === "backlog",
    );
    if (index >= 0) job = pool.backgroundQueue.splice(index, 1)[0];
  }
  if (job === undefined && open && held < Math.max(1, pool.size - 1))
    job = pool.backgroundQueue.shift();
  if (job?.cancel !== undefined) clearTimeout(job.cancel);
  return job;
};

const notStarted = () =>
  new ValidationWorkerError({
    message: "import check job not started in time",
    notStarted: true,
  });

/**
 * Queues `job`; when it has a `startBy`, it is cancelled (rejected with
 * `notStarted`) if still queued then, and at once if that time has passed.
 */
export const queueBackgroundJob = (pool: Lanes, job: BackgroundJob): void => {
  if (job.startBy !== undefined && job.startBy <= Date.now())
    return job.reject(notStarted());
  pool.backgroundQueue.push(job);
  if (job.startBy === undefined) return;
  job.cancel = setTimeout(() => {
    const index = pool.backgroundQueue.indexOf(job);
    if (index < 0) return;
    pool.backgroundQueue.splice(index, 1);
    job.reject(notStarted());
  }, job.startBy - Date.now());
};

/** Rejects every queued import-check job with `error`. */
export const rejectBackgroundJobs = (
  pool: Lanes,
  error: ValidationWorkerError,
): void => {
  for (const job of pool.backgroundQueue.splice(0)) {
    if (job.cancel !== undefined) clearTimeout(job.cancel);
    job.reject(error);
  }
};

/**
 * Rejects the jobs of `slot`, whose worker failed (`cause`): the one it
 * ran, and the import-check job reserved for it, which never ran (its
 * worker failed before the job was sent to it: `workerUnavailable`).
 */
export const rejectSlotJobs = (slot: WorkerSlot, cause: unknown): void => {
  const message = `validation worker ${slot.index} failed`;
  slot.inFlight?.reject(new ValidationWorkerError({ message, cause }));
  slot.reservedFor?.reject(
    new ValidationWorkerError({
      message: `${message} before the job was sent to it`,
      cause,
      workerUnavailable: true,
    }),
  );
};
