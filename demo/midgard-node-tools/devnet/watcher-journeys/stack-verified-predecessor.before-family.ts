import { existsSync } from "node:fs";
import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";

/**
 * What a family writes into its directory before it fixes its predecessor
 * or publishes anything on L1: its workflow baseline and timing plan, its
 * stage timings, a failure record, the confirmed L1 transactions it read,
 * the fault commit's planned late-shift window, a transfer it did not hand
 * over, and checkpoints staging set aside and no longer resumes.
 *
 * Every other entry is staging progress, so its predecessor is fixed:
 * - a signed or committed fault (`staged.json`), or a transition trace's,
 *   which is saved only once its deposit is on L1;
 * - history committed on the predecessor (`history-<name>.json`);
 * - an L1 user event signed or published in preparation (`deposit.json`,
 *   `withdrawal-<n>.json`, `forced-order.json`), saved before either of the
 *   above exists;
 * - the honest transfer built on the predecessor and handed to our node
 *   (`successor-transfer.json`);
 * - the devnet's own deposit trace and timeout correction, and everything
 *   from the fault's DA, proof, successor and result on.
 * An unknown entry counts as progress too.
 */
const BEFORE_FAULT_WORK = new Set([
  "workflow-baseline.json",
  "timing-plan.json",
  "timings.ndjson",
  "failure.json",
  "chain-reads.ndjson",
  "late-shift.json",
  "successor-transfer-deferred.json",
  "archive",
]);

/**
 * An unsigned fault draft: built on its predecessor but never signed or
 * published. Staging's own recovery sets it aside once the tail moves.
 */
const unsignedDraft = async (path: string) => {
  try {
    const staged = JSON.parse(await readFile(path, "utf8")) as unknown;
    return (
      typeof staged === "object" &&
      staged !== null &&
      !["signedCommit", "commitTxHash", "depositEvent"].some(
        (field) => field in staged,
      )
    );
  } catch {
    return false;
  }
};

/** Whether a family's directory shows staging progress. */
const stagingResumes = async (familyDirectory: string) => {
  if (!existsSync(familyDirectory)) return false;
  for (const name of await readdir(familyDirectory)) {
    if (BEFORE_FAULT_WORK.has(name)) continue;
    if (
      name === "staged.json" &&
      (await unsignedDraft(join(familyDirectory, name)))
    )
      continue;
    return true;
  }
  return false;
};

/**
 * The check before one family. A closed session, as after a failed family,
 * does nothing. A family resuming its staging keeps its fixed predecessor,
 * whose staged fault tail would read as faulty. A refusal closes the
 * shared session, so no later family sends anything.
 */
export const verifiedPredecessorBeforeFamily = async (
  input: Readonly<{
    familyDirectory: string;
    isClosed(): boolean;
    close(): Promise<void>;
    check(): Promise<unknown>;
    onStage(name: string): void;
  }>,
): Promise<"checked" | "staging-resumed" | "session-closed"> => {
  if (input.isClosed()) return "session-closed";
  if (await stagingResumes(input.familyDirectory)) {
    input.onStage("skipped: the family resumes its staging");
    return "staging-resumed";
  }
  try {
    await input.check();
  } catch (cause) {
    // A close that fails is reported again by the suite's own close.
    await input.close().catch(() => undefined);
    throw cause;
  }
  return "checked";
};
