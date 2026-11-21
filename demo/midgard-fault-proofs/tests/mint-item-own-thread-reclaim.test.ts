/** A mint-item header removed against another prover's record while a
 * thread of ours still stands on it. The run cancels that thread back to our
 * wallet, waits while the node's mempool holds the cancel, and resolves by
 * another record once it has landed, reporting the reclaim as a reclaim and
 * never as our conviction. */
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  expectReclaimedThenResolved,
  FINAL_DEPTH,
  fixture,
  OUR_THREAD,
} from "./support/mint-item-own-thread-reclaim.fixture.js";

/** The run waiting on our cancel in the node's mempool. */
const awaitingCancel = (reclaimTx: string) => ({
  kind: "pending",
  reason: expect.stringContaining(
    `transaction ${reclaimTx} spending our thread ${OUR_THREAD} awaits inclusion`,
  ),
});

afterEach(() => vi.restoreAllMocks());

describe("mint-item: our thread on a header another record removed", () => {
  it("reclaims our thread before resolving on another prover's removal, sending nothing again after a restart", async () => {
    const f = await fixture();
    f.removedBy.kind = "other";
    const port = f.port();
    const removal = f.resolution;
    await expect(f.execute(port)).resolves.toMatchObject(
      awaitingCancel(f.reclaimTx),
    );
    expect(port.preflight).toHaveBeenCalledWith(
      expect.objectContaining({ threadOutRef: OUR_THREAD, step: 2 }),
    );
    expect(port.submit).toHaveBeenCalledTimes(1);
    // A restart before it lands: a fresh process and port find the cancel
    // in the mempool and send nothing.
    f.restart();
    const restarted = f.port();
    await expect(f.execute(restarted)).resolves.toMatchObject(
      awaitingCancel(f.reclaimTx),
    );
    expect(restarted.submit).not.toHaveBeenCalled();
    expect(f.sent()).toEqual([]);
    // Landed: resolved.
    f.land(1);
    expectReclaimedThenResolved(await f.execute(restarted), removal, f);
    expect(restarted.submit).not.toHaveBeenCalled();
  });

  it.each(["own", "other"] as const)(
    "sends no cancel while the removal against the %s record is short of release finality",
    async (removedBy) => {
      const f = await fixture();
      f.removedBy.kind = removedBy;
      const port = f.port();
      for (const depth of [1, FINAL_DEPTH - 1]) {
        f.removalAt.depth = depth;
        await expect(f.execute(port)).resolves.toMatchObject({
          kind: "pending",
          reason: expect.stringContaining(
            `mintItemNonCanonical header removal awaits release finality before our thread ${OUR_THREAD} is reclaimed`,
          ),
        });
      }
      expect(port.preflight).not.toHaveBeenCalled();
      expect(port.submit).not.toHaveBeenCalled();
      f.removalAt.depth = FINAL_DEPTH;
      await expect(f.execute(port)).resolves.toMatchObject(
        awaitingCancel(f.reclaimTx),
      );
      expect(port.submit).toHaveBeenCalledTimes(1);
    },
  );

  it("resolves at once when another prover's record removed the header and no thread of ours stands", async () => {
    const f = await fixture({ stepsTaken: 0 });
    f.removedBy.kind = "other";
    const port = f.port();
    const outcome = await f.execute(port);
    expect(outcome).toMatchObject({
      kind: "resolved_by_another_record",
      resolution: f.resolution,
    });
    expect((outcome as { reclaimed?: unknown }).reclaimed).toBeUndefined();
    expect(port.submit).not.toHaveBeenCalled();
  });

  it("holds under the named alarm, never resolving, while our thread stands and no cancel is wired", async () => {
    for (const removedBy of ["own", "other"] as const) {
      const f = await fixture();
      f.removedBy.kind = removedBy;
      for (const reclaim of [
        { ...f.port(), unavailable: () => "no cancel script is deployed" },
        // Without a cancel port, the stage derivation still names our
        // thread on another prover's removal.
        ...(removedBy === "other" ? [undefined] : []),
      ])
        await expect(f.execute(reclaim)).resolves.toMatchObject({
          kind: "pending",
          resumeOnObservation: true,
          namedState: {
            name: "fraud-proof-thread-reclaim-unavailable",
            alarm: true,
          },
          reason: expect.stringContaining(OUR_THREAD),
        });
    }
  });

  it("never resolves while our threads on the removed header cannot be listed", async () => {
    const f = await fixture({ stepsTaken: 0 });
    f.removedBy.kind = "other";
    // No list of our threads at all: held under the named alarm.
    for (let pass = 0; pass < 2; pass += 1)
      await expect(f.execute(undefined)).resolves.toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
        namedState: {
          name: "fraud-proof-own-threads-unobservable",
          alarm: true,
        },
        reason: expect.stringContaining(
          "cannot list our threads on the removed header, so none of them can be shown to be spent: this family's workflow has no view of our threads",
        ),
      });
    // A list that cannot be read fails the run with its cause, as for every
    // family on the shared runner; nothing is resolved.
    const unreadable = {
      ...f.port(),
      observe: vi.fn(async () => {
        throw new Error("our threads' L1 view is unreachable");
      }),
    };
    for (let pass = 0; pass < 2; pass += 1)
      await expect(f.execute(unreadable)).rejects.toThrow(
        "our threads' L1 view is unreachable",
      );
  });
});
