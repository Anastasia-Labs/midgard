/** A mint-item header removed against a record this run did not create,
 * a sibling run's on our key or another prover's, while a thread of ours
 * still stands on it. The run cancels that thread back to our wallet as a
 * journaled move, waits out rollback recovery, and only then resolves by
 * another record, reporting the reclaim as a reclaim and never as our
 * conviction. */
import { afterEach, describe, expect, it, vi } from "vitest";

import * as funding from "../src/workflow/funding-reservation-permit.js";
import {
  expectReclaimedThenResolved,
  FINAL_DEPTH,
  fixture,
  OUR_THREAD,
  RECLAIM_TX,
  REMOVAL,
  SIBLING_PROOF,
} from "./support/mint-item-own-thread-reclaim.fixture.js";

afterEach(() => vi.restoreAllMocks());

describe("mint-item: our thread on a header another record removed", () => {
  it.each(["sibling", "other"] as const)(
    "reclaims our thread before resolving on a removal against the %s record",
    async (removedBy) => {
      const f = fixture();
      f.removedBy.kind = removedBy;
      const port = f.port();
      const removal =
        removedBy === "sibling"
          ? {
              removalTxHash: REMOVAL,
              referencedProofTokenOutRef: `${SIBLING_PROOF}#0`,
              confirmationDepth: FINAL_DEPTH,
            }
          : f.resolution;
      await expect(f.execute(port)).resolves.toMatchObject({
        kind: "pending",
        reason: expect.stringContaining(RECLAIM_TX),
      });
      expect(port.preflight).toHaveBeenCalledWith(
        expect.objectContaining({ threadOutRef: OUR_THREAD, step: 2 }),
      );
      expect(port.submit).toHaveBeenCalledTimes(1);
      // A restart before it lands: a fresh port, nothing rebuilt or resent.
      const restarted = f.port();
      await expect(f.execute(restarted)).resolves.toMatchObject({
        kind: "pending",
      });
      // Landed, still within rollback recovery: not yet resolved.
      f.land(1);
      await expect(f.execute(restarted)).resolves.toMatchObject({
        kind: "pending",
        reason: expect.stringMatching(/awaits release finality/u),
      });
      f.land(FINAL_DEPTH);
      expectReclaimedThenResolved(await f.execute(restarted), removal);
      expect(restarted.preflight).not.toHaveBeenCalled();
      expect(restarted.submit).not.toHaveBeenCalled();
      expect(f.release).not.toHaveBeenCalled();
    },
  );

  it.each(["sibling", "other"] as const)(
    "still waits on our cancel one block short of release finality on the %s record",
    async (removedBy) => {
      const f = fixture();
      f.removedBy.kind = removedBy;
      const port = f.port();
      await expect(f.execute(port)).resolves.toMatchObject({
        kind: "pending",
        reason: expect.stringContaining(RECLAIM_TX),
      });
      f.land(FINAL_DEPTH - 1);
      await expect(f.execute(port)).resolves.toMatchObject({
        kind: "pending",
        reason: expect.stringContaining(
          `reclaim ${RECLAIM_TX} of our thread ${OUR_THREAD} awaits release finality`,
        ),
      });
      f.land(FINAL_DEPTH);
      await expect(f.execute(port)).resolves.toMatchObject({
        kind: "resolved_by_another_record",
      });
    },
  );

  it.each(["sibling", "other"] as const)(
    "sends no cancel while the %s record's removal is short of release finality",
    async (removedBy) => {
      const f = fixture();
      f.removedBy.kind = removedBy;
      const port = f.port();
      for (const depth of [1, FINAL_DEPTH - 1]) {
        f.removalAt.depth = depth;
        await expect(f.execute(port)).resolves.toMatchObject({
          kind: "pending",
          resumeOnObservation: true,
          reason: expect.stringContaining(
            `header removal awaits release finality before our thread ${OUR_THREAD} is reclaimed`,
          ),
        });
      }
      expect(port.preflight).not.toHaveBeenCalled();
      expect(port.submit).not.toHaveBeenCalled();
      f.removalAt.depth = FINAL_DEPTH;
      await expect(f.execute(port)).resolves.toMatchObject({
        kind: "pending",
        reason: expect.stringContaining(RECLAIM_TX),
      });
      expect(port.submit).toHaveBeenCalledTimes(1);
    },
  );

  it.each(["sibling", "other"] as const)(
    "names the funding permit's refusal of our cancel on the %s record, and sends it once the cause is gone",
    async (removedBy) => {
      const f = fixture();
      f.removedBy.kind = removedBy;
      const port = f.port();
      const refusal = "thread reclaim differs from its exact binding";
      const prepare = vi
        .spyOn(funding, "prepareWorkflowFundingReservationTransaction")
        .mockRejectedValueOnce(new Error(refusal))
        .mockRejectedValueOnce(new Error(refusal));
      for (let pass = 0; pass < 2; pass += 1)
        await expect(f.execute(port)).resolves.toMatchObject({
          kind: "pending",
          resumeOnObservation: true,
          namedState: {
            name: "fraud-proof-thread-reclaim-refused",
            alarm: true,
          },
          reason: expect.stringContaining(
            `the funding permit refused the cancel of our thread ${OUR_THREAD}; it is built again at the next observation: ${refusal}`,
          ),
        });
      expect(port.submit).not.toHaveBeenCalled();
      expect(
        f.entries.some(
          ({ event }) =>
            event.kind === "submission_intent" &&
            event.actionId === `reclaim:${OUR_THREAD}`,
        ),
      ).toBe(false);
      await expect(f.execute(port)).resolves.toMatchObject({
        kind: "pending",
        reason: expect.stringContaining(RECLAIM_TX),
      });
      expect(prepare).toHaveBeenCalledTimes(3);
      expect(port.submit).toHaveBeenCalledTimes(1);
    },
  );

  it("resolves at once when another prover's record removed the header and no thread of ours stands", async () => {
    const f = fixture({ stepsTaken: 0 });
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
    for (const removedBy of ["sibling", "other"] as const) {
      const f = fixture();
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

  it("holds under the named alarm, never resolving, while our threads on the removed header cannot be listed", async () => {
    const f = fixture({ stepsTaken: 0 });
    f.removedBy.kind = "other";
    const unreadable = {
      ...f.port(),
      observe: vi.fn(async () => {
        throw new Error("our threads' L1 view is unreachable");
      }),
    };
    // No list of our threads at all, or one that cannot be read: nothing
    // shows that none of ours stands on the header.
    for (const [reclaim, detail] of [
      [undefined, "this deployment's L1 view has no list of our threads"],
      [unreadable, "our threads' L1 view is unreachable"],
    ] as const)
      for (let pass = 0; pass < 2; pass += 1)
        await expect(f.execute(reclaim)).resolves.toMatchObject({
          kind: "pending",
          resumeOnObservation: true,
          namedState: {
            name: "fraud-proof-own-threads-unobservable",
            alarm: true,
          },
          reason: expect.stringContaining(
            `cannot list our threads on the removed header, so none of them can be shown to be spent: ${detail}`,
          ),
        });
  });
});
