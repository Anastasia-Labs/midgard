import "./prover-funding-recovery.registration.js";

import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import { expect, it, vi } from "vitest";

import { setupFundingRecoveryFixture as setup } from "../support/fault-proof-funding-fixture.js";
import { expiredNotFound } from "./prover-funding-recovery.retirement-fixture.js";

it.each(["status_only", "shallow"] as const)(
  "keeps the exact attempt's consumed input leased after %s absence, and retires it only after deep evidence",
  async (mode) => {
    const test = await setup();
    test.useUnspentPendingInputs();
    const deep = expiredNotFound(test.transactionHash);
    const point = { ...deep.retirement.canonicalPoint, blockNo: "2210" };
    vi.mocked(test.adapter.reconcile).mockResolvedValue(
      mode === "status_only"
        ? { kind: "not_found" }
        : {
            ...deep,
            retirement: {
              ...deep.retirement,
              canonicalPoint: {
                ...point,
                pointId: computeFraudProofRawL1PointId(point),
              },
            },
          },
    );
    const attempt = test.run(await test.recover());
    if (mode === "shallow") {
      await expect(attempt).rejects.toThrow("recovery horizon");
      expect(await test.records()).toEqual([test.pending]);
      expect(test.adapter.observe).not.toHaveBeenCalled();
    } else {
      // Owner ruling (whichever lands wins): absence at the tip supersedes the
      // attempt at once. The workflow is free; the attempt's consumed input
      // stays leased until retirement so a replacement must spend it.
      await attempt;
      const [superseded] = await test.records();
      expect(superseded!.pendingTransition).toBeNull();
      expect(test.adapter.observe).toHaveBeenCalled();
    }
    expect(await test.store.readReservedOutRefs({})).toContain(
      test.pending.pendingTransition!.consumedOutRefs[0],
    );
    expect(test.adapter.preflight).not.toHaveBeenCalled();
    expect(test.adapter.submit).not.toHaveBeenCalled();
    const beforeRestart = await test.records();
    await test.restartStore();
    expect(await test.records()).toEqual(beforeRestart);
    vi.mocked(test.adapter.reconcile).mockResolvedValue(deep);
    await test.run(await test.recover());
    expect((await test.records())[0]).toMatchObject({
      pendingTransition: null,
      activeInputs: [],
    });
    expect(await test.store.readReservedOutRefs({})).not.toContain(
      test.pending.pendingTransition!.consumedOutRefs[0],
    );
    expect(test.adapter.preflight).not.toHaveBeenCalled();
    expect(test.adapter.submit).not.toHaveBeenCalled();
  },
);
