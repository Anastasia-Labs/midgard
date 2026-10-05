import "./prover-funding-recovery.registration.js";

import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import { expect, it, vi } from "vitest";

import { setupFundingRecoveryFixture as setup } from "../support/fault-proof-funding-fixture.js";
import { expiredNotFound } from "./prover-funding-recovery.retirement-fixture.js";

it.each(["status_only", "shallow"] as const)(
  "keeps exact attempts and funding after %s absence, and retires only after deep evidence",
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
    if (mode === "shallow")
      await expect(attempt).rejects.toThrow("recovery horizon");
    else await attempt;
    expect(await test.records()).toEqual([test.pending]);
    expect(await test.store.readReservedOutRefs({})).toContain(
      test.pending.pendingTransition!.consumedOutRefs[0],
    );
    expect(test.adapter.observe).not.toHaveBeenCalled();
    expect(test.adapter.preflight).not.toHaveBeenCalled();
    expect(test.adapter.submit).not.toHaveBeenCalled();
    await test.restartStore();
    expect(await test.records()).toEqual([test.pending]);
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
