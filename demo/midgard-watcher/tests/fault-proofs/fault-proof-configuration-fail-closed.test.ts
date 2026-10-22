import { afterEach, describe, expect, it } from "vitest";

import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";
import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.js";

afterEach(cleanupFundingRecoveryFixtures);

// A watcher whose installation cannot serve an objective is misconfigured as a
// whole: the process fails closed instead of holding one objective, and
// nothing is minted or run for it.
describe("installation faults stop the watcher instead of quarantining an objective", () => {
  it("keeps a category with no installed runner fail-closed", async () => {
    const test = await setupFaultProofExecution(false, {
      installedRunner: false,
    });
    const before = await test.fixture.records();
    await expect(test.execution.execute(test.input)).rejects.toThrow(
      "fault-proof execution has no installed doubleSpend runner",
    );
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(test.execution.readiness()).toEqual([]);
    expect(await test.fixture.records()).toEqual(before);
  });
});
