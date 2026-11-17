/**
 * A failed collateral accounting of the workflow's pending attempt does not
 * hold its replacement: the journal names the failure, the attempt is
 * superseded, and the next action is funded from the wallet as the chain
 * shows it. The superseded attempt's later reads retry the accounting.
 */
import { FraudProofL1UnavailableError } from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import {
  forfeitureOf,
  heldOutRefs,
  showLanding,
} from "../support/collateral-landing.js";
import {
  cleanupFundingRecoveryFixtures,
  FixtureProcessStop,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

it("supersedes a pending attempt whose forfeiture could not be reversed and funds its replacement without the collateral return a rollback removed", async () => {
  const fixture = await setupFundingRecoveryFixture(
    false,
    true,
    false,
    20n,
    44,
    3,
    100n,
    true,
  );
  const collateral = fixture.collateralInput.outRef;
  const back = fixture.collateralReturn!;
  const events = async () =>
    (await fixture.journal.load(fixture.initial.workflowId)).map(
      ({ event }) => event,
    );

  // The attempt lands failing phase 2; the process stops once the forfeiture
  // commits, before the attempt is superseded.
  showLanding(fixture, true);
  fixture.chain.mockResolvedValue({ kind: "not_found", landedInvalid: true });
  fixture.stopAt("abandonPendingTransition", "before");
  await expect(fixture.run(await fixture.recover())).rejects.toBeInstanceOf(
    FixtureProcessStop,
  );
  const forfeited = await events();
  expect(forfeited.at(-1)).toEqual(forfeitureOf(fixture));
  await fixture.restartStore();
  const journal = await fixture.recover();
  expect(await heldOutRefs(fixture)).toContain(back.outRef);

  // A rollback drops the landing and the attempt's validity window ends. The
  // provider cannot answer for the collateral the first time it is asked.
  showLanding(fixture, false);
  fixture.chain.mockResolvedValue({ kind: "not_found" });
  const readable = fixture.resolveInputs.getMockImplementation()!;
  fixture.resolveInputs.mockImplementationOnce(async (refs) => {
    if (refs.includes(collateral))
      throw new Error("L1 provider did not answer");
    return await readable(refs);
  });
  const replacement = { actionId: "init", input: { actionKind: "proof.init" } };
  vi.mocked(fixture.adapter.observe).mockResolvedValue({
    kind: "action_required",
    action: replacement,
  });
  // The test ends at the replacement's preflight, once it is funded.
  const ended = new FraudProofL1UnavailableError("test ends at preflight");
  vi.mocked(fixture.adapter.preflight).mockRejectedValue(ended);

  await expect(fixture.run(journal)).rejects.toBe(ended);
  expect((await events()).slice(forfeited.length)).toEqual([
    {
      kind: "stalled",
      reason: expect.stringMatching(
        new RegExp(
          `^collateral accounting failed for attempt ${fixture.transactionHash}: .*L1 provider did not answer$`,
          "u",
        ),
      ),
    },
    {
      kind: "reconciled",
      actionId: "init",
      outcome: "not_found",
      txHash: fixture.transactionHash,
    },
  ]);
  expect(fixture.adapter.preflight).toHaveBeenCalledTimes(1);
  expect(vi.mocked(fixture.adapter.preflight).mock.calls[0]![0].action).toEqual(
    replacement,
  );
  const funded = await heldOutRefs(fixture);
  expect(funded).not.toContain(back.outRef);
  expect(funded.length).toBeGreaterThan(0);
});
