/**
 * A proof whose correction is included, but not yet past release finality,
 * keeps its objective open for about half a day. Its own transactions'
 * outputs (change, released collateral) must fund the next fault meanwhile;
 * only the inputs its attempts consumed stay leased, so an exact re-landing
 * after a rollback still holds them.
 */
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
} from "@al-ft/midgard-fault-proofs";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { afterEach, expect, it, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  finality,
  setupFundingRecoveryFixture,
  sourcesFor,
} from "../support/fault-proof-funding-fixture.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";
import { correctionOn } from "./included-correction.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** A second double spend, in another header, detected and journalled the way
 * the watcher records a live fault. */
const detectSecondFault = async (journalRoot: string) => {
  const sharedInput = outRefCbor(92, 0n);
  const block = await buildCanonicalBlockFixture({
    endTime: 40n,
    transactions: [
      buildFixtureTransaction({ spendInputs: [sharedInput], fee: 1n }),
      buildFixtureTransaction({ spendInputs: [sharedInput], fee: 2n }),
    ],
  });
  const observation = {
    ...authenticatedHeaderObservation(block),
    confirmationDepth: 30,
  };
  const decision = await classifyHeader({
    classifier: await createHeaderClassifier({
      deploymentFingerprint: deploymentIdentity.manifestId,
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
      releaseFinalityAuthority: finality,
    }),
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: 30,
      }),
    sources: sourcesFor(block.payloadEnvelopeCbor),
  });
  if (decision.decision !== "fault_detected")
    throw new Error("second header must classify as a double spend");
  const decisions = await openWatcherFaultDecisionJournal({
    directory: journalRoot,
    deploymentFingerprint: deploymentIdentity.manifestId,
    launchScope: installedLaunchScope(decision.launchScope),
    authenticationKey: TEST_JOURNAL_KEY,
  });
  await decisions.appendLiveDecision(decision);
  return decision;
};

it("funds a second fault's proof from the outputs of a proof whose correction is included but not yet final", async () => {
  const fixture = await setupFundingRecoveryFixture(false, true);
  const consumed = fixture.plan.inputs.find(
    ({ role }) => role === "funding",
  )!.outRef;
  const change = `${fixture.transactionHash}#0`;
  // Fault A: its init confirms, then its correction is included within the
  // recovery window.
  const journal = await fixture.recover();
  await fixture.run(journal);
  const { policy, terminal } = await correctionOn(
    fixture,
    fixture.transactionHash,
  );
  expect(terminal.observedAt.confirmationDepth).toBeLessThanOrEqual(
    policy.automaticRecoveryMaxDepth + 1,
  );
  vi.mocked(fixture.adapter.observe).mockResolvedValue({
    kind: "completed",
    terminal,
  });
  await fixture.run(journal, () => new Date(Date.now() + 60_000));
  const included = await journal.load(fixture.initial.workflowId);
  expect(included.at(-1)!.event.kind).toBe("terminal_included");

  // A's objective stays open: its reservation is live, and the input its
  // init spent stays leased for an exact re-landing after a rollback.
  const [own] = await fixture.records();
  expect(own!.state).toBe("active");
  expect(await fixture.store.readReservedOutRefs({})).toContain(consumed);

  // Fault B, inside its window, from the same single prover wallet.
  const second = await detectSecondFault(fixture.journalRoot);
  expect(second.headerHash).not.toBe(fixture.old.headerHash);
  const admitted = await fixture.createPermit(second, "1");
  expect(admitted.permit).toBeDefined();
  const reservation = (await fixture.records()).find(
    ({ decisionDigest }) => decisionDigest === second.decisionDigest,
  );
  expect(reservation?.state).toBe("active");
  const funding = reservation!.activeInputs.filter(
    ({ role }) => role === "funding",
  );
  const collateral = reservation!.activeInputs.filter(
    ({ role }) => role === "collateral",
  );
  expect(funding.length).toBeGreaterThan(0);
  expect(collateral.length).toBeGreaterThan(0);
  // A's change funds B; A's spent input never does.
  expect(reservation!.activeInputs.map(({ outRef }) => outRef)).toContain(
    change,
  );
  expect(reservation!.activeInputs.map(({ outRef }) => outRef)).not.toContain(
    consumed,
  );
  // Both proofs' leases hold together without contention.
  const reserved = await fixture.store.readReservedOutRefs({});
  expect(reserved).toEqual(expect.arrayContaining([consumed, change]));
});
