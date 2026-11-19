import {
  beginWorkflowFundingReservationAction,
  createWorkflowActuationPermitController,
  readWorkflowFundingRecovery,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it } from "vitest";

import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.production-dependencies.js";
import { createWatcherProverFundingAuthorityFactory } from "../../src/funding/prover-funding-authority.create-watcher-prover-funding-authority-factory.js";
import { setupFundingRecoveryFixture } from "../support/fault-proof-funding-fixture.setup-funding-recovery-fixture.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  walletAddress,
} from "../support/fault-proof-funding-fixture.sources-for.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";
import { collateralLimitTransition } from "./collateral-limit-transition.js";
import { runtimeAuthority } from "./prover-funding-calculation.runtime-authority.js";

/**
 * The ledger's collateral input limit bounds each transaction, not the
 * reservation: a signed step posting two reserved coins as collateral
 * recovers exactly after the limit falls to one, its collateral stays
 * reserved, and the next action begins over the same reservation.
 */
afterEach(async () => {
  await cleanupFundingRecoveryFixtures();
});
it("recovers an exact signed step posting two collateral coins after the limit falls to one, then begins the next action", async () => {
  const fixture = await setupFundingRecoveryFixture(
    false,
    false,
    false,
    20n,
    44,
    1,
  );
  const current = await runtimeAuthority(deploymentIdentity, 45, 3, 600);
  const before = await fixture.records();
  const factory = createWatcherProverFundingAuthorityFactory({
    journalRoot: fixture.journalRoot,
    workflowJournal: fixture.workflowJournal,
    journalAuthenticationKey: TEST_JOURNAL_KEY,
    launchScope: installedLaunchScope(fixture.old.launchScope),
    deploymentIdentity,
    protocolParameters: current,
    protocolParameterHistory: fixture.protocolParameterHistory,
    store: fixture.store,
  });
  const controller = createWorkflowActuationPermitController({
    decision: fixture.fresh,
    rollbackGeneration: "2",
  });
  const permit = await factory.create({
    category: "doubleSpend",
    runner: fixture.runner,
    actuationPermit: controller.permit,
    rollbackGeneration: "2",
    decisionDigest: fixture.fresh.decisionDigest,
    walletAddress,
    readWalletUtxos: fixture.readWalletUtxos,
    resolveInputs: fixture.resolveInputs,
    reservationMode: "resume_only",
    resolveProtocolInputAuthority: async () => {
      throw new Error("unexpected protocol read");
    },
  });
  const journal = fixture.bind(fixture.fresh, {
    permit,
    controller,
    releaseUnused: async () =>
      factory.releaseUnused({ actuationPermit: controller.permit }),
  });
  await fixture.run(journal);
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
  const signedPrefix = await journal.load(fixture.initial.workflowId);
  // The prior attempt is confirmed inside the recovery horizon, not retired.
  expect(signedPrefix.map(({ event }) => event.kind)).not.toContain(
    "signed_attempt_retired",
  );
  expect(signedPrefix.at(-1)!.event).toEqual({
    kind: "confirmed",
    actionId: "init",
    txHash: fixture.transactionHash,
  });
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: "next", input: { actionKind: "step-one" } },
  });
  const after = (await fixture.records())[0]!;
  expect(after).toMatchObject({
    reservationId: before[0]!.reservationId,
    policyDigest: before[0]!.policyDigest,
    reservationBasisDigest: before[0]!.reservationBasisDigest,
  });
  expect(await journal.load(fixture.initial.workflowId)).toEqual(signedPrefix);
  const transition = collateralLimitTransition(after, 2);
  const posted = after.activeInputs.slice(1, 3).map(({ outRef }) => outRef);
  await fixture.prepareAttempt(
    { plan: fixture.plan, expectedRevision: after.revision, ...transition },
    {
      preflight: {
        ...fixture.events.preflight,
        actionId: "next",
        txHash: transition.transactionHash,
      },
      submissionIntent: {
        ...fixture.events.submissionIntent,
        actionId: "next",
        actionInput: { actionKind: "step-one" },
        txHash: transition.transactionHash,
      },
    },
  );
  await fixture.restartStore();
  // Return to the exact original digest, whose limit admits one collateral
  // input per transaction.
  const decreased = await runtimeAuthority(deploymentIdentity, 44, 1);
  const restore = async () => {
    const controller = createWorkflowActuationPermitController({
      decision: fixture.fresh,
      rollbackGeneration: "3",
    });
    const factory = createWatcherProverFundingAuthorityFactory({
      journalRoot: fixture.journalRoot,
      workflowJournal: fixture.workflowJournal,
      journalAuthenticationKey: TEST_JOURNAL_KEY,
      launchScope: installedLaunchScope(fixture.old.launchScope),
      deploymentIdentity,
      protocolParameters: decreased,
      protocolParameterHistory: fixture.protocolParameterHistory,
      store: fixture.store,
    });
    const permit = await factory.create({
      category: "doubleSpend",
      runner: fixture.runner,
      actuationPermit: controller.permit,
      rollbackGeneration: "3",
      decisionDigest: fixture.fresh.decisionDigest,
      walletAddress,
      readWalletUtxos: fixture.readWalletUtxos,
      resolveInputs: fixture.resolveInputs,
      reservationMode: "resume_only",
      resolveProtocolInputAuthority: async () => {
        throw new Error("unexpected protocol read");
      },
    });
    return fixture.bind(fixture.fresh, {
      permit,
      controller,
      releaseUnused: async () =>
        factory.releaseUnused({ actuationPermit: controller.permit }),
    });
  };
  const recoveredJournal = await restore();
  const recovered = await readWorkflowFundingRecovery(recoveredJournal);
  expect(recovered.transition!.signedTransactionCborHex).toBe(
    transition.signedTransactionCborHex,
  );
  // Mirror inclusion: the produced change replaces the one spent input.
  for (const outRef of transition.consumedOutRefs) {
    const index = fixture.walletUtxos.findIndex(
      (utxo) => `${utxo.txHash}#${utxo.outputIndex}` === outRef,
    );
    if (index >= 0) fixture.walletUtxos.splice(index, 1);
  }
  fixture.walletUtxos.push({
    txHash: transition.transactionHash,
    outputIndex: 0,
    address: walletAddress,
    assets: { lovelace: BigInt(transition.producedInputs[0]!.lovelace) },
  });
  await fixture.run(recoveredJournal);
  expect((await fixture.records())[0]!.pendingTransition).toBeNull();
  const recoveredEntries = await recoveredJournal.load(
    fixture.initial.workflowId,
  );
  expect(recoveredEntries.slice(0, signedPrefix.length)).toEqual(signedPrefix);
  expect(
    (await fixture.records())[0]!.activeInputs.map(({ outRef }) => outRef),
  ).toEqual(expect.arrayContaining(posted));
  await beginWorkflowFundingReservationAction({
    journal: recoveredJournal,
    action: { actionId: "after-decrease", input: { actionKind: "step-one" } },
  });
  expect((await fixture.records())[0]).toMatchObject({
    reservationId: before[0]!.reservationId,
    policyDigest: before[0]!.policyDigest,
  });
  expect(await recoveredJournal.load(fixture.initial.workflowId)).toEqual(
    recoveredEntries,
  );
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
});
