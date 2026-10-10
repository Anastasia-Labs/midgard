import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import {
  beginWorkflowFundingReservationAction,
  createWorkflowActuationPermitController,
  readWorkflowFundingRecovery,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import * as authorityCreation from "../../src/funding/prover-funding-authority.create-watcher-prover-funding-authority.js";
import { createWatcherProverFundingAuthorityFactory } from "../../src/funding/prover-funding-authority.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";
import { capacityTransition } from "./parameter-capacity-transition.js";
import { runtimeAuthority } from "./prover-funding-calculation.runtime-authority.js";
afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});
it("recovers exact signed leases through cap one to three to one, then refreshes under the current cap", async () => {
  const fixture = await setupFundingRecoveryFixture(
    false,
    false,
    false,
    false,
    false,
    20n,
    44,
    1,
  );
  const current = await runtimeAuthority(deploymentIdentity, 45, 3, 600);
  const create = vi.spyOn(
    authorityCreation,
    "createWatcherProverFundingAuthority",
  );
  const before = await fixture.records();
  const factory = createWatcherProverFundingAuthorityFactory({
    journalRoot: fixture.journalRoot,
    journalAuthenticationKey: TEST_JOURNAL_KEY,
    launchScope: fixture.old.launchScope,
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
  expect(
    BigInt(
      create.mock.calls[0]![0].selectionCalculation!
        .maximumSlashCollateralLovelace,
    ),
  ).toBe(3_000_000_000n);
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
  expect(
    after.activeInputs.filter(({ role }) => role === "collateral"),
  ).toHaveLength(2);
  expect(after).toMatchObject({
    reservationId: before[0]!.reservationId,
    policyDigest: before[0]!.policyDigest,
    reservationBasisDigest: before[0]!.reservationBasisDigest,
  });
  expect(await journal.load(fixture.initial.workflowId)).toEqual(signedPrefix);
  const transition = capacityTransition(after);
  const handoff = {
    ...fixture.handoff,
    expectedJournalSequence: signedPrefix.length,
    preflight: {
      ...fixture.handoff.preflight,
      actionId: "next",
      txHash: transition.transactionHash,
    },
    submissionIntent: {
      ...fixture.handoff.submissionIntent,
      actionId: "next",
      actionInput: { actionKind: "step-one" },
      txHash: transition.transactionHash,
    },
  };
  await fixture.store.prepareTransition({
    plan: fixture.plan,
    expectedRevision: after.revision,
    handoff,
    ...transition,
  });
  await fixture.restartStore();
  // Return to the exact original digest. Historical two-input leases still
  // require authenticated capacity, while the next action can use only one.
  const decreased = await runtimeAuthority(deploymentIdentity, 44, 1);
  const restore = async () => {
    const controller = createWorkflowActuationPermitController({
      decision: fixture.fresh,
      rollbackGeneration: "3",
    });
    const factory = createWatcherProverFundingAuthorityFactory({
      journalRoot: fixture.journalRoot,
      journalAuthenticationKey: TEST_JOURNAL_KEY,
      launchScope: fixture.old.launchScope,
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
  const database = new DatabaseSync(
    join(fixture.journalRoot, "watcher.sqlite"),
  );
  try {
    const saved = database
      .prepare(
        "SELECT canonical_json, authentication_tag FROM watcher_prover_funding_capacity_v1 WHERE reservation_id = ?",
      )
      .get(after.reservationId)!;
    database
      .prepare(
        "DELETE FROM watcher_prover_funding_capacity_v1 WHERE reservation_id = ?",
      )
      .run(after.reservationId);
    await expect(restore()).rejects.toThrow(
      "exceeds its collateral input bound",
    );
    database
      .prepare(
        "INSERT OR REPLACE INTO watcher_prover_funding_capacity_v1 VALUES (?, ?, ?)",
      )
      .run(
        after.reservationId,
        saved.canonical_json!,
        saved.authentication_tag!,
      );
  } finally {
    database.close();
  }
  const recoveredJournal = await restore();
  const recovered = await readWorkflowFundingRecovery(recoveredJournal);
  expect(recovered.transition!.signedTransactionCborHex).toBe(
    transition.signedTransactionCborHex,
  );
  // Mirror inclusion: only exact produced ordinary change replaces spent inputs.
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
  await beginWorkflowFundingReservationAction({
    journal: recoveredJournal,
    action: { actionId: "after-decrease", input: { actionKind: "step-one" } },
  });
  expect(
    (await fixture.records())[0]!.activeInputs.filter(
      ({ role }) => role === "collateral",
    ),
  ).toHaveLength(1);
  expect(await recoveredJournal.load(fixture.initial.workflowId)).toEqual(
    recoveredEntries,
  );
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
});
