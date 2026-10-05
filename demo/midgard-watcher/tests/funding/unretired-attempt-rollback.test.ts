import {
  beginWorkflowFundingReservationAction,
  createWorkflowActuationPermitController,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import { createWatcherProverFundingAuthorityFactory } from "../../src/funding/prover-funding-authority.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { runtimeAuthority } from "./prover-funding-calculation.runtime-authority.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

it("re-lands the exact bytes of a confirmed, unretired attempt rolled back after its collateral was repriced", async () => {
  const fixture = await setupFundingRecoveryFixture();
  fixture.walletUtxos.push({
    txHash: "cd".repeat(32),
    outputIndex: 0,
    address: walletAddress,
    assets: { lovelace: 4_000_000_000n },
  });
  const factory = createWatcherProverFundingAuthorityFactory({
    journalRoot: fixture.journalRoot,
    launchScope: fixture.old.launchScope,
    deploymentIdentity,
    protocolParameters: await runtimeAuthority(deploymentIdentity, 45, 3, 600),
    protocolParameterHistory: fixture.protocolParameterHistory,
    store: fixture.store,
  });
  const controller = createWorkflowActuationPermitController({
    decision: fixture.fresh,
    rollbackGeneration: "2",
  });
  const journal = fixture.bind(fixture.fresh, {
    permit: await factory.create({
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
    }),
    controller,
    releaseUnused: async () =>
      factory.releaseUnused({ actuationPermit: controller.permit }),
  });
  await fixture.run(journal);
  const confirmed = await journal.load(fixture.initial.workflowId);
  expect(confirmed.at(-1)!.event).toEqual({
    kind: "confirmed",
    actionId: "init",
    txHash: fixture.transactionHash,
  });
  // The next step reprices at once; it does not wait for retirement.
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: "next", input: { actionKind: "step-one" } },
  });
  expect(
    (await fixture.records())[0]!.activeInputs
      .filter(({ role }) => role === "collateral")
      .map(({ outRef }) => outRef),
  ).toEqual([`${"cd".repeat(32)}#0`]);

  // A rollback within k drops the confirmed attempt: its funding input is
  // unspent again, its change no longer exists, and the chain asks for it.
  const change = `${fixture.transactionHash}#0`;
  const restored = fixture.plan.inputs.find(({ role }) => role === "funding")!;
  const [restoredHash, restoredIndex] = restored.outRef.split("#");
  fixture.walletUtxos.splice(
    fixture.walletUtxos.findIndex(
      ({ txHash, outputIndex }) => `${txHash}#${outputIndex}` === change,
    ),
    1,
    {
      txHash: restoredHash!,
      outputIndex: Number(restoredIndex),
      address: walletAddress,
      assets: { lovelace: BigInt(restored.lovelace) },
    },
  );
  vi.mocked(fixture.adapter.observe).mockResolvedValue({
    kind: "action_required",
    action: { actionId: "init", input: { actionKind: "proof.init" } },
  });
  const rebroadcast: string[] = [];
  vi.mocked(fixture.adapter.reconcile).mockImplementation(
    async ({ txHash, signedTransactionCborHex, authorizeResubmission }) => {
      if (
        signedTransactionCborHex === undefined ||
        authorizeResubmission === undefined
      )
        throw new Error("reobserved attempt lost its exact signed bytes");
      await authorizeResubmission({
        transactionHash: txHash!,
        signedTransactionCborHex,
      });
      rebroadcast.push(signedTransactionCborHex);
      return { kind: "pending", txHash: txHash! };
    },
  );
  const result = await fixture.run(
    journal,
    () => new Date(Date.now() + 60_000),
  );

  expect(result).toMatchObject({ kind: "pending" });
  expect(rebroadcast).toEqual([fixture.signedTransactionCborHex]);
  const entries = await journal.load(fixture.initial.workflowId);
  expect(entries.slice(0, confirmed.length)).toEqual(confirmed);
  expect(
    entries.slice(confirmed.length).map(({ event }) => ({
      kind: event.kind,
      txHash: "txHash" in event ? event.txHash : undefined,
    })),
  ).toEqual([
    { kind: "reobserved", txHash: fixture.transactionHash },
    { kind: "rebroadcast_intent", txHash: fixture.transactionHash },
    { kind: "reconciled", txHash: fixture.transactionHash },
  ]);
  expect((await fixture.records())[0]!.pendingTransition?.transactionHash).toBe(
    fixture.transactionHash,
  );
  // A fresh, different transaction is never signed for the rolled-back intent.
  expect(fixture.adapter.preflight).not.toHaveBeenCalled();
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
});
