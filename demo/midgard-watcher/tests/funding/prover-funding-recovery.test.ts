import { createHash } from "node:crypto";
import { mkdir, readdir, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  assertWorkflowJournalActuation,
  beginWorkflowFundingReservationAction,
  bindWorkflowActuationJournal,
  bindWorkflowActuationRecoveryIdentity,
  bindWorkflowPreflightTransaction,
  computeFraudProofWorkflowId,
  createWorkflowActuationPermitController,
  DirectoryFraudProofWorkflowJournalStore,
  journalJsonDigest,
  normalizeJournalJson,
  prepareWorkflowFundingReservationTransaction,
  workflowActuationDecisionDigest,
  WorkflowFundingReservationUnavailableError,
  type WorkflowFundingSubmissionHandoff,
} from "@al-ft/midgard-fault-proofs";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import { authorizeWatcherProverFundingRecovery } from "../../src/funding/prover-funding-recovery.js";
import { watcherDeploymentAppliedScriptHashes } from "../../src/runtime/deployment-identity.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  key,
  setupFundingRecoveryFixture as setup,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { fundingTerminal } from "./funding-handoff-fixture.js";

afterEach(cleanupFundingRecoveryFixtures);

// The setup uses the production classifier, opaque permits, actual signed bytes,
// SQLite leases and directory journals. Only canonical transaction observations
// are controlled; all recovery and lease rotation run through the orchestrator.
describe("funding recovery across authenticated observation refresh", () => {
  it("refreshes externally spent unsigned change after confirmation without rotating healthy inputs", async () => {
    const test = await setup();
    const admitted = await test.createPermit(test.fresh, "2");
    const journal = test.bind(test.fresh, admitted);
    await test.run(journal);
    const before = await test.records();
    const action = { actionId: "next", input: { actionKind: "proof.init" } };
    await beginWorkflowFundingReservationAction({ journal, action });
    expect(await test.records()).toEqual(before);
    expect(test.readWalletUtxos).not.toHaveBeenCalled();
    const spent = before[0]!.activeInputs.find(
      ({ role }) => role === "funding",
    )!.outRef;
    test.walletUtxos.splice(
      0,
      test.walletUtxos.length,
      ...test.walletUtxos.filter(
        (utxo) => `${utxo.txHash}#${utxo.outputIndex}` !== spent,
      ),
    );
    await beginWorkflowFundingReservationAction({ journal, action });
    expect(test.readWalletUtxos).toHaveBeenCalledOnce();
    expect(
      (await test.records())[0]!.activeInputs.map(({ outRef }) => outRef),
    ).not.toContain(spent);
    expect((await test.records())[0]!.reservationId).toBe(
      before[0]!.reservationId,
    );
  });

  it("keeps unavailable funding pending and refuses revoked or reconciliation-only refill", async () => {
    const test = await setup();
    test.useUnspentPendingInputs();
    const admitted = await test.createPermit(test.fresh, "2");
    const journal = test.bind(test.fresh, admitted);
    vi.mocked(test.adapter.reconcile).mockResolvedValue({ kind: "not_found" });
    await test.run(journal);
    const idle = await test.records();
    test.readWalletUtxos.mockResolvedValueOnce([]);
    const begin = () =>
      beginWorkflowFundingReservationAction({
        journal,
        action: { actionId: "next", input: { actionKind: "proof.init" } },
      });
    await expect(begin()).rejects.toBeInstanceOf(
      WorkflowFundingReservationUnavailableError,
    );
    expect(await test.records()).toEqual(idle);
    test.readWalletUtxos.mockResolvedValueOnce(
      test.walletUtxos.map((utxo) => ({ ...utxo, address: "foreign-wallet" })),
    );
    await expect(begin()).rejects.toThrow("foreign address");
    expect(await test.records()).toEqual(idle);
    test.readWalletUtxos.mockImplementationOnce(async () => {
      admitted.controller.restrictToReconciliation("target gone");
      return test.walletUtxos;
    });
    await expect(begin()).rejects.toThrow("reconciliation-only");
    expect(await test.records()).toEqual(idle);
    admitted.controller.revoke("rollback");
    await expect(begin()).rejects.toThrow();
    expect(await test.records()).toEqual(idle);
  });

  it("does not hide substituted present inputs behind another missing funding input", async () => {
    const test = await setup();
    const journal = await test.recover();
    await test.run(journal);
    const before = await test.records();
    const active = test.walletUtxos.filter((utxo) =>
      before[0]!.activeInputs.some(
        ({ outRef }) => outRef === `${utxo.txHash}#${utxo.outputIndex}`,
      ),
    );
    test.resolveInputs.mockResolvedValueOnce(
      active
        .slice(1)
        .map((utxo, index) =>
          index === 0 ? { ...utxo, assets: { lovelace: 1n } } : utxo,
        ),
    );
    await expect(
      beginWorkflowFundingReservationAction({
        journal,
        action: { actionId: "next", input: { actionKind: "proof.init" } },
      }),
    ).rejects.toThrow("changed reserved lovelace");
    expect(test.readWalletUtxos).not.toHaveBeenCalled();
    expect(await test.records()).toEqual(before);
  });

  it.each(["funding", "collateral"] as const)(
    "selects fresh wallet inputs in the same execution after retiring a spent %s input",
    async (role) => {
      const test = await setup(false, false, false, role === "collateral");
      test.useUnspentPendingInputs();
      const spent = test.plan.inputs.find((input) => input.role === role)!;
      const admitted = await test.createPermit(test.fresh, "2");
      const journal = test.bind(test.fresh, admitted);
      test.walletUtxos.splice(
        0,
        test.walletUtxos.length,
        ...test.walletUtxos.filter(
          (utxo) => `${utxo.txHash}#${utxo.outputIndex}` !== spent.outRef,
        ),
        ...[0, 1].map((outputIndex) => ({
          txHash: "ac".repeat(32),
          outputIndex,
          address: walletAddress,
          assets: { lovelace: 2_000_000_000n },
        })),
      );
      vi.mocked(test.adapter.reconcile).mockResolvedValue({
        kind: "not_found",
      });
      await test.run(journal);
      const [idle] = await test.records();
      expect(idle).toMatchObject({
        state: "active",
        activeInputs: [],
        pendingTransition: null,
      });
      await beginWorkflowFundingReservationAction({
        journal,
        action: {
          actionId: "replacement",
          input: { actionKind: "proof.init" },
        },
      });
      const [refreshed] = await test.records();
      expect(refreshed!.reservationId).toBe(idle!.reservationId);
      expect(refreshed!.activeInputs.map(({ outRef }) => outRef)).not.toContain(
        spent.outRef,
      );
      expect(
        refreshed!.activeInputs.some(({ outRef }) =>
          outRef.startsWith("ac".repeat(32)),
        ),
      ).toBe(true);
      expect(BigInt(refreshed!.revision)).toBe(BigInt(idle!.revision) + 1n);
      await test.restartStore();
      expect(await test.records()).toEqual([refreshed]);
    },
  );

  it("releases a cancelled unused reservation after runner exit, preserving its identity for refresh", async () => {
    const test = await setup(false, false, true);
    test.useUnspentPendingInputs();
    test.original.controller.revoke(
      "target rolled back before first submission",
    );
    try {
      await expect(test.run(test.journal)).rejects.toThrow();
    } finally {
      await test.original.releaseUnused();
    }
    const idle = (await test.records())[0]!;
    expect(idle).toMatchObject({
      reservationId: test.plan.reservationId,
      revision: "1",
      activeInputs: [],
      state: "active",
    });
    const resumedJournal = await test.recover();
    await beginWorkflowFundingReservationAction({
      journal: resumedJournal,
      action: { actionId: "next", input: { actionKind: "proof.init" } },
    });
    const resumed = (await test.records())[0]!;
    expect(resumed.activeInputs).toEqual(test.pending.activeInputs);
    expect(resumed.revision).toBe("2");
    // The cancelled runner captured revision zero. Its late finalizer cannot
    // reclaim the newer runner's refill of the same execution.
    await test.original.releaseUnused();
    expect(await test.records()).toEqual([resumed]);
  });

  it("reclaims crash-before-journal reservations at startup and never steals a signed DB-before-journal handoff", async () => {
    const unused = await setup(false, false, true);
    await rm(unused.journalDirectory, { recursive: true });
    await unused.restartStore();
    await unused.releaseUnusedAtStartup();
    expect((await unused.records())[0]).toMatchObject({
      reservationId: unused.plan.reservationId,
      activeInputs: [],
      revision: "1",
    });
    const signed = await setup(true);
    expect(
      (await signed.journal.load(signed.initial.workflowId)).some(
        ({ event }) => event.kind === "submission_intent",
      ),
    ).toBe(false);
    await signed.original.releaseUnused();
    await signed.restartStore();
    await signed.releaseUnusedAtStartup();
    expect(await signed.records()).toEqual([signed.pending]);
    expect(
      await signed.store.readPendingHandoff({
        reservationId: signed.plan.reservationId,
      }),
    ).not.toBeNull();
  });

  it("scopes runner cleanup to its captured reservation and retains journal-only submission uncertainty", async () => {
    const test = await setup(false, false, true);
    await rm(test.journalDirectory, { recursive: true });
    test.walletUtxos.push(
      ...[0, 1, 2, 3].map((outputIndex) => ({
        txHash: "a1".repeat(32),
        outputIndex,
        address: walletAddress,
        assets: { lovelace: 2_000_000_000n },
      })),
    );
    const next = await test.createPermit(test.fresh, "2");
    const before = await test.records();
    expect(before).toHaveLength(2);
    const other = before.find(
      (record) => record.reservationId !== test.plan.reservationId,
    )!;
    await test.original.releaseUnused();
    expect(await test.records()).toContainEqual(other);
    expect(
      (await test.records()).find(
        (record) => record.reservationId === test.plan.reservationId,
      )!.activeInputs,
    ).toEqual([]);
    await next.releaseUnused();
    const uncertain = await setup(false, false, true);
    await uncertain.append(uncertain.handoff.preflight);
    await uncertain.append(uncertain.handoff.submissionIntent);
    await uncertain.original.releaseUnused();
    await uncertain.releaseUnusedAtStartup();
    expect(await uncertain.records()).toEqual([uncertain.pending]);
  });

  it("refuses reconciliation funding before selection when the durable workflow is absent", async () => {
    const test = await setup();
    await rm(test.journalDirectory, { recursive: true });
    const controller = createWorkflowActuationPermitController({
      decision: test.fresh,
      rollbackGeneration: "2",
    });
    controller.restrictToReconciliation("target no longer present");
    await expect(
      test.createPermit(test.fresh, "2", controller),
    ).rejects.toThrow(
      "reconciliation funding requires its existing durable workflow",
    );
    expect(await test.records()).toEqual([test.pending]);
  });

  it("retains the exact existing reservation under reconciliation-only authority", async () => {
    const test = await setup();
    const controller = createWorkflowActuationPermitController({
      decision: test.fresh,
      rollbackGeneration: "2",
    });
    controller.restrictToReconciliation("target no longer present");
    const admitted = await test.createPermit(test.fresh, "2", controller);
    const result = await test.run(test.bind(test.fresh, admitted));
    expect(result).toMatchObject({ workflowId: test.initial.workflowId });
    expect((await test.records())[0]).toMatchObject({
      reservationId: test.plan.reservationId,
      pendingTransition: null,
    });
    expect(test.adapter.preflight).not.toHaveBeenCalled();
    expect(test.adapter.submit).not.toHaveBeenCalled();
  });

  it.each([false, true])(
    "yields safely expired idle inputs under reconciliation authority (previously acknowledged: %s)",
    async (acknowledgedBeforeReadOnly) => {
      const test = await setup();
      test.useUnspentPendingInputs();
      vi.mocked(test.adapter.reconcile).mockResolvedValue({
        kind: "not_found",
      });
      if (acknowledgedBeforeReadOnly) {
        await test.run(await test.recover());
        expect((await test.records())[0]!.activeInputs).toEqual([]);
        await test.restartStore();
      }
      const readOnly = async () => {
        const controller = createWorkflowActuationPermitController({
          decision: test.fresh,
          rollbackGeneration: "2",
        });
        controller.restrictToReconciliation(
          "another header owns the execution slot",
        );
        return test.bind(
          test.fresh,
          await test.createPermit(test.fresh, "2", controller),
        );
      };
      await test.run(await readOnly());
      const idle = (await test.records())[0]!;
      expect(idle).toMatchObject({
        state: "active",
        activeInputs: [],
        pendingTransition: null,
      });
      await test.restartStore();
      await test.run(await readOnly());
      expect(await test.records()).toEqual([idle]);
      const resumed = await test.recover();
      await beginWorkflowFundingReservationAction({
        journal: resumed,
        action: { actionId: "next", input: { actionKind: "proof.init" } },
      });
      const refreshed = (await test.records())[0]!;
      expect(refreshed.reservationId).toBe(idle.reservationId);
      expect(refreshed.activeInputs.length).toBeGreaterThan(0);
      expect(BigInt(refreshed.revision)).toBe(BigInt(idle.revision) + 1n);
      expect(test.adapter.preflight).not.toHaveBeenCalled();
      expect(test.adapter.submit).not.toHaveBeenCalled();
    },
  );

  it("releases acknowledged idle inputs when the existing submission permit becomes reconciliation-only", async () => {
    const test = await setup();
    test.useUnspentPendingInputs();
    vi.mocked(test.adapter.reconcile).mockResolvedValue({ kind: "not_found" });
    const admitted = await test.createPermit(test.fresh, "2");
    const journal = test.bind(test.fresh, admitted);
    await test.run(journal);
    const [acknowledged] = await test.records();
    expect(acknowledged).toMatchObject({
      activeInputs: [],
      pendingTransition: null,
    });
    expect(
      await test.store.readAbandonmentHandoff({
        reservationId: test.plan.reservationId,
      }),
    ).toBeNull();

    // The running watcher restricts this same controller after target removal;
    // it does not mint a new funding authority merely to finish reconciliation.
    admitted.controller.restrictToReconciliation("target was corrected");
    await expect(test.run(journal)).resolves.toMatchObject({ kind: "pending" });
    const [idle] = await test.records();
    expect(idle).toMatchObject({
      reservationId: test.plan.reservationId,
      state: "active",
      activeInputs: [],
      pendingTransition: null,
    });
    expect(idle!.revision).toBe(acknowledged!.revision);
    await test.run(journal);
    expect(await test.records()).toEqual([idle]);
    expect(test.adapter.preflight).not.toHaveBeenCalled();
    expect(test.adapter.submit).not.toHaveBeenCalled();

    admitted.controller.revoke("canonical rollback");
    await expect(test.run(journal)).rejects.toThrow("revoked");
    expect(await test.records()).toEqual([idle]);
  });

  it("keeps unresolved signed inputs leased when the existing submission permit becomes reconciliation-only", async () => {
    const test = await setup();
    test.useUnspentPendingInputs();
    vi.mocked(test.adapter.reconcile).mockResolvedValue({
      kind: "unknown",
      reason: "canonical boundary unavailable",
    });
    const admitted = await test.createPermit(test.fresh, "2");
    const journal = test.bind(test.fresh, admitted);
    admitted.controller.restrictToReconciliation(
      "target was removed during observation",
    );
    await expect(test.run(journal)).resolves.toMatchObject({ kind: "pending" });
    expect(await test.records()).toEqual([test.pending]);
    expect(
      (await test.records())[0]!.pendingTransition!.signedTransactionCborHex,
    ).toBe(test.signedTransactionCborHex);
    expect(test.adapter.preflight).not.toHaveBeenCalled();
    expect(test.adapter.submit).not.toHaveBeenCalled();
  });

  it("yields an interrupted abandonment whose expiry is no longer authenticated during read-only recovery", async () => {
    const test = await setup();
    test.useUnspentPendingInputs();
    vi.mocked(test.adapter.reconcile).mockResolvedValue({ kind: "not_found" });
    const journal = await test.recover();
    const append = journal.append.bind(journal);
    vi.spyOn(journal, "append").mockImplementation(async (entry, sequence) => {
      if (
        entry.event.kind === "reconciled" &&
        entry.event.outcome === "not_found"
      )
        throw new Error("crash before durable abandonment acknowledgement");
      await append(entry, sequence);
    });
    await expect(test.run(journal)).rejects.toThrow("crash before durable");
    const retained = await test.records();
    await test.restartStore();
    vi.mocked(test.adapter.reconcile).mockResolvedValue({
      kind: "pending",
      txHash: test.transactionHash,
    });
    const controller = createWorkflowActuationPermitController({
      decision: test.fresh,
      rollbackGeneration: "2",
    });
    controller.restrictToReconciliation("parent recovery owns execution slot");
    expect(
      await test.run(
        test.bind(
          test.fresh,
          await test.createPermit(test.fresh, "2", controller),
        ),
      ),
    ).toMatchObject({
      kind: "pending",
      reason: expect.stringContaining(
        "no longer has authenticated replacement evidence: pending",
      ),
    });
    expect(await test.records()).toEqual(retained);
    expect(
      await test.store.readAbandonmentHandoff({
        reservationId: test.plan.reservationId,
      }),
    ).not.toBeNull();
  });

  it("retains leases for an unknown signed attempt under reconciliation authority", async () => {
    const test = await setup();
    test.useUnspentPendingInputs();
    vi.mocked(test.adapter.reconcile).mockResolvedValue({
      kind: "unknown",
      reason: "canonical boundary unavailable",
    });
    const controller = createWorkflowActuationPermitController({
      decision: test.fresh,
      rollbackGeneration: "2",
    });
    controller.restrictToReconciliation(
      "another header owns the execution slot",
    );
    expect(
      await test.run(
        test.bind(
          test.fresh,
          await test.createPermit(test.fresh, "2", controller),
        ),
      ),
    ).toMatchObject({ kind: "pending" });
    expect(await test.records()).toEqual([test.pending]);
  });

  it("retains the same signed attempt across spaced rebroadcasts and restarts", async () => {
    const test = await setup();
    test.useUnspentPendingInputs();
    const broadcasts = vi.fn();
    let clock = Date.now() + 31_000;
    const now = () => new Date(clock);
    vi.mocked(test.adapter.reconcile).mockImplementation(
      async ({ txHash, signedTransactionCborHex, authorizeResubmission }) => {
        expect(signedTransactionCborHex).toBe(test.signedTransactionCborHex);
        expect(authorizeResubmission).toBeDefined();
        await authorizeResubmission!({
          transactionHash: txHash!,
          signedTransactionCborHex: signedTransactionCborHex!,
        });
        broadcasts(signedTransactionCborHex);
        return { kind: "pending", txHash: txHash! };
      },
    );
    await test.run(await test.recover(), now);
    await test.restartStore();
    clock += 31_000;
    await test.run(await test.recover(), now);
    await test.restartStore();
    clock += 31_000;
    const result = await test.run(await test.recover(), now);
    expect(result).toMatchObject({
      kind: "pending",
    });
    expect(broadcasts).toHaveBeenCalledTimes(3);
    expect(
      (await test.journal.load(test.initial.workflowId)).filter(
        ({ event }) => event.kind === "rebroadcast_intent",
      ),
    ).toHaveLength(3);
    expect(
      (await test.journal.load(test.initial.workflowId))
        .filter(({ event }) => event.kind === "submission_intent")
        .map(({ event }) => event),
    ).toEqual([test.handoff.submissionIntent]);
    expect(await test.records()).toEqual([test.pending]);
    expect(test.adapter.submit).not.toHaveBeenCalled();
    expect(test.adapter.preflight).not.toHaveBeenCalled();
  });

  it.each([true, "after_preflight"] as const)(
    "restores the exact signed action after interruption at the preparation commit (%s)",
    async (boundary) => {
      const test = await setup(boundary);
      expect(test.originalEntries.map(({ event }) => event.kind)).toEqual([
        "started",
        "prepared",
        ...(boundary === "after_preflight" ? ["preflight_passed"] : []),
      ]);
      const result = await test.run(await test.recover());
      expect(result).toMatchObject({
        kind: "pending",
        workflowId: test.initial.workflowId,
      });
      const entries = await test.journal.load(test.initial.workflowId);
      expect(
        entries
          .filter(({ event }) => event.kind === "preflight_passed")
          .map(({ event }) => event),
      ).toEqual([test.handoff.preflight]);
      expect(
        entries
          .filter(({ event }) => event.kind === "submission_intent")
          .map(({ event }) => event),
      ).toEqual([test.handoff.submissionIntent]);
      expect(test.adapter.preflight).not.toHaveBeenCalled();
      expect(test.adapter.submit).not.toHaveBeenCalled();
      expect(test.adapter.reconcile).toHaveBeenCalledWith(
        expect.objectContaining({ txHash: test.transactionHash }),
      );
      const [record] = await test.records();
      expect(record).toMatchObject({
        reservationId: test.plan.reservationId,
        revision: "2",
        pendingTransition: null,
      });
      await test.restartStore();
      await test.run(await test.recover());
      expect(await test.records()).toEqual([record]);
      expect(test.adapter.reconcile).toHaveBeenCalledTimes(1);
    },
  );

  it("finishes the exact terminal after release commits but completion append is interrupted", async () => {
    const test = await setup();
    await test.run(await test.recover());
    // Confirm a second actual signed funding transaction and record the normal
    // removal lifecycle, so terminal normalization sees both confirmed actions.
    const funding = (await test.records())[0]!.activeInputs.find(
      ({ role }) => role === "funding",
    )!;
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(test.transactionHash),
        0n,
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    const remaining = BigInt(funding.lovelace) - 1_000_000n;
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(walletAddress),
        CML.Value.from_coin(remaining),
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, 1_000_000n);
    const witnesses = CML.TransactionWitnessSet.new();
    const vkeys = CML.VkeywitnessList.new();
    vkeys.add(
      CML.Vkeywitness.new(
        key.to_public(),
        key.sign(CML.hash_transaction(body).to_raw_bytes()),
      ),
    );
    witnesses.set_vkeywitnesses(vkeys);
    const transactionHash = CML.hash_transaction(body).to_hex();
    const handoff: WorkflowFundingSubmissionHandoff = {
      ...test.handoff,
      expectedJournalSequence: (
        await test.journal.load(test.initial.workflowId)
      ).length,
      preflight: {
        ...test.handoff.preflight,
        actionId: "remove",
        txHash: transactionHash,
      },
      submissionIntent: {
        kind: "submission_intent",
        actionId: "remove",
        actionInput: { actionKind: "proof.remove" },
        attempt: 1,
        txHash: transactionHash,
      },
    };
    await test.store.prepareTransition({
      plan: test.plan,
      expectedRevision: (await test.records())[0]!.revision,
      handoff,
      actionKind: "proof.remove",
      transactionHash,
      signedTransactionCborHex: CML.Transaction.new(
        body,
        witnesses,
        true,
        undefined,
      ).to_cbor_hex(),
      transactionBodySha256: createHash("sha256")
        .update(body.to_cbor_bytes())
        .digest("hex"),
      consumedOutRefs: [funding.outRef],
      producedInputs: [
        {
          outRef: `${transactionHash}#0`,
          role: "funding",
          lovelace: remaining.toString(),
          assets: [],
        },
      ],
    });
    await test.append(handoff.preflight);
    await test.append(handoff.submissionIntent);
    await test.append({
      kind: "submitted",
      actionId: "remove",
      attempt: 1,
      txHash: transactionHash,
    });
    await test.run(await test.recover());
    const terminal = fundingTerminal(
      test.old.headerHash,
      test.transactionHash,
      transactionHash,
    );
    vi.mocked(test.adapter.observe).mockResolvedValue({
      kind: "completed",
      terminal,
    });
    const journal = await test.recover();
    const append = journal.append.bind(journal);
    const interrupted = new Error(
      "interrupted after release before completion append",
    );
    vi.spyOn(journal, "append").mockImplementation(async (entry, sequence) => {
      if (entry.event.kind !== "completed") return append(entry, sequence);
      expect((await test.records())[0]).toMatchObject({
        state: "released",
        activeInputs: [],
      });
      expect(
        await test.store.readCompletionHandoff({
          reservationId: test.plan.reservationId,
        }),
      ).toMatchObject({ completion: entry.event });
      throw interrupted;
    });
    await expect(test.run(journal)).rejects.toBe(interrupted);
    const [released] = await test.records();
    expect(
      (await test.journal.load(test.initial.workflowId)).some(
        ({ event }) => event.kind === "completed",
      ),
    ).toBe(false);
    await test.restartStore();
    test.terminalVerify.mockClear();
    vi.mocked(test.adapter.observe)
      .mockClear()
      .mockImplementation(async () => {
        throw new Error("released recovery must use its recorded terminal");
      });
    test.terminalVerify.mockRejectedValueOnce(
      new Error("canonical terminal is no longer authenticated"),
    );
    expect(await test.run(await test.recover())).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining(
        "canonical terminal is no longer authenticated",
      ),
    });
    expect(await test.records()).toEqual([released]);
    expect(
      (await test.journal.load(test.initial.workflowId)).some(
        ({ event }) => event.kind === "completed",
      ),
    ).toBe(false);
    test.terminalVerify.mockClear();
    const result = await test.run(await test.recover());
    expect(result).toMatchObject({
      kind: "completed",
      workflowId: test.initial.workflowId,
      terminal,
    });
    expect(test.terminalVerify).toHaveBeenCalledTimes(1);
    expect(test.adapter.observe).not.toHaveBeenCalled();
    expect(test.adapter.preflight).not.toHaveBeenCalled();
    expect(test.adapter.submit).not.toHaveBeenCalled();
    expect(await test.records()).toEqual([released]);
    expect(
      (await test.journal.load(test.initial.workflowId)).filter(
        ({ event }) => event.kind === "completed",
      ),
    ).toHaveLength(1);
  });

  it("reconciles the original signed init and rotates its durable leases exactly once", async () => {
    const test = await setup();
    const journal = await test.recover();
    expect(workflowActuationDecisionDigest(journal)).toBe(
      test.old.decisionDigest,
    );
    expect(await test.records()).toEqual([test.pending]);
    const result = await test.run(journal);
    expect(result).toMatchObject({
      kind: "pending",
      workflowId: test.initial.workflowId,
    });
    const [record] = await test.records();
    expect(record).toMatchObject({
      reservationId: test.plan.reservationId,
      decisionDigest: test.old.decisionDigest,
      state: "active",
      pendingTransition: null,
    });
    expect(record!.activeInputs).toEqual(
      expect.arrayContaining(
        test.plan.inputs.filter(({ role }) => role === "collateral"),
      ),
    );
    expect(
      record!.activeInputs.some(
        ({ outRef }) => outRef === `${test.transactionHash}#0`,
      ),
    ).toBe(true);
    expect(vi.mocked(test.adapter.reconcile)).toHaveBeenCalledTimes(1);
    expect(vi.mocked(test.adapter.observe)).toHaveBeenCalledTimes(1);
    await test.run(await test.recover());
    expect(await test.records()).toEqual([record]);
    expect(vi.mocked(test.adapter.reconcile)).toHaveBeenCalledTimes(1);
    expect(vi.mocked(test.adapter.submit)).not.toHaveBeenCalled();
    expect(vi.mocked(test.adapter.prepare)).not.toHaveBeenCalled();
    expect(await readdir(test.journalDirectory)).toEqual([
      test.initial.workflowId,
    ]);
    const entries = await test.journal.load(test.initial.workflowId);
    expect(
      entries.filter(({ event }) => event.kind === "confirmed"),
    ).toHaveLength(1);
    expect(
      entries.filter(({ event }) => event.kind === "submission_intent"),
    ).toHaveLength(1);
  });

  it("survives a crash after SQLite confirmation commits but before the workflow records it", async () => {
    const test = await setup();
    const journal = await test.recover();
    const crash = new Error(
      "process interrupted before reconciliation journal append",
    );
    vi.spyOn(journal, "append").mockImplementationOnce(async (entry) => {
      expect(entry.event).toEqual({
        kind: "reconciled",
        actionId: "init",
        outcome: "confirmed",
        txHash: test.transactionHash,
      });
      expect((await test.records())[0]).toMatchObject({
        revision: "2",
        pendingTransition: null,
      });
      throw crash;
    });
    await expect(test.run(journal)).rejects.toBe(crash);
    const [committed] = await test.records();
    expect(await test.journal.load(test.initial.workflowId)).toEqual(
      test.originalEntries,
    );
    expect(test.adapter.observe).not.toHaveBeenCalled();
    await test.restartStore();
    expect(await test.run(await test.recover())).toMatchObject({
      kind: "pending",
      workflowId: test.initial.workflowId,
    });
    expect(await test.records()).toEqual([committed]);
    expect(test.adapter.reconcile).toHaveBeenCalledTimes(2);
    expect(test.adapter.observe).toHaveBeenCalledTimes(1);
    expect(test.adapter.submit).not.toHaveBeenCalled();
    const entries = await test.journal.load(test.initial.workflowId);
    expect(entries.slice(0, test.originalEntries.length)).toEqual(
      test.originalEntries,
    );
    expect(
      entries.filter(({ event }) => event.kind === "confirmed"),
    ).toHaveLength(1);
    expect(
      entries.filter(({ event }) => event.kind === "submission_intent"),
    ).toHaveLength(1);
    expect(await readdir(test.journalDirectory)).toEqual([
      test.initial.workflowId,
    ]);
  });

  it("retains unknown submissions and their original signed bytes without another action", async () => {
    const test = await setup();
    vi.mocked(test.adapter.reconcile).mockResolvedValue({
      kind: "pending",
      txHash: test.transactionHash,
    });
    expect(await test.run(await test.recover())).toMatchObject({
      kind: "pending",
      workflowId: test.initial.workflowId,
    });
    expect(await test.records()).toEqual([test.pending]);
    expect(
      (await test.records())[0]!.pendingTransition!.signedTransactionCborHex,
    ).toBe(test.signedTransactionCborHex);
    expect(test.adapter.observe).not.toHaveBeenCalled();
    expect(test.adapter.submit).not.toHaveBeenCalled();
  });

  it("quarantines rejected/conflicting lineage without releasing collateral or submitting", async () => {
    const test = await setup();
    vi.mocked(test.adapter.reconcile).mockResolvedValue({
      kind: "conflict",
      reason: "canonical transaction consumes different inputs",
    });
    expect(await test.run(await test.recover())).toMatchObject({
      kind: "stalled",
    });
    expect((await test.records())[0]).toMatchObject({
      reservationId: test.plan.reservationId,
      state: "conflict",
      activeInputs: test.pending.activeInputs,
    });
    expect(test.adapter.observe).not.toHaveBeenCalled();
    expect(test.adapter.submit).not.toHaveBeenCalled();
    await expect(test.recover()).rejects.toThrow(
      "unique non-conflicted original funding reservation",
    );
  });

  it("honors fresh authority revocation and seals the journal execution identity", async () => {
    const test = await setup();
    const admitted = await test.createPermit(test.fresh, "2");
    const journal = test.bind(test.fresh, admitted);
    expect(() =>
      bindWorkflowActuationRecoveryIdentity({
        permit: admitted.controller.permit,
        category: "doubleSpend",
        rollbackGeneration: "2",
        originalDecision: test.fresh,
      }),
    ).toThrow("replace its execution identity");
    admitted.controller.revoke("canonical rollback");
    expect(() =>
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: deploymentIdentity.manifestId,
        category: "doubleSpend",
        headerHash: test.old.headerHash,
        checkpoint: "before_reconcile",
      }),
    ).toThrow("revoked");
    await expect(test.run(journal)).rejects.toThrow("revoked");
    expect(await test.records()).toEqual([test.pending]);
    const firstBinding = createWorkflowActuationPermitController({
      decision: test.fresh,
      rollbackGeneration: "2",
    });
    bindWorkflowActuationJournal({
      journal: new DirectoryFraudProofWorkflowJournalStore(
        test.journalDirectory,
      ),
      permit: firstBinding.permit,
      category: "doubleSpend",
      deploymentFingerprint: deploymentIdentity.manifestId,
      headerHash: test.fresh.headerHash,
      decisionDigest: test.fresh.decisionDigest,
    });
    expect(() =>
      bindWorkflowActuationRecoveryIdentity({
        permit: firstBinding.permit,
        category: "doubleSpend",
        rollbackGeneration: "2",
        originalDecision: test.old,
      }),
    ).toThrow("after journal binding");
  });

  it.each(["before_not_found", "after_not_found"] as const)(
    "recovers abandonment interrupted %s with exact signed bytes",
    async (boundary) => {
      const test = await setup(false, true);
      test.useUnspentPendingInputs();
      vi.mocked(test.adapter.reconcile).mockImplementation(
        async ({ txHash, signedTransactionCborHex }) => {
          expect(txHash).toBe(test.transactionHash);
          expect(signedTransactionCborHex).toBe(test.signedTransactionCborHex);
          return { kind: "not_found" };
        },
      );
      const journal = await test.recover();
      const append = journal.append.bind(journal);
      const crash = new Error(`crash ${boundary}`);
      vi.spyOn(journal, "append").mockImplementation(
        async (entry, sequence) => {
          if (
            entry.event.kind !== "reconciled" ||
            entry.event.outcome !== "not_found"
          )
            return append(entry, sequence);
          expect((await test.records())[0]).toMatchObject({
            revision: "2",
            pendingTransition: null,
            activeInputs: test.pending.activeInputs,
          });
          expect(
            await test.store.readAbandonmentHandoff({
              reservationId: test.plan.reservationId,
            }),
          ).toMatchObject({
            transition: {
              signedTransactionCborHex: test.signedTransactionCborHex,
              transactionHash: test.transactionHash,
            },
            handoff: { reconciliation: entry.event },
          });
          if (boundary === "after_not_found") await append(entry, sequence);
          throw crash;
        },
      );
      await expect(test.run(journal)).rejects.toBe(crash);
      expect(test.adapter.observe).not.toHaveBeenCalled();
      await test.restartStore();
      vi.mocked(test.adapter.reconcile).mockResolvedValueOnce({
        kind: "pending",
        txHash: test.transactionHash,
      });
      expect(await test.run(await test.recover())).toMatchObject({
        kind: "stalled",
      });
      expect((await test.records())[0]).toMatchObject({
        revision: "2",
        pendingTransition: null,
      });
      expect(
        await test.store.readAbandonmentHandoff({
          reservationId: test.plan.reservationId,
        }),
      ).not.toBeNull();
      expect(test.adapter.observe).not.toHaveBeenCalled();
      expect(await test.run(await test.recover())).toMatchObject({
        kind: "pending",
        workflowId: test.initial.workflowId,
      });
      const [acknowledged] = await test.records();
      expect(acknowledged).toMatchObject({
        revision: "4",
        pendingTransition: null,
        activeInputs: [],
      });
      expect(
        await test.store.readAbandonmentHandoff({
          reservationId: test.plan.reservationId,
        }),
      ).toBeNull();
      const entries = await test.journal.load(test.initial.workflowId);
      expect(
        entries.filter(
          ({ event }) =>
            event.kind === "reconciled" && event.outcome === "not_found",
        ),
      ).toHaveLength(1);
      expect(
        entries
          .filter(({ event }) => event.kind === "submission_intent")
          .map(({ event }) => event),
      ).toEqual([test.handoff.submissionIntent]);
      expect(test.adapter.reconcile).toHaveBeenCalledTimes(3);
      expect(test.adapter.preflight).not.toHaveBeenCalled();
      expect(test.adapter.submit).not.toHaveBeenCalled();
      await test.restartStore();
      await test.run(await test.recover());
      expect(await test.records()).toEqual([acknowledged]);
      expect(test.adapter.reconcile).toHaveBeenCalledTimes(3);
    },
  );

  it("abandons a canonically absent expired intent without inventing confirmation or submission", async () => {
    const test = await setup();
    vi.mocked(test.adapter.reconcile).mockResolvedValue({ kind: "not_found" });
    expect(await test.run(await test.recover())).toMatchObject({
      kind: "pending",
      workflowId: test.initial.workflowId,
    });
    expect((await test.records())[0]).toMatchObject({
      reservationId: test.plan.reservationId,
      state: "active",
      pendingTransition: null,
      activeInputs: [],
    });
    expect(test.adapter.observe).toHaveBeenCalledTimes(1);
    expect(test.adapter.submit).not.toHaveBeenCalled();
    expect(
      (await test.journal.load(test.initial.workflowId)).some(
        ({ event }) => event.kind === "confirmed",
      ),
    ).toBe(false);
  });

  it("refuses a self-consistent prepared artifact bound to another payload", async () => {
    const test = await setup();
    const entry = test.originalEntries.find(
      ({ event }) => event.kind === "prepared",
    )!;
    if (entry.event.kind !== "prepared")
      throw new Error("missing fixture artifact");
    const artifact = {
      ...entry.event.artifact,
      evidenceBinding: {
        headerHash: test.old.headerHash,
        payloadEnvelopeSha256: "fe".repeat(32),
        payloadSha256: test.old.payloadSha256,
      },
    };
    await writeFile(
      join(
        test.journalDirectory,
        test.initial.workflowId,
        `${entry.sequence.toString().padStart(8, "0")}.json`,
      ),
      JSON.stringify({
        ...entry,
        event: {
          ...entry.event,
          artifact,
          artifactDigest: journalJsonDigest(normalizeJournalJson(artifact)),
        },
      }),
    );
    await expect(test.recover()).rejects.toThrow("prepared artifact differs");
    expect(await test.records()).toEqual([test.pending]);
  });

  it("refuses a journal intent inconsistent with the reservation's exact signed transaction", async () => {
    const test = await setup();
    for (const entry of test.originalEntries) {
      if (!("txHash" in entry.event)) continue;
      await writeFile(
        join(
          test.journalDirectory,
          test.initial.workflowId,
          `${entry.sequence.toString().padStart(8, "0")}.json`,
        ),
        JSON.stringify({
          ...entry,
          event: { ...entry.event, txHash: "fe".repeat(32) },
        }),
      );
    }
    await expect(test.recover()).rejects.toThrow(
      "funding handoff conflicts with an existing journal action",
    );
    expect(await test.records()).toEqual([test.pending]);
  });

  it("refuses two durable same-target executions before reserving", async () => {
    const test = await setup();
    const identity = {
      ...test.initial.identity,
      decisionDigest: test.fresh.decisionDigest,
    };
    const workflowId = computeFraudProofWorkflowId(identity);
    await new DirectoryFraudProofWorkflowJournalStore(
      test.journalDirectory,
    ).append(
      {
        ...test.originalEntries[0]!,
        identity,
        workflowId,
      },
      0,
    );
    await expect(test.recover()).rejects.toThrow(
      "multiple candidate executions",
    );
    expect(await test.records()).toEqual([test.pending]);
  });

  it("rejects changed fault evidence even when its digest is well formed", async () => {
    const test = await setup();
    const controller = createWorkflowActuationPermitController({
      decision: test.fresh,
      rollbackGeneration: "2",
    });
    await authorizeWatcherProverFundingRecovery({
      journalRoot: test.journalRoot,
      deploymentIdentity,
      actuationPermit: controller.permit,
      category: "doubleSpend",
      rollbackGeneration: "2",
      store: test.store,
    });
    expect(() =>
      bindWorkflowActuationRecoveryIdentity({
        permit: controller.permit,
        category: "doubleSpend",
        rollbackGeneration: "2",
        originalDecision: (() => {
          const { decisionDigest: _digest, ...changed } = {
            ...test.old,
            replayDigest: "ab".repeat(32),
          };
          return {
            ...changed,
            decisionDigest: journalJsonDigest(normalizeJournalJson(changed)),
          };
        })(),
      }),
    ).toThrow("changed the classified fault evidence");
  });
});

it.each([
  "fraudProofValueNotPreservedUnionMint",
  "fraudProofDoubleWithdraw",
] as const)(
  "admits manifest-bound %s spending custody while refusing policy and unknown addresses",
  async (name) => {
    const context = await setup(false, false, true);
    context.useUnspentPendingInputs();
    const journal = context.bind(
      context.old,
      await context.createPermit(context.old, "1"),
    );
    const hashes = watcherDeploymentAppliedScriptHashes(deploymentIdentity);
    const action = { actionId: "init", input: { actionKind: "proof.init" } };
    await beginWorkflowFundingReservationAction({ journal, action });
    const input = context.plan.inputs.find(({ role }) => role === "funding")!;
    const build = (hash: string) => {
      const inputs = CML.TransactionInputList.new();
      const [txHash, index] = input.outRef.split("#");
      inputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(txHash!),
          BigInt(index!),
        ),
      );
      const outputs = CML.TransactionOutputList.new();
      const prototype = CML.TransactionOutput.new(
        CML.Address.from_bech32(
          credentialToAddress(
            deploymentIdentity.network,
            scriptHashToCredential(hash),
          ),
        ),
        CML.Value.from_coin(1_000_000n),
      );
      const custody = CML.TransactionOutput.new(
        prototype.address(),
        CML.Value.from_coin(CML.min_ada_required(prototype, 4310n)),
      );
      outputs.add(custody);
      outputs.add(
        CML.TransactionOutput.new(
          CML.Address.from_bech32(walletAddress),
          CML.Value.from_coin(
            BigInt(input.lovelace) - custody.amount().coin() - 1_000_000n,
          ),
        ),
      );
      const body = CML.TransactionBody.new(inputs, outputs, 1_000_000n);
      const witnesses = CML.TransactionWitnessSet.new();
      const signatures = CML.VkeywitnessList.new();
      signatures.add(
        CML.Vkeywitness.new(
          key.to_public(),
          key.sign(CML.hash_transaction(body).to_raw_bytes()),
        ),
      );
      witnesses.set_vkeywitnesses(signatures);
      const tx = CML.Transaction.new(body, witnesses, true);
      const signed = {
        toHash: () => CML.hash_transaction(body).to_hex(),
        toTransaction: () => tx,
      } as Parameters<typeof bindWorkflowPreflightTransaction>[1];
      const preflight = bindWorkflowPreflightTransaction(
        { txHash: signed.toHash() },
        signed,
      );
      return prepareWorkflowFundingReservationTransaction({
        journal,
        action,
        preflight,
        handoff: {
          ...context.handoff,
          preflight: { ...context.handoff.preflight, txHash: signed.toHash() },
          submissionIntent: {
            ...context.handoff.submissionIntent,
            txHash: signed.toHash(),
          },
        },
      });
    };
    for (const rejected of [
      hashes.fraudProofMint!,
      hashes.stateQueueCommitWithdraw!,
      "ab".repeat(28),
    ])
      await expect(build(rejected)).rejects.toThrow(
        "funding output escapes the governed contract roster",
      );
    await expect(build(hashes[name]!)).resolves.toBeUndefined();
    expect(
      (await context.records())[0]?.pendingTransition?.transactionHash,
    ).toMatch(/^[0-9a-f]{64}$/u);
  },
  120_000,
);

describe("additive deployed funding roster recovery", () => {
  it("refuses a stored policy outside the exact previously deployed roster", async () => {
    const test = await setup(true, false, false, false, "changed-role");
    const before = await test.records();
    await expect(test.createPermit(test.fresh, "2")).rejects.toThrow(
      "restored prover funding reservation identity mismatch",
    );
    expect(await test.records()).toEqual(before);
  });

  it("reuses an old-policy signed handoff through actual journal recovery without changing reservation identity", async () => {
    const test = await setup(true, false, false, false, true);
    const before = (await test.records())[0]!;
    const handoff = await test.store.readPendingHandoff({
      reservationId: before.reservationId,
    });
    expect(handoff).not.toBeNull();
    const admitted = await test.createPermit(test.fresh, "2");
    expect(await test.records()).toEqual([before]);
    expect(
      await test.store.readPendingHandoff({
        reservationId: before.reservationId,
      }),
    ).toEqual(handoff);
    const journal = test.bind(test.fresh, admitted);
    await test.run(journal);
    const entries = await journal.load(test.initial.workflowId);
    expect(
      entries.some(
        ({ event }) =>
          event.kind === "submission_intent" &&
          event.txHash === test.transactionHash,
      ),
    ).toBe(true);
    expect(
      entries.some(
        ({ event }) =>
          event.kind === "confirmed" && event.txHash === test.transactionHash,
      ),
    ).toBe(true);
    const after = (await test.records())[0]!;
    expect(after.pendingTransition).toBeNull();
    for (const field of [
      "reservationId",
      "policyDigest",
      "reservationBasisDigest",
      "decisionDigest",
      "deploymentFingerprint",
    ] as const)
      expect(after[field]).toBe(before[field]);
    expect(test.adapter.submit).not.toHaveBeenCalled();
    await test.restartStore();
    await test.createPermit(test.fresh, "3");
    expect(await test.records()).toEqual([after]);
  });

  it("refreshes stale wallet inputs under the original reservation identity after roster correction", async () => {
    const test = await setup(false, false, false, false, true);
    const journal = test.bind(
      test.fresh,
      await test.createPermit(test.fresh, "2"),
    );
    await test.run(journal);
    const before = (await test.records())[0]!;
    const spent = before.activeInputs.find(
      ({ role }) => role === "funding",
    )!.outRef;
    test.walletUtxos.splice(
      0,
      test.walletUtxos.length,
      ...test.walletUtxos.filter(
        (utxo) => `${utxo.txHash}#${utxo.outputIndex}` !== spent,
      ),
    );
    await beginWorkflowFundingReservationAction({
      journal,
      action: { actionId: "next", input: { actionKind: "proof.init" } },
    });
    const after = (await test.records())[0]!;
    expect(test.readWalletUtxos).toHaveBeenCalledOnce();
    expect(after.activeInputs.map(({ outRef }) => outRef)).not.toContain(spent);
    for (const field of [
      "reservationId",
      "policyDigest",
      "reservationBasisDigest",
    ] as const)
      expect(after[field]).toBe(before[field]);
  });
});

describe("empty pre-start funding recovery directories", () => {
  const emptyExecution = async (test: Awaited<ReturnType<typeof setup>>) => {
    const directory = join(test.journalDirectory, test.initial.workflowId);
    for (const name of await readdir(directory))
      await rm(join(directory, name));
    return directory;
  };

  it("starts under a fresh decision beside an empty unused execution and then recovers its durable journal", async () => {
    const test = await setup(false, false, true);
    const directory = await emptyExecution(test);
    await test.releaseUnusedAtStartup();
    const before = (await test.records())[0]!;
    expect(before.activeInputs).toEqual([]);
    const admitted = await test.createPermit(test.fresh, "2");
    const journal = test.bind(test.fresh, admitted);
    await test.run(journal);
    const records = await test.records();
    expect(records).toHaveLength(2);
    expect(
      records.find(
        ({ reservationId }) => reservationId === before.reservationId,
      ),
    ).toEqual(before);
    expect(
      records.find(
        ({ decisionDigest }) => decisionDigest === test.fresh.decisionDigest,
      )?.state,
    ).toBe("active");
    expect(await readdir(directory)).toEqual([]);
    await expect(test.createPermit(test.fresh, "3")).resolves.toBeDefined();
    expect(await test.records()).toEqual(records);
  });

  it("ignores a genuinely empty sibling while recovering one durable signed execution", async () => {
    const test = await setup(true);
    const empty = join(test.journalDirectory, "ab".repeat(32));
    await mkdir(empty);
    const before = await test.records();
    const handoff = await test.store.readPendingHandoff({
      reservationId: test.plan.reservationId,
    });
    await expect(test.recover()).resolves.toBeDefined();
    expect(await test.records()).toEqual(before);
    expect(
      await test.store.readPendingHandoff({
        reservationId: test.plan.reservationId,
      }),
    ).toEqual(handoff);
    expect(await readdir(empty)).toEqual([]);
  });

  it("refuses reconciliation-only admission with only an empty pre-start directory", async () => {
    const test = await setup(false, false, true);
    await emptyExecution(test);
    const before = await test.records();
    const controller = createWorkflowActuationPermitController({
      decision: test.old,
      rollbackGeneration: "2",
    });
    controller.restrictToReconciliation("target gone");
    await expect(test.createPermit(test.old, "2", controller)).rejects.toThrow(
      "requires its existing durable workflow",
    );
    expect(await test.records()).toEqual(before);
  });

  it.each([".00000000.persisted.tmp", "unexpected.json"])(
    "preserves and rejects a nonempty pre-start directory containing %s",
    async (name) => {
      const test = await setup(false, false, true);
      const directory = await emptyExecution(test);
      await writeFile(join(directory, name), "durable bytes");
      const before = await test.records();
      await expect(test.recover()).rejects.toThrow(
        "foreign or missing execution identity",
      );
      expect(await readdir(directory)).toEqual([name]);
      expect(await test.records()).toEqual(before);
    },
  );

  it("refuses to ignore an empty directory with a DB-before-journal signed handoff", async () => {
    const test = await setup(true);
    await emptyExecution(test);
    const before = await test.records();
    const handoff = await test.store.readPendingHandoff({
      reservationId: test.plan.reservationId,
    });
    await expect(test.recover()).rejects.toThrow(
      "empty workflow directory has durable funding history",
    );
    expect(await test.records()).toEqual(before);
    expect(
      await test.store.readPendingHandoff({
        reservationId: test.plan.reservationId,
      }),
    ).toEqual(handoff);
  });

  it("keeps resolved signed history protected after its abandonment handoff is acknowledged", async () => {
    const test = await setup();
    test.useUnspentPendingInputs();
    const journal = await test.recover();
    vi.mocked(test.adapter.reconcile).mockResolvedValue({ kind: "not_found" });
    await test.run(journal);
    const before = await test.records();
    expect(before[0]!.pendingTransition).toBeNull();
    expect(before[0]!.lastConfirmedTransitionDigest).toBeNull();
    expect(
      await test.store.readAbandonmentHandoff({
        reservationId: test.plan.reservationId,
      }),
    ).toBeNull();
    expect(
      await test.store.hasSignedHistory!({
        reservationId: test.plan.reservationId,
      }),
    ).toBe(true);
    await emptyExecution(test);
    await expect(test.recover()).rejects.toThrow(
      "empty workflow directory has durable funding history",
    );
    expect(await test.records()).toEqual(before);
  });
});
