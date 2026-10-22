import "./prover-funding-recovery.registration.js";

import { createHash } from "node:crypto";

import {
  assertWorkflowJournalActuation,
  beginWorkflowFundingReservationAction,
  bindWorkflowActuationJournal,
  bindWorkflowActuationRecoveryIdentity,
  computeFraudProofWorkflowId,
  createWorkflowActuationPermitController,
  journalJsonDigest,
  normalizeJournalJson,
  workflowActuationDecisionDigest,
  WorkflowFundingReservationUnavailableError,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { authorizeWatcherProverFundingRecovery } from "../../src/funding/prover-funding-recovery.js";
import {
  deploymentIdentity,
  FixtureProcessStop,
  key,
  setupFundingRecoveryFixture as setup,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";
import { fundingTerminal } from "./funding-handoff-fixture.js";
import { registerConfirmedFundingRefillTest } from "./prover-funding-recovery.authenticated-confirmed-refill.js";
// The setup uses the production classifier, opaque permits, actual signed bytes,
// SQLite leases and directory journals. Only canonical transaction observations
// are controlled; all recovery and lease rotation run through the orchestrator.
import { expiredNotFound } from "./prover-funding-recovery.retirement-fixture.js";

describe("funding recovery across authenticated observation refresh", () => {
  registerConfirmedFundingRefillTest();

  it("keeps unavailable funding pending and refuses revoked or reconciliation-only refill", async () => {
    const test = await setup();
    test.useUnspentPendingInputs();
    const admitted = await test.createPermit(test.fresh, "2");
    const journal = test.bind(test.fresh, admitted);
    test.chain.mockResolvedValue(expiredNotFound(test.transactionHash));
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
      const test = await setup(false, role === "collateral");
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
      test.chain.mockResolvedValue(expiredNotFound(test.transactionHash));
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
    const test = await setup(true);
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

  it("reclaims a never-signed reservation at startup and keeps a signed one", async () => {
    const unused = await setup(true);
    unused.workflowJournal.forgetObjective(unused.objective);
    await unused.restartStore();
    await unused.releaseUnusedAtStartup();
    expect((await unused.records())[0]).toMatchObject({
      reservationId: unused.plan.reservationId,
      activeInputs: [],
      revision: "1",
    });
    const signed = await setup();
    await signed.original.releaseUnused();
    await signed.restartStore();
    await signed.releaseUnusedAtStartup();
    expect(await signed.records()).toEqual([signed.pending]);
  });

  it("scopes runner cleanup to its captured reservation and retains journal-only submission uncertainty", async () => {
    const test = await setup(true);
    test.workflowJournal.forgetObjective(test.objective);
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
    const uncertain = await setup(true);
    await uncertain.append(uncertain.events.preflight);
    await uncertain.append(uncertain.events.submissionIntent);
    await uncertain.original.releaseUnused();
    await uncertain.releaseUnusedAtStartup();
    expect(await uncertain.records()).toEqual([uncertain.pending]);
  });

  it("refuses reconciliation funding before selection when the durable workflow is absent", async () => {
    const test = await setup();
    test.workflowJournal.forgetObjective(test.objective);
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
      test.chain.mockResolvedValue(expiredNotFound(test.transactionHash));
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
    test.chain.mockResolvedValue(expiredNotFound(test.transactionHash));
    const admitted = await test.createPermit(test.fresh, "2");
    const journal = test.bind(test.fresh, admitted);
    await test.run(journal);
    const [acknowledged] = await test.records();
    expect(acknowledged).toMatchObject({
      activeInputs: [],
      pendingTransition: null,
    });

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
    test.chain.mockResolvedValue({
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

  it("retains leases for an unknown signed attempt under reconciliation authority", async () => {
    const test = await setup();
    test.useUnspentPendingInputs();
    test.chain.mockResolvedValue({
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
    ).toEqual([test.events.submissionIntent]);
    expect(await test.records()).toEqual([test.pending]);
    expect(test.adapter.submit).not.toHaveBeenCalled();
    expect(test.adapter.preflight).not.toHaveBeenCalled();
  });

  it.each(["before", "after"] as const)(
    "completes once when the process stops %s the release transaction",
    async (boundary) => {
      const test = await setup();
      await test.run(await test.recover());
      // Confirm a second actual signed funding transaction and record the normal
      // removal lifecycle, so terminal normalization sees both confirmed actions.
      const funding = (await test.records())[0]!.activeInputs.find(
        ({ outRef }) => outRef === `${test.transactionHash}#0`,
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
      const events = {
        preflight: {
          ...test.events.preflight,
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
      } as const;
      await test.prepareAttempt(
        {
          plan: test.plan,
          expectedRevision: (await test.records())[0]!.revision,
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
        },
        events,
      );
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
      test.stopAt("release", boundary);
      await expect(test.run(await test.recover())).rejects.toBeInstanceOf(
        FixtureProcessStop,
      );
      const completedEntries = async () =>
        (await test.journal.load(test.initial.workflowId)).filter(
          ({ event }) => event.kind === "completed",
        );
      // The release and its completion commit together, or neither does.
      const [stopped] = await test.records();
      expect(stopped!.state).toBe(boundary === "after" ? "released" : "active");
      expect(await completedEntries()).toHaveLength(
        boundary === "after" ? 1 : 0,
      );
      await test.restartStore();
      if (boundary === "after")
        // The workflow is complete: it is never executed again.
        await expect(test.recover()).rejects.toThrow(
          "refuses a new execution over a completed workflow",
        );
      else
        expect(await test.run(await test.recover())).toMatchObject({
          kind: "completed",
          workflowId: test.initial.workflowId,
          terminal,
        });
      expect(test.adapter.preflight).not.toHaveBeenCalled();
      expect(test.adapter.submit).not.toHaveBeenCalled();
      expect(await test.records()).toMatchObject([
        { state: "released", activeInputs: [] },
      ]);
      expect(await test.store.readReservedOutRefs({})).toEqual([]);
      expect(await completedEntries()).toHaveLength(1);
    },
  );

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
    expect(test.workflowJournal.readWorkflowIds(10)).toEqual([
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

  it.each(["before", "after"] as const)(
    "confirms once when the process stops %s the confirmation transaction",
    async (boundary) => {
      const test = await setup();
      test.stopAt("confirmTransition", boundary);
      await expect(test.run(await test.recover())).rejects.toBeInstanceOf(
        FixtureProcessStop,
      );
      // The confirmation and its journal entries commit together, or
      // neither does.
      const confirmations = async () =>
        (await test.journal.load(test.initial.workflowId)).filter(
          ({ event }) =>
            event.kind === "confirmed" ||
            (event.kind === "reconciled" && event.outcome === "confirmed"),
        );
      expect(await confirmations()).toHaveLength(boundary === "after" ? 2 : 0);
      expect((await test.records())[0]!.pendingTransition === null).toBe(
        boundary === "after",
      );
      await test.restartStore();
      expect(await test.run(await test.recover())).toMatchObject({
        kind: "pending",
        workflowId: test.initial.workflowId,
      });
      const [committed] = await test.records();
      expect(committed).toMatchObject({
        state: "active",
        pendingTransition: null,
      });
      expect(
        committed!.activeInputs.some(
          ({ outRef }) => outRef === `${test.transactionHash}#0`,
        ),
      ).toBe(true);
      expect(test.adapter.reconcile).toHaveBeenCalledTimes(
        boundary === "after" ? 1 : 2,
      );
      expect(test.adapter.submit).not.toHaveBeenCalled();
      expect(await confirmations()).toHaveLength(2);
      const entries = await test.journal.load(test.initial.workflowId);
      expect(entries.slice(0, test.originalEntries.length)).toEqual(
        test.originalEntries,
      );
      expect(
        entries.filter(({ event }) => event.kind === "submission_intent"),
      ).toHaveLength(1);
    },
  );

  it("retains unknown submissions and their original signed bytes without another action", async () => {
    const test = await setup();
    test.chain.mockResolvedValue({
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
    test.chain.mockResolvedValue({
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
      journal: test.objectiveJournal(),
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

  it("abandons a canonically absent expired intent without inventing confirmation or submission", async () => {
    const test = await setup();
    test.chain.mockResolvedValue(expiredNotFound(test.transactionHash));
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
    await test.rewriteJournalEntries([
      {
        ...entry,
        event: {
          ...entry.event,
          artifact,
          artifactDigest: journalJsonDigest(normalizeJournalJson(artifact)),
        },
      },
    ]);
    await expect(test.recover()).rejects.toThrow("prepared artifact differs");
    expect(await test.records()).toEqual([test.pending]);
  });

  it("refuses two durable same-target executions before reserving", async () => {
    const test = await setup();
    const identity = {
      ...test.initial.identity,
      decisionDigest: test.fresh.decisionDigest,
    };
    const workflowId = computeFraudProofWorkflowId(identity);
    await test.objectiveJournal().append(
      {
        ...test.originalEntries[0]!,
        identity,
        workflowId,
      },
      0,
    );
    await expect(test.recover()).rejects.toThrow("multiple durable executions");
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
      journalAuthenticationKey: TEST_JOURNAL_KEY,
      workflowJournal: test.workflowJournal,
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
