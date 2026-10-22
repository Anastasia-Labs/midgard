import "node:crypto";
import "node:fs/promises";
import "node:path";
import "@al-ft/midgard-fault-proofs";
import "@lucid-evolution/lucid";
import "vitest";
import "../../src/funding/prover-funding-reservation.js";
import "../../src/funding/sqlite-prover-funding-reservation-store.js";
import "./sqlite-prover-funding-reservation-store.signed-transition.js";

import { createHash } from "node:crypto";
import { rm } from "node:fs/promises";

import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { parseWatcherProverFundingReservationRecord } from "../../src/funding/prover-funding-reservation.js";
import {
  isWatcherProverFundingReservationConflict,
  unsafeOpenWatcherSqliteProverFundingReservationStoreForTest,
} from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import {
  attemptOf,
  openStore,
  plan,
  prepareTransition,
  retirementFixture,
  signedTransition,
  temporaryDirectories,
} from "./sqlite-prover-funding-reservation-store.signed-transition.js";

afterEach(async () => {
  await Promise.all(
    temporaryDirectories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

describe("SQLite prover funding reservation store V1", () => {
  it("reclaims unused inputs with exact revision checks across process connections and idle refills", async () => {
    const opened = await openStore();
    const peer =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
        { path: opened.path },
        () => undefined,
      );
    const currentPlan = plan("aa", "66");
    try {
      await opened.runtime.store.reserve(currentPlan);
      const initial = parseWatcherProverFundingReservationRecord(
        (await opened.runtime.store.readAll())[0],
      );
      await expect(peer.store.releaseUnused!(initial)).resolves.toBe(true);
      const idle = parseWatcherProverFundingReservationRecord(
        (await opened.runtime.store.readAll())[0],
      );
      expect(idle).toMatchObject({
        state: "active",
        revision: "1",
        activeInputs: [],
      });
      await expect(opened.runtime.store.releaseUnused!(initial)).resolves.toBe(
        false,
      );
      await opened.runtime.store.reserve(currentPlan, idle.revision);
      const refreshed = parseWatcherProverFundingReservationRecord(
        (await opened.runtime.store.readAll())[0],
      );
      expect(refreshed.revision).toBe("2");
      await expect(peer.store.releaseUnused!(refreshed)).resolves.toBe(true);
      const again = parseWatcherProverFundingReservationRecord(
        (await opened.runtime.store.readAll())[0],
      );
      await opened.runtime.store.reserve(currentPlan, again.revision);
      const beforePrepare = parseWatcherProverFundingReservationRecord(
        (await opened.runtime.store.readAll())[0],
      );
      const signed = await prepareTransition(peer.store, {
        plan: currentPlan,
        expectedRevision: beforePrepare.revision,
        actionKind: "proof.init",
        ...signedTransition(),
        consumedOutRefs: [currentPlan.inputs[0]!.outRef],
      });
      // The second process won preparation: its signed attempt protects all
      // leases.
      await expect(
        opened.runtime.store.releaseUnused!(beforePrepare),
      ).resolves.toBe(false);
      await expect(opened.runtime.store.releaseUnused!(signed)).resolves.toBe(
        false,
      );
      expect(await opened.runtime.store.readAll()).toEqual([signed]);
    } finally {
      peer.close();
      opened.runtime.close();
    }
  });

  it("reobserves a confirmed attempt with current inputs and retains its signed bytes across restart", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    const signed = signedTransition();
    await opened.runtime.store.reserve(currentPlan);
    const prepared = await prepareTransition(opened.runtime.store, {
      plan: currentPlan,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...signed,
      consumedOutRefs: [currentPlan.inputs[0]!.outRef],
    });
    const confirmed = await opened.runtime.store.confirmTransition({
      journal: [],
      plan: currentPlan,
      expectedRevision: prepared.revision,
      transactionHash: signed.transactionHash,
      transitionDigest: prepared.pendingTransition!.transitionDigest,
    });
    opened.runtime.close();
    const runtime =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
        { path: opened.path },
        () => undefined,
      );
    try {
      const restored = await runtime.store.reobserveTransition!({
        journal: [],
        plan: currentPlan,
        expectedRevision: confirmed.revision,
        transactionHash: signed.transactionHash,
        inputs: currentPlan.inputs,
      });
      expect(restored.activeInputs).toEqual(currentPlan.inputs);
      expect(restored.pendingTransition?.signedTransactionCborHex).toBe(
        signed.signedTransactionCborHex,
      );
      await expect(runtime.store.readAll()).resolves.toHaveLength(1);
      const included = await runtime.store.confirmTransition({
        journal: [],
        plan: currentPlan,
        expectedRevision: restored.revision,
        transactionHash: signed.transactionHash,
        transitionDigest: restored.pendingTransition!.transitionDigest,
      });
      expect(
        included.activeInputs.some(
          ({ outRef }) => outRef === signed.producedInputs[0]!.outRef,
        ),
      ).toBe(true);
    } finally {
      runtime.close();
    }
  });

  it("keeps an included attempt's capital leased for a re-land across restart until final release", async () => {
    const opened = await openStore();
    let runtime = opened.runtime;
    const first = plan("aa", "66");
    const signed = signedTransition({
      collateralHash: "12".repeat(32),
      validityUpperBound: 1000n,
    });
    try {
      await runtime.store.reserve(first);
      const prepared = await prepareTransition(runtime.store, {
        plan: first,
        expectedRevision: "0",
        actionKind: "proof.remove",
        ...signed,
        consumedOutRefs: [first.inputs[0]!.outRef],
      });
      const confirmed = await runtime.store.confirmTransition({
        journal: [],
        plan: first,
        expectedRevision: prepared.revision,
        transactionHash: signed.transactionHash,
        transitionDigest: prepared.pendingTransition!.transitionDigest,
      });
      // Consumed and produced capital stays leased for a re-land; the
      // confirmed transaction's collateral lease ended at confirmation.
      const reserved = await runtime.store.readReservedOutRefs({});
      expect(reserved).toEqual(
        expect.arrayContaining([
          first.inputs[0]!.outRef,
          signed.producedInputs[0]!.outRef,
        ]),
      );
      expect(reserved).not.toContain(`${"12".repeat(32)}#0`);
      const competitorBase = plan("bb", "77");
      const competitor = {
        ...competitorBase,
        inputs: competitorBase.inputs.map((i) =>
          i.role === "collateral"
            ? { ...i, outRef: `${"13".repeat(32)}#0` }
            : i,
        ),
      };
      await expect(runtime.store.reserve(competitor)).rejects.toThrow(
        "already reserved",
      );
      runtime.close();
      runtime =
        await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
          { path: opened.path },
          () => undefined,
        );
      const reopened = await runtime.store.reobserveTransition!({
        journal: [],
        plan: first,
        expectedRevision: confirmed.revision,
        transactionHash: signed.transactionHash,
        inputs: first.inputs,
      });
      expect(reopened.pendingTransition?.signedTransactionCborHex).toBe(
        signed.signedTransactionCborHex,
      );
      const reincluded = await runtime.store.confirmTransition({
        journal: [],
        plan: first,
        expectedRevision: reopened.revision,
        transactionHash: signed.transactionHash,
        transitionDigest: reopened.pendingTransition!.transitionDigest,
      });
      const released = await runtime.store.release({
        journal: [],
        plan: first,
        expectedRevision: reincluded.revision,
      });
      expect(released.state).toBe("released");
      expect(await runtime.store.readReservedOutRefs({})).toEqual([]);
      await runtime.store.reserve(competitor);
      await expect(
        runtime.store.reobserveTransition!({
          journal: [],
          plan: first,
          expectedRevision: released.revision,
          transactionHash: signed.transactionHash,
          inputs: [],
        }),
      ).rejects.toThrow();
    } finally {
      runtime.close();
    }
  });

  it("refuses a prepare whose journal intent is another decision's or another transaction's", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    await opened.runtime.store.reserve(currentPlan);
    const [reserved] = await opened.runtime.store.readAll();
    const input = {
      plan: currentPlan,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...signedTransition(),
      consumedOutRefs: [`${"11".repeat(32)}#0`],
    };
    const attempt = attemptOf(input);
    const intent = (
      decisionDigest: string,
      txHash: string,
    ): FraudProofWorkflowJournalEntry => ({
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId: attempt.workflowId,
      identity: {
        schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        deploymentFingerprint: currentPlan.deploymentFingerprint,
        decisionDigest,
        category: "doubleSpend",
        target: { kind: "state_queue_header", headerHash: "55".repeat(28) },
      },
      sequence: attempt.intentSequence,
      recordedAt: new Date().toISOString(),
      event: {
        kind: "submission_intent",
        actionId: input.actionKind,
        actionInput: attempt.actionInput,
        attempt: 1,
        txHash,
      },
    });
    for (const journal of [
      [intent("77".repeat(32), input.transactionHash)],
      [intent(currentPlan.decisionDigest, "99".repeat(32))],
    ]) {
      await expect(
        opened.runtime.store.prepareTransition({ ...input, attempt, journal }),
      ).rejects.toThrow();
      expect(await opened.runtime.store.readAll()).toEqual([reserved]);
      expect(
        await opened.runtime.store.readRecordedSubmissions!({
          reservationId: currentPlan.reservationId,
        }),
      ).toEqual([]);
    }
    opened.runtime.close();
  });

  it("retains exact action and signed bytes after preparation commits and the process restarts", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    await opened.runtime.store.reserve(currentPlan);
    const input = {
      plan: currentPlan,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...signedTransition(),
      consumedOutRefs: [`${"11".repeat(32)}#0`],
    };
    await expect(
      (async () => {
        await prepareTransition(opened.runtime.store, input);
        throw new Error("interrupted after preparation commit");
      })(),
    ).rejects.toThrow("interrupted after preparation commit");
    opened.runtime.close();
    const reopened =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
        { path: opened.path },
        () => undefined,
      );
    try {
      const { plan: _plan, expectedRevision: _revision, ...transition } = input;
      expect(
        await reopened.store.readPendingTransition({
          reservationId: currentPlan.reservationId,
        }),
      ).toEqual(transition);
      expect(await reopened.store.readAll()).toHaveLength(1);
    } finally {
      reopened.close();
    }
  });

  it("keeps a released reservation released across restart, with no leases and no further change", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    await opened.runtime.store.reserve(currentPlan);
    await opened.runtime.store.release({
      journal: [],
      plan: currentPlan,
      expectedRevision: "0",
    });
    opened.runtime.close();
    const reopened =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
        { path: opened.path },
        () => undefined,
      );
    try {
      const [released] = await reopened.store.readAll();
      expect(released).toMatchObject({
        state: "released",
        revision: "1",
        activeInputs: [],
        pendingTransition: null,
      });
      expect(await reopened.store.readReservedOutRefs({})).toEqual([]);
      for (const expectedRevision of ["0", "1"])
        await expect(
          reopened.store.release({
            journal: [],
            plan: currentPlan,
            expectedRevision,
          }),
        ).rejects.toThrow("release mismatch");
      await expect(
        prepareTransition(reopened.store, {
          plan: currentPlan,
          expectedRevision: "1",
          actionKind: "proof.init",
          ...signedTransition(),
          consumedOutRefs: [`${"11".repeat(32)}#0`],
        }),
      ).rejects.toThrow();
      expect(await reopened.store.readAll()).toEqual([released]);
    } finally {
      reopened.close();
    }
  });

  it("supersedes a pending attempt once, keeps its signed bytes, and admits a replacement that spends its input", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    await opened.runtime.store.reserve(currentPlan);
    const signed = signedTransition();
    const pending = await prepareTransition(opened.runtime.store, {
      plan: currentPlan,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...signed,
      consumedOutRefs: [`${"11".repeat(32)}#0`],
    });
    const input = {
      journal: [],
      plan: currentPlan,
      expectedRevision: pending.revision,
      transitionDigest: pending.pendingTransition!.transitionDigest,
      retirement: retirementFixture(signed.transactionHash),
    };
    const abandoned =
      await opened.runtime.store.abandonPendingTransition(input);
    expect(abandoned.pendingTransition).toBeNull();
    await expect(
      opened.runtime.store.abandonPendingTransition({
        ...input,
        expectedRevision: abandoned.revision,
      }),
    ).rejects.toThrow();
    await expect(
      opened.runtime.store.release({
        journal: [],
        plan: currentPlan,
        expectedRevision: pending.revision,
      }),
    ).rejects.toThrow("release mismatch");
    const replacement = {
      plan: currentPlan,
      actionKind: "proof.init",
      ...signedTransition({
        outputLovelace: 98_000_000n,
        feeLovelace: 2_000_000n,
      }),
      consumedOutRefs: [`${"11".repeat(32)}#0`],
    };
    const prepared = await prepareTransition(opened.runtime.store, {
      ...replacement,
      expectedRevision: abandoned.revision,
    });
    await expect(
      prepareTransition(opened.runtime.store, {
        ...replacement,
        expectedRevision: abandoned.revision,
      }),
    ).rejects.toThrow("cannot prepare transition");
    expect(prepared.pendingTransition?.transactionHash).toBe(
      replacement.transactionHash,
    );
    opened.runtime.close();
    const reopened =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
        { path: opened.path },
        () => undefined,
      );
    try {
      expect(
        await reopened.store.readLegacyAbandonedTransactions!({
          reservationId: currentPlan.reservationId,
        }),
      ).toMatchObject([
        {
          transition: {
            signedTransactionCborHex: signed.signedTransactionCborHex,
          },
          retirement: input.retirement,
        },
      ]);
      expect(
        (
          (await reopened.store.readRecordedSubmissions!({
            reservationId: currentPlan.reservationId,
          })) as readonly { transition: { transactionHash: string } }[]
        )
          .map(({ transition }) => transition.transactionHash)
          .sort(),
      ).toEqual([signed.transactionHash, replacement.transactionHash].sort());
    } finally {
      reopened.close();
    }
  });

  it("confirms only the exact pending hash and digest, once, across restart", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    const signed = signedTransition();
    await opened.runtime.store.reserve(currentPlan);
    const pending = await prepareTransition(opened.runtime.store, {
      plan: currentPlan,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...signed,
      consumedOutRefs: [`${"11".repeat(32)}#0`],
    });
    const confirmation = {
      journal: [],
      plan: currentPlan,
      expectedRevision: pending.revision,
      transactionHash: signed.transactionHash,
      transitionDigest: pending.pendingTransition!.transitionDigest,
    };
    await expect(
      opened.runtime.store.confirmTransition({
        ...confirmation,
        transactionHash: "ff".repeat(32),
      }),
    ).rejects.toThrow("confirmation mismatch");
    const confirmed =
      await opened.runtime.store.confirmTransition(confirmation);
    opened.runtime.close();
    const restarted =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
        { path: opened.path },
        () => undefined,
      );
    try {
      const again = {
        ...confirmation,
        expectedRevision: confirmed.revision,
      };
      for (const changed of [
        {},
        { transactionHash: "ff".repeat(32) },
        { transitionDigest: "ff".repeat(32) },
        { expectedRevision: pending.revision },
      ]) {
        await expect(
          restarted.store.confirmTransition({ ...again, ...changed }),
        ).rejects.toThrow("confirmation mismatch");
        expect(await restarted.store.readAll()).toEqual([confirmed]);
      }
      const conflict = await restarted.store.markConflict({
        journal: [],
        plan: currentPlan,
        expectedRevision: confirmed.revision,
        code: "unexpected_spend",
      });
      await expect(
        restarted.store.confirmTransition({
          ...again,
          expectedRevision: conflict.revision,
        }),
      ).rejects.toThrow("confirmation mismatch");
      expect(await restarted.store.readAll()).toEqual([conflict]);
    } finally {
      restarted.close();
    }
  });

  it("persists protocol-funded income without consuming reserved wallet inputs", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    await opened.runtime.store.reserve(currentPlan);
    const input = {
      plan: currentPlan,
      expectedRevision: "0",
      actionKind: "proof.remove",
      ...signedTransition({
        inputHash: "99".repeat(32),
        nonCanonicalBody: true,
      }),
      consumedOutRefs: [],
    };
    // Empty consumption is derived from the signed body, never trusted from
    // the caller. A normal wallet spend cannot be disguised as protocol-funded.
    await expect(
      prepareTransition(opened.runtime.store, {
        ...input,
        ...signedTransition(),
      }),
    ).rejects.toThrow("differs from the signed transaction");
    const pending = await prepareTransition(opened.runtime.store, input);
    expect(pending.activeInputs).toEqual(currentPlan.inputs);
    expect(pending.pendingTransition?.consumedOutRefs).toEqual([]);
    opened.runtime.close();

    const reopened =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
        { path: opened.path },
        () => undefined,
      );
    try {
      expect(await reopened.store.readAll()).toEqual([pending]);
      await expect(
        reopened.store.confirmTransition({
          journal: [],
          plan: currentPlan,
          expectedRevision: pending.revision,
          transactionHash: pending.pendingTransition!.transactionHash,
          transitionDigest: "ff".repeat(32),
        }),
      ).rejects.toThrow("confirmation mismatch");
      const confirmed = await reopened.store.confirmTransition({
        journal: [],
        plan: currentPlan,
        expectedRevision: pending.revision,
        transactionHash: pending.pendingTransition!.transactionHash,
        transitionDigest: pending.pendingTransition!.transitionDigest,
      });
      expect(confirmed.pendingTransition).toBeNull();
      expect(confirmed.activeInputs).toEqual(
        [...currentPlan.inputs, ...input.producedInputs].sort((left, right) =>
          left.outRef.localeCompare(right.outRef),
        ),
      );
      expect(
        await reopened.store.readConfirmedInput({
          reservationId: currentPlan.reservationId,
          outRef: input.producedInputs[0]!.outRef,
        }),
      ).toMatchObject({
        sourceActionKind: "proof.remove",
        outRef: input.producedInputs[0]!.outRef,
      });
    } finally {
      reopened.close();
    }
  });

  it("persists exact noncanonical signed wire across restart and rejects re-encoded identity", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    await opened.runtime.store.reserve(currentPlan);
    const signed = signedTransition({ nonCanonicalBody: true });
    const transaction = CML.Transaction.from_cbor_hex(
      signed.signedTransactionCborHex,
    );
    expect(transaction.to_canonical_cbor_hex()).not.toBe(
      signed.signedTransactionCborHex,
    );
    const canonicalBodySha = createHash("sha256")
      .update(Buffer.from(transaction.body().to_canonical_cbor_hex(), "hex"))
      .digest("hex");
    expect(canonicalBodySha).not.toBe(signed.transactionBodySha256);
    const input = {
      plan: currentPlan,
      expectedRevision: "0",
      actionKind: "proof.step-01",
      ...signed,
      consumedOutRefs: [`${"11".repeat(32)}#0`],
    };
    await expect(
      prepareTransition(opened.runtime.store, {
        ...input,
        transactionBodySha256: canonicalBodySha,
      }),
    ).rejects.toThrow("differs from the signed transaction");
    await expect(
      prepareTransition(opened.runtime.store, {
        ...input,
        signedTransactionCborHex: transaction.to_canonical_cbor_hex(),
      }),
    ).rejects.toThrow("funding witness");
    const prepared = await prepareTransition(opened.runtime.store, input);
    expect(prepared.pendingTransition?.signedTransactionCborHex).toBe(
      signed.signedTransactionCborHex,
    );
    expect(prepared.pendingTransition?.transactionBodySha256).toBe(
      signed.transactionBodySha256,
    );
    opened.runtime.close();
    const reopened =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
        { path: opened.path },
        () => undefined,
      );
    expect(await reopened.store.readAll()).toEqual([prepared]);
    reopened.close();
  });

  it("atomically persists descendant rotation and restart conflict state", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    expect(await opened.runtime.store.reserve(currentPlan)).toBe("reserved");
    expect(await opened.runtime.store.reserve(currentPlan)).toBe("unchanged");
    for (const field of ["policyDigest", "reservationBasisDigest"] as const) {
      await expect(
        opened.runtime.store.reserve({
          ...currentPlan,
          [field]: "ff".repeat(32),
        }),
      ).rejects.toThrow("identity mismatch");
    }

    const signed = signedTransition();
    const prepared = await prepareTransition(opened.runtime.store, {
      plan: currentPlan,
      expectedRevision: "0",
      actionKind: "proof.step-01",
      ...signed,
      consumedOutRefs: [`${"11".repeat(32)}#0`],
    });
    expect(prepared).toMatchObject({
      revision: "1",
      state: "active",
      pendingTransition: {
        transactionHash: signed.transactionHash,
      },
    });
    await expect(
      opened.runtime.store.readConfirmedInput({
        reservationId: currentPlan.reservationId,
        outRef: `${signed.transactionHash}#0`,
      }),
    ).resolves.toBeNull();
    await expect(
      opened.runtime.store.release({
        journal: [],
        plan: currentPlan,
        expectedRevision: "1",
      }),
    ).rejects.toThrow("release mismatch");

    opened.runtime.close();
    const restarted =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
        { path: opened.path },
        () => undefined,
      );
    expect(await restarted.store.readAll()).toEqual([prepared]);

    await expect(
      restarted.store.confirmTransition({
        journal: [],
        plan: currentPlan,
        expectedRevision: "0",
        transactionHash: prepared.pendingTransition!.transactionHash,
        transitionDigest: prepared.pendingTransition!.transitionDigest,
      }),
    ).rejects.toThrow("confirmation mismatch");
    const confirmed = await restarted.store.confirmTransition({
      journal: [],
      plan: currentPlan,
      expectedRevision: "1",
      transactionHash: prepared.pendingTransition!.transactionHash,
      transitionDigest: prepared.pendingTransition!.transitionDigest,
    });
    expect(confirmed).toMatchObject({
      revision: "2",
      pendingTransition: null,
      activeInputs: [
        { outRef: `${"12".repeat(32)}#0`, role: "collateral" },
        { outRef: `${signed.transactionHash}#0`, role: "funding" },
      ],
    });
    await expect(
      restarted.store.readConfirmedInput({
        reservationId: currentPlan.reservationId,
        outRef: `${signed.transactionHash}#0`,
      }),
    ).resolves.toEqual({
      sourceActionKind: "proof.step-01",
      sourceOutputIndex: 0,
      outRef: `${signed.transactionHash}#0`,
      resolvedOutputCborHex: CML.Transaction.from_cbor_hex(
        signed.signedTransactionCborHex,
      )
        .body()
        .outputs()
        .get(0)
        .to_canonical_cbor_hex(),
    });
    await expect(
      restarted.store.readConfirmedInput({
        reservationId: "bb".repeat(32),
        outRef: `${signed.transactionHash}#0`,
      }),
    ).resolves.toBeNull();
    await expect(
      restarted.store.readConfirmedInput({
        reservationId: currentPlan.reservationId,
        outRef: `${"ff".repeat(32)}#0`,
      }),
    ).resolves.toBeNull();
    const secondSigned = signedTransition({
      inputHash: signed.transactionHash,
      outputLovelace: 98_000_000n,
    });
    const secondPrepared = await prepareTransition(restarted.store, {
      plan: currentPlan,
      expectedRevision: "2",
      actionKind: "proof.step-02",
      ...secondSigned,
      consumedOutRefs: [`${signed.transactionHash}#0`],
    });
    const secondConfirmed = await restarted.store.confirmTransition({
      journal: [],
      plan: currentPlan,
      expectedRevision: "3",
      transactionHash: secondPrepared.pendingTransition!.transactionHash,
      transitionDigest: secondPrepared.pendingTransition!.transitionDigest,
    });
    expect(secondConfirmed).toMatchObject({
      revision: "4",
      activeInputs: [
        { outRef: `${"12".repeat(32)}#0`, role: "collateral" },
        { outRef: `${secondSigned.transactionHash}#0`, role: "funding" },
      ],
    });

    let collision: unknown;
    try {
      await restarted.store.reserve(plan("bb", "99", `${"12".repeat(32)}#0`));
    } catch (error) {
      collision = error;
    }
    expect(isWatcherProverFundingReservationConflict(collision)).toBe(true);
    if (isWatcherProverFundingReservationConflict(collision)) {
      expect(collision.conflict).toEqual({
        code: "reservation_collision",
        outRef: `${"12".repeat(32)}#0`,
      });
    }
    expect(
      isWatcherProverFundingReservationConflict(
        Object.assign(new Error("lookalike"), {
          conflict: {
            code: "reservation_collision",
            outRef: `${"12".repeat(32)}#0`,
          },
        }),
      ),
    ).toBe(false);

    const conflicted = await restarted.store.markConflict({
      journal: [],
      plan: currentPlan,
      expectedRevision: "4",
      code: "unexpected_spend",
    });
    expect(conflicted).toMatchObject({
      revision: "5",
      state: "conflict",
      conflictCode: "unexpected_spend",
    });
    restarted.close();

    const secondRestart =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
        { path: opened.path },
        () => undefined,
      );
    expect(await secondRestart.store.readAll()).toEqual([conflicted]);
    await expect(
      secondRestart.store.readConfirmedInput({
        reservationId: currentPlan.reservationId,
        outRef: `${signed.transactionHash}#0`,
      }),
    ).resolves.toMatchObject({ outRef: `${signed.transactionHash}#0` });
    secondRestart.close();
  });

  it.each(["publish-raw-datum-preimage", "proof.step-01"])(
    "keeps separate confirmed lineage for repeated %s transactions across restart",
    async (actionKind) => {
      const opened = await openStore();
      let runtime = opened.runtime;
      const currentPlan = plan("aa", "66");
      try {
        await runtime.store.reserve(currentPlan);
        const firstSigned = signedTransition();
        const firstPrepared = await prepareTransition(runtime.store, {
          plan: currentPlan,
          expectedRevision: "0",
          actionKind,
          ...firstSigned,
          consumedOutRefs: [`${"11".repeat(32)}#0`],
        });
        const firstConfirmed = await runtime.store.confirmTransition({
          journal: [],
          plan: currentPlan,
          expectedRevision: firstPrepared.revision,
          transactionHash: firstPrepared.pendingTransition!.transactionHash,
          transitionDigest: firstPrepared.pendingTransition!.transitionDigest,
        });
        const secondSigned = signedTransition({
          inputHash: firstSigned.transactionHash,
          outputLovelace: 98_000_000n,
          nonCanonicalBody: true,
        });
        const secondInput = {
          plan: currentPlan,
          expectedRevision: firstConfirmed.revision,
          actionKind,
          ...secondSigned,
          consumedOutRefs: [`${firstSigned.transactionHash}#0`],
        };
        const secondPrepared = await prepareTransition(
          runtime.store,
          secondInput,
        );
        await expect(
          prepareTransition(runtime.store, secondInput),
        ).rejects.toThrow("cannot prepare transition");
        await expect(
          runtime.store.readConfirmedInput({
            reservationId: currentPlan.reservationId,
            outRef: `${secondSigned.transactionHash}#0`,
          }),
        ).resolves.toBeNull();
        runtime.close();
        runtime =
          await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
            { path: opened.path },
            () => undefined,
          );
        expect(await runtime.store.readAll()).toEqual([secondPrepared]);
        expect(secondPrepared.pendingTransition?.signedTransactionCborHex).toBe(
          secondSigned.signedTransactionCborHex,
        );
        await expect(
          runtime.store.confirmTransition({
            journal: [],
            plan: currentPlan,
            expectedRevision: secondPrepared.revision,
            transactionHash: secondPrepared.pendingTransition!.transactionHash,
            transitionDigest: firstPrepared.pendingTransition!.transitionDigest,
          }),
        ).rejects.toThrow("confirmation mismatch");
        const secondConfirmed = await runtime.store.confirmTransition({
          journal: [],
          plan: currentPlan,
          expectedRevision: secondPrepared.revision,
          transactionHash: secondPrepared.pendingTransition!.transactionHash,
          transitionDigest: secondPrepared.pendingTransition!.transitionDigest,
        });
        expect(
          secondConfirmed.activeInputs.filter(({ role }) => role === "funding"),
        ).toEqual(secondSigned.producedInputs);
        const assertConfirmedHistory = async () => {
          for (const signed of [firstSigned, secondSigned]) {
            await expect(
              runtime.store.readConfirmedInput({
                reservationId: currentPlan.reservationId,
                outRef: `${signed.transactionHash}#0`,
              }),
            ).resolves.toEqual({
              sourceActionKind: actionKind,
              sourceOutputIndex: 0,
              outRef: `${signed.transactionHash}#0`,
              resolvedOutputCborHex: CML.Transaction.from_cbor_hex(
                signed.signedTransactionCborHex,
              )
                .body()
                .outputs()
                .get(0)
                .to_canonical_cbor_hex(),
            });
          }
        };
        await assertConfirmedHistory();
        const thirdSigned = signedTransition({
          inputHash: secondSigned.transactionHash,
          outputLovelace: 97_000_000n,
        });
        const thirdPrepared = await prepareTransition(runtime.store, {
          plan: currentPlan,
          expectedRevision: secondConfirmed.revision,
          actionKind,
          ...thirdSigned,
          consumedOutRefs: [`${secondSigned.transactionHash}#0`],
        });
        const abandoned = await runtime.store.abandonPendingTransition({
          journal: [],
          retirement: retirementFixture(thirdSigned.transactionHash),
          plan: currentPlan,
          expectedRevision: thirdPrepared.revision,
          transitionDigest: thirdPrepared.pendingTransition!.transitionDigest,
        });
        expect(abandoned.activeInputs).toEqual(secondConfirmed.activeInputs);
        runtime.close();
        runtime =
          await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
            { path: opened.path },
            () => undefined,
          );
        expect(await runtime.store.readAll()).toEqual([abandoned]);
        await assertConfirmedHistory();
        await expect(
          runtime.store.readConfirmedInput({
            reservationId: currentPlan.reservationId,
            outRef: `${thirdSigned.transactionHash}#0`,
          }),
        ).resolves.toBeNull();
      } finally {
        runtime.close();
      }
    },
  );

  it("rejects unreserved consumption and substituted produced out-refs", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    await opened.runtime.store.reserve(currentPlan);

    await expect(
      prepareTransition(opened.runtime.store, {
        plan: currentPlan,
        expectedRevision: "0",
        actionKind: "proof.step-01",
        ...signedTransition({ inputHash: "13".repeat(32) }),
        consumedOutRefs: [`${"13".repeat(32)}#0`],
      }),
    ).rejects.toThrow("unreserved input");
    const signed = signedTransition();
    await expect(
      prepareTransition(opened.runtime.store, {
        plan: currentPlan,
        expectedRevision: "0",
        actionKind: "proof.step-01",
        signedTransactionCborHex: signed.signedTransactionCborHex,
        transactionHash: signed.transactionHash,
        transactionBodySha256: signed.transactionBodySha256,
        consumedOutRefs: [`${"11".repeat(32)}#0`],
        producedInputs: [
          {
            outRef: `${"78".repeat(32)}#0`,
            role: "funding",
            lovelace: "99000000",
            assets: [],
          },
        ],
      }),
    ).rejects.toThrow("differs from the signed transaction");
    expect(await opened.runtime.store.readAll()).toMatchObject([
      { revision: "0", pendingTransition: null },
    ]);
    opened.runtime.close();
  });
});
