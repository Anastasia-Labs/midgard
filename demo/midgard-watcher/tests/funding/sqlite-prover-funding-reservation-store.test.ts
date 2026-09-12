import { createHash } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  journalJsonDigest,
  normalizeJournalJson,
  type WorkflowFundingAbandonmentHandoff,
  type WorkflowFundingCompletionHandoff,
  type WorkflowFundingSubmissionHandoff,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import type {
  WatcherProverFundingReservationPlan,
  WatcherProverFundingReservationStore,
} from "../../src/funding/prover-funding-reservation.js";
import {
  isWatcherProverFundingReservationConflict,
  unsafeOpenWatcherSqliteProverFundingReservationStoreForTest,
} from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import { fundingTerminal } from "./funding-handoff-fixture.js";

const temporaryDirectories: string[] = [];
const fundingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x55));
const fundingKeyHash = fundingKey.to_public().hash().to_hex();
const walletAddress = CML.Address.from_raw_bytes(
  Buffer.concat([Buffer.from([0x60]), Buffer.from(fundingKeyHash, "hex")]),
).to_bech32();

const signedTransition = ({
  inputHash = "11".repeat(32),
  outputLovelace = 99_000_000n,
  nonCanonicalBody = false,
  feeLovelace = 1_000_000n,
}: {
  readonly inputHash?: string;
  readonly outputLovelace?: bigint;
  readonly nonCanonicalBody?: boolean;
  readonly feeLovelace?: bigint;
} = {}) => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(inputHash), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(walletAddress),
      CML.Value.from_coin(outputLovelace),
    ),
  );
  const canonicalBody = CML.TransactionBody.new(inputs, outputs, feeLovelace);
  const body = nonCanonicalBody
    ? CML.TransactionBody.from_cbor_hex(
        "bf" + canonicalBody.to_cbor_hex().slice(2) + "ff",
      )
    : canonicalBody;
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      fundingKey.to_public(),
      fundingKey.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  const transaction = CML.Transaction.new(body, witnesses, true, undefined);
  const transactionHash = CML.hash_transaction(body).to_hex();
  return Object.freeze({
    signedTransactionCborHex: transaction.to_cbor_hex(),
    transactionHash,
    transactionBodySha256: createHash("sha256")
      .update(Buffer.from(body.to_cbor_hex(), "hex"))
      .digest("hex"),
    producedInputs: Object.freeze([
      Object.freeze({
        outRef: `${transactionHash}#0`,
        role: "funding" as const,
        lovelace: outputLovelace.toString(),
        assets: Object.freeze([]),
      }),
    ]),
  });
};

afterEach(async () => {
  await Promise.all(
    temporaryDirectories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

const openStore = async () => {
  const directory = await mkdtemp(
    join(process.cwd(), ".watcher-funding-reservation-test-"),
  );
  temporaryDirectories.push(directory);
  const path = join(directory, "watcher.sqlite");
  return {
    path,
    runtime: await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
      { path },
      () => undefined,
    ),
  };
};

const plan = (
  reservationByte: string,
  decisionByte: string,
  fundingOutRef = `${"11".repeat(32)}#0`,
): WatcherProverFundingReservationPlan =>
  Object.freeze({
    schemaVersion:
      "midgard-watcher-production-prover-funding-reservation-plan-v1",
    deploymentFingerprint: "22".repeat(32),
    decisionDigest: decisionByte.repeat(32),
    policyDigest: "33".repeat(32),
    reservationBasisDigest: "44".repeat(32),
    fundingPaymentKeyHash: fundingKeyHash,
    walletAddress,
    inputs: Object.freeze([
      Object.freeze({
        outRef: fundingOutRef,
        role: "funding" as const,
        lovelace: "100000000",
        assets: Object.freeze([]),
      }),
      Object.freeze({
        outRef: `${"12".repeat(32)}#0`,
        role: "collateral" as const,
        lovelace: "5000000",
        assets: Object.freeze([]),
      }),
    ]),
    fundingLovelace: "100000000",
    collateralLovelace: "5000000",
    assets: Object.freeze([]),
    reservationId: reservationByte.repeat(32),
  });

const handoffIdentity = (plan: WatcherProverFundingReservationPlan) => {
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: plan.deploymentFingerprint,
    decisionDigest: plan.decisionDigest,
    category: "doubleSpend" as const,
    target: {
      kind: "state_queue_header" as const,
      headerHash: "55".repeat(28),
    },
  };
  return {
    identity,
    workflowId: computeFraudProofWorkflowId(identity),
    preparedArtifactDigest: "66".repeat(32),
    expectedJournalSequence: 2,
  };
};
const submissionHandoff = (
  input: Omit<
    Parameters<WatcherProverFundingReservationStore["prepareTransition"]>[0],
    "handoff"
  >,
): WorkflowFundingSubmissionHandoff => ({
  ...handoffIdentity(input.plan),
  preflight: {
    kind: "preflight_passed",
    actionId: input.actionKind,
    txHash: input.transactionHash,
    localEvaluator: "test-uplc",
    referenceScripts: [
      {
        role: "init",
        outRef: `${"77".repeat(32)}#0`,
        scriptHash: "88".repeat(28),
      },
    ],
  },
  submissionIntent: {
    kind: "submission_intent",
    actionId: input.actionKind,
    actionInput: { actionKind: input.actionKind },
    txHash: input.transactionHash,
    attempt: 1,
  },
});
const prepareTransition = (
  store: WatcherProverFundingReservationStore,
  input: Omit<
    Parameters<WatcherProverFundingReservationStore["prepareTransition"]>[0],
    "handoff"
  >,
) => store.prepareTransition({ ...input, handoff: submissionHandoff(input) });
const abandonmentHandoff = (
  plan: WatcherProverFundingReservationPlan,
  transactionHash: string,
  actionKind: string,
): WorkflowFundingAbandonmentHandoff => ({
  ...handoffIdentity(plan),
  submissionIntent: {
    kind: "submission_intent",
    actionId: actionKind,
    actionInput: { actionKind },
    attempt: 1,
    txHash: transactionHash,
  },
  reconciliation: {
    kind: "reconciled",
    actionId: actionKind,
    outcome: "not_found",
    txHash: transactionHash,
  },
});
const completionHandoff = (
  plan: WatcherProverFundingReservationPlan,
): WorkflowFundingCompletionHandoff => {
  const terminal = fundingTerminal(
    "55".repeat(28),
    "aa".repeat(32),
    "bb".repeat(32),
  );
  return {
    ...handoffIdentity(plan),
    completion: {
      kind: "completed",
      terminal,
      terminalDigest: journalJsonDigest(normalizeJournalJson(terminal)),
    },
  };
};

describe("SQLite prover funding reservation store V1", () => {
  it("atomically rejects a handoff for another decision or signed transaction", async () => {
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
    for (const handoff of [
      submissionHandoff({ ...input, plan: plan("aa", "77") }),
      submissionHandoff({ ...input, transactionHash: "99".repeat(32) }),
    ]) {
      await expect(
        opened.runtime.store.prepareTransition({ ...input, handoff }),
      ).rejects.toThrow();
      expect(await opened.runtime.store.readAll()).toEqual([reserved]);
      expect(
        await opened.runtime.store.readPendingHandoff({
          reservationId: currentPlan.reservationId,
        }),
      ).toBeNull();
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
      expect(
        await reopened.store.readPendingHandoff({
          reservationId: currentPlan.reservationId,
        }),
      ).toEqual({ transition, handoff: submissionHandoff(input) });
      expect(await reopened.store.readAll()).toHaveLength(1);
    } finally {
      reopened.close();
    }
  });

  it("retains the exact terminal across release interruption without reopening leases", async () => {
    const opened = await openStore();
    const currentPlan = plan("aa", "66");
    await opened.runtime.store.reserve(currentPlan);
    const handoff = completionHandoff(currentPlan);
    await expect(
      (async () => {
        await opened.runtime.store.release({
          plan: currentPlan,
          expectedRevision: "0",
          handoff,
        });
        throw new Error("interrupted after release commit");
      })(),
    ).rejects.toThrow("interrupted after release commit");
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
      expect(
        await reopened.store.readCompletionHandoff({
          reservationId: currentPlan.reservationId,
        }),
      ).toEqual(handoff);
      await expect(
        reopened.store.release({
          plan: currentPlan,
          expectedRevision: "1",
          handoff,
        }),
      ).resolves.toEqual(released);
      await expect(
        reopened.store.release({
          plan: currentPlan,
          expectedRevision: "0",
          handoff,
        }),
      ).rejects.toThrow("release mismatch");
      await expect(
        reopened.store.release({
          plan: currentPlan,
          expectedRevision: "1",
          handoff: { ...handoff, expectedJournalSequence: 3 },
        }),
      ).rejects.toThrow("handoff mismatch");
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

  it("blocks replacement until exact abandonment acknowledgement and archives signed bytes", async () => {
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
    const handoff = abandonmentHandoff(
      currentPlan,
      signed.transactionHash,
      "proof.init",
    );
    const input = {
      plan: currentPlan,
      expectedRevision: pending.revision,
      transitionDigest: pending.pendingTransition!.transitionDigest,
      handoff,
    };
    const abandoned =
      await opened.runtime.store.abandonPendingTransition(input);
    expect(
      await opened.runtime.store.abandonPendingTransition({
        ...input,
        expectedRevision: abandoned.revision,
      }),
    ).toEqual(abandoned);
    const replacement = {
      plan: currentPlan,
      actionKind: "proof.init",
      ...signedTransition({
        outputLovelace: 98_000_000n,
        feeLovelace: 2_000_000n,
      }),
      consumedOutRefs: [`${"11".repeat(32)}#0`],
    };
    await expect(
      prepareTransition(opened.runtime.store, {
        ...replacement,
        expectedRevision: abandoned.revision,
      }),
    ).rejects.toThrow("cannot prepare transition");
    await expect(
      opened.runtime.store.release({
        plan: currentPlan,
        expectedRevision: abandoned.revision,
        handoff: completionHandoff(currentPlan),
      }),
    ).rejects.toThrow("release mismatch");
    for (const changed of [
      { ...handoff, expectedJournalSequence: 3 },
      {
        ...handoff,
        submissionIntent: {
          ...handoff.submissionIntent,
          txHash: "99".repeat(32),
        },
        reconciliation: { ...handoff.reconciliation, txHash: "99".repeat(32) },
      },
    ])
      await expect(
        opened.runtime.store.acknowledgeAbandonment({
          plan: currentPlan,
          expectedRevision: abandoned.revision,
          handoff: changed,
        }),
      ).rejects.toThrow("acknowledgement mismatch");
    await expect(
      opened.runtime.store.acknowledgeAbandonment({
        plan: currentPlan,
        expectedRevision: pending.revision,
        handoff,
      }),
    ).rejects.toThrow("acknowledgement mismatch");
    const acknowledged = await opened.runtime.store.acknowledgeAbandonment({
      plan: currentPlan,
      expectedRevision: abandoned.revision,
      handoff,
    });
    expect(
      await opened.runtime.store.acknowledgeAbandonment({
        plan: currentPlan,
        expectedRevision: acknowledged.revision,
        handoff,
      }),
    ).toEqual(acknowledged);
    expect(
      await opened.runtime.store.readAbandonmentHandoff({
        reservationId: currentPlan.reservationId,
      }),
    ).toBeNull();
    const prepared = await prepareTransition(opened.runtime.store, {
      ...replacement,
      expectedRevision: acknowledged.revision,
    });
    await expect(
      prepareTransition(opened.runtime.store, {
        ...replacement,
        expectedRevision: acknowledged.revision,
      }),
    ).rejects.toThrow("cannot prepare transition");
    expect(prepared.pendingTransition?.transactionHash).toBe(
      replacement.transactionHash,
    );
    opened.runtime.close();
    const database = new DatabaseSync(opened.path, { readOnly: true });
    try {
      const rows = database
        .prepare(
          "SELECT canonical_json, acknowledged_revision FROM watcher_prover_funding_abandonment_v1",
        )
        .all();
      expect(rows).toHaveLength(1);
      expect(rows[0]!.acknowledged_revision).toBe(acknowledged.revision);
      expect(JSON.parse(String(rows[0]!.canonical_json))).toMatchObject({
        transition: {
          signedTransactionCborHex: signed.signedTransactionCborHex,
        },
        handoff,
      });
      const submissions = database
        .prepare(
          "SELECT canonical_json FROM watcher_prover_funding_handoff_v1 WHERE kind = 'submission'",
        )
        .all();
      expect(
        submissions
          .map(
            (row) =>
              JSON.parse(String(row.canonical_json)).transition.transactionHash,
          )
          .sort(),
      ).toEqual([signed.transactionHash, replacement.transactionHash].sort());
    } finally {
      database.close();
    }
  });

  it("acknowledges only the exact last confirmed hash, digest and current revision after restart", async () => {
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
      const acknowledged = {
        ...confirmation,
        expectedRevision: confirmed.revision,
      };
      expect(await restarted.store.confirmTransition(acknowledged)).toEqual(
        confirmed,
      );
      for (const changed of [
        { transactionHash: "ff".repeat(32) },
        { transitionDigest: "ff".repeat(32) },
        { expectedRevision: pending.revision },
      ]) {
        await expect(
          restarted.store.confirmTransition({ ...acknowledged, ...changed }),
        ).rejects.toThrow("confirmation mismatch");
        expect(await restarted.store.readAll()).toEqual([confirmed]);
      }
      const conflict = await restarted.store.markConflict({
        plan: currentPlan,
        expectedRevision: confirmed.revision,
        code: "unexpected_spend",
      });
      await expect(
        restarted.store.confirmTransition({
          ...acknowledged,
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
          plan: currentPlan,
          expectedRevision: pending.revision,
          transactionHash: pending.pendingTransition!.transactionHash,
          transitionDigest: "ff".repeat(32),
        }),
      ).rejects.toThrow("confirmation mismatch");
      const confirmed = await reopened.store.confirmTransition({
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
        handoff: completionHandoff(currentPlan),
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
        plan: currentPlan,
        expectedRevision: "0",
        transactionHash: prepared.pendingTransition!.transactionHash,
        transitionDigest: prepared.pendingTransition!.transitionDigest,
      }),
    ).rejects.toThrow("confirmation mismatch");
    const confirmed = await restarted.store.confirmTransition({
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
            plan: currentPlan,
            expectedRevision: secondPrepared.revision,
            transactionHash: secondPrepared.pendingTransition!.transactionHash,
            transitionDigest: firstPrepared.pendingTransition!.transitionDigest,
          }),
        ).rejects.toThrow("confirmation mismatch");
        const secondConfirmed = await runtime.store.confirmTransition({
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
          handoff: abandonmentHandoff(
            currentPlan,
            thirdSigned.transactionHash,
            actionKind,
          ),
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
