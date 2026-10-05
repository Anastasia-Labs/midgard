import type { TxSignBuilder, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import type { EventHistoryBuildContext } from "../src/user-events/history-build.js";
import {
  EventHistoryInputReservedError,
  type EventHistorySubmissionAttempt,
  type EventHistorySubmissionCheckpoint,
  type EventHistorySubmissionDriver,
  type EventHistorySubmissionOutcome,
  EventHistorySubmissionPendingError,
  eventHistorySubmissionRequestHash,
  submitEventHistory,
} from "../src/user-events/history-submit.js";
import { EVENT_HISTORY_ABANDONED_RETENTION_MS } from "../src/user-events/history-submit-abandoned.js";

// The state machine is under test, not transaction construction: each build
// returns a distinct fake body, as a rebuild against a moved head would.
vi.mock("../src/user-events/history-build.js", async (importOriginal) => ({
  ...(await importOriginal<
    typeof import("../src/user-events/history-build.js")
  >()),
  buildEventHistoryPublication: vi.fn(async () => ({
    tx: build(),
    publicationOutputIndex: 0,
  })),
  buildEventHistoryAdmission: vi.fn(async () => ({
    tx: build(),
    orderOutputIndex: 0,
  })),
}));

let builds = 0;
const hashOf = (build: number) => build.toString(16).padStart(64, "0");
const build = () => {
  const hash = hashOf(++builds);
  return {
    toHash: () => hash,
    toCBOR: () => `body-${hash}`,
  } as unknown as TxSignBuilder;
};
const owner = "aa".repeat(28);
const nonce: UTxO = {
  txHash: "bb".repeat(32),
  outputIndex: 0,
  address: "wallet",
  assets: { lovelace: 10_000_000n },
};
const request = {
  payload: {
    DepositPayload: {
      event: {
        id: { transactionId: nonce.txHash, outputIndex: 0n },
        info: {
          l2_address: {
            paymentCredential: { PublicKeyCredential: [owner] as [string] },
            stakeCredential: null,
          },
          l2_network_id: 0n,
          l2_datum: null,
        },
      },
    },
  },
  reclaimAuth: { PublicKeyCredential: [owner] as [string] },
  nonce,
  assets: { lovelace: 5_000_000n },
  structuralLovelace: 2_000_000n,
  structuralRefundKey: owner,
};
const context = {
  lucid: { utxosByOutRef: async () => [nonce] },
  applied: { policyId: "cc".repeat(28) },
  recipe: {
    hubPolicyId: "dd".repeat(28),
    kind: "Deposit",
    initializationNonce: { transactionId: "ee".repeat(32), outputIndex: 0n },
    protectionDurationMs: 60_000n,
    inlineLimitBytes: 512n,
    maxPayloadBytes: 15_000n,
    maxPayloadNodes: 1_024n,
  },
} as unknown as Omit<EventHistoryBuildContext, "fundingInputs">;

/** A journal whose `save` refuses the first `held` pending attempts, as the
 * node journal does while another local submission holds the list head. */
const setup = (
  held: number,
  deadlineMs = 10_000_000,
  inlineLimitBytes = context.recipe.inlineLimitBytes,
) => {
  builds = 0;
  let clock = 1_000_000;
  let refusals = 0;
  let saved: EventHistorySubmissionCheckpoint | undefined;
  const history: EventHistorySubmissionCheckpoint[] = [];
  const submitted: EventHistorySubmissionAttempt[] = [];
  const driver: EventHistorySubmissionDriver = {
    save: async (next) => {
      if (next.pending !== undefined && refusals < held) {
        refusals++;
        throw new EventHistoryInputReservedError("ff".repeat(32) + "#0");
      }
      saved = next;
      history.push(next);
    },
    submit: async (_tx, attempt) => {
      expect(saved?.pending).toEqual(attempt);
      submitted.push(attempt);
      return { kind: "Confirmed" };
    },
    reconcile: async () => ({ kind: "Confirmed" }),
    funding: async () => [],
    now: () => clock,
    waitUntil: async (target) => {
      clock = Math.max(clock, target);
    },
  };
  const run = (
    overrides: Partial<EventHistorySubmissionDriver> = {},
    resumed?: { checkpoint: EventHistorySubmissionCheckpoint; spent?: true },
  ) =>
    submitEventHistory({
      context: {
        ...context,
        recipe: { ...context.recipe, inlineLimitBytes },
        ...(resumed?.spent && {
          lucid: { utxosByOutRef: async () => [] },
        }),
      } as typeof context,
      request,
      checkpoint: resumed?.checkpoint,
      driver: { ...driver, ...overrides },
      maxAttempts: 2,
      deadlineMs,
      validityDurationMs: 120_000,
      outputVisibilityAttempts: 1,
      retryDelayMs: 5_000,
    });
  return {
    run,
    saved: () => saved,
    history,
    submitted,
    refusals: () => refusals,
    now: () => clock,
  };
};

describe("history submission meeting a locally reserved input", () => {
  it("rebuilds past more refusals than attempts and records only the landed body", async () => {
    const s = setup(5);
    const result = await s.run();
    expect(s.refusals()).toBe(5);
    expect(builds).toBe(6);
    // Only the sixth build was ever recorded, broadcast or admitted.
    const landed = hashOf(6);
    expect(s.submitted.map((attempt) => attempt.txHash)).toEqual([landed]);
    expect(s.history.map((checkpoint) => checkpoint.pending?.txHash)).toEqual([
      landed,
      undefined,
    ]);
    expect(result.admission.txHash).toBe(landed);
    expect(result.pending).toBeUndefined();
    expect(s.now()).toBe(1_000_000 + 5 * 5_000);
  });

  it("waits the same way before recording an external publication", async () => {
    const s = setup(3, undefined, 1n);
    const result = await s.run();
    expect(s.submitted.map(({ phase, txHash }) => [phase, txHash])).toEqual([
      ["Publication", hashOf(4)],
      ["Admission", hashOf(5)],
    ]);
    expect(result.publication?.txHash).toBe(hashOf(4));
    expect(result.admission.txHash).toBe(hashOf(5));
  });

  it("stops at the deadline as a rerun with nothing in flight", async () => {
    const s = setup(Number.MAX_SAFE_INTEGER, 1_000_000 + 60_000);
    const failure = await s.run().then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(failure).toBeInstanceOf(EventHistorySubmissionPendingError);
    const pending = failure as EventHistorySubmissionPendingError;
    expect(pending.resumeAfterMs).toBeDefined();
    expect(pending.checkpoint.pending).toBeUndefined();
    // Waiting on the holder spent the deadline, not the two attempts.
    expect(builds).toBeGreaterThan(2);
    expect(s.history).toEqual([]);
    expect(s.submitted).toEqual([]);
  });

  it.each([
    { phase: "admission", inlineLimitBytes: context.recipe.inlineLimitBytes },
    { phase: "publication", inlineLimitBytes: 1n },
  ])(
    "reports exhausted $phase rebuild attempts as a rerun with nothing in flight",
    async ({ phase, inlineLimitBytes }) => {
      const s = setup(0, undefined, inlineLimitBytes);
      const failure = await s
        .run({ submit: async () => ({ kind: "InputConflict" }) })
        .then(
          () => undefined,
          (error: unknown) => error,
        );
      expect(failure).toBeInstanceOf(EventHistorySubmissionPendingError);
      const pending = failure as EventHistorySubmissionPendingError;
      expect(pending.message).toBe(
        `History ${phase} exhausted its input-conflict attempts`,
      );
      expect(pending.resumeAfterMs).toBe(s.now());
      expect(pending.checkpoint.pending).toBeUndefined();
    },
  );

  it("propagates any other checkpoint failure unchanged without broadcasting", async () => {
    const s = setup(0);
    const storage = new Error("checkpoint storage unavailable");
    await expect(
      s.run({
        save: async () => {
          throw storage;
        },
      }),
    ).rejects.toBe(storage);
    expect(builds).toBe(1);
    expect(s.submitted).toEqual([]);
  });
});

const rejection = (run: Promise<unknown>) =>
  run.then(
    () => {
      throw new Error("The submission unexpectedly completed");
    },
    (error: unknown) => {
      expect(error).toBeInstanceOf(EventHistorySubmissionPendingError);
      return error as EventHistorySubmissionPendingError;
    },
  );
const requestHash = () =>
  eventHistorySubmissionRequestHash(
    context.applied.policyId,
    request,
    context.recipe,
  );
const attemptOf = (build: number) => ({
  phase: "Admission" as const,
  txHash: hashOf(build),
  outputIndex: 0,
  transactionCbor: `body-${hashOf(build)}`,
});
const status =
  (confirmed: readonly number[]) =>
  async (
    attempt: EventHistorySubmissionAttempt,
  ): Promise<EventHistorySubmissionOutcome> => ({
    kind: confirmed.some((build) => attempt.txHash === hashOf(build))
      ? "Confirmed"
      : "Pending",
  });

describe("abandoned history attempts", () => {
  /** Admission 1 expires unseen and is abandoned, then admission 2 meets
   * BadInputs: on L1, admission 1 landed on a fork the provider left. */
  const interrupted = async () => {
    const s = setup(0);
    let submits = 0;
    const observe = vi.fn(status([1]));
    const failure = await rejection(
      s.run({
        submit: async () => {
          if (++submits === 1) return { kind: "InputConflict" };
          throw new Error("BadInputs");
        },
        observe,
      }),
    );
    expect(failure.checkpoint.pending?.txHash).toBe(hashOf(2));
    expect(failure.checkpoint.abandoned).toEqual([
      { ...attemptOf(1), abandonedAtMs: 1_000_000 },
    ]);
    // An unspent nonce means no admission landed, so nothing is observed.
    expect(observe).not.toHaveBeenCalled();
    return { s, checkpoint: failure.checkpoint };
  };

  it("adopts an abandoned admission that landed and abandons the pending one", async () => {
    const { s, checkpoint } = await interrupted();
    const reconcile = vi.fn(async () => ({ kind: "InputConflict" }) as const);
    const result = await s.run(
      { reconcile, observe: status([1]) },
      { checkpoint, spent: true },
    );
    expect(result.admission).toEqual(attemptOf(1));
    expect(result.pending).toBeUndefined();
    expect(result.abandoned?.map((attempt) => attempt.txHash)).toEqual([
      hashOf(2),
    ]);
    expect(s.saved()).toEqual(result);
    expect(reconcile).not.toHaveBeenCalled();
    expect(builds).toBe(2);
  });

  it("adopts it once the pending admission settles unseen, instead of stranding the nonce", async () => {
    const { s, checkpoint } = await interrupted();
    let observed = 0;
    const result = await s.run(
      {
        reconcile: async () => ({ kind: "InputConflict" }),
        // The provider sees the landed fork only after the pending settled.
        observe: async (attempt) =>
          ++observed === 1 ? { kind: "Pending" } : status([1])(attempt),
      },
      { checkpoint, spent: true },
    );
    expect(result.admission).toEqual(attemptOf(1));
    expect(result.abandoned?.map((attempt) => attempt.txHash)).toEqual([
      hashOf(2),
    ]);
    expect(builds).toBe(2);
  });

  it("never adopts an abandoned admission that has not landed", async () => {
    const { s, checkpoint } = await interrupted();
    const failure = await rejection(
      s.run(
        {
          reconcile: async () => ({ kind: "InputConflict" }),
          observe: status([]),
        },
        { checkpoint, spent: true },
      ),
    );
    expect(failure.message).toBe(
      "History nonce is unavailable; reconcile its spending transaction",
    );
    expect(failure.checkpoint.admission).toBeUndefined();
    expect(
      failure.checkpoint.abandoned?.map((attempt) => attempt.txHash),
    ).toEqual([hashOf(1), hashOf(2)]);
    expect(builds).toBe(2);
  });

  it("keeps an abandoned attempt for exactly the retention bound", async () => {
    const s = setup(0);
    const kept = {
      ...attemptOf(101),
      abandonedAtMs: 1_000_000 - EVENT_HISTORY_ABANDONED_RETENTION_MS + 1,
    };
    let submits = 0;
    const result = await s.run(
      {
        submit: async () =>
          ++submits === 1 ? { kind: "InputConflict" } : { kind: "Confirmed" },
      },
      {
        checkpoint: {
          requestHash: requestHash(),
          abandoned: [
            {
              ...attemptOf(100),
              abandonedAtMs: 1_000_000 - EVENT_HISTORY_ABANDONED_RETENTION_MS,
            },
            kept,
          ],
        },
      },
    );
    expect(result.admission.txHash).toBe(hashOf(2));
    expect(result.abandoned).toEqual([
      kept,
      { ...attemptOf(1), abandonedAtMs: 1_000_000 },
    ]);
  });
});

describe("resuming a stored admission receipt", () => {
  const receipt = attemptOf(99);

  it("waits out another submission's hold on a receipt input, then reconciles it", async () => {
    const s = setup(1);
    const result = await s.run(
      {},
      { checkpoint: { requestHash: requestHash(), admission: receipt } },
    );
    expect(result.admission).toEqual(receipt);
    expect(s.refusals()).toBe(1);
    expect(s.now()).toBe(1_000_000 + 5_000);
    expect(builds).toBe(0);
  });

  it("stops at the deadline as a rerun that keeps the receipt", async () => {
    const s = setup(Number.MAX_SAFE_INTEGER, 1_000_000 + 12_000);
    const failure = await rejection(
      s.run(
        {},
        { checkpoint: { requestHash: requestHash(), admission: receipt } },
      ),
    );
    expect(failure.resumeAfterMs).toBeDefined();
    expect(failure.checkpoint.admission).toEqual(receipt);
    expect(failure.checkpoint.pending).toBeUndefined();
    expect(s.history).toEqual([]);
    expect(builds).toBe(0);
  });
});
