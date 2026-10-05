import type { TxSignBuilder, UTxO } from "@lucid-evolution/lucid";
import { beforeEach, describe, expect, it, vi } from "vitest";

import {
  buildEventHistoryAdmission,
  buildEventHistoryPublication,
  type EventHistoryBuildContext,
} from "../src/user-events/history-build.js";
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

// The state machine is under test, not transaction construction: each build
// returns a distinct fake body.
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
// A one-byte inline limit makes every payload external, so it is published.
const recipe = {
  hubPolicyId: "dd".repeat(28),
  kind: "Deposit",
  initializationNonce: { transactionId: "ee".repeat(32), outputIndex: 0n },
  protectionDurationMs: 60_000n,
  inlineLimitBytes: 1n,
  maxPayloadBytes: 15_000n,
  maxPayloadNodes: 1_024n,
} as const;
const policyId = "cc".repeat(28);
const requestHash = eventHistorySubmissionRequestHash(
  policyId,
  request,
  recipe as unknown as EventHistoryBuildContext["recipe"],
);
const attemptOf = (
  build: number,
  phase: EventHistorySubmissionAttempt["phase"],
): EventHistorySubmissionAttempt => ({
  phase,
  txHash: hashOf(build),
  outputIndex: 0,
  transactionCbor: `body-${hashOf(build)}`,
});
const confirmedOnly =
  (landed: readonly number[]) =>
  async (
    attempt: EventHistorySubmissionAttempt,
  ): Promise<EventHistorySubmissionOutcome> => ({
    kind: landed.some((build) => attempt.txHash === hashOf(build))
      ? "Confirmed"
      : "Pending",
  });

const START_MS = 1_000_000;
const VALIDITY_MS = 120_000;

const run = (
  driver: Partial<EventHistorySubmissionDriver>,
  checkpoint?: EventHistorySubmissionCheckpoint,
  { nonceSpent = false } = {},
) => {
  const saves: EventHistorySubmissionCheckpoint[] = [];
  const submitted: EventHistorySubmissionAttempt[] = [];
  const result = submitEventHistory({
    context: {
      lucid: {
        utxosByOutRef: async (refs: readonly UTxO[]) =>
          nonceSpent && refs[0]?.txHash === nonce.txHash ? [] : [nonce],
      },
      applied: { policyId },
      recipe,
    } as unknown as Omit<EventHistoryBuildContext, "fundingInputs">,
    request,
    checkpoint,
    driver: {
      save: async (next) => {
        saves.push(next);
      },
      submit: async (_tx, attempt) => {
        submitted.push(attempt);
        return { kind: "Confirmed" };
      },
      reconcile: async () => ({ kind: "Confirmed" }),
      funding: async () => [],
      now: () => START_MS,
      waitUntil: async () => {},
      ...driver,
    },
    maxAttempts: 2,
    deadlineMs: 10_000_000,
    validityDurationMs: VALIDITY_MS,
    outputVisibilityAttempts: 1,
    retryDelayMs: 5_000,
  });
  return { result, saves, submitted };
};

beforeEach(() => {
  builds = 0;
  vi.mocked(buildEventHistoryPublication).mockClear();
  vi.mocked(buildEventHistoryAdmission).mockClear();
});

describe("history publication validity", () => {
  it("bounds a publication body exactly like the admission built after it", async () => {
    const { result, submitted } = run({});
    await result;
    const validTo = START_MS - 60_000 + VALIDITY_MS;
    expect(vi.mocked(buildEventHistoryPublication).mock.calls[0]?.[3]).toBe(
      validTo,
    );
    expect(
      vi.mocked(buildEventHistoryAdmission).mock.calls[0]?.[1].validTo,
    ).toBe(validTo);
    expect(submitted.map(({ phase }) => phase)).toEqual([
      "Publication",
      "Admission",
    ]);
  });
});

describe("abandoned history publications", () => {
  const dead = attemptOf(90, "Publication");

  it.each([
    { observed: [90], adopted: true },
    { observed: [], adopted: false },
  ])(
    "settles an expired pending publication and adopts it only once it is seen landed ($adopted)",
    async ({ observed, adopted }) => {
      const observe = vi.fn(confirmedOnly(observed));
      const { result } = run(
        { reconcile: async () => ({ kind: "InputConflict" }), observe },
        { requestHash, pending: dead },
      );
      const done = await result;
      expect(observe).toHaveBeenCalledWith(dead);
      if (adopted) {
        expect(done.publicationAttempt).toEqual(dead);
        expect(done.abandoned).toEqual([]);
        expect(buildEventHistoryPublication).not.toHaveBeenCalled();
        return;
      }
      // Nothing landed, so the submission publishes once more and keeps the
      // abandoned body in case a rollback lands it.
      expect(done.publicationAttempt).toEqual(attemptOf(1, "Publication"));
      expect(done.abandoned).toEqual([{ ...dead, abandonedAtMs: START_MS }]);
      expect(buildEventHistoryPublication).toHaveBeenCalledTimes(1);
    },
  );

  it("abandons a stored publication receipt that can no longer land and publishes again", async () => {
    const { result } = run(
      {
        reconcile: async (attempt) => ({
          kind: attempt.txHash === dead.txHash ? "InputConflict" : "Confirmed",
        }),
        observe: confirmedOnly([]),
      },
      { requestHash, publication: dead, publicationAttempt: dead },
    );
    const done = await result;
    expect(done.publicationAttempt).toEqual(attemptOf(1, "Publication"));
    expect(done.admission?.txHash).toBe(hashOf(2));
    expect(done.abandoned).toEqual([{ ...dead, abandonedAtMs: START_MS }]);
  });

  describe("with an expired pending admission whose inputs another submission holds", () => {
    const stale = attemptOf(92, "Admission");
    const stored = {
      requestHash,
      publication: dead,
      publicationAttempt: dead,
      pending: stale,
    };
    const holding = () => {
      const saves: EventHistorySubmissionCheckpoint[] = [];
      return {
        saves,
        save: async (next: EventHistorySubmissionCheckpoint) => {
          // The node journal refuses any checkpoint that still carries the
          // stale admission while the other submission holds its inputs.
          if (next.pending?.txHash === stale.txHash)
            throw new EventHistoryInputReservedError("ff".repeat(32) + "#0");
          saves.push(next);
        },
      };
    };

    it("settles the admission, abandons both and publishes again", async () => {
      const { save, saves } = holding();
      const { result } = run(
        {
          save,
          reconcile: async (attempt) => ({
            kind: [dead.txHash, stale.txHash].includes(attempt.txHash)
              ? "InputConflict"
              : "Confirmed",
          }),
          observe: confirmedOnly([]),
        },
        stored,
      );
      const done = await result;
      expect(saves[0]?.pending).toBeUndefined();
      expect(done.publicationAttempt).toEqual(attemptOf(1, "Publication"));
      expect(done.admission?.txHash).toBe(hashOf(2));
      expect(done.abandoned).toEqual([
        { ...stale, abandonedAtMs: START_MS },
        { ...dead, abandonedAtMs: START_MS },
      ]);
    });

    it("never abandons an admission whose status is unresolved", async () => {
      const { save, saves } = holding();
      const { result } = run(
        {
          save,
          reconcile: async (attempt) => ({
            kind: attempt.txHash === dead.txHash ? "InputConflict" : "Pending",
          }),
        },
        stored,
      );
      const failure = await result.then(
        () => undefined,
        (error: unknown) => error,
      );
      expect(failure).toBeInstanceOf(EventHistorySubmissionPendingError);
      const pending = failure as EventHistorySubmissionPendingError;
      expect(pending.message).toBe(
        "History transaction confirmation is unresolved",
      );
      expect(pending.checkpoint).toEqual(stored);
      expect(saves).toEqual([]);
      expect(builds).toBe(0);
    });
  });

  it("keeps a stored publication receipt whose status is unresolved", async () => {
    const stored = { requestHash, publication: dead, publicationAttempt: dead };
    const { result, saves } = run(
      { reconcile: async () => ({ kind: "Pending" }) },
      stored,
    );
    const failure = await result.then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(failure).toBeInstanceOf(EventHistorySubmissionPendingError);
    expect((failure as EventHistorySubmissionPendingError).message).toBe(
      "Original history publication confirmation is unresolved",
    );
    expect(saves).toEqual([]);
    expect(builds).toBe(0);
  });

  it("completes with an abandoned admission a rollback landed instead of publishing again", async () => {
    const landed = attemptOf(91, "Admission");
    const { result, submitted } = run(
      { observe: confirmedOnly([91]) },
      {
        requestHash,
        abandoned: [
          { ...dead, abandonedAtMs: START_MS - 1 },
          { ...landed, abandonedAtMs: START_MS - 1 },
        ],
      },
      { nonceSpent: true },
    );
    const done = await result;
    expect(done.admission).toEqual(landed);
    expect(builds).toBe(0);
    expect(submitted).toEqual([]);
  });
});
