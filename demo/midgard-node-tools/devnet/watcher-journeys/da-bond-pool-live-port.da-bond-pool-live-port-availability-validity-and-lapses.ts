import "./da-bond-pool-live-port.da-bond-pool-live-port-header-commit-validity.js";

import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  OgmiosJsonRpcError,
  paymentCredentialOf,
  TxSubmitError,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  availabilityAttemptRecovery,
  availabilityEndingError,
  AvailabilityIntentLapsedError,
  availabilitySubmissionToAwait,
  awaitAvailabilityInclusion,
  awaitQuietJournal,
  landAvailabilitySubmission,
  ledgerValidityRefusal,
  MAX_LAPSED_REPLANS,
  prepareAvailabilityAttempt,
} from "./da-bond-pool-live-port.js";

describe("DA bond pool live port: availability validity and lapses", () => {
  const txId = "cd".repeat(32);
  // What the provider's submit rejects with, inside Lucid's TxSubmitError and
  // the FiberFailure of Effect.runPromise.
  const ogmiosFailure = (code: number, message: string, data: unknown) =>
    Effect.runPromise(
      Effect.fail(
        new TxSubmitError({
          cause: new OgmiosJsonRpcError({
            code,
            message,
            data,
            method: "submitTransaction",
            id: null,
          }),
        }),
      ),
    ).catch((error: unknown) => error as Error);
  const OUTSIDE = "The transaction is outside of its validity interval.";
  const lowerBound = () =>
    ogmiosFailure(3118, OUTSIDE, {
      validityInterval: { invalidBefore: 3210, invalidAfter: 3330 },
      currentSlot: 3193,
    });
  const upperBound = () =>
    ogmiosFailure(3118, OUTSIDE, {
      validityInterval: { invalidBefore: 3210, invalidAfter: 3330 },
      currentSlot: 3331,
    });
  const scriptFailure = () =>
    ogmiosFailure(
      3010,
      "Some scripts of the transactions terminated with error(s).",
      {
        validationError: "ValueNotConserved",
      },
    );
  const LAPSED = "Expired with every normal input canonically unspent";
  const result = (
    status: SDK.DaAvailabilityOperationResult["status"],
  ): SDK.DaAvailabilityOperationResult =>
    ({ txHash: txId, status }) as SDK.DaAvailabilityOperationResult;
  const scripted = (
    steps: readonly (
      | SDK.DaAvailabilityOperationResult["status"]
      | (() => Promise<Error>)
      | Error
    )[],
  ) => {
    let index = 0;
    return async () => {
      const step = steps[Math.min(index, steps.length - 1)]!;
      index += 1;
      if (typeof step === "function") throw await step();
      if (step instanceof Error) throw step;
      return [result(step)];
    };
  };
  const wait = (
    reconcile: () => Promise<readonly SDK.DaAvailabilityOperationResult[]>,
    record: { state?: string; detail?: string | null } | undefined = {
      state: "pending",
    },
    log: (line: string) => void = () => undefined,
  ) => {
    let time = 0;
    return awaitAvailabilityInclusion({
      txId,
      reconcile,
      journalRecord: () => record,
      timeoutMs: 60_000,
      pollMs: 2_000,
      wait: async (ms) => {
        time += ms;
      },
      now: () => time,
      log,
    });
  };

  it("reads the ledger's validity refusal and which bound it failed", async () => {
    expect(ledgerValidityRefusal(await lowerBound())).toMatchObject({
      bound: "lower",
    });
    expect(ledgerValidityRefusal(await upperBound())).toMatchObject({
      bound: "upper",
    });
    expect(ledgerValidityRefusal(await lowerBound())?.text).toContain(
      '"currentSlot":3193',
    );
    // A plain error carrying the same structured data, as a provider may.
    expect(
      ledgerValidityRefusal(
        Object.assign(new Error("RejectTx"), {
          data: { validityInterval: { invalidAfter: 3330 }, currentSlot: 3331 },
        }),
      ),
    ).toMatchObject({ bound: "upper" });
    expect(ledgerValidityRefusal(await scriptFailure())).toBeUndefined();
    expect(
      ledgerValidityRefusal(
        new Error('refused: {"validityInterval":{"invalidBefore":3210}}'),
      ),
    ).toBeUndefined();
    expect(
      ledgerValidityRefusal(
        Object.assign(new Error("other"), {
          code: 3010,
          data: { validityInterval: {}, currentSlot: 1 },
        }),
      ),
    ).toBeUndefined();
  });

  it("names only an unspent-input expiry as lapsed", () => {
    expect(
      availabilityEndingError(txId, "expired", {
        state: "expired",
        detail: LAPSED,
      }),
    ).toBeInstanceOf(AvailabilityIntentLapsedError);
    const spent = availabilityEndingError(txId, "expired", {
      state: "expired",
      detail:
        "Expired with a normal input spent elsewhere and another still unspent",
    });
    expect(spent).not.toBeInstanceOf(AvailabilityIntentLapsedError);
    expect(spent.message).toContain("spent elsewhere");
    expect(
      availabilityEndingError(txId, "conflict", {
        state: "conflict",
        detail: LAPSED,
      }),
    ).not.toBeInstanceOf(AvailabilityIntentLapsedError);
  });

  it("ends the wait with the lapsed error only for a lapsed expiry", async () => {
    await expect(
      wait(scripted(["expired"]), { state: "expired", detail: LAPSED }),
    ).rejects.toBeInstanceOf(AvailabilityIntentLapsedError);
    const spent = await wait(scripted(["expired"]), {
      state: "expired",
      detail: "Expired with a normal input spent elsewhere",
    }).catch((error: unknown) => error);
    expect(spent).toBeInstanceOf(Error);
    expect(spent).not.toBeInstanceOf(AvailabilityIntentLapsedError);
    const conflict = await wait(scripted(["conflict"]), {
      state: "conflict",
    }).catch((error: unknown) => error);
    expect(conflict).not.toBeInstanceOf(AvailabilityIntentLapsedError);
    expect((conflict as Error).message).toContain("ended conflict");
  });

  it("keeps reconciling through a rebroadcast refused on either validity bound", async () => {
    const lines: string[] = [];
    await expect(
      wait(scripted([lowerBound, upperBound, "included"]), undefined, (line) =>
        lines.push(line),
      ),
    ).resolves.toBeUndefined();
    expect(lines).toHaveLength(2);
    expect(lines[0]).toContain("before its validity interval's start");
    expect(lines[1]).toContain("past its validity interval's end");
    expect(lines[1]).toContain('"currentSlot":3331');
  });

  it("keeps reconciling through a transient canonical-view error until the transaction is included", async () => {
    await expect(
      wait(
        scripted([
          new Error("L1 provider follower unavailable: behind_node_tip"),
          new Error(
            "Availability transaction inclusion changed during its canonical read",
          ),
          "included",
        ]),
      ),
    ).resolves.toBeUndefined();
    await expect(
      wait(
        scripted([
          new Error("L1 provider follower unavailable: behind_node_tip"),
          "expired",
        ]),
      ),
    ).rejects.toThrow(`Availability transaction ${txId} ended expired`);
  });

  it("still fails at once on a script failure", async () => {
    let calls = 0;
    const failure = await wait(async () => {
      calls += 1;
      throw await scriptFailure();
    }).catch((error: unknown) => error);
    expect(calls).toBe(1);
    expect(ledgerValidityRefusal(failure)).toBeUndefined();
  });

  it("awaits a journaled first broadcast the ledger refused, and nothing else", async () => {
    const pending = () => "pending";
    expect(
      availabilitySubmissionToAwait(await lowerBound(), txId, pending),
    ).toBe(txId);
    expect(
      availabilitySubmissionToAwait(await upperBound(), txId, pending),
    ).toBe(txId);
    expect(
      availabilitySubmissionToAwait(
        new Error("L1 provider follower unavailable: behind_node_tip"),
        txId,
        pending,
      ),
    ).toBe(txId);
    // Not journaled, not built, or not a validity or canonical error.
    expect(
      availabilitySubmissionToAwait(await lowerBound(), txId, () => undefined),
    ).toBeUndefined();
    expect(
      availabilitySubmissionToAwait(await lowerBound(), undefined, pending),
    ).toBeUndefined();
    expect(
      availabilitySubmissionToAwait(await scriptFailure(), txId, pending),
    ).toBeUndefined();
    expect(
      availabilitySubmissionToAwait(
        new Error("value not conserved"),
        txId,
        pending,
      ),
    ).toBeUndefined();
  });

  it("re-plans a lapsed transaction up to the cap and retries only an unjournaled transient error", async () => {
    const lapsed = new AvailabilityIntentLapsedError(txId);
    const transient = new Error(
      "L1 provider follower unavailable: behind_node_tip",
    );
    const state = {
      journaled: true,
      lapses: 0,
      attempt: 1,
      maxTransientAttempts: 5,
    };
    for (let lapses = 0; lapses < MAX_LAPSED_REPLANS; lapses += 1)
      expect(availabilityAttemptRecovery(lapsed, { ...state, lapses })).toBe(
        "replan",
      );
    expect(
      availabilityAttemptRecovery(lapsed, {
        ...state,
        lapses: MAX_LAPSED_REPLANS,
      }),
    ).toBe("throw");
    expect(
      availabilityAttemptRecovery(transient, { ...state, journaled: false }),
    ).toBe("retry");
    // Once journaled, the transaction may be in flight: never re-plan it.
    expect(availabilityAttemptRecovery(transient, state)).toBe("throw");
    expect(
      availabilityAttemptRecovery(transient, {
        ...state,
        journaled: false,
        attempt: 5,
      }),
    ).toBe("throw");
    for (const error of [
      new Error(`Availability transaction ${txId} ended conflict`),
      await scriptFailure(),
      new Error("Availability open of aa plans close, expected open"),
    ])
      expect(
        availabilityAttemptRecovery(error, { ...state, journaled: false }),
      ).toBe("throw");
  });

  it("waits for a fresh ledger tip after settling the journal and before reading the boundary", async () => {
    const calls: string[] = [];
    await expect(
      prepareAvailabilityAttempt({
        quietJournal: async () => {
          calls.push("quiet");
        },
        awaitFreshTip: async () => {
          await Promise.resolve();
          calls.push("fresh");
        },
        readBoundary: async () => {
          calls.push("boundary");
          return "point";
        },
      }),
    ).resolves.toBe("point");
    expect(calls).toEqual(["quiet", "fresh", "boundary"]);
  });

  describe("settles the journal before planning", () => {
    // What the SDK's reconcile rejects with when its rebroadcast is refused:
    // the provider's raw Ogmios error, outside any Effect.
    const rawOgmios = (code: number, message: string, data: unknown) =>
      new OgmiosJsonRpcError({
        code,
        message,
        data,
        method: "submitTransaction",
        id: null,
      });
    const allSpent = () =>
      rawOgmios(
        3997,
        "The transaction couldn't be added to the mempool. A justification is given as 'data.error'.",
        {
          error:
            "All inputs are spent. Transaction has probably already been included",
        },
      );
    const quiet = (
      steps: readonly (
        | readonly SDK.DaAvailabilityOperationResult["status"][]
        | (() => Error)
      )[],
      timeoutMs = 60_000,
    ) => {
      let calls = 0;
      let time = 0;
      const thrown: Error[] = [];
      const done = awaitQuietJournal({
        reconcile: async () => {
          const step = steps[Math.min(calls, steps.length - 1)]!;
          calls += 1;
          if (typeof step === "function") {
            const error = step();
            thrown.push(error);
            throw error;
          }
          return step.map(result);
        },
        timeoutMs,
        pollMs: 2_000,
        wait: async (ms) => {
          time += ms;
        },
        now: () => time,
      });
      return { done, calls: () => calls, thrown };
    };

    it("reconciles through a raw spent-input or validity refusal until the journal settles", async () => {
      const spent = quiet([allSpent, ["included"]]);
      await expect(spent.done).resolves.toBeUndefined();
      expect(spent.calls()).toBe(2);
      const bounds = quiet([
        () =>
          rawOgmios(3118, OUTSIDE, {
            validityInterval: { invalidBefore: 3210, invalidAfter: 3330 },
            currentSlot: 3193,
          }),
        () =>
          rawOgmios(3118, OUTSIDE, {
            validityInterval: { invalidBefore: 3210, invalidAfter: 3330 },
            currentSlot: 3331,
          }),
        ["submitted"],
        ["expired", "confirmed"],
      ]);
      await expect(bounds.done).resolves.toBeUndefined();
      expect(bounds.calls()).toBe(4);
    });

    it("fails at once on a script failure", async () => {
      const failure = await scriptFailure();
      const run = quiet([() => failure, []]);
      await expect(run.done).rejects.toBe(failure);
      expect(run.calls()).toBe(1);
    });

    it("rethrows the last unsettled refusal once the deadline passes", async () => {
      const run = quiet([allSpent], 10_000);
      const error = await run.done.catch((caught: unknown) => caught);
      expect(run.calls()).toBeGreaterThan(1);
      expect(error).toBe(run.thrown.at(-1));
    });

    it("fails on a conflicting intent and on an intent still open at the deadline", async () => {
      await expect(quiet([allSpent, ["conflict"]]).done).rejects.toThrow(
        "Availability journal holds a conflicting intent",
      );
      await expect(quiet([["waiting"]], 10_000).done).rejects.toThrow(
        "Availability journal did not settle",
      );
    });
  });

  describe("lands one availability submission", () => {
    const submission = (
      execute: (
        journal: (txId: string) => void,
      ) => Promise<SDK.DaAvailabilityOperationResult>,
      records: Record<string, { state?: string; detail?: string }> = {},
    ) => {
      const awaited: string[] = [];
      const lines: string[] = [];
      let built: string | undefined;
      const landed = landAvailabilitySubmission({
        label: `timeout ${txId}`,
        // The executor builds, journals the intent as pending, then
        // broadcasts it.
        execute: () =>
          execute((id) => {
            built = id;
            records[id] = { state: "pending", ...records[id] };
          }),
        builtTxId: () => built,
        journalRecord: (id) => records[id],
        awaitIncluded: async (id) => {
          awaited.push(id);
        },
        log: (line) => lines.push(line),
      });
      return { landed, awaited, lines };
    };

    it("awaits a journaled transaction whose first broadcast the ledger refused", async () => {
      const { landed, awaited, lines } = submission(async (journal) => {
        journal(txId);
        throw await lowerBound();
      });
      await expect(landed).resolves.toEqual({ kind: "included", txId });
      expect(awaited).toEqual([txId]);
      expect(lines[0]).toContain(`journaled ${txId}`);
      expect(lines[0]).toContain('"currentSlot":3193');
    });

    it("rethrows a script failure, or a refusal before anything was journaled", async () => {
      const failure = await scriptFailure();
      const script = submission(async (journal) => {
        journal(txId);
        throw failure;
      });
      await expect(script.landed).rejects.toBe(failure);
      expect(script.awaited).toEqual([]);
      const refusal = await lowerBound();
      const early = submission(async () => {
        throw refusal;
      });
      await expect(early.landed).rejects.toBe(refusal);
      expect(early.awaited).toEqual([]);
    });

    it("awaits a submitted transaction and fails on another hash or an ending", async () => {
      const submitted = submission(async (journal) => {
        journal(txId);
        return result("submitted");
      });
      await expect(submitted.landed).resolves.toEqual({
        kind: "included",
        txId,
      });
      expect(submitted.awaited).toEqual([txId]);
      await expect(
        submission(async (journal) => {
          journal("ef".repeat(32));
          return result("submitted");
        }).landed,
      ).rejects.toThrow(`Availability executor returned ${txId}`);
      await expect(
        submission(
          async (journal) => {
            journal(txId);
            return result("expired");
          },
          { [txId]: { state: "expired", detail: LAPSED } },
        ).landed,
      ).rejects.toBeInstanceOf(AvailabilityIntentLapsedError);
    });

    it("returns the reconciled result when nothing was built", async () => {
      const reconciled = result("waiting");
      const { landed, awaited } = submission(async () => reconciled);
      await expect(landed).resolves.toEqual({
        kind: "reconciled",
        result: reconciled,
      });
      expect(awaited).toEqual([]);
    });
  });

  it("re-plans the expiry the SDK journals for an intent whose inputs stayed unspent", async () => {
    const account = generateEmulatorAccount({ lovelace: 100_000_000n });
    const emulator = new Emulator([account]);
    emulator.awaitBlock(5);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const directory = mkdtempSync(join(tmpdir(), "da-bond-pool-lapse-"));
    const journal = openAvailabilityOperationJournal(
      join(directory, "journal.sqlite"),
    );
    try {
      const deploymentIdentity = "aa".repeat(32);
      const actor = paymentCredentialOf(account.address).hash;
      const headerHash = "bb".repeat(28);
      const context: SDK.DaAvailabilityOperationContext = {
        deploymentIdentity,
        actor,
        journal,
        stateQueuePolicyId: "cc".repeat(28),
        minimumConfirmationDepth: 30,
        transactionLimits: {
          maxTxSize: 16384,
          maxTxExMem: 16500000n,
          maxTxExSteps: 10000000000n,
          coinsPerUtxoByte: 4310n,
          feeCeilings: { prepare: 1000000n },
        },
        assertActuationCurrent: () => {},
        observe: async () => ({ status: "unspent", currentSlot: 0 }),
        submit: async (signedCbor) =>
          SDK.inspectDaAvailabilitySignedIntent({
            deploymentIdentity,
            actor,
            headerHash,
            action: "prepare",
            signedCbor,
          }).txHash,
      };
      const { txHash } = await SDK.runDaAvailabilityOperation(context, {
        action: "prepare",
        headerHash,
        build: async () =>
          SDK.buildDaAvailabilityFundingPreparationTx(lucid, {
            fundingInput: (await lucid.wallet().getUtxos())[0]!,
            outputLovelace: 50_000_000n,
            feeLovelace: 1_000_000n,
            validFrom: BigInt(emulator.now() - 60_000),
            validTo: BigInt(emulator.now() + 60_000),
          }),
      });
      // No block took it before its validity ended, and nothing spent its
      // inputs: the SDK's own reconcile journals that ending.
      await expect(
        awaitAvailabilityInclusion({
          txId: txHash,
          reconcile: () =>
            SDK.reconcileDaAvailabilityOperations({
              ...context,
              observe: async (intent) => ({
                status: "unspent",
                currentSlot: intent.validUntilSlot,
              }),
            }),
          journalRecord: (id) => journal.findTransaction(id) ?? undefined,
          timeoutMs: 0,
          pollMs: 0,
          wait: async () => undefined,
          log: () => undefined,
        }),
      ).rejects.toBeInstanceOf(AvailabilityIntentLapsedError);
    } finally {
      journal.close();
      rmSync(directory, { recursive: true, force: true });
    }
  });
});
