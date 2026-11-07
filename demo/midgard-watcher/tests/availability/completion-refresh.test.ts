import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { afterEach, expect, it, vi } from "vitest";

import { fixture, utxo } from "../support/availability-challenge-fixture.js";
import { completionLedger, deferred } from "./completion-refresh.fixture.js";
import {
  ADA,
  answered,
  io,
  liveChallenge,
  observation,
  PARAMETERS,
  runtime,
  timedOut,
  TIMEOUT_COLLATERAL,
  withJournal,
} from "./concurrent-challenges.fixture.js";

afterEach(() => {
  vi.restoreAllMocks();
  vi.useRealTimers();
});

it.each([
  [8620n, 4310n],
  [4310n, 8620n],
])(
  "refreshes a Close once and signs under its winning limits %s -> %s",
  async (oldCost, newCost) => {
    const ledger = await completionLedger(oldCost, newCost);
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL + 10n * ADA, "f1"),
      utxo(1, 100n * ADA, "f2"),
    ];
    const build = vi
      .fn()
      .mockImplementationOnce(() =>
        Effect.fail(
          new SDK.DaAvailabilityTransactionError("parameter epoch changed"),
        ),
      )
      .mockImplementation(() => Effect.succeed({ tx: ledger.transaction() }));
    io.closeBuild = build;
    const header = liveChallenge("71");
    const watcher = await runtime("d0".repeat(32));
    try {
      await watcher.reconcile(observation([answered(header.challenged)]), true);
      expect(watcher.status(), JSON.stringify(watcher.status())).toMatchObject({
        action: "close",
        phase: "waiting",
      });
      expect(watcher.status().detail).toBeUndefined();
      expect(ledger.signs()).toBe(1);
      expect(io.submitTx).toHaveBeenCalledTimes(1);
      expect(build).toHaveBeenCalledTimes(2);
      expect(ledger.instances).toHaveLength(3);
      const [base, attempt] = ledger.instances;
      expect(base!.refresh).not.toHaveBeenCalled();
      expect(base!.walletRead).not.toHaveBeenCalled();
      expect(base!.lucid.config().protocolParameters).toEqual(ledger.initial);
      expect(attempt!.refresh).toHaveBeenCalledTimes(2);
      expect(attempt!.walletRead).toHaveBeenCalledTimes(3);
      const [context, operation] = io.run.mock.calls[0]!;
      expect(operation.preparationScope).toBeDefined();
      expect(io.sourceSignals).toContain(operation.preparationScope.signal);
      expect(operation.unsignedDeadlineMs).toBeUndefined();
      expect(operation.preparationScope.deadlineEpochMs).toBeUndefined();
      expect(context.transactionLimits).toEqual(ledger.limits(attempt!.lucid));
      expect(operation.preparationScope.signal.aborted).toBe(true);
      const intent = ledger.sdk.inspectDaAvailabilitySignedIntent({
        deploymentIdentity: "d0".repeat(32),
        actor: ledger.actor,
        headerHash: header.challenged.headerHash,
        action: "close",
        signedCbor: io.submitTx.mock.calls[0]![0],
      });
      expect(intent.completesWorkflow).toBe(true);
      const retained = withJournal((journal) => ({
        pending: journal.pending("d0".repeat(32), ledger.actor),
        reserved: journal.reservedOutRefs(ledger.actor),
      }));
      expect(retained.pending).toHaveLength(1);
      expect(retained.pending[0]!.intent.signedCbor).toBe(
        io.submitTx.mock.calls[0]![0],
      );
      expect(retained.reserved).toContain(intent.spentOutRefs[0]);
      // Retirement of this unsigned scope does not retire its signed obligation.
      // Recovery reobserves independently and keeps unknown bytes/reservations.
      io.operation.mockResolvedValue({
        status: "unknown",
        reason: "awaiting canonical evidence",
      });
      await watcher.reconcile(observation([answered(header.challenged)]), true);
      expect(ledger.signs()).toBe(1);
      expect(build).toHaveBeenCalledTimes(2);
      expect(
        withJournal((journal) => ({
          pending: journal.pending("d0".repeat(32), ledger.actor),
          reserved: journal.reservedOutRefs(ledger.actor),
        })),
      ).toEqual(retained);

      if (newCost < oldCost)
        expect(() =>
          ledger.sdk.assertDaAvailabilitySignedLimits(
            intent,
            ledger.limits(base!.lucid),
          ),
        ).toThrow("minimum ADA");
      else {
        const stale = {
          ...intent,
          signedCbor: ledger
            .transaction({ cost: oldCost })
            .toTransaction()
            .to_cbor_hex(),
        };
        expect(() =>
          ledger.sdk.assertDaAvailabilitySignedLimits(
            stale,
            ledger.limits(base!.lucid),
          ),
        ).not.toThrow();
        expect(() =>
          ledger.sdk.assertDaAvailabilitySignedLimits(
            stale,
            context.transactionLimits,
          ),
        ).toThrow("minimum ADA");
      }
    } finally {
      await watcher.close();
    }
  },
);

it("reselects the Timeout pool/collateral and preserves its terminal burn and exact fee part on refresh", async () => {
  const ledger = await completionLedger();
  io.utxos = [
    utxo(0, TIMEOUT_COLLATERAL + 10n * ADA, "f1"),
    utxo(1, 100n * ADA, "f2"),
  ];
  const header = timedOut(fixture("72", BigInt(Date.now())).challenged);
  const oldPool = { ...utxo(0, 100_000n * ADA, "a1"), address: "pool" };
  const nextPool = { ...utxo(0, 100_001n * ADA, "a2"), address: "pool" };
  io.tipPool = oldPool;
  const build = vi
    .fn()
    .mockImplementationOnce(() => {
      io.tipPool = nextPool;
      throw new SDK.DaAvailabilityTransactionError("parameter epoch changed");
    })
    .mockImplementation(() =>
      ledger.transaction({
        fee: PARAMETERS.max_timeout_fee_lovelace + 1n,
        burnHeader: header.headerHash,
      }),
    );
  io.timeoutTx = build;
  const watcher = await runtime("d0".repeat(32));
  try {
    await watcher.reconcile(observation([header]), true);
    expect(watcher.status(), JSON.stringify(watcher.status())).toMatchObject({
      action: "timeout",
      phase: "waiting",
    });
    expect(watcher.status().detail).toBeUndefined();
    expect(build).toHaveBeenCalledTimes(2);
    expect(build.mock.calls.map(([input]) => input.pool)).toEqual([
      oldPool,
      nextPool,
    ]);
    const [context, operation] = io.run.mock.calls[0]!;
    expect(operation.completesWorkflow).toBe(true);
    expect(operation.preparationScope.deadlineEpochMs).toBeUndefined();
    expect(ledger.signs()).toBe(1);
    const intent = ledger.sdk.inspectDaAvailabilitySignedIntent({
      deploymentIdentity: "d0".repeat(32),
      actor: context.actor,
      headerHash: header.headerHash,
      action: "timeout",
      completesWorkflow: true,
      signedCbor: io.submitTx.mock.calls[0]![0],
    });
    expect(() =>
      ledger.sdk.assertDaAvailabilitySignedLimits(
        intent,
        context.transactionLimits,
        1n,
      ),
    ).not.toThrow();
    expect(() =>
      ledger.sdk.assertDaAvailabilitySignedLimits(
        intent,
        context.transactionLimits,
        0n,
      ),
    ).toThrow();
  } finally {
    await watcher.close();
  }
});

it("retires a pending completion refresh before signing and isolates its late mutation from the next attempt", async () => {
  const ledger = await completionLedger();
  io.utxos = [
    utxo(0, TIMEOUT_COLLATERAL + 10n * ADA, "f1"),
    utxo(1, 100n * ADA, "f2"),
  ];
  const started = deferred<void>();
  const finish = deferred<void>();
  ledger.onAllocate((instance, index) => {
    if (index !== 1) return;
    const original = instance.switchProvider;
    Object.assign(instance, {
      switchProvider: async (...values: Parameters<typeof original>) => {
        started.resolve();
        await finish.promise;
        await original(...values);
        const config = instance.config();
        Object.assign(instance, {
          config: () => ({ ...config, protocolParameters: ledger.refreshed }),
        });
      },
    });
  });
  io.closeBuild = () =>
    Effect.succeed({
      tx: ledger.transaction({ cost: ledger.initial.coinsPerUtxoByte }),
    } as never);
  const header = liveChallenge("73");
  const watcher = await runtime("d0".repeat(32));
  try {
    const state = observation([answered(header.challenged)]);
    const old = watcher.reconcile(state, true);
    await started.promise;
    const signals = [...io.sourceSignals];
    watcher.invalidateForRollback();
    expect(signals.every((signal) => signal.aborted)).toBe(true);
    await old;
    expect(io.run).not.toHaveBeenCalled();
    expect(ledger.signs()).toBe(0);
    await watcher.reconcile(state, true);
    expect(watcher.status(), JSON.stringify(watcher.status())).toMatchObject({
      action: "close",
    });
    expect(ledger.instances).toHaveLength(4);
    finish.resolve();
    await new Promise<void>((resolve) => setImmediate(resolve));
    expect(ledger.instances[1]!.lucid).not.toBe(ledger.instances[2]!.lucid);
    expect(ledger.instances[0]!.refresh).not.toHaveBeenCalled();
    expect(io.run).toHaveBeenCalledTimes(1);
    expect(ledger.instances[1]!.lucid.config().protocolParameters).toEqual(
      ledger.refreshed,
    );
    expect(ledger.instances[2]!.lucid.config().protocolParameters).toEqual(
      ledger.initial,
    );
  } finally {
    finish.resolve();
    await watcher.close();
  }
});

it("expires the same completion scope during wallet selection before any signing or new journal reservation", async () => {
  const ledger = await completionLedger();
  vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "performance"] });
  io.utxos = [
    utxo(0, TIMEOUT_COLLATERAL + 10n * ADA, "f1"),
    utxo(1, 100n * ADA, "f2"),
  ];
  const entered = deferred<void>();
  const finish = deferred<typeof io.utxos>();
  ledger.onAllocate((instance, index) => {
    if (index !== 1) return;
    const wallet = instance.wallet();
    Object.assign(instance, {
      wallet: () => ({
        ...wallet,
        getUtxos: async () => {
          entered.resolve();
          return await finish.promise;
        },
      }),
    });
  });
  const watcher = await runtime("d0".repeat(32));
  try {
    const state = observation([answered(liveChallenge("74").challenged)]);
    const pending = watcher.reconcile(state, true);
    await entered.promise;
    await vi.advanceTimersByTimeAsync(10_001);
    await pending;
    expect(watcher.status(), JSON.stringify(watcher.status())).toMatchObject({
      phase: "waiting",
      detail: expect.stringContaining("read attempt expired"),
    });
    expect(io.run).not.toHaveBeenCalled();
    expect(ledger.signs()).toBe(0);
    expect(
      withJournal((journal) => journal.reservedOutRefs(ledger.actor)),
    ).toEqual([]);
    expect(io.sourceSignals.every((signal) => signal.aborted)).toBe(true);
  } finally {
    finish.resolve(io.utxos);
    await watcher.close();
  }
});

it.each([false, true])(
  "refuses changed Timeout terminality after refresh (initial terminal=%s)",
  async (terminal) => {
    const ledger = await completionLedger();
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL + 10n * ADA, "f1"),
      utxo(1, 100n * ADA, "f2"),
    ];
    const header = timedOut(fixture("75", BigInt(Date.now())).challenged);
    const descendant = fixture("76").attested.queue!;
    if (!terminal) header.descendant = descendant;
    io.tipPool = { ...utxo(0, 100_000n * ADA, "a1"), address: "pool" };
    let built = 0;
    io.timeoutTx = () => {
      built += 1;
      if (built === 1) {
        if (terminal) header.descendant = descendant;
        else delete header.descendant;
        throw new SDK.DaAvailabilityTransactionError("parameter epoch changed");
      }
      return ledger.transaction({
        fee: PARAMETERS.max_timeout_fee_lovelace,
        burnHeader: terminal ? header.headerHash : undefined,
      });
    };
    const watcher = await runtime("d0".repeat(32));
    try {
      await watcher.reconcile(observation([header]), true);
      expect(ledger.signs()).toBe(0);
      expect(watcher.status().detail).toContain(
        "Refreshed availability transition changed",
      );
      expect(built).toBe(1);
      expect(ledger.instances[1]!.refresh).toHaveBeenCalledTimes(2);
      expect(ledger.signs()).toBe(0);
      expect(io.submitTx).not.toHaveBeenCalled();
      expect(
        withJournal((journal) => journal.reservedOutRefs(ledger.actor)),
      ).toEqual([]);
    } finally {
      await watcher.close();
    }
  },
);

it("selects a fresh completion scope after another header consumes its Open cutoff", async () => {
  vi.useFakeTimers({
    toFake: ["Date", "performance", "setTimeout", "clearTimeout"],
  });
  const now = 1_800_000_000_000;
  vi.setSystemTime(now);
  const window = BigInt(
    SELECTED_DEPLOYMENT_PROFILE.timing.da_challenge_window_ms,
  );
  const withheld = fixture("77", BigInt(now) - window + 100n);
  const complete = liveChallenge("78");
  io.utxos = [utxo(0, TIMEOUT_COLLATERAL, "f1"), utxo(1, 10n * ADA, "f2")];
  io.attestedCommitment.mockImplementation(async () => {
    await vi.advanceTimersByTimeAsync(100);
    return withheld.commitment;
  });
  const watcher = await runtime();
  try {
    await watcher.reconcile(
      observation([withheld.attested, answered(complete.challenged)]),
      true,
    );
    expect(io.attestedCommitment).toHaveBeenCalledTimes(1);
    expect(io.sourceSignals[0]!.reason).toBeInstanceOf(
      SDK.DaAvailabilityReadScopeExpiredError,
    );
    expect(watcher.status(), JSON.stringify(watcher.status())).toMatchObject({
      action: "close",
      missedOpenDeadlines: [withheld.attested.headerHash],
    });
    const [, operation] = io.run.mock.calls[0]!;
    expect(operation.preparationScope).toBeDefined();
    expect(operation.preparationScope.deadlineEpochMs).toBeUndefined();
    expect(operation.unsignedDeadlineMs).toBeUndefined();
    expect(io.lucidInstances).toHaveLength(2);
  } finally {
    await watcher.close();
  }
});
