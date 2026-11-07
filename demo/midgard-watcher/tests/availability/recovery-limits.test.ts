import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { afterEach, expect, it, vi } from "vitest";

import { utxo } from "../support/availability-challenge-fixture.js";
import { completionLedger } from "./completion-refresh.fixture.js";
import {
  ADA,
  answered,
  io,
  liveChallenge,
  observation,
  OPENING,
  PARAMETERS,
  runtime,
  TIMEOUT_COLLATERAL,
  withheld,
  withJournal,
} from "./concurrent-challenges.fixture.js";

afterEach(() => {
  vi.restoreAllMocks();
  vi.useRealTimers();
});

const funds = () => {
  io.utxos = [
    utxo(0, TIMEOUT_COLLATERAL + 10n * ADA, "f1"),
    utxo(1, OPENING, "f2"),
    utxo(2, 100n * ADA, "f3"),
  ];
};
const retained = (actor: string) =>
  withJournal((journal) => ({
    pending: journal.pending("d0".repeat(32), actor),
    reserved: journal.reservedOutRefs(actor),
  }));

it.each([
  PARAMETERS.max_close_fee_lovelace - 1n,
  PARAMETERS.max_close_fee_lovelace + 1n,
])(
  "retains the deployment fee ceiling while refreshing a persisted intent (fee=%s)",
  async (fee) => {
    const ledger = await completionLedger();
    funds();
    ledger.setCanonical(ledger.refreshed);
    io.reconcile.mockImplementation(
      ledger.sdk.reconcileDaAvailabilityOperations,
    );
    const header = answered(liveChallenge("7f").challenged);
    const signed = await ledger
      .transaction({ fee })
      .sign.withWallet()
      .complete();
    const intent = ledger.sdk.inspectDaAvailabilitySignedIntent({
      deploymentIdentity: "d0".repeat(32),
      actor: ledger.actor,
      headerHash: header.headerHash,
      action: "close",
      signedCbor: signed.toCBOR(),
    });
    withJournal((journal) => {
      const lease = journal.acquire(
        ledger.actor,
        "retained-fixture",
        Date.now(),
        60_000,
      );
      try {
        journal.persist(lease, intent, Date.now());
      } finally {
        journal.release(lease);
      }
    });
    const before = retained(ledger.actor);
    const watcher = await runtime("d0".repeat(32));
    try {
      await watcher.reconcile(observation([header]), true);
      if (fee < PARAMETERS.max_close_fee_lovelace) {
        expect(
          io.submitTx,
          JSON.stringify(watcher.status()),
        ).toHaveBeenCalledTimes(1);
        expect(io.submitTx.mock.calls[0]![0]).toBe(intent.signedCbor);
      } else {
        expect(watcher.status().detail).toContain("deployment fee");
        expect(io.submitTx).not.toHaveBeenCalled();
      }
      expect(ledger.signs()).toBe(1);
      expect(io.run).not.toHaveBeenCalled();
      expect(retained(ledger.actor).pending[0]!.intent).toEqual(
        before.pending[0]!.intent,
      );
      expect(retained(ledger.actor).reserved).toEqual(before.reserved);
    } finally {
      await watcher.close();
    }
  },
);

it.each([
  [8620n, 4310n],
  [4310n, 8620n],
])(
  "rebroadcasts the same refreshed Close after an unknown read with current minADA %s -> %s",
  async (oldCost, newCost) => {
    const ledger = await completionLedger(oldCost, newCost);
    funds();
    io.reconcile.mockImplementation(
      ledger.sdk.reconcileDaAvailabilityOperations,
    );
    let builds = 0;
    io.closeBuild = () =>
      ++builds === 1
        ? Effect.fail(
            new SDK.DaAvailabilityTransactionError("parameter epoch changed"),
          )
        : Effect.succeed({ tx: ledger.transaction() } as never);
    const state = observation([answered(liveChallenge("79").challenged)]);
    const watcher = await runtime("d0".repeat(32));
    try {
      await watcher.reconcile(state, true);
      expect(watcher.status(), JSON.stringify(watcher.status())).toMatchObject({
        action: "close",
      });
      expect(io.submitTx).toHaveBeenCalledTimes(1);
      const signed = io.submitTx.mock.calls[0]![0];
      const before = retained(ledger.actor);
      expect(before.pending[0]!.intent.signedCbor).toBe(signed);
      const preparation = io.run.mock.calls[0]![1].preparationScope;
      expect(preparation.signal.aborted).toBe(true);
      io.operation.mockResolvedValue({
        status: "unknown",
        reason: "canonical read unavailable",
      });
      await watcher.reconcile(state, true);
      expect(retained(ledger.actor)).toEqual(before);
      expect(io.submitTx).toHaveBeenCalledTimes(1);
      io.operation.mockResolvedValue({ status: "unspent", currentSlot: 1 });
      await watcher.reconcile(state, true);
      expect(watcher.status(), JSON.stringify(watcher.status())).toMatchObject({
        phase: "waiting",
      });
      expect(watcher.status().detail).toBeUndefined();
      expect(io.submitTx).toHaveBeenCalledTimes(2);
      expect(io.submitTx.mock.calls[1]![0]).toBe(signed);
      expect(ledger.signs()).toBe(1);
      expect(builds).toBe(2);
      expect(ledger.instances).toHaveLength(4);
      expect(ledger.instances[3]!.refresh).toHaveBeenCalledTimes(1);
      expect(ledger.instances[3]!.lucid.config().protocolParameters).toEqual(
        ledger.refreshed,
      );
      expect(ledger.instances[0]!.lucid.config().protocolParameters).toEqual(
        ledger.initial,
      );
      expect(retained(ledger.actor).pending[0]!.intent).toEqual(
        before.pending[0]!.intent,
      );
      expect(retained(ledger.actor).reserved).toEqual(before.reserved);
    } finally {
      await watcher.close();
    }
  },
);

it("uses fresh recovery limits when the protocol changes after construction and before first broadcast", async () => {
  const ledger = await completionLedger(4310n, 8620n);
  funds();
  io.reconcile.mockImplementation(ledger.sdk.reconcileDaAvailabilityOperations);
  io.closeBuild = () => {
    const tx = ledger.transaction({ cost: 4310n });
    ledger.setCanonical(ledger.refreshed);
    return Effect.succeed({ tx } as never);
  };
  const state = observation([answered(liveChallenge("70").challenged)]);
  const watcher = await runtime("d0".repeat(32));
  try {
    await watcher.reconcile(state, true);
    expect(ledger.signs()).toBe(1);
    expect(io.submitTx).not.toHaveBeenCalled();
    expect(watcher.status().detail).toContain("minimum ADA");
    const before = retained(ledger.actor);
    expect(before.pending).toHaveLength(1);
    expect(before.reserved.length).toBeGreaterThan(0);
    ledger.setCanonical(ledger.initial);
    await watcher.reconcile(state, true);
    expect(io.submitTx, JSON.stringify(watcher.status())).toHaveBeenCalledTimes(
      1,
    );
    expect(io.submitTx.mock.calls[0]![0]).toBe(
      before.pending[0]!.intent.signedCbor,
    );
    expect(ledger.signs()).toBe(1);
  } finally {
    await watcher.close();
  }
});

it("rechecks recovery ownership at the actual submit handoff after the SDK's last check", async () => {
  const ledger = await completionLedger();
  funds();
  io.closeBuild = () =>
    Effect.succeed({
      tx: ledger.transaction({ cost: ledger.initial.coinsPerUtxoByte }),
    } as never);
  const state = observation([answered(liveChallenge("7e").challenged)]);
  const watcher = await runtime("d0".repeat(32));
  let revoke = true;
  io.run.mockImplementation(async (context, operation) => {
    const submit = context.submit;
    return await ledger.sdk.runDaAvailabilityOperation(
      {
        ...context,
        submit: (cbor) => {
          // The SDK has finished its scoped assertion. The production consumer
          // must still fence this external actuation at its own ownership seam.
          if (revoke) {
            revoke = false;
            watcher.invalidateForRollback();
          }
          return submit(cbor);
        },
      },
      operation,
    );
  });
  try {
    await watcher.reconcile(state, true);
    expect(ledger.signs()).toBe(1);
    expect(io.submitTx).not.toHaveBeenCalled();
    const before = retained(ledger.actor);
    expect(before.pending).toHaveLength(1);
    expect(before.reserved.length).toBeGreaterThan(0);
    await watcher.reconcile(state, true);
    expect(io.submitTx, JSON.stringify(watcher.status())).toHaveBeenCalledTimes(
      1,
    );
    expect(io.submitTx.mock.calls[0]![0]).toBe(
      before.pending[0]!.intent.signedCbor,
    );
    expect(ledger.signs()).toBe(1);
    expect(retained(ledger.actor).pending[0]!.intent).toEqual(
      before.pending[0]!.intent,
    );
  } finally {
    await watcher.close();
  }
});

it.each([
  [8620n, 4310n],
  [4310n, 8620n],
])(
  "recovers the same signed Open independently after its absolute cutoff %s -> %s",
  async (oldCost, newCost) => {
    vi.useFakeTimers({ toFake: ["Date"] });
    const ledger = await completionLedger(oldCost, newCost);
    funds();
    io.reconcile.mockImplementation(
      ledger.sdk.reconcileDaAvailabilityOperations,
    );
    let builds = 0;
    io.openBuild = () =>
      ++builds === 1
        ? Effect.fail(
            new SDK.DaAvailabilityTransactionError("parameter epoch changed"),
          )
        : Effect.succeed({ tx: ledger.transaction() } as never);
    const header = withheld("7a");
    const state = observation([header.attested]);
    const watcher = await runtime("d0".repeat(32));
    try {
      await watcher.reconcile(state, true);
      expect(
        io.submitTx,
        JSON.stringify(watcher.status()),
      ).toHaveBeenCalledTimes(1);
      const signed = io.submitTx.mock.calls[0]![0];
      const before = retained(ledger.actor);
      const preparation = io.run.mock.calls[0]![1].preparationScope;
      vi.setSystemTime(
        Date.now() +
          SELECTED_DEPLOYMENT_PROFILE.timing.da_challenge_window_ms +
          1,
      );
      expect(preparation.deadlineEpochMs).toBeLessThan(Date.now());
      await watcher.reconcile(state, true);
      expect(watcher.status(), JSON.stringify(watcher.status())).toMatchObject({
        phase: "waiting",
      });
      expect(watcher.status().detail).toBeUndefined();
      expect(io.submitTx).toHaveBeenCalledTimes(2);
      expect(io.submitTx.mock.calls[1]![0]).toBe(signed);
      expect(ledger.signs()).toBe(1);
      expect(builds).toBe(2);
      expect(retained(ledger.actor).pending[0]!.intent).toEqual(
        before.pending[0]!.intent,
      );
      expect(retained(ledger.actor).reserved).toEqual(before.reserved);
    } finally {
      await watcher.close();
    }
  },
);

it("holds an existing signed Close when current minimumADA increases, then resumes its exact bytes when valid again", async () => {
  const ledger = await completionLedger(4310n, 8620n);
  funds();
  io.reconcile.mockImplementation(ledger.sdk.reconcileDaAvailabilityOperations);
  io.closeBuild = () =>
    Effect.succeed({ tx: ledger.transaction({ cost: 4310n }) } as never);
  const state = observation([answered(liveChallenge("7b").challenged)]);
  const watcher = await runtime("d0".repeat(32));
  try {
    await watcher.reconcile(state, true);
    expect(io.submitTx, JSON.stringify(watcher.status())).toHaveBeenCalledTimes(
      1,
    );
    const signed = io.submitTx.mock.calls[0]![0];
    const before = retained(ledger.actor);
    ledger.setCanonical(ledger.refreshed);
    await watcher.reconcile(state, true);
    expect(watcher.status().detail).toContain("minimum ADA");
    expect(io.submitTx).toHaveBeenCalledTimes(1);
    expect(ledger.signs()).toBe(1);
    expect(retained(ledger.actor).pending[0]!.intent).toEqual(
      before.pending[0]!.intent,
    );
    expect(retained(ledger.actor).reserved).toEqual(before.reserved);
    ledger.setCanonical(ledger.initial);
    await watcher.reconcile(state, true);
    expect(watcher.status().detail).toBeUndefined();
    expect(io.submitTx).toHaveBeenCalledTimes(2);
    expect(io.submitTx.mock.calls[1]![0]).toBe(signed);
    expect(ledger.signs()).toBe(1);
  } finally {
    await watcher.close();
  }
});

it("does not rebroadcast when the post-parameter exact-point read loses canonical evidence", async () => {
  const ledger = await completionLedger();
  funds();
  io.reconcile.mockImplementation(ledger.sdk.reconcileDaAvailabilityOperations);
  io.closeBuild = () =>
    Effect.succeed({
      tx: ledger.transaction({ cost: ledger.initial.coinsPerUtxoByte }),
    } as never);
  const state = observation([answered(liveChallenge("7c").challenged)]);
  const watcher = await runtime("d0".repeat(32));
  try {
    await watcher.reconcile(state, true);
    expect(io.submitTx, JSON.stringify(watcher.status())).toHaveBeenCalledTimes(
      1,
    );
    const before = retained(ledger.actor);
    io.operation
      .mockResolvedValueOnce({ status: "unspent", currentSlot: 1 })
      .mockResolvedValue({
        status: "unknown",
        reason: "point changed while refreshing protocol parameters",
      });
    await watcher.reconcile(state, true);
    expect(watcher.status().phase).toBe("waiting");
    expect(io.submitTx).toHaveBeenCalledTimes(1);
    expect(ledger.signs()).toBe(1);
    expect(retained(ledger.actor)).toEqual(before);
  } finally {
    await watcher.close();
  }
});

it.each(["rollback", "deadline"])(
  "fences late recovery %s before submit and keeps its signed reservations",
  async (retirement) => {
    const ledger = await completionLedger();
    funds();
    io.reconcile.mockImplementation(
      ledger.sdk.reconcileDaAvailabilityOperations,
    );
    io.closeBuild = () =>
      Effect.succeed({
        tx: ledger.transaction({ cost: ledger.initial.coinsPerUtxoByte }),
      } as never);
    const state = observation([answered(liveChallenge("7d").challenged)]);
    const watcher = await runtime("d0".repeat(32));
    try {
      await watcher.reconcile(state, true);
      expect(
        io.submitTx,
        JSON.stringify(watcher.status()),
      ).toHaveBeenCalledTimes(1);
      const before = retained(ledger.actor);
      if (retirement === "deadline")
        vi.useFakeTimers({
          toFake: ["performance", "setTimeout", "clearTimeout"],
        });
      let reads = 0;
      io.operation.mockImplementation(async () => {
        if (++reads === 2) {
          if (retirement === "rollback") watcher.invalidateForRollback();
          else await vi.advanceTimersByTimeAsync(10_001);
        }
        return { status: "unspent", currentSlot: 1 };
      });
      await watcher.reconcile(state, true);
      expect(reads).toBe(2);
      expect(io.submitTx).toHaveBeenCalledTimes(1);
      expect(ledger.signs()).toBe(1);
      expect(retained(ledger.actor)).toEqual(before);
    } finally {
      await watcher.close();
    }
  },
);
