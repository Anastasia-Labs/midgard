import { rmSync } from "node:fs";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import {
  createDaAvailabilityOperationObserver,
  createDaAvailabilityReadScope,
  type DaAvailabilityOperationContext,
  runDaAvailabilityOperation,
} from "@al-ft/midgard-sdk";
import {
  CML,
  type LucidEvolution,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  dirs,
  journals,
  scene,
} from "./helpers/availability-sdk-read-scope.js";
beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1000);
});
afterEach(() => {
  journals.splice(0).forEach((j) => j.close());
  dirs.splice(0).forEach((d) => rmSync(d, { recursive: true, force: true }));
  vi.useRealTimers();
});

describe("shared SDK availability read scopes", () => {
  it("bounds initial authority and aborts its admitted transport before building", async () => {
    const s = scene();
    let cancelled = false;
    const context = {
      ...s.context,
      assertActuationCurrent: async (
        scope?: ReturnType<typeof createDaAvailabilityReadScope>,
      ) => {
        await scope!.read(
          (signal) =>
            new Promise<void>((_resolve, reject) => {
              signal.addEventListener(
                "abort",
                () => {
                  cancelled = true;
                  reject(signal.reason);
                },
                { once: true },
              );
            }),
        );
      },
    };
    const run = runDaAvailabilityOperation(context, s.operation);
    const rejected = expect(run).rejects.toThrow(/deadline 1100 reached/);
    await vi.advanceTimersByTimeAsync(100);
    await rejected;
    expect(cancelled).toBe(true);
    expect(s.build).not.toHaveBeenCalled();
    expect(s.sign).not.toHaveBeenCalled();
    expect(
      s.journal.pending(context.deploymentIdentity, context.actor),
    ).toEqual([]);
  });

  it("does not restart a preparation budget after earlier discovery consumed it", async () => {
    const s = scene();
    const scope = createDaAvailabilityReadScope({
      deadlineEpochMs: 1100,
      attemptTimeoutMs: 100,
      nowMs: Date.now,
      monotonicMs: Date.now,
    });
    try {
      const discovery = scope.read(async () => {
        await new Promise((resolve) => setTimeout(resolve, 80));
      });
      await vi.advanceTimersByTimeAsync(80);
      await discovery;
      const run = runDaAvailabilityOperation(s.context, {
        ...s.operation,
        preparationScope: scope,
        build: async () => {
          await new Promise((resolve) => setTimeout(resolve, 30));
          return s.tx;
        },
      });
      const rejected = expect(run).rejects.toThrow(/deadline 1100 reached/);
      await vi.advanceTimersByTimeAsync(20);
      await rejected;
      await vi.advanceTimersByTimeAsync(10);
      expect(s.sign).not.toHaveBeenCalled();
      expect(s.context.submit).not.toHaveBeenCalled();
    } finally {
      scope.close();
    }
  });

  it("bounds a stalled final authority read using the original build deadline", async () => {
    const s = scene();
    let checks = 0;
    let cancelled = false;
    const context = {
      ...s.context,
      assertActuationCurrent: async (
        scope?: ReturnType<typeof createDaAvailabilityReadScope>,
      ) => {
        if (++checks === 2)
          await scope!.read(
            (signal) =>
              new Promise<void>((_resolve, reject) =>
                signal.addEventListener(
                  "abort",
                  () => {
                    cancelled = true;
                    reject(signal.reason);
                  },
                  { once: true },
                ),
              ),
          );
      },
    };
    const run = runDaAvailabilityOperation(context, s.operation);
    const rejected = expect(run).rejects.toThrow(/deadline 1100 reached/);
    await vi.advanceTimersByTimeAsync(100);
    await rejected;
    expect(checks).toBe(2);
    expect(cancelled).toBe(true);
    expect(s.sign).not.toHaveBeenCalled();
  });

  it("refuses an external preparation scope that could extend the actual protocol deadline", async () => {
    const s = scene();
    const scope = createDaAvailabilityReadScope({
      deadlineEpochMs: 1200,
      attemptTimeoutMs: 200,
      monotonicMs: Date.now,
    });
    try {
      await expect(
        runDaAvailabilityOperation(s.context, {
          ...s.operation,
          preparationScope: scope,
        }),
      ).rejects.toThrow(/exceeds the operation deadline/);
      expect(s.build).not.toHaveBeenCalled();
    } finally {
      scope.close();
    }
  });

  it("shares one signed evidence budget across multiple retained anchors", async () => {
    const s = scene();
    const included: DaAvailabilityOperationContext["observe"] = async (
      intent,
    ) => ({
      status: "included",
      txHash: intent.txHash,
      inclusionPoint: "20:" + "aa".repeat(32),
      confirmationDepth: 10,
      currentSlot: 100,
    });
    expect(
      (
        await runDaAvailabilityOperation(
          { ...s.context, observe: included },
          s.operation,
        )
      ).status,
    ).toBe("confirmed");
    // A distinct signed transaction is needed for the second retained anchor.
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex("cd".repeat(32)),
        0n,
      ),
    );
    const body = CML.TransactionBody.new(
      inputs,
      s.tx.toTransaction().body().outputs(),
      100_000n,
    );
    body.set_validity_interval_start(0n);
    body.set_ttl(1001n);
    const key = CML.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(7));
    const witnesses = CML.TransactionWitnessSet.new();
    const vkeys = CML.VkeywitnessList.new();
    vkeys.add(CML.make_vkey_witness(CML.hash_transaction(body), key));
    witnesses.set_vkeywitnesses(vkeys);
    const signed = CML.Transaction.new(body, witnesses, true);
    const tx = {
      toTransaction: () => signed,
      sign: {
        withWallet: () => ({
          complete: async () => ({ toCBOR: () => signed.to_cbor_hex() }),
        }),
      },
    } as unknown as TxSignBuilder;
    expect(
      (
        await runDaAvailabilityOperation(
          { ...s.context, observe: included },
          {
            ...s.operation,
            headerHash: "44".repeat(28),
            build: async () => tx,
          },
        )
      ).status,
    ).toBe("confirmed");
    expect(s.journal.finalizedAnchors(s.context.actor)).toHaveLength(2);
    const before = s.journal.finalizedAnchors(s.context.actor);
    const freshBuild = vi.fn(async () => s.tx);
    const observe = vi.fn(
      async (
        intent: Parameters<DaAvailabilityOperationContext["observe"]>[0],
      ) => {
        await new Promise((resolve) => setTimeout(resolve, 30));
        return included(intent);
      },
    );
    const run = runDaAvailabilityOperation(
      { ...s.context, observe, observationTimeoutMs: 50 },
      { ...s.operation, headerHash: "55".repeat(28), build: freshBuild },
    );
    await vi.advanceTimersByTimeAsync(50);
    expect((await run).status).toBe("waiting");
    await vi.advanceTimersByTimeAsync(10);
    expect(observe).toHaveBeenCalledTimes(2);
    expect(freshBuild).not.toHaveBeenCalled();
    expect(s.journal.finalizedAnchors(s.context.actor)).toEqual(before);
  });

  it("returns unresolved on signed observation timeout and recovers the same bytes after SQLite reopen", async () => {
    const s = scene();
    expect(
      (await runDaAvailabilityOperation(s.context, s.operation)).status,
    ).toBe("submitted");
    const before = s.journal.pending(
      s.context.deploymentIdentity,
      s.context.actor,
    );
    const refs = s.journal.reservedOutRefs(s.context.actor);
    let resolveObservation!: (value: {
      status: "included";
      txHash: string;
      inclusionPoint: string;
      confirmationDepth: number;
    }) => void;
    const observe = vi.fn(
      () =>
        new Promise<Parameters<typeof resolveObservation>[0]>((resolve) => {
          resolveObservation = resolve;
        }),
    );
    vi.setSystemTime(1200);
    const run = runDaAvailabilityOperation(
      { ...s.context, observe, observationTimeoutMs: 50 },
      s.operation,
    );
    let outcome: Awaited<typeof run> | undefined;
    void run.then((result) => {
      outcome = result;
    });
    await vi.advanceTimersByTimeAsync(50);
    expect(outcome?.status).toBe("waiting");
    await run;
    resolveObservation({
      status: "included",
      txHash: before[0]!.intent.txHash,
      inclusionPoint: "20:" + "aa".repeat(32),
      confirmationDepth: 10,
    });
    await vi.advanceTimersByTimeAsync(0);
    expect(
      s.journal.pending(s.context.deploymentIdentity, s.context.actor),
    ).toEqual(before);
    expect(s.journal.reservedOutRefs(s.context.actor)).toEqual(refs);
    expect(s.sign).toHaveBeenCalledTimes(1);
    expect(s.context.submit).toHaveBeenCalledTimes(1);
    s.journal.close();
    journals.splice(journals.indexOf(s.journal), 1);
    const reopened = openAvailabilityOperationJournal(s.path);
    journals.push(reopened);
    expect(
      reopened.pending(s.context.deploymentIdentity, s.context.actor)[0]!.intent
        .signedCbor,
    ).toBe(before[0]!.intent.signedCbor);
    expect(reopened.reservedOutRefs(s.context.actor)).toEqual(refs);
    const recovered = await runDaAvailabilityOperation(
      {
        ...s.context,
        journal: reopened,
        observe: async () => ({
          status: "included",
          txHash: before[0]!.intent.txHash,
          inclusionPoint: "20:" + "aa".repeat(32),
          confirmationDepth: 10,
        }),
      },
      s.operation,
    );
    expect(recovered.status).toBe("confirmed");
    expect(s.sign).toHaveBeenCalledTimes(1);
  });

  it("bounds signed initial authority separately from an expired unsigned cutoff", async () => {
    const s = scene();
    await runDaAvailabilityOperation(s.context, s.operation);
    const before = s.journal.pending(
      s.context.deploymentIdentity,
      s.context.actor,
    );
    vi.setSystemTime(1200);
    const context = {
      ...s.context,
      observationTimeoutMs: 30,
      assertActuationCurrent: async () => new Promise<void>(() => {}),
    };
    const run = runDaAvailabilityOperation(context, s.operation);
    await vi.advanceTimersByTimeAsync(30);
    expect((await run).status).toBe("waiting");
    expect(
      s.journal.pending(context.deploymentIdentity, context.actor),
    ).toEqual(before);
    expect(s.build).toHaveBeenCalledTimes(1);
  });

  it("does not turn an integrity error from an observation adapter into a timeout", async () => {
    const s = scene();
    await runDaAvailabilityOperation(s.context, s.operation);
    await expect(
      runDaAvailabilityOperation(
        {
          ...s.context,
          observe: async () => {
            throw new Error("canonical mismatch");
          },
        },
        s.operation,
      ),
    ).rejects.toThrow("canonical mismatch");
    expect(
      s.journal.pending(s.context.deploymentIdentity, s.context.actor),
    ).toHaveLength(1);
  });

  it("aborts an admitted observer boundary read and never starts a later provider read", async () => {
    const s = scene();
    await runDaAvailabilityOperation(s.context, s.operation);
    const intent = s.journal.pending(
      s.context.deploymentIdentity,
      s.context.actor,
    )[0]!.intent;
    const readTransactionStatus = vi.fn();
    let aborted = false;
    const observer = createDaAvailabilityOperationObserver({
      lucid: {} as LucidEvolution,
      readTransactionStatus,
      readBoundary: async (scope) =>
        scope!.read(
          (signal) =>
            new Promise((_resolve, reject) => {
              signal.addEventListener(
                "abort",
                () => {
                  aborted = true;
                  reject(signal.reason);
                },
                { once: true },
              );
            }),
        ),
    });
    const scope = createDaAvailabilityReadScope({ attemptTimeoutMs: 40 });
    try {
      const read = observer(intent, scope);
      const rejected = expect(read).rejects.toThrow(/attempt expired/);
      await vi.advanceTimersByTimeAsync(40);
      await rejected;
      expect(aborted).toBe(true);
      expect(readTransactionStatus).not.toHaveBeenCalled();
    } finally {
      scope.close();
    }
  });
});
