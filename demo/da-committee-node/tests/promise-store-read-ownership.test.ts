import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import { createCommitteePromiseAdmissionSource } from "../src/availability/create-promise-admission-source.js";
import { promiseAdmissionFixture } from "./helpers/promise-admission.js";

it("joins the actual JSON count before returning an expired snapshot", async () => {
  const f = await promiseAdmissionFixture();
  const journal = openAvailabilityOperationJournal(
    join(f.dir, "source.sqlite"),
  );
  let release!: () => void;
  let started!: () => void;
  const entered = new Promise<void>((resolve) => {
    started = resolve;
  });
  const blocked = new Promise<void>((resolve) => {
    release = resolve;
  });
  const realUsage = f.store.promiseStoreResourceUsage.bind(f.store);
  const limits = {
    storeRecords: 512,
    storeEncodedBytes: 8 * 1024 * 1024,
    journalEntries: 1024,
  };
  let observed: unknown;
  let settled = false;
  let pending: Promise<unknown> | undefined;
  f.store.promiseStoreResourceUsage = (received) => {
    observed = received;
    pending = (async () => {
      started();
      await blocked;
      const value = await realUsage(received);
      settled = true;
      return value;
    })();
    return pending as ReturnType<typeof realUsage>;
  };
  const actuation = vi.fn(async () => {});
  const boundary = vi.fn(async () => ({
    pointId: "1:point",
    slot: 1,
    blockHash: "12".repeat(32),
    blockNo: 1,
  }));
  const source = createCommitteePromiseAdmissionSource({
    config: { ...f.config, cardanoL1Source: { networkMagic: 1 } },
    deployment: {} as SDK.DaAvailabilityDeployment,
    actorId: "34".repeat(28),
    store: f.store,
    journal,
    lucid: {
      wallet: () => ({ getUtxos: async () => [] }),
    } as unknown as LucidEvolution,
    ogmiosUrl: "ws://unused",
    currentCursor: async () => {
      throw new Error("must not enter cursor after expired count");
    },
    readBoundary: boundary,
    assertActuationCurrent: actuation,
    sourceResourceLimits: limits,
  });
  let monotonic = 0;
  const scope = SDK.createDaAvailabilityReadScope({
    attemptTimeoutMs: 1000,
    monotonicMs: () => monotonic,
  });
  let returned = false;
  const controllerResult = source.readSnapshot(scope);
  void controllerResult.then(
    () => {
      returned = true;
    },
    () => {
      returned = true;
    },
  );
  try {
    await entered;
    expect(observed).toBe(limits);
    monotonic = 1001;
    expect(() => scope.assertCurrent()).toThrow();
    await new Promise<void>((resolve) => setTimeout(resolve, 10));
    expect(returned).toBe(false);
    expect(settled).toBe(false);
    release();
    await expect(controllerResult).rejects.toThrow();
    expect(actuation).not.toHaveBeenCalled();
    expect(boundary).not.toHaveBeenCalled();
    // The refusal retains callback ownership through local read completion.
    expect(settled).toBe(true);
    expect(await f.store.listDaSignatures()).toEqual([]);
    expect(journal.retainedRecordCount()).toBe(0);
  } finally {
    scope.close();
    release();
    await pending;
    await controllerResult.catch(() => undefined);
    journal.close();
    await f.store.close();
  }
  expect(settled).toBe(true);
});
