import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { promiseAdmissionFixture } from "./helpers/promise-admission.js";

const pause = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));

describe("one committee admission read scope", () => {
  it.each(["snapshot", "final"])(
    "joins failed %s source cleanup before returning refusal",
    async (phase) => {
      const f = await promiseAdmissionFixture();
      let release!: () => void;
      const released = new Promise<void>((resolve) => {
        release = resolve;
      });
      let entered!: () => void;
      const draining = new Promise<void>((resolve) => {
        entered = resolve;
      });
      let joined = 0;
      Object.assign(f.source, {
        openReadScope: () =>
          SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 10000 }),
        readSnapshot: async () => {
          if (phase === "snapshot") throw new Error("protocol HTTP503");
          return f.getSnapshot();
        },
        assertCurrent: async () => {
          throw new Error("final protocol HTTP503");
        },
        drainReadResources: async () => {
          entered();
          await released;
          joined++;
        },
      });
      const service = f.service();
      await service.initialize();
      let returned = false;
      const tick = service.tick().then((result) => {
        returned = true;
        return result;
      });
      try {
        expect(
          await Promise.race([
            draining.then(() => "draining"),
            tick.then(() => "returned"),
          ]),
        ).toBe("draining");
        expect(returned).toBe(false);
        expect(f.sign).not.toHaveBeenCalled();
      } finally {
        release();
        expect(await tick).toMatchObject({ signedHeaders: 0, errors: [] });
      }
      expect(joined).toBe(2);
      expect(await f.store.listDaSignatures()).toEqual([]);
    },
  );

  it("keeps the candidate scope through the final signing fence and closes refused candidates", async () => {
    const f = await promiseAdmissionFixture();
    const scopes: SDK.DaAvailabilityReadScope[] = [];
    Object.assign(f.source, {
      openReadScope: () => {
        const scope = SDK.createDaAvailabilityReadScope({
          attemptTimeoutMs: 1000,
        });
        scopes.push(scope);
        return scope;
      },
    });
    const service = f.service();
    await service.initialize();
    expect(await service.tick()).toMatchObject({
      signedHeaders: 1,
      errors: [],
    });
    expect(scopes).toHaveLength(2);
    expect(scopes.every((scope) => scope.signal.aborted)).toBe(true);
    expect(f.sign).toHaveBeenCalledTimes(1);
  });

  it("refuses a late final fence without renewing the snapshot budget", async () => {
    const f = await promiseAdmissionFixture();
    let started = 0;
    let completed = 0;
    Object.assign(f.source, {
      openReadScope: () =>
        SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 10 }),
      assertCurrent: async () => {
        started++;
        await pause(30);
        completed++;
      },
    });
    const service = f.service();
    await service.initialize();
    expect(await service.tick()).toMatchObject({
      signedHeaders: 0,
      errors: [],
    });
    expect(f.sign).not.toHaveBeenCalled();
    expect(started).toBeGreaterThan(0);
    expect(completed).toBe(started);
    expect(await f.store.listDaSignatures()).toEqual([]);
  });

  it("joins an expired durable signature read before returning admission", async () => {
    const f = await promiseAdmissionFixture();
    let started = 0;
    let completed = 0;
    const original = f.store.listDaSignatures.bind(f.store);
    Object.assign(f.source, {
      openReadScope: () =>
        SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 10 }),
    });
    Object.assign(f.store, {
      listDaSignatures: async () => {
        started++;
        await pause(30);
        const result = await original();
        completed++;
        return result;
      },
    });
    const service = f.service();
    await service.initialize();
    await service.tick();
    expect(started).toBeGreaterThan(0);
    expect(completed).toBe(started);
    expect(f.sign).not.toHaveBeenCalled();
  });

  it("awaits snapshot-owned evidence writes instead of racing them into the next admission", async () => {
    const f = await promiseAdmissionFixture();
    let completed = 0;
    Object.assign(f.source, {
      openReadScope: () =>
        SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 5 }),
      readSnapshot: async () => {
        await pause(20);
        completed++;
        return f.getSnapshot();
      },
    });
    const service = f.service();
    await service.initialize();
    expect(await service.tick()).toMatchObject({
      signedHeaders: 0,
      errors: [],
    });
    expect(completed).toBe(2);
    expect(f.sign).not.toHaveBeenCalled();
  });
});
