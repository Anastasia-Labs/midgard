import { rmSync } from "node:fs";

import { NativeLedgerKupmios } from "@al-ft/midgard-core/native-reward-account";
import * as SDK from "@al-ft/midgard-sdk";
import type { TxSignBuilder } from "@lucid-evolution/lucid";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  committeeOwnedReadTransports,
  registerCommitteeReadOwner,
} from "../src/availability/committee-owned-read-transports.js";
import { committeePromiseActorRuntime } from "../src/availability/promise-actor-runtime.js";
import { committeeAttemptProvider } from "../src/l1/availability-scoped-lucid.js";
import { minimalConfig } from "./helpers.js";
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
  journals.splice(0).forEach((journal) => journal.close());
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true }));
  vi.restoreAllMocks();
  vi.useRealTimers();
});

describe("committee actor runtime drainage", () => {
  it.each(["unsigned callback", "public provider callback"])(
    "keeps a real SDK timed-out %s busy after its journal lease is released",
    async (kind) => {
      const s = scene();
      const breach = vi.fn();
      const runtime = committeePromiseActorRuntime(breach);
      const scope = SDK.createDaAvailabilityReadScope({
        deadlineEpochMs: 1100,
        attemptTimeoutMs: 30,
        nowMs: Date.now,
        monotonicMs: Date.now,
      });
      let finish!: (value: TxSignBuilder) => void;
      const physical = new Promise<TxSignBuilder>((resolve) => {
        finish = resolve;
      });
      const owner = committeeOwnedReadTransports();
      registerCommitteeReadOwner(scope, owner);
      let build = () => physical;
      if (kind === "public provider callback") {
        // Delay the actual public provider boundary; transport parity and physical
        // socket retirement are covered by the separate real TCP provider suite.
        vi.spyOn(NativeLedgerKupmios.prototype, "getUtxos").mockImplementation(
          async () => {
            await physical;
            return [];
          },
        );
        const config = minimalConfig({
          dir: dirs.at(-1)!,
          manifestPath: "fixture",
          deploymentInfoPath: "fixture",
          signerSeed: "00".repeat(31) + "01",
          signerPublicKey: "00".repeat(32),
        });
        const provider = committeeAttemptProvider({
          config: { ...config, cardanoL1Source: { networkMagic: 2 } },
          kupoUrl: "http://unused",
          ogmiosUrl: "http://unused",
          scope,
          requestRefusalMs: 30,
          limits: {
            requestRefusalMs: 30,
            httpResponseBytes: 1000,
            webSocketMessageBytes: 1000,
            rawUtxos: 1,
          },
        });
        build = async () => {
          await provider.getUtxos("fixture");
          return s.tx;
        };
      }
      let tracked!: Promise<TxSignBuilder>;
      const run = SDK.runDaAvailabilityOperation(s.context, {
        ...s.operation,
        preparationScope: scope,
        build: () => {
          tracked = runtime.trackUnsigned(scope, build);
          return tracked;
        },
      });
      const refused = expect(run).rejects.toThrow(/read deadline 1100 reached/);
      await vi.advanceTimersByTimeAsync(30);
      await refused;
      expect(
        s.journal.actorSnapshot(s.context.actor, s.context.deploymentIdentity),
      ).toMatchObject({
        retainedRecordCount: 0,
        reservedResourceCount: 0,
        lease: { expiresAtMs: 0 },
      });
      expect(() => runtime.assertIdle()).toThrow("has not drained");
      expect(breach).toHaveBeenCalledWith("unsigned_actor_attempt_expired");
      const nextScope = SDK.createDaAvailabilityReadScope({
        attemptTimeoutMs: 100,
        nowMs: Date.now,
        monotonicMs: Date.now,
      });
      const overlapping = vi.fn(async () => s.tx);
      await expect(
        runtime.trackUnsigned(nextScope, overlapping),
      ).rejects.toThrow("has not drained");
      expect(overlapping).not.toHaveBeenCalled();
      let joined = false;
      const join = runtime.join().then(() => {
        joined = true;
      });
      await Promise.resolve();
      expect(joined).toBe(false);
      finish(s.tx);
      await expect(tracked).rejects.toThrow(/read deadline 1100 reached/);
      await join;
      expect(joined).toBe(true);
      await owner.drain();
      owner.assertDrained();
      nextScope.close();
      expect(() => runtime.assertIdle()).not.toThrow();
      expect(s.sign).not.toHaveBeenCalled();
      expect(s.context.submit).not.toHaveBeenCalled();
      expect(s.journal.retainedRecordCount()).toBe(0);
      scope.close();
    },
  );

  it("drains successful isolated unsigned work before closing its scope", async () => {
    const breach = vi.fn();
    const runtime = committeePromiseActorRuntime(breach);
    const scope = SDK.createDaAvailabilityReadScope({
      attemptTimeoutMs: 100,
      nowMs: Date.now,
      monotonicMs: Date.now,
    });
    await expect(
      runtime.trackUnsigned(scope, async () => "built"),
    ).resolves.toBe("built");
    scope.close();
    expect(() => runtime.assertIdle()).not.toThrow();
    expect(breach).not.toHaveBeenCalled();
  });
});
