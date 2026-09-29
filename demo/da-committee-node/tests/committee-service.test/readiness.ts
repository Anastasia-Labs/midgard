import { DaGossipTopic } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { expect, it, vi } from "vitest";

import { availabilityParametersFromConfig } from "../../src/availability/factory.js";
import { CommitteeService } from "../../src/committee-service.js";
import {
  createDaBondPoolMonitor,
  createDaBondPoolWiring,
  daBondPoolCheckFromStatus,
  type DaBondPoolEvent,
} from "../../src/coordinator/pool-monitor.js";
import { DaPeerRegistry } from "../../src/da/libp2p/DaPeerRegistry.js";
import { type StateQueueProvider } from "../../src/l1/state-queue-scanner.js";
import { loadDaSigner } from "../../src/signer.js";
import { JsonFileCommitteeStore } from "../../src/store.js";
import {
  createCommitteeTickRunner,
  L1_VIEW_UNAVAILABLE_EXIT_CODE,
} from "../../src/tick-runner.js";
import {
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from ".././helpers.js";
import { withFinalSnapshot } from ".././helpers/final-snapshot.js";
import { openJsonCommitteeStore } from "./fixtures.js";

export const registerReadinessTests = () => {
  it("registers the store-backed conflict handler before libp2p startup", async () => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const registry = DaPeerRegistry.fromConfig(config.daTransport);
    const setGossipHandler = vi.fn();

    new CommitteeService({
      config,
      store: await JsonFileCommitteeStore.open(dir),
      stateQueueProvider: { fetchStateQueueNodes: async () => [] },
      payloadSource: {
        fetchPayloadCandidates: async () => ({
          ok: false,
          attempts: [],
        }),
      },
      daLibp2pNode: { setGossipHandler, publishGossip: vi.fn() },
      daPeerRegistry: registry,
    });

    expect(setGossipHandler).toHaveBeenCalledWith(
      DaGossipTopic.conflicts,
      expect.any(Function),
    );
  });

  it("is not ready while local chain-sync is still catching up to the tip", async () => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    let catchUp:
      | { events: number; cursorSlot: number; tipSlot: number }
      | undefined;
    const service = new CommitteeService({
      config,
      store: await openJsonCommitteeStore(dir),
      stateQueueProvider: {
        ...withFinalSnapshot({ fetchStateQueueNodes: async () => [] }),
        chainSyncCatchUpProgress: () => catchUp,
      } as StateQueueProvider,
      payloadSource: payloadSourceFromBytes(Buffer.alloc(0)),
    });
    await service.initialize();
    await service.tick();
    await expect(service.readinessSnapshot()).resolves.toMatchObject({
      ready: true,
    });

    // A later sync that is still catching up leaves the last tick's view
    // behind: the member reports not ready until it reaches the tip.
    catchUp = { events: 4_096, cursorSlot: 1_000, tipSlot: 90_000 };
    await expect(service.readinessSnapshot()).resolves.toMatchObject({
      ready: false,
      reasons: [
        "l1_chain_sync_catching_up: events=4096, cursorSlot=1000, tipSlot=90000",
      ],
    });
    catchUp = undefined;
    await expect(service.readinessSnapshot()).resolves.toMatchObject({
      ready: true,
    });
  });

  it("is not ready while the L1 submitter's plain ADA cannot fund the next attestation round", async () => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const service = new CommitteeService({
      config,
      store: await openJsonCommitteeStore(dir),
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [],
      }),
      payloadSource: payloadSourceFromBytes(Buffer.alloc(0)),
    });
    await service.initialize();
    await service.tick();
    const check = {
      checkedAt: "2026-09-26T00:00:00.000Z",
      plainAdaLovelace: 79_999_999n,
      requiredLovelace: 80_000_000n,
    };

    await expect(
      service.readinessSnapshot({
        l1SubmitterFunding: { ...check, sufficient: false },
      }),
    ).resolves.toMatchObject({
      ready: false,
      reasons: [
        "l1_submitter_fee_funding_short: plainAdaLovelace=79999999, requiredLovelace=80000000, checkedAt=2026-09-26T00:00:00.000Z",
      ],
    });
    await expect(
      service.readinessSnapshot({
        l1SubmitterFunding: {
          ...check,
          plainAdaLovelace: 80_000_000n,
          sufficient: true,
        },
      }),
    ).resolves.toMatchObject({ ready: true, reasons: [] });
  });

  it("is not ready while the pooled DA bond is short or Withdrawing, and ready again after a top-up or cancel", async () => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const service = new CommitteeService({
      config,
      store: await openJsonCommitteeStore(dir),
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [],
      }),
      payloadSource: payloadSourceFromBytes(Buffer.alloc(0)),
    });
    await service.initialize();
    await service.tick();
    const parameters = availabilityParametersFromConfig(config);
    const bonded =
      parameters.da_bond_pool_floor_lovelace + parameters.da_bond_lovelace;
    const monitor = createDaBondPoolMonitor({ writeEvent: () => undefined });
    const readiness = async (
      lovelace: bigint,
      datum: SDK.DaBondPoolDatum,
      checkedAt: string,
    ) => {
      monitor.record(
        daBondPoolCheckFromStatus(
          SDK.daBondPoolStatus({ lovelace, datum, parameters }),
          checkedAt,
        ),
      );
      return service.readinessSnapshot({ daBondPool: monitor.latest() });
    };

    await expect(readiness(bonded, "Bonded", "t0")).resolves.toMatchObject({
      ready: true,
      reasons: [],
    });
    await expect(readiness(bonded - 1n, "Bonded", "t1")).resolves.toMatchObject(
      {
        ready: false,
        reasons: [
          `da_bond_pool_backing_short: backing=${(parameters.da_bond_lovelace - 1n).toString()}, required=${parameters.da_bond_lovelace.toString()}, checkedAt=t1`,
        ],
      },
    );
    // A read failure keeps the last good check's reason.
    monitor.recordReadFailure(new Error("kupo unavailable"));
    await expect(
      service.readinessSnapshot({ daBondPool: monitor.latest() }),
    ).resolves.toMatchObject({ ready: false, reasons: [expect.any(String)] });
    await expect(readiness(bonded, "Bonded", "t2")).resolves.toMatchObject({
      ready: true,
      reasons: [],
    });
    await expect(
      readiness(bonded, { Withdrawing: { unlock_at: 42n } }, "t3"),
    ).resolves.toMatchObject({
      ready: false,
      reasons: ["da_bond_pool_withdrawing: unlockAt=42, checkedAt=t3"],
    });
    await expect(readiness(bonded, "Bonded", "t4")).resolves.toMatchObject({
      ready: true,
      reasons: [],
    });
  });

  it("carries a drained pool read from the submitter hooks, through the tick runner, to one event and a not-ready snapshot", async () => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const service = new CommitteeService({
      config,
      store: await openJsonCommitteeStore(dir),
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [],
      }),
      payloadSource: payloadSourceFromBytes(Buffer.alloc(0)),
    });
    await service.initialize();
    const parameters = availabilityParametersFromConfig(config);
    const events: DaBondPoolEvent[] = [];
    const wiring = createDaBondPoolWiring({
      writeEvent: (event) => events.push(event),
    });
    // The submitter reports each pool read through the factory hooks.
    let lovelace = parameters.da_bond_pool_floor_lovelace;
    let reads = 0;
    let readFails = false;
    const coordinator = {
      checkDaBondPool: async () => {
        reads += 1;
        if (readFails) {
          // As the submitter does: report the failure, then rethrow.
          const error = new Error("kupo unavailable");
          wiring.coordinatorHooks.recordDaBondPoolReadFailure(error);
          throw error;
        }
        const check = daBondPoolCheckFromStatus(
          SDK.daBondPoolStatus({ lovelace, datum: "Bonded", parameters }),
          `t${reads.toString()}`,
        );
        wiring.coordinatorHooks.recordDaBondPool(check);
        return check;
      },
    };
    const runner = createCommitteeTickRunner({
      tick: async () => service.tick(),
      runAvailabilityResponse: async () => undefined,
      ...wiring.tickRunnerDeps(coordinator),
      runRetention: async () => undefined,
      latestL1View: () => service.latestL1View(),
      latestL1ProgressAtMs: () => service.latestL1ProgressAtMs(),
      setRetentionReadiness: () => undefined,
      l1ViewFatalMs: 60_000,
      startedAtMs: Date.now(),
      nowMs: () => Date.now(),
      write: () => undefined,
      shutdown: async () => undefined,
      exit: () => undefined,
      shutdownGraceMs: 10,
    });
    expect(wiring.tickRunnerDeps(undefined)).toEqual({});

    await runner.runTick();
    expect(reads).toBe(1);
    await expect(
      service.readinessSnapshot(wiring.readiness()),
    ).resolves.toMatchObject({
      ready: false,
      reasons: [
        `da_bond_pool_backing_short: backing=0, required=${parameters.da_bond_lovelace.toString()}, checkedAt=t1`,
      ],
    });
    expect(events).toEqual([
      expect.objectContaining({
        event: "da_bond_pool_backing_short",
        backing: "0",
      }),
    ]);

    // A failed read reaches the monitor through the failure hook: one event,
    // and the last good check's reason stays.
    readFails = true;
    await runner.runTick();
    expect(reads).toBe(2);
    await expect(
      service.readinessSnapshot(wiring.readiness()),
    ).resolves.toMatchObject({
      ready: false,
      reasons: [expect.stringContaining("da_bond_pool_backing_short: ")],
    });
    expect(events.slice(1)).toEqual([
      expect.objectContaining({
        event: "da_bond_pool_read_failed",
        error: "kupo unavailable",
      }),
    ]);

    readFails = false;
    lovelace += parameters.da_bond_lovelace;
    await runner.runTick();
    await expect(
      service.readinessSnapshot(wiring.readiness()),
    ).resolves.toMatchObject({ ready: true, reasons: [] });
    expect(events.map(({ event }) => event)).toEqual([
      "da_bond_pool_backing_short",
      "da_bond_pool_read_failed",
      "da_bond_pool_backing_restored",
    ]);
  });

  it("keeps a tick whose local chain-sync is still moving toward the tip clear of the L1-view deadline", async () => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    let nowMs = Date.parse("2026-09-26T00:00:00.000Z");
    let catchUp:
      | { events: number; cursorSlot: number; tipSlot: number }
      | undefined;
    const service = new CommitteeService({
      config,
      store: await openJsonCommitteeStore(dir),
      stateQueueProvider: {
        ...withFinalSnapshot({ fetchStateQueueNodes: async () => [] }),
        chainSyncCatchUpProgress: () => catchUp,
      } as StateQueueProvider,
      payloadSource: payloadSourceFromBytes(Buffer.alloc(0)),
      now: () => new Date(nowMs),
    });
    await service.initialize();
    const fatalMs = 240_000;
    const exits: number[] = [];
    const runner = createCommitteeTickRunner({
      // The first tick of a member far behind: synchronizeToTip walks
      // chain-sync for longer than the deadline before any view exists.
      tick: () => new Promise<never>(() => undefined),
      runAvailabilityResponse: async () => undefined,
      runRetention: async () => undefined,
      latestL1View: () => service.latestL1View(),
      latestL1ProgressAtMs: () => service.latestL1ProgressAtMs(),
      setRetentionReadiness: () => undefined,
      l1ViewFatalMs: fatalMs,
      startedAtMs: nowMs,
      nowMs: () => nowMs,
      write: () => undefined,
      shutdown: async () => undefined,
      exit: (code) => exits.push(code),
      shutdownGraceMs: 10,
    });
    void runner.runTick();

    for (let chunk = 1; chunk <= 3; chunk += 1) {
      nowMs += fatalMs;
      catchUp = {
        events: chunk * 4_096,
        cursorSlot: chunk * 1_000,
        tipSlot: 90_000,
      };
      await runner.runTick();
      expect(exits).toEqual([]);
    }
    // A cursor that stops moving is no progress.
    nowMs += fatalMs + 1;
    await runner.runTick();
    expect(exits).toEqual([L1_VIEW_UNAVAILABLE_EXIT_CODE]);
  });
};
