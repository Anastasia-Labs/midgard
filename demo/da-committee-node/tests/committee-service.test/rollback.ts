import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { CommitteeService } from "../../src/committee-service.js";
import {
  type CanonicalChainPoint,
  type ChainSyncCursor,
  type ChainSyncEvent,
  type ChainSyncEventSource,
  FileChainSyncConsumerCursorStore,
  FileChainSyncCursorStore,
  LocalNodeChainAuthority,
  LocalNodeStateQueueProvider,
} from "../../src/l1/provider.js";
import { loadDaSigner, validateDaSignerMembership } from "../../src/signer.js";
import { JsonFileCommitteeStore } from "../../src/store.js";
import { bytesToHex } from "../../src/utils/hex.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from ".././helpers.js";
import { withFinalSnapshot } from ".././helpers/final-snapshot.js";
import {
  expectedCommitment,
  failPayloadSource,
  openJsonCommitteeStore,
  retainedSignedPayload,
  runAnchorAheadOfObservations,
} from "./fixtures.js";

export const registerRollbackTests = () => {
  it("persists L1 disappearance quarantine across restart and prevents processing or rebroadcast", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "21";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configWithDaHash = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    });
    const firstStore = await openJsonCommitteeStore(dir);
    const first = new CommitteeService({
      config: configWithDaHash,
      store: firstStore,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
      payloadSource: payloadSourceFromBytes(payloadCbor),
      signer,
      signerValidation,
      coordinator: {
        publishSignature: async () => "posted",
      },
    });
    await first.initialize();
    await expect(first.tick()).resolves.toMatchObject({ signedHeaders: 1 });
    await expect(firstStore.getL1SourceState()).resolves.toMatchObject({
      status: "healthy",
      observations: [{ headerHash, hasPersistedDecision: true }],
    });
    await firstStore.saveL1Submission({
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      headerHash,
      txKind: "init",
      txHash: "71".repeat(32),
      inputsUsed: [],
      submittedAt: "2026-07-28T00:00:00.000Z",
      resultStatus: "submitted",
    });
    // The anchor already past the header's disappearance, so the scan of
    // the empty queue succeeds and only the persisted decision tells.
    await runAnchorAheadOfObservations(firstStore, [
      { headerHash: null, outRef: `${"00".repeat(32)}#0` },
    ]);
    await firstStore.savePeerBroadcast({
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      peerId: "peer-a",
      headerHash,
      availabilityCommitmentDigest: expectedCommitment(
        configWithDaHash,
        headerHash,
        payloadCbor,
      ).commitmentDigest,
      signerIndex: 0,
      status: "pending",
      attempts: 1,
      nextAttemptAt: "2026-07-28T00:01:00.000Z",
      updatedAt: "2026-07-28T00:00:00.000Z",
    });

    await firstStore.close();
    const restartedStore = await openJsonCommitteeStore(dir);
    const disappeared = new CommitteeService({
      config: configWithDaHash,
      store: restartedStore,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [],
      }),
      payloadSource: failPayloadSource("quarantine must suppress DA fetch"),
      signer,
      signerValidation,
    });
    await disappeared.initialize();
    await expect(disappeared.tick()).resolves.toMatchObject({
      scannedHeaders: 0,
      signedHeaders: 0,
      errors: [expect.stringContaining("decision_disappeared")],
    });
    await expect(restartedStore.getL1SourceState()).resolves.toMatchObject({
      status: "quarantined",
      quarantineReason: expect.stringContaining("decision_disappeared"),
    });
    await expect(
      restartedStore.getStateQueueHeader(headerHash),
    ).resolves.toMatchObject({
      status: "conflicted",
      validationErrors: [expect.stringContaining("l1_source_quarantined")],
    });
    // Quarantine holds decisions, not the bytes this member signed.
    await expect(
      retainedSignedPayload(restartedStore, configWithDaHash, headerHash),
    ).resolves.toEqual(payloadCbor);
    await expect(
      restartedStore.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment(
          configWithDaHash,
          headerHash,
          payloadCbor,
        ).commitmentDigest,
        signerIndex: 0,
      }),
    ).resolves.toMatchObject({ broadcastStatus: "post_failed" });
    await expect(restartedStore.listL1Submissions()).resolves.toMatchObject([
      {
        headerHash,
        resultStatus: "failed",
        failureCause: expect.stringContaining("l1_source_quarantined"),
      },
    ]);
    const [quarantinedBroadcast] =
      await restartedStore.listPeerBroadcasts(headerHash);
    expect(quarantinedBroadcast).toMatchObject({
      status: "failed",
      lastError: expect.stringContaining("l1_source_quarantined"),
    });
    expect(quarantinedBroadcast).not.toHaveProperty("nextAttemptAt");
    await expect(
      restartedStore.listDecisionOutbox(headerHash),
    ).resolves.toMatchObject([
      {
        status: "failed",
        lastError: expect.stringContaining("l1_source_quarantined"),
        quarantineReason: expect.stringContaining("decision_disappeared"),
        quarantinedAt: expect.any(String),
      },
    ]);

    let publishCalls = 0;
    await restartedStore.close();
    const afterQuarantine = new CommitteeService({
      config: configWithDaHash,
      store: await openJsonCommitteeStore(dir),
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
      payloadSource: failPayloadSource("quarantine must survive restart"),
      signer,
      signerValidation,
      coordinator: {
        retryPublishedSignatures: true,
        publishSignature: async () => {
          publishCalls += 1;
          return "posted";
        },
      },
    });
    await afterQuarantine.initialize();
    await expect(afterQuarantine.tick()).resolves.toMatchObject({
      scannedHeaders: 0,
      signedHeaders: 0,
      errors: [expect.stringContaining("L1 source quarantined")],
    });
    expect(publishCalls).toBe(0);
  });

  it("consumes a restarted local-node rollback feed and quarantines a persisted decision", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "23";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configured = {
      ...config,
      l1Source: {
        sourceMode: "local_node" as const,
        authorityNodeId: "node-a",
        chainSyncProviderUrl: "chain-sync:ogmios:ws://ogmios.local",
        chainSyncCursorPath: `${dir}/chain-sync.json`,
        queryProviderUrls: ["kupmios:http://kupo.local|ws://ogmios.local"],
      },
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configured.daParams,
      signer,
      signerIndex: 0,
    });
    const initialPoint = {
      network: "Preprod",
      slot: 10,
      blockHash: "11".repeat(32),
      providerSource: "chain-sync:node-a",
      observedAt: "2026-07-28T00:00:00.000Z",
    };
    const initialCursor: ChainSyncCursor = {
      sequence: 0,
      point: initialPoint,
      rollbackGeneration: 0,
    };
    let consumedCursor: ChainSyncCursor | undefined;
    const firstProvider = withFinalSnapshot({
      fetchStateQueueNodes: async () => [
        makeObservedNode({
          header,
          headerHash,
          depth: 10,
          slot: 10,
          blockHash: "11".repeat(32),
        }),
      ],
      currentChainSyncCursor: async () => initialCursor,
      replayChainSyncEvents: async () => [],
      loadConsumedChainSyncCursor: async () => consumedCursor,
      acknowledgeChainSyncCursor: async (cursor: ChainSyncCursor) => {
        consumedCursor = cursor;
        return { rollbackSinceCapture: false };
      },
    });
    const firstStore = await openJsonCommitteeStore(dir);
    const first = new CommitteeService({
      config: configured,
      store: firstStore,
      stateQueueProvider: firstProvider,
      payloadSource: payloadSourceFromBytes(payloadCbor),
      signer,
      signerValidation,
    });
    await first.initialize();
    await expect(first.tick()).resolves.toMatchObject({ signedHeaders: 1 });
    await expect(firstStore.getL1SourceState()).resolves.toMatchObject({
      status: "healthy",
      observations: [
        {
          headerHash,
          slot: 10,
          blockHash: "11".repeat(32),
          hasPersistedDecision: true,
        },
      ],
    });
    expect(consumedCursor).toEqual(initialCursor);

    await firstStore.close();
    const rollbackPoint = {
      network: "Preprod",
      slot: 9,
      blockHash: "22".repeat(32),
      providerSource: "chain-sync:node-a",
      observedAt: "1970-01-01T00:00:00.000Z",
    };
    const currentPoint = {
      ...rollbackPoint,
      slot: 12,
      blockHash: "33".repeat(32),
    };
    const currentCursor: ChainSyncCursor = {
      sequence: 2,
      point: currentPoint,
      rollbackGeneration: 1,
    };
    const rollbackEvents: readonly ChainSyncEvent[] = [
      { direction: "roll_backward", point: rollbackPoint },
      { direction: "roll_forward", point: currentPoint },
    ];
    const replayChainSyncEvents = async (
      afterSequence: number,
    ): Promise<readonly ChainSyncEvent[]> => {
      expect(afterSequence).toBe(initialCursor.sequence);
      return rollbackEvents;
    };
    const restartedProvider = withFinalSnapshot({
      fetchStateQueueNodes: async () => [
        makeObservedNode({
          header,
          headerHash,
          depth: 10,
          slot: 10,
          blockHash: "11".repeat(32),
        }),
      ],
      currentChainSyncCursor: async () => currentCursor,
      replayChainSyncEvents,
      loadConsumedChainSyncCursor: async () => consumedCursor,
      acknowledgeChainSyncCursor: async () => {
        throw new Error("quarantined rollback must not be acknowledged");
      },
    });
    const restartedStore = await openJsonCommitteeStore(dir);
    const restarted = new CommitteeService({
      config: configured,
      store: restartedStore,
      stateQueueProvider: restartedProvider,
      payloadSource: failPayloadSource(
        "rollback quarantine must suppress DA fetch",
      ),
      signer,
      signerValidation,
    });
    await restarted.initialize();

    await expect(restarted.tick()).resolves.toMatchObject({
      scannedHeaders: 0,
      signedHeaders: 0,
      errors: [expect.stringContaining("chain_sync_rollback")],
    });
    await expect(restartedStore.getL1SourceState()).resolves.toMatchObject({
      status: "quarantined",
      quarantineReason: expect.stringContaining(
        `l1_source_chain_sync_rollback:${headerHash}:9:${"22".repeat(32)}`,
      ),
    });
  });

  describe("acknowledging the chain-sync feed a tick checked", () => {
    /**
     * A signing committee on a real local-node chain authority and durable
     * consumer. The test extends or forks the chain while a tick is still
     * running, after its rollback check, and synchronizes the authority there
     * as the tick's own queries do.
     */
    const midTickCommittee = async (seedByte: string) => {
      const dir = await tempDir();
      const { header, headerHash, payloadCbor } = await makePayloadFixture();
      const seed = "00".repeat(31) + seedByte;
      const signer = await loadDaSigner(`hex:${seed}`);
      const config = minimalConfig({
        dir,
        manifestPath: `${dir}/manifest.json`,
        deploymentInfoPath: `${dir}/deployment.json`,
        signerSeed: seed,
        signerPublicKey: signer.publicKeyHex,
      });
      const configured = {
        ...config,
        l1Source: {
          sourceMode: "local_node" as const,
          authorityNodeId: "node-a",
          chainSyncProviderUrl: "chain-sync:ogmios:ws://ogmios.local",
          chainSyncCursorPath: `${dir}/chain-sync.json`,
          queryProviderUrls: ["kupmios:http://kupo.local|ws://ogmios.local"],
        },
        daParams: {
          ...config.daParams,
          committeeSignersHash: bytesToHex(
            blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
          ),
        },
      };
      const signerValidation = validateDaSignerMembership({
        daParams: configured.daParams,
        signer,
        signerIndex: 0,
      });
      const pointAt = (slot: number, fork = 0): CanonicalChainPoint => ({
        network: configured.network,
        slot,
        blockHash: `${fork.toString(16).padStart(2, "0")}${slot.toString(16).padStart(62, "0")}`,
        providerSource: "chain-sync:node-a",
        observedAt: "2026-07-28T00:00:00.000Z",
      });
      const chain = {
        points: Array.from({ length: 12 }, (_, index) => pointAt(index + 1)),
      };
      // Follows `chain`; a cursor that left it rolls back to the newest point
      // below it, so a fork's blocks sit above the old tip's slot.
      const source: ChainSyncEventSource = {
        next: async (cursor) => {
          const tip = chain.points.at(-1)!;
          const at = chain.points.findIndex(
            (point) =>
              point.slot === cursor?.point.slot &&
              point.blockHash === cursor.point.blockHash,
          );
          if (cursor !== undefined && at < 0) {
            return {
              event: {
                direction: "roll_backward",
                point: chain.points
                  .filter((point) => point.slot < cursor.point.slot)
                  .at(-1)!,
              },
              tip,
            };
          }
          const next = chain.points[at + 1];
          return next === undefined
            ? { tip }
            : { event: { direction: "roll_forward", point: next }, tip };
        },
      };
      const authority = new LocalNodeChainAuthority(
        "node-a",
        configured.network,
        source,
        new FileChainSyncCursorStore(`${dir}/chain-sync.json`, "11".repeat(32)),
      );
      const local = new LocalNodeStateQueueProvider(
        authority,
        [
          {
            fetchStateQueueNodes: () => {
              throw new Error("the test serves the snapshot itself");
            },
            currentChainPoint: () => authority.currentPoint(),
          },
        ],
        ["query:node-a:0"],
        new FileChainSyncConsumerCursorStore(
          `${dir}/chain-sync-consumer.json`,
          "11".repeat(32),
        ),
      );
      const node = makeObservedNode({
        header,
        headerHash,
        depth: 10,
        slot: 10,
        blockHash: pointAt(10).blockHash,
      });
      const replays: (readonly ChainSyncEvent[])[] = [];
      // Each runs once: right after the tick's snapshot is read, or at its
      // first header upsert, after its rollback check and before it
      // acknowledges the feed.
      const inject: {
        afterSnapshot?: () => Promise<void>;
        afterRollbackCheck?: () => Promise<void>;
      } = {};
      const runInjected = async (point: keyof typeof inject) => {
        const run = inject[point];
        inject[point] = undefined;
        await run?.();
      };
      const finalSnapshot = withFinalSnapshot({
        // As on the local-node provider, a snapshot first synchronizes the
        // authority to the tip.
        fetchStateQueueNodes: async () => {
          await authority.synchronizeToTip();
          return [node];
        },
        currentChainSyncCursor: () => local.currentChainSyncCursor(),
        replayChainSyncEvents: async (afterSequence: number) => {
          const events = await local.replayChainSyncEvents(afterSequence);
          replays.push(events);
          return events;
        },
        loadConsumedChainSyncCursor: () => local.loadConsumedChainSyncCursor(),
        acknowledgeChainSyncCursor: (cursor: ChainSyncCursor) =>
          local.acknowledgeChainSyncCursor(cursor),
      });
      const stateQueueProvider = Object.assign(
        Object.create(finalSnapshot) as typeof finalSnapshot,
        {
          fetchStateQueueSnapshot: async () => {
            const snapshot = await finalSnapshot.fetchStateQueueSnapshot();
            await runInjected("afterSnapshot");
            return snapshot;
          },
        },
      );
      const store = await openJsonCommitteeStore(dir);
      const tickStore = new Proxy(store, {
        get: (target, property) => {
          if (property === "upsertStateQueueHeader") {
            return async (
              ...args: Parameters<
                JsonFileCommitteeStore["upsertStateQueueHeader"]
              >
            ) => {
              await runInjected("afterRollbackCheck");
              return target.upsertStateQueueHeader(...args);
            };
          }
          const value = Reflect.get(target, property) as unknown;
          return typeof value === "function" ? value.bind(target) : value;
        },
      });
      const events: Record<string, unknown>[] = [];
      const service = new CommitteeService({
        config: configured,
        store: tickStore,
        stateQueueProvider,
        payloadSource: payloadSourceFromBytes(payloadCbor),
        signer,
        signerValidation,
        coordinator: { publishSignature: async () => "posted" },
        writeEvent: (line) => {
          events.push(JSON.parse(line) as Record<string, unknown>);
        },
      });
      await service.initialize();
      const extend = (slots: readonly number[], fork = 0) => {
        chain.points.push(...slots.map((slot) => pointAt(slot, fork)));
      };
      /** The node forks back to slot 8, below the header's slot 10. */
      const forkBelowHeader = async () => {
        chain.points.length = 8;
        extend([20, 21], 1);
        await authority.synchronizeToTip();
      };
      const rollbackQuarantine = `l1_source_chain_sync_rollback:${headerHash}:8:${pointAt(8).blockHash}`;
      return {
        service,
        store,
        authority,
        local,
        extend,
        forkBelowHeader,
        rollbackQuarantine,
        inject,
        replays,
        events,
      };
    };

    describe.each(["afterSnapshot", "afterRollbackCheck"] as const)(
      "with the chain moving %s",
      (point) => {
        it("acknowledges the snapshot's cursor and replays exactly the newer events next tick", async () => {
          const { service, store, authority, local, extend, inject, replays } =
            await midTickCommittee("61");
          await expect(service.tick()).resolves.toMatchObject({
            signedHeaders: 1,
            errors: [],
          });
          await expect(
            local.loadConsumedChainSyncCursor(),
          ).resolves.toMatchObject({ sequence: 11 });

          // Two blocks before the next tick; five more while it runs.
          extend([13, 14]);
          inject[point] = async () => {
            extend([15, 16, 17, 18, 19]);
            await authority.synchronizeToTip();
          };
          await expect(service.tick()).resolves.toMatchObject({ errors: [] });
          expect(inject[point]).toBeUndefined();
          await expect(
            local.loadConsumedChainSyncCursor(),
          ).resolves.toMatchObject({ sequence: 13, rollbackGeneration: 0 });
          await expect(authority.currentCursor()).resolves.toMatchObject({
            sequence: 18,
          });

          // The next tick replays exactly the five events after the cursor
          // the last one's snapshot was read at.
          await expect(service.tick()).resolves.toMatchObject({ errors: [] });
          expect(
            replays
              .at(-1)!
              .map(({ direction, point: { slot } }) => [direction, slot]),
          ).toEqual([15, 16, 17, 18, 19].map((slot) => ["roll_forward", slot]));
          await expect(
            local.loadConsumedChainSyncCursor(),
          ).resolves.toMatchObject({ sequence: 18 });
          expect((await store.getL1SourceState())?.status).toBe("healthy");
        });

        it("quarantines on the next tick when the chain rolled back below a decision the first tick made", async () => {
          const {
            service,
            store,
            local,
            forkBelowHeader,
            rollbackQuarantine,
            inject,
            events,
          } = await midTickCommittee("62");
          inject[point] = forkBelowHeader;
          await expect(service.tick()).resolves.toMatchObject({
            signedHeaders: 1,
            errors: [],
          });
          expect(inject[point]).toBeUndefined();
          // The decision was made at the snapshot's cursor, which is what the
          // tick acknowledged, so the rollback after it is still to replay.
          await expect(local.loadConsumedChainSyncCursor()).resolves.toEqual(
            expect.objectContaining({ sequence: 11, rollbackGeneration: 0 }),
          );
          expect(events).toContainEqual({
            event: "l1_chain_sync_rollback_after_acknowledged_cursor",
            sequence: 11,
            rollbackGeneration: 0,
          });
          expect((await store.getL1SourceState())?.status).toBe("healthy");

          await expect(service.tick()).resolves.toMatchObject({
            signedHeaders: 0,
            errors: [expect.stringContaining("chain_sync_rollback")],
          });
          await expect(store.getL1SourceState()).resolves.toMatchObject({
            status: "quarantined",
            quarantineReason: rollbackQuarantine,
          });
        });
      },
    );

    it("quarantines on the next tick when the chain rolled back below a decision after the rollback check, with a consumed cursor recorded", async () => {
      const {
        service,
        store,
        local,
        extend,
        forkBelowHeader,
        rollbackQuarantine,
        inject,
      } = await midTickCommittee("63");
      await expect(service.tick()).resolves.toMatchObject({
        signedHeaders: 1,
        errors: [],
      });
      extend([13, 14]);
      inject.afterRollbackCheck = forkBelowHeader;
      await expect(service.tick()).resolves.toMatchObject({ errors: [] });
      expect(inject.afterRollbackCheck).toBeUndefined();
      await expect(local.loadConsumedChainSyncCursor()).resolves.toMatchObject({
        sequence: 13,
        rollbackGeneration: 0,
      });
      expect((await store.getL1SourceState())?.status).toBe("healthy");

      await expect(service.tick()).resolves.toMatchObject({
        signedHeaders: 0,
        errors: [expect.stringContaining("chain_sync_rollback")],
      });
      await expect(store.getL1SourceState()).resolves.toMatchObject({
        status: "quarantined",
        quarantineReason: rollbackQuarantine,
      });
    });

    it("quarantines in the same tick when the chain rolled back below a recorded decision after the snapshot", async () => {
      const {
        service,
        store,
        extend,
        forkBelowHeader,
        rollbackQuarantine,
        inject,
      } = await midTickCommittee("64");
      await expect(service.tick()).resolves.toMatchObject({
        signedHeaders: 1,
        errors: [],
      });
      // The rollback check replays through the authority's current cursor,
      // so a rollback below a decision already recorded is found at once.
      extend([13, 14]);
      inject.afterSnapshot = forkBelowHeader;
      await expect(service.tick()).resolves.toMatchObject({
        signedHeaders: 0,
        errors: [expect.stringContaining("chain_sync_rollback")],
      });
      await expect(store.getL1SourceState()).resolves.toMatchObject({
        status: "quarantined",
        quarantineReason: rollbackQuarantine,
      });
    });
  });

  it("quarantines restart state when the exact L1 authority changes", async () => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "25";
    const signer = await loadDaSigner(`hex:${seed}`);
    const baseConfig = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const config = {
      ...baseConfig,
      l1Source: {
        sourceMode: "local_node" as const,
        authorityNodeId: "fixture-node",
        chainSyncProviderUrl: "chain-sync:fixture:/tmp/state-queue.json",
        chainSyncCursorPath: `${dir}/chain-sync.json`,
        queryProviderUrls: ["fixture:/tmp/state-queue.json"],
      },
    };
    const firstStore = await openJsonCommitteeStore(dir);
    const first = new CommitteeService({
      config,
      store: firstStore,
      stateQueueProvider: { fetchStateQueueNodes: async () => [] },
      payloadSource: failPayloadSource("no payload should be fetched"),
    });
    await first.initialize();
    await expect(first.tick()).resolves.toMatchObject({ scannedHeaders: 0 });
    const persisted = await firstStore.getL1SourceState();
    expect(persisted).toMatchObject({
      status: "healthy",
      authoritySha256: expect.stringMatching(/^[0-9a-f]{64}$/u),
    });

    await firstStore.close();
    const restartedStore = await openJsonCommitteeStore(dir);
    const restarted = new CommitteeService({
      config: {
        ...config,
        l1Source: {
          ...config.l1Source,
          authorityNodeId: "replacement-node",
        },
      },
      store: restartedStore,
      stateQueueProvider: { fetchStateQueueNodes: async () => [] },
      payloadSource: failPayloadSource("authority drift must fail at startup"),
    });
    await expect(restarted.initialize()).rejects.toThrow(
      /does not match configured source mode\/network/u,
    );
    const quarantined = await restartedStore.getL1SourceState();
    expect(quarantined).toMatchObject({
      status: "quarantined",
      quarantineReason: expect.stringContaining(
        "l1_source_configuration_changed",
      ),
      authoritySha256: expect.stringMatching(/^[0-9a-f]{64}$/u),
    });
    expect(quarantined?.authoritySha256).not.toBe(persisted?.authoritySha256);
  });

  it("quarantines a persisted decision when a stale query view loses finality", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "22";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      dir,
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const configured = {
      ...config,
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: configured.daParams,
      signer,
      signerIndex: 0,
    });
    const firstStore = await openJsonCommitteeStore(dir);
    const first = new CommitteeService({
      config: configured,
      store: firstStore,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
      payloadSource: payloadSourceFromBytes(payloadCbor),
      signer,
      signerValidation,
    });
    await first.initialize();
    await expect(first.tick()).resolves.toMatchObject({ signedHeaders: 1 });

    let publishCalls = 0;
    await firstStore.close();
    const stale = new CommitteeService({
      config: configured,
      store: await openJsonCommitteeStore(dir),
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 0 }),
        ],
      }),
      payloadSource: failPayloadSource("stale L1 view must not fetch DA"),
      signer,
      signerValidation,
      coordinator: {
        retryPublishedSignatures: true,
        publishSignature: async () => {
          publishCalls += 1;
          return "posted";
        },
      },
    });
    await stale.initialize();
    await expect(stale.tick()).resolves.toMatchObject({
      scannedHeaders: 0,
      errors: [expect.stringContaining("lost_finality")],
    });
    expect(publishCalls).toBe(0);
  });
};
