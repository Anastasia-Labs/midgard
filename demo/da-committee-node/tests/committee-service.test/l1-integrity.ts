import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { CommitteeService } from "../../src/committee-service.js";
import { type ChainSyncCursor } from "../../src/l1/provider.js";
import { L1SourceIntegrityError } from "../../src/l1/source-integrity.js";
import { loadDaSigner, validateDaSignerMembership } from "../../src/signer.js";
import { createCommitteeTickRunner } from "../../src/tick-runner.js";
import { bytesToHex } from "../../src/utils/hex.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from ".././helpers.js";
import { failPayloadSource, openJsonCommitteeStore } from "./fixtures.js";

export const registerL1IntegrityTests = () => {
  it("fails readiness after a scanner tick failure", async () => {
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
    const store = await openJsonCommitteeStore(dir);
    const service = new CommitteeService({
      config,
      store,
      stateQueueProvider: {
        fetchStateQueueNodes: async () => {
          throw new Error("scanner unavailable");
        },
      },
      payloadSource: failPayloadSource("payload should not be fetched"),
    });
    await service.initialize();

    await expect(service.tick()).rejects.toThrow("scanner unavailable");
    await expect(service.readinessSnapshot()).resolves.toMatchObject({
      ready: false,
      scanner: {
        status: "failed",
        errors: ["scanner unavailable"],
      },
      reasons: [
        "last state queue scanner tick failed",
        "L1 state queue has no durable replay anchor yet: no decision is made until its history is final",
      ],
    });
  });

  describe("L1 observation versus integrity failures", () => {
    /**
     * A local-node committee that has signed nothing yet. `source.failure`,
     * when set, is thrown by the state-queue snapshot read; `consumedCursor`
     * is the durable rollback-feed consumer cursor.
     */
    const observedCommittee = async (seedByte: string) => {
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
      const cursor: ChainSyncCursor = {
        sequence: 0,
        point: {
          network: "Preprod",
          slot: 10,
          blockHash: "11".repeat(32),
          providerSource: "chain-sync:node-a",
          observedAt: "2026-07-28T00:00:00.000Z",
        },
        rollbackGeneration: 0,
      };
      const node = makeObservedNode({
        header,
        headerHash,
        depth: 10,
        slot: 10,
        blockHash: "11".repeat(32),
      });
      const source: {
        failure?: Error;
        consumedCursor?: ChainSyncCursor;
        nowMs: number;
      } = { nowMs: Date.parse("2026-07-28T00:00:00.000Z") };
      const stateQueueProvider = {
        fetchStateQueueNodes: async () => [node],
        fetchStateQueueSnapshot: async () => {
          if (source.failure !== undefined) throw source.failure;
          return {
            nodes: [node],
            confirmedHeaderHash: "00".repeat(28),
            confirmedStateOutRef: `${"00".repeat(32)}#0`,
            observedChainPoint: node.chainPoint,
            chainSyncCursor: cursor,
          };
        },
        currentChainSyncCursor: async () => cursor,
        replayChainSyncEvents: async () => [],
        loadConsumedChainSyncCursor: async () => source.consumedCursor,
        acknowledgeChainSyncCursor: async (next: ChainSyncCursor) => {
          source.consumedCursor = next;
          return { rollbackSinceCapture: false };
        },
      };
      const store = await openJsonCommitteeStore(dir);
      const service = new CommitteeService({
        config: configured,
        store,
        stateQueueProvider,
        payloadSource: payloadSourceFromBytes(payloadCbor),
        signer,
        signerValidation,
        coordinator: { publishSignature: async () => "posted" },
        now: () => new Date(source.nowMs),
      });
      await service.initialize();
      /** Everything a tick could persist about the L1 source and the header. */
      const durable = async () => ({
        l1Source: await store.getL1SourceState(),
        header: await store.getStateQueueHeader(headerHash),
        payload: await store.getDaPayload(headerHash),
      });
      return { service, store, source, headerHash, durable };
    };

    // The five transients observed live, each as its provider raises it.
    const transients = [
      "Kupmios query surfaces are not aligned after 8 reads: Kupo=100:" +
        "33".repeat(32) +
        ", Ogmios=101:" +
        "44".repeat(32),
      "Ogmios replay tip height is invalid",
      "local_node query surface query:node-a:0 changed chain point while its snapshot was read",
      "Ogmios chain-sync request timed out",
      "Kupmios chain point changed while deriving block confirmations",
    ];

    it.each(transients)(
      "fails only the tick on a transient (%s) and resumes on the next healthy tick",
      async (message) => {
        const { service, source, durable } = await observedCommittee("41");

        // Before any decision exists.
        source.failure = new Error(message);
        const beforeFirst = await durable();
        await expect(service.tick()).rejects.toThrow(message);
        expect(await durable()).toEqual(beforeFirst);
        expect(service.latestL1View()).toBeUndefined();

        source.failure = undefined;
        await expect(service.tick()).resolves.toMatchObject({
          scannedHeaders: 1,
          signedHeaders: 1,
          errors: [],
        });
        const view = service.latestL1View();
        expect(view).toBeDefined();

        // With a persisted decision the transient still quarantines nothing.
        source.failure = new Error(message);
        source.nowMs += 60_000;
        const beforeSecond = await durable();
        expect(beforeSecond.l1Source).toMatchObject({
          status: "healthy",
          observations: [{ hasPersistedDecision: true }],
        });
        await expect(service.tick()).rejects.toThrow(message);
        const afterSecond = await durable();
        expect(afterSecond).toEqual(beforeSecond);
        expect(afterSecond.l1Source?.status).toBe("healthy");
        expect(afterSecond.header?.status).not.toBe("conflicted");
        expect(afterSecond.payload?.validationStatus).not.toBe("conflicted");
        // No fresh L1 view was accepted, so its age keeps growing.
        expect(service.latestL1View()).toBe(view);
        await expect(service.readinessSnapshot()).resolves.toMatchObject({
          scanner: { status: "failed", errors: [message] },
        });

        source.failure = undefined;
        await expect(service.tick()).resolves.toMatchObject({
          scannedHeaders: 1,
          signedHeaders: 0,
          errors: [],
        });
        await expect(service.readinessSnapshot()).resolves.toMatchObject({
          scanner: { status: "ok", errors: [] },
        });
        expect(service.latestL1View()?.observedAtMs).toBe(source.nowMs);
        expect((await durable()).l1Source?.status).toBe("healthy");
      },
    );

    it("quarantines a persisted decision on an L1 source integrity failure", async () => {
      const { service, source, headerHash, durable } =
        await observedCommittee("42");
      await expect(service.tick()).resolves.toMatchObject({
        signedHeaders: 1,
      });

      source.failure = new L1SourceIntegrityError(
        "state queue root provider disagreement in local_node mode",
      );
      await expect(service.tick()).rejects.toThrow(
        /state queue root provider disagreement/u,
      );
      const after = await durable();
      expect(after.l1Source).toMatchObject({
        status: "quarantined",
        quarantineReason:
          "l1_source_integrity_failed: state queue root provider disagreement in local_node mode",
      });
      expect(after.header).toMatchObject({
        headerHash,
        status: "conflicted",
        validationErrors: [expect.stringContaining("l1_source_quarantined")],
      });

      // The quarantine is persisted: a healthy source no longer clears it.
      source.failure = undefined;
      await expect(service.tick()).resolves.toMatchObject({
        scannedHeaders: 0,
        signedHeaders: 0,
        errors: [expect.stringContaining("l1_source_integrity_failed")],
      });
    });

    it("quarantines a persisted decision whose rollback feed lost its durable consumer cursor", async () => {
      const { service, source, durable } = await observedCommittee("43");
      await expect(service.tick()).resolves.toMatchObject({
        signedHeaders: 1,
      });
      expect(source.consumedCursor).toBeDefined();

      source.consumedCursor = undefined;
      await expect(service.tick()).rejects.toThrow(
        /lack a durable chain-sync consumer cursor/u,
      );
      await expect(durable()).resolves.toMatchObject({
        l1Source: {
          status: "quarantined",
          quarantineReason: expect.stringContaining(
            "l1_source_rollback_feed_failed: persisted L1 decisions lack a durable chain-sync consumer cursor",
          ),
        },
        header: { status: "conflicted" },
      });
    });

    it("lets a sustained observation failure reach the L1-view deadline without quarantining or exiting", async () => {
      const { service, source, durable } = await observedCommittee("44");
      const fatalMs = 240_000;
      const runner = createCommitteeTickRunner({
        tick: async () => service.tick(),
        runAvailabilityResponse: async () => undefined,
        runRetention: async () => undefined,
        latestL1View: () => service.latestL1View(),
        latestL1ProgressAtMs: () => service.latestL1ProgressAtMs(),
        setRetentionReadiness: () => undefined,
        l1ViewFatalMs: fatalMs,
        startedAtMs: source.nowMs,
        nowMs: () => source.nowMs,
        write: () => undefined,
      });
      await runner.runTick();
      expect(service.latestL1View()?.observedAtMs).toBe(source.nowMs);

      source.failure = new Error("Ogmios chain-sync request timed out");
      for (let elapsed = 0; elapsed < fatalMs; elapsed += 60_000) {
        source.nowMs += 60_000;
        await runner.runTick();
        expect(runner.liveness().l1ViewUnavailable).toBeUndefined();
      }
      source.nowMs += 1;
      await runner.runTick();
      expect(runner.liveness().l1ViewUnavailable).toBeDefined();
      expect((await durable()).l1Source?.status).toBe("healthy");
    });
  });
};
