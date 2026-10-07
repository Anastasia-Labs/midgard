import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { CommitteeService } from "../../src/committee-service.js";
import { StoreBackedDaAttestationProtocol } from "../../src/da/libp2p/attestations.js";
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
import {
  commitmentAuthority,
  failPayloadSource,
  openTestCommitteeStore,
} from "./fixtures.js";

export const registerL1IntegrityTests = () => {
  it("fails readiness after a scanner tick failure", async () => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const store = await openTestCommitteeStore();
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
        daAttestation?: SDK.DaAvailabilityStateQueueStatus;
        consumedCursor?: ChainSyncCursor;
        nowMs: number;
      } = { nowMs: Date.parse("2026-07-28T00:00:00.000Z") };
      const stateQueueProvider = {
        fetchStateQueueNodes: async () => [node],
        fetchStateQueueSnapshot: async () => {
          if (source.failure !== undefined) throw source.failure;
          return {
            nodes: [
              {
                ...node,
                daAttestation: source.daAttestation ?? node.daAttestation,
              },
            ],
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
      const store = await openTestCommitteeStore();
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
      return {
        service,
        store,
        source,
        headerHash,
        durable,
        node,
        config: configured,
        signerValidation,
      };
    };

    it.each([
      {
        kind: "Attested",
        status: { Attested: { commitment_hash: "33".repeat(32) } },
      },
      {
        kind: "Challenged",
        status: {
          Challenged: {
            commitment_hash: "33".repeat(32),
            challenge_asset_name: "44".repeat(32),
          },
        },
      },
      {
        kind: "Published",
        status: { Published: { terminal_commitment: "55".repeat(32) } },
      },
    ] satisfies readonly {
      kind: string;
      status: SDK.DaAvailabilityStateQueueStatus;
    }[])(
      "surfaces an observed $kind disagreement with a signed Unattested output to readiness and peers",
      async ({ status }) => {
        const {
          service,
          store,
          source,
          headerHash,
          durable,
          node,
          config,
          signerValidation,
        } = await observedCommittee("45");
        await expect(service.tick()).resolves.toMatchObject({
          signedHeaders: 1,
          errors: [],
        });
        // Identical repeated chain observation agrees with the durable decision.
        await expect(service.tick()).resolves.toMatchObject({
          signedHeaders: 0,
          errors: [],
        });
        const before = await durable();
        const signatures = await store.listDaSignatures(headerHash);
        expect(signatures).toHaveLength(1);
        const protocol = new StoreBackedDaAttestationProtocol({
          deploymentFingerprint: config.deploymentFingerprint,
          localPeerId: "local",
          committeeValidation: signerValidation,
          availabilityCommitmentAuthority: commitmentAuthority(config),
          store,
        });
        const peerRequest = { record: signatures[0]!, sourcePeerId: "peer-a" };
        await expect(protocol.acceptAttestation(peerRequest)).resolves.toEqual({
          status: "accepted",
        });
        await expect(
          protocol.attestationsByHeader({
            deploymentFingerprint: config.deploymentFingerprint,
            headerHash,
          }),
        ).resolves.toHaveLength(1);

        source.daAttestation = status;
        const message = `state-queue status disagreement at unchanged output ${node.outRef}: stored=Unattested, observed=${SDK.daAvailabilityStateQueueStatusIdentity(status)}`;
        const result = service.tick();
        await expect(result).rejects.toBeInstanceOf(L1SourceIntegrityError);
        await expect(result).rejects.toThrow(message);
        const reason = `l1_source_integrity_failed: ${message}`;
        const after = await durable();
        expect(after.l1Source).toMatchObject({
          status: "quarantined",
          quarantineReason: reason,
        });
        expect(after.header).toMatchObject({
          status: "conflicted",
          validationErrors: [`l1_source_quarantined:${reason}`],
        });
        expect(after.payload).toEqual(before.payload);
        await expect(service.readinessSnapshot()).resolves.toMatchObject({
          ready: false,
          l1Source: { status: "quarantined", quarantineReason: reason },
          scanner: { status: "failed", errors: [message] },
        });
        await expect(protocol.acceptAttestation(peerRequest)).resolves.toEqual({
          status: "rejected",
          reason: "L1 source is quarantined",
        });
        await expect(
          protocol.attestationsByHeader({
            deploymentFingerprint: config.deploymentFingerprint,
            headerHash,
          }),
        ).resolves.toEqual([]);
        await expect(store.listDaSignatures(headerHash)).resolves.toMatchObject(
          [{ broadcastStatus: "post_failed" }],
        );
        await expect(
          store.listDecisionOutbox(headerHash),
        ).resolves.toMatchObject([
          { status: "failed", lastError: `l1_source_quarantined:${reason}` },
        ]);

        // A peer's valid signature or a healthy reread cannot clear a durable hold.
        source.daAttestation = undefined;
        await expect(service.tick()).resolves.toEqual({
          scannedHeaders: 0,
          signedHeaders: 0,
          reconciledHeaders: 0,
          skippedHeaders: 0,
          payloadFetches: [],
          errors: [`L1 source quarantined: ${reason}`],
        });
      },
    );

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
