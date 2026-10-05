import { blake2b } from "@noble/hashes/blake2.js";
import { expect, it } from "vitest";

import { CommitteeService } from "../../src/committee-service.js";
import { type DaPayloadSource } from "../../src/da/source.js";
import { loadDaSigner, validateDaSignerMembership } from "../../src/signer.js";
import { PostgresCommitteeStore } from "../../src/store/postgres.js";
import { bytesToHex } from "../../src/utils/hex.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from ".././helpers.js";
import { withFinalSnapshot } from ".././helpers/final-snapshot.js";
import { terminateInstanceLockSessions } from ".././helpers/postgres-database.js";
import {
  attestedDaStatus,
  expectedCommitment,
  failPayloadSource,
  openJsonCommitteeStore,
  postgresDatabases,
  runAnchorAheadOfObservations,
} from "./fixtures.js";

export const registerSigningTests = () => {
  it("fetches, verifies, signs, and persists one finalized unattested header", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "01";
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
    const store = await openJsonCommitteeStore(dir);
    const payloadSource = payloadSourceFromBytes(payloadCbor);
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
      payloadSource,
      signer,
      signerValidation,
    });
    await service.initialize();
    await expect(service.readinessSnapshot()).resolves.toMatchObject({
      ready: false,
      deployment: {
        configuredFingerprint: configWithDaHash.deploymentFingerprint,
        storeFingerprint: configWithDaHash.deploymentFingerprint,
        storeMatchesConfigured: true,
      },
      scanner: { status: "not_started" },
      reasons: ["state queue scanner has not completed a tick"],
    });
    const result = await service.tick();
    expect(result).toMatchObject({ scannedHeaders: 1, signedHeaders: 1 });
    await expect(
      store.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment(
          configWithDaHash,
          headerHash,
          payloadCbor,
        ).commitmentDigest,
        signerIndex: 0,
      }),
    ).resolves.toMatchObject({ headerHash, signerIndex: 0 });
    await expect(
      service.readinessSnapshot({ localPeerId: "committee-peer" }),
    ).resolves.toMatchObject({
      ready: true,
      deployment: {
        configuredFingerprint: configWithDaHash.deploymentFingerprint,
        storeFingerprint: configWithDaHash.deploymentFingerprint,
        storeMatchesConfigured: true,
      },
      peer: {
        localPeerId: "committee-peer",
        l1SubmitterPreflight: { status: "not_required" },
      },
      scanner: { status: "ok", scannedHeaders: 1, signedHeaders: 1 },
      counts: {
        discoveredHeaders: 1,
        missingPayloads: 0,
        verifiedPayloads: 1,
        signatures: 1,
      },
      reasons: [],
    });
    // A completed retention cycle is `ok` whether or not the opt-in deadline
    // alert flagged payloads: every merged payload ages into the threshold on
    // its way to pruning, so the alert count is shown but never blocks.
    for (const alerting of [0, 1]) {
      await expect(
        service.readinessSnapshot({
          localPeerId: "committee-peer",
          retention: {
            status: "ok",
            checkedAt: "2026-08-29T00:00:00.000Z",
            scanned: 1,
            retained: 1,
            prunable: 0,
            alerting,
          },
        }),
      ).resolves.toMatchObject({
        ready: true,
        retention: { status: "ok", alerting },
        reasons: [],
      });
    }
  });

  it("durably begins signature effects before publish and replays one deterministic effect immediately after an acknowledgement crash", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "61";
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
    const completeDecisionEffect =
      firstStore.completeDecisionEffect.bind(firstStore);
    let failAcknowledgement = true;
    firstStore.completeDecisionEffect = async (args) => {
      if (failAcknowledgement) {
        failAcknowledgement = false;
        throw new Error("simulated crash after publish before durable ack");
      }
      await completeDecisionEffect(args);
    };
    const publishedWitnesses: string[] = [];
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
      coordinator: {
        publishSignature: async (signature) => {
          publishedWitnesses.push(signature.signatureWitness);
          await expect(
            firstStore.listDecisionOutbox(headerHash),
          ).resolves.toMatchObject([
            {
              effectKind: "signature_publish",
              status: "pending",
              attemptCount: 1,
            },
          ]);
          await expect(firstStore.getL1SourceState()).resolves.toMatchObject({
            status: "healthy",
            observations: [{ headerHash, hasPersistedDecision: true }],
          });
          await expect(
            firstStore.getDaSignature({
              headerHash,
              availabilityCommitmentDigest: expectedCommitment(
                configured,
                headerHash,
                payloadCbor,
              ).commitmentDigest,
              signerIndex: 0,
            }),
          ).resolves.toMatchObject({
            headerHash,
            broadcastStatus: "local",
          });
          return "posted";
        },
      },
    });
    await first.initialize();
    await expect(first.tick()).resolves.toMatchObject({
      errors: [
        expect.stringContaining(
          "simulated crash after publish before durable ack",
        ),
      ],
    });
    await expect(
      firstStore.listDecisionOutbox(headerHash),
    ).resolves.toMatchObject([
      {
        effectKind: "signature_publish",
        status: "pending",
        attemptCount: 1,
      },
    ]);

    await firstStore.close();
    const restartedStore = await openJsonCommitteeStore(dir);
    const restarted = new CommitteeService({
      config: configured,
      store: restartedStore,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
      payloadSource: failPayloadSource(
        "durable local signature must suppress payload refetch",
      ),
      signer,
      signerValidation,
      coordinator: {
        publishSignature: async (signature) => {
          publishedWitnesses.push(signature.signatureWitness);
          return "posted";
        },
      },
    });
    await restarted.initialize();
    await expect(restarted.tick()).resolves.toMatchObject({
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [],
    });
    expect(publishedWitnesses).toHaveLength(2);
    expect(new Set(publishedWitnesses)).toHaveLength(1);
    await expect(
      restartedStore.listDecisionOutbox(headerHash),
    ).resolves.toMatchObject([
      {
        effectKind: "signature_publish",
        status: "published",
        attemptCount: 2,
      },
    ]);
  });

  it("republishes at once, on a Postgres store, the signature a crashed process left pending mid-publish", async () => {
    const dir = await tempDir();
    const database = await postgresDatabases.create();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "64";
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
    const stateQueueProvider = withFinalSnapshot({
      fetchStateQueueNodes: async () => [
        makeObservedNode({ header, headerHash, depth: 10 }),
      ],
    });
    const publishedWitnesses: string[] = [];
    let lockLost!: () => void;
    const lost = new Promise<void>((resolve) => {
      lockLost = resolve;
    });
    const crashedStore = await PostgresCommitteeStore.open(database.url, {
      onInstanceLockSuspended: () => lockLost(), // the session ending is the crash
    });
    let crashedStoreOpen = true;
    let restartedStore: PostgresCommitteeStore | undefined;
    try {
      let enteredPublish!: () => void;
      const entered = new Promise<void>((resolve) => {
        enteredPublish = resolve;
      });
      const crashed = new CommitteeService({
        config: configured,
        store: crashedStore,
        stateQueueProvider,
        payloadSource: payloadSourceFromBytes(payloadCbor),
        signer,
        signerValidation,
        coordinator: {
          // The process dies inside the publish: it never returns.
          publishSignature: async (signature) => {
            publishedWitnesses.push(signature.signatureWitness);
            enteredPublish();
            return new Promise<never>(() => undefined);
          },
        },
      });
      await crashed.initialize();
      void crashed.tick().catch(() => undefined);
      await entered;
      await expect(
        crashedStore.listDecisionOutbox(headerHash),
      ).resolves.toMatchObject([
        { effectKind: "signature_publish", status: "pending", attemptCount: 1 },
      ]);
      const terminated = await terminateInstanceLockSessions(database);
      if (terminated > 0) {
        await lost;
      }
      crashedStoreOpen = false;
      await crashedStore.close();

      restartedStore = await PostgresCommitteeStore.open(database.url);
      const restarted = new CommitteeService({
        config: configured,
        store: restartedStore,
        stateQueueProvider,
        payloadSource: failPayloadSource(
          "durable local signature must suppress payload refetch",
        ),
        signer,
        signerValidation,
        coordinator: {
          publishSignature: async (signature) => {
            publishedWitnesses.push(signature.signatureWitness);
            return "posted";
          },
        },
      });
      await restarted.initialize();
      await expect(restarted.tick()).resolves.toMatchObject({
        signedHeaders: 0,
        skippedHeaders: 1,
        errors: [],
      });
      expect(publishedWitnesses).toHaveLength(2);
      expect(new Set(publishedWitnesses)).toHaveLength(1);
      await expect(
        restartedStore.listDecisionOutbox(headerHash),
      ).resolves.toMatchObject([
        {
          effectKind: "signature_publish",
          status: "published",
          attemptCount: 2,
        },
      ]);
      await expect(restarted.readinessSnapshot()).resolves.toMatchObject({
        scanner: { status: "ok", errors: [] },
      });
      expect(terminated).toBe(1);
    } finally {
      if (crashedStoreOpen) {
        await crashedStore.close();
      }
      await restartedStore?.close();
    }
  });

  it("defers, without failing the tick, an external effect whose attempt another worker still runs", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "62";
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
    const store = await openJsonCommitteeStore(dir);
    let publishCalls = 0;
    let enteredPublish!: () => void;
    let releasePublish!: () => void;
    const entered = new Promise<void>((resolve) => {
      enteredPublish = resolve;
    });
    const released = new Promise<void>((resolve) => {
      releasePublish = resolve;
    });
    const coordinator = {
      publishSignature: async () => {
        publishCalls += 1;
        enteredPublish();
        await released;
        return "posted" as const;
      },
    };
    const service = () =>
      new CommitteeService({
        config: configured,
        store,
        stateQueueProvider: withFinalSnapshot({
          fetchStateQueueNodes: async () => [
            makeObservedNode({ header, headerHash, depth: 10 }),
          ],
        }),
        payloadSource: payloadSourceFromBytes(payloadCbor),
        signer,
        signerValidation,
        coordinator,
      });
    const first = service();
    const second = service();
    await first.initialize();
    await second.initialize();
    const firstTick = first.tick();
    await entered;
    await expect(second.tick()).resolves.toMatchObject({
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [],
    });
    expect(publishCalls).toBe(1);
    await expect(store.listDecisionOutbox(headerHash)).resolves.toMatchObject([
      { effectKind: "signature_publish", status: "pending", attemptCount: 1 },
    ]);
    releasePublish();
    await expect(firstTick).resolves.toMatchObject({
      signedHeaders: 1,
      errors: [],
    });
    expect(publishCalls).toBe(1);
  });

  it("processes every other header of a tick in which one effect is deferred, and stays ready", async () => {
    const dir = await tempDir();
    const fixtures = [await makePayloadFixture(3), await makePayloadFixture(2)];
    const seed = "00".repeat(31) + "63";
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
    const store = await openJsonCommitteeStore(dir);
    const nodes = fixtures.map(({ header, headerHash }, index) =>
      makeObservedNode({
        header,
        headerHash,
        depth: 10,
        outRef: `${(0xab + index).toString(16).repeat(32)}#0`,
      }),
    );
    const payloadSource: DaPayloadSource = {
      fetchPayloadCandidates: async (headerHash) => {
        const fixture = fixtures.find(
          (candidate) => candidate.headerHash === headerHash,
        );
        if (fixture === undefined) {
          throw new Error(`no payload fixture for ${headerHash}`);
        }
        return {
          ok: true,
          candidates: [
            {
              sourcePeerId: "fixture-peer",
              payloadCbor: fixture.payloadCbor,
              payloadSchemaVersion: 1,
            },
          ],
          attempts: [],
        };
      },
    };
    const published: string[] = [];
    let blockedHeader: string | undefined;
    let enteredPublish!: () => void;
    let releasePublish!: () => void;
    const entered = new Promise<void>((resolve) => {
      enteredPublish = resolve;
    });
    const released = new Promise<void>((resolve) => {
      releasePublish = resolve;
    });
    const events: Record<string, unknown>[] = [];
    const service = () =>
      new CommitteeService({
        config: configured,
        store,
        stateQueueProvider: withFinalSnapshot({
          fetchStateQueueNodes: async () => nodes,
        }),
        payloadSource,
        signer,
        signerValidation,
        coordinator: {
          publishSignature: async (signature) => {
            published.push(signature.headerHash);
            if (blockedHeader === undefined) {
              blockedHeader = signature.headerHash;
              enteredPublish();
              await released;
            }
            return "posted" as const;
          },
        },
        writeEvent: (line) => {
          events.push(JSON.parse(line) as Record<string, unknown>);
        },
      });
    const first = service();
    const second = service();
    await first.initialize();
    await second.initialize();
    const firstTick = first.tick();
    await entered;
    const otherHeader = fixtures
      .map(({ headerHash }) => headerHash)
      .find((headerHash) => headerHash !== blockedHeader)!;

    await expect(second.tick()).resolves.toMatchObject({
      scannedHeaders: 2,
      signedHeaders: 1,
      errors: [],
    });
    expect(published).toEqual([blockedHeader, otherHeader]);
    expect(events).toContainEqual(
      expect.objectContaining({
        event: "decision_effect_deferred",
        effectKind: "signature_publish",
        headerHash: blockedHeader,
      }),
    );
    const readiness = await second.readinessSnapshot();
    expect(readiness.scanner).toMatchObject({ status: "ok", errors: [] });
    expect(readiness.reasons).not.toContainEqual(
      expect.stringMatching(/tick (failed|completed with errors)/u),
    );
    await expect(store.listDecisionOutbox(otherHeader)).resolves.toMatchObject([
      { effectKind: "signature_publish", status: "published" },
    ]);
    await expect(
      store.listDecisionOutbox(blockedHeader!),
    ).resolves.toMatchObject([
      { effectKind: "signature_publish", status: "pending", attemptCount: 1 },
    ]);

    releasePublish();
    await expect(firstTick).resolves.toMatchObject({ errors: [] });
    expect(published).toEqual([blockedHeader, otherHeader]);
    await expect(
      store.listDecisionOutbox(blockedHeader!),
    ).resolves.toMatchObject([
      { effectKind: "signature_publish", status: "published", attemptCount: 1 },
    ]);
  });

  it("quarantines an attested replacement of a signed header that authenticated replay from the durable anchor does not explain", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = "00".repeat(31) + "31";
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
    const firstOutRef = `${"ab".repeat(32)}#0`;
    const attestedOutRef = `${"ac".repeat(32)}#1`;
    let sourceView: "unattested" | "attested" = "unattested";
    const store = await openJsonCommitteeStore(dir);
    const service = new CommitteeService({
      config: configured,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          sourceView === "attested"
            ? makeObservedNode({
                header,
                headerHash,
                daAttestation: attestedDaStatus(),
                outRef: attestedOutRef,
                slot: 2,
                blockHash: "de".repeat(32),
                depth: 10,
              })
            : makeObservedNode({
                header,
                headerHash,
                outRef: firstOutRef,
                slot: 1,
                depth: 10,
              }),
        ],
      }),
      payloadSource: payloadSourceFromBytes(payloadCbor),
      signer,
      signerValidation,
      coordinator: {
        publishSignature: async () => "posted",
      },
    });
    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      errors: [],
    });

    // The scan of the replaced output succeeds from an anchor already past
    // it, but no authenticated step leads from the signed output to it.
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      stateQueueReplayAnchor: {
        queue: [
          { headerHash: null, outRef: `${"00".repeat(32)}#0` },
          { headerHash, outRef: firstOutRef },
        ],
      },
    });
    await runAnchorAheadOfObservations(store, [
      { headerHash: null, outRef: `${"00".repeat(32)}#0` },
      { headerHash, outRef: attestedOutRef },
    ]);
    sourceView = "attested";
    await expect(service.tick()).resolves.toMatchObject({
      signedHeaders: 0,
      errors: [expect.stringContaining("decision_forked")],
    });
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      status: "quarantined",
      quarantineReason: expect.stringContaining("decision_forked"),
      observations: [
        {
          headerHash,
          stateQueueOutRef: firstOutRef,
          stateQueueStatus: "unattested",
          hasPersistedDecision: true,
        },
      ],
    });
  });
};
