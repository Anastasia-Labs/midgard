import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { CommitteeService } from "../../src/committee-service.js";
import { STORE_INTEGRITY } from "../../src/committee-service.l1-tick.js";
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
import { fakeL1Source } from ".././helpers/fake-l1-source.js";
import { failPayloadSource, openTestCommitteeStore } from "./fixtures.js";

export const registerL1IntegrityTests = () => {
  it("fails readiness after a tick failure", async () => {
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
      l1: fakeL1Source({
        fetchStateQueueNodes: async () => {
          throw new Error("follower facts unavailable");
        },
      }),
      payloadSource: failPayloadSource("payload should not be fetched"),
    });
    await service.initialize();

    await expect(service.tick()).rejects.toThrow("follower facts unavailable");
    await expect(service.readinessSnapshot()).resolves.toMatchObject({
      ready: false,
      scanner: {
        status: "failed",
        errors: ["follower facts unavailable"],
      },
      reasons: ["last state queue scanner tick failed"],
    });
  });

  describe("L1 read failures versus store integrity", () => {
    /**
     * A committee that has signed nothing yet. `source.failure`, when set,
     * is thrown by the follower's view read; `source.daAttestation`
     * replaces the node's DA status at its unchanged output.
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
        nowMs: number;
      } = { nowMs: Date.parse("2026-07-28T00:00:00.000Z") };
      const store = await openTestCommitteeStore();
      const service = new CommitteeService({
        config: configured,
        store,
        l1: fakeL1Source({
          fetchStateQueueNodes: async () => {
            if (source.failure !== undefined) throw source.failure;
            return [
              {
                ...node,
                daAttestation: source.daAttestation ?? node.daAttestation,
              },
            ];
          },
        }),
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
        signatures: await store.listDaSignatures(headerHash),
      });
      return { service, store, source, headerHash, durable, node };
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
      "holds every decision on a stored Unattested record the facts show $kind at the same output, and stays held",
      async ({ status }) => {
        const { service, source, durable, node } =
          await observedCommittee("45");
        await expect(service.tick()).resolves.toMatchObject({
          signedHeaders: 1,
          errors: [],
        });
        await expect(service.tick()).resolves.toMatchObject({
          signedHeaders: 0,
          errors: [],
        });
        const before = await durable();
        expect(before.signatures).toHaveLength(1);

        source.daAttestation = status;
        const reason = `${STORE_INTEGRITY}: state-queue status at unchanged output ${node.outRef}: stored=Unattested, observed=${SDK.daAvailabilityStateQueueStatusIdentity(status)}`;
        await expect(service.tick()).resolves.toMatchObject({
          signedHeaders: 0,
          held: [reason],
        });
        // Nothing the tick could persist moved, and the process is up.
        expect(await durable()).toEqual(before);
        const snapshot = await service.readinessSnapshot();
        expect(snapshot.ready).toBe(false);
        expect(snapshot.reasons).toContain(reason);

        // The facts agreeing again do not clear it: only an operator does.
        source.daAttestation = undefined;
        await expect(service.tick()).resolves.toMatchObject({
          held: [reason],
        });
        expect((await service.readinessSnapshot()).reasons).toContain(reason);
      },
    );

    it("fails only the tick on a read failure and resumes on the next healthy tick", async () => {
      const message = "follower store connection reset";
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

      // With a persisted decision the failure still changes nothing durable.
      source.failure = new Error(message);
      source.nowMs += 60_000;
      const beforeSecond = await durable();
      await expect(service.tick()).rejects.toThrow(message);
      expect(await durable()).toEqual(beforeSecond);
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
    });

    it("lets a sustained read failure reach the L1-view deadline as a readiness reason, without exiting", async () => {
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
      const before = await durable();

      source.failure = new Error("follower store connection reset");
      for (let elapsed = 0; elapsed < fatalMs; elapsed += 60_000) {
        source.nowMs += 60_000;
        await runner.runTick();
        expect(runner.liveness().l1ViewUnavailable).toBeUndefined();
      }
      source.nowMs += 1;
      await runner.runTick();
      expect(runner.liveness().l1ViewUnavailable).toBeDefined();
      expect(await durable()).toEqual(before);
    });
  });
};
