import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  CommitteeService,
  DA_PARAMS_STARTUP_RETRY,
} from "../src/committee-service.js";
import type {
  DaAttestationChainReader,
  OnChainDaParams,
} from "../src/l1/da-attestation-reader.js";
import { L1SourceIntegrityError } from "../src/l1/source-integrity.js";
import { loadDaSigner, validateDaSignerMembership } from "../src/signer.js";
import { JsonFileCommitteeStore } from "../src/store.js";
import { bytesToHex } from "../src/utils/hex.js";
import { minimalConfig, tempDir } from "./helpers.js";

describe("committee node startup DA params checks", () => {
  const startup = async (
    fetchDaParams: (
      committeeSignersHash: string,
    ) => ReturnType<DaAttestationChainReader["fetchDaParams"]>,
  ) => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const cosigner = await loadDaSigner(`hex:${"00".repeat(31)}02`);
    // Q63 (F04 §4) floors the governed thresholds at two, so the smallest
    // representable committee has two sorted-unique members. The signer's own
    // index follows that ordering rather than being assumed to be zero.
    const committeeKeys = [signer.publicKeyHex, cosigner.publicKeyHex].sort();
    const committeeHex = committeeKeys.join("");
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const config = {
      ...minimalConfig({
        dir,
        manifestPath: `${dir}/manifest.json`,
        deploymentInfoPath: `${dir}/deployment.json`,
        signerSeed: seed,
        signerPublicKey: signer.publicKeyHex,
      }),
      daParams: {
        committeeHex,
        committeeSignersHash,
        threshold: 2,
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: config.daParams,
      signer,
      signerIndex: committeeKeys.indexOf(signer.publicKeyHex),
    });
    const store = await JsonFileCommitteeStore.open(dir);
    const events: string[] = [];
    const reads = { count: 0 };
    const service = new CommitteeService({
      config,
      store,
      stateQueueProvider: { fetchStateQueueNodes: async () => [] },
      payloadSource: {
        fetchPayloadCandidates: async () => {
          throw new Error("payload source should not be used during startup");
        },
      },
      signer,
      signerValidation,
      daChainReader: {
        fetchDaParams: async () => {
          reads.count += 1;
          return fetchDaParams(committeeSignersHash);
        },
        fetchDaAttestationCandidates: async () => [],
      } satisfies DaAttestationChainReader,
      daParamsStartupRetry: { attempts: 5, initialDelayMs: 1, maxDelayMs: 5 },
      writeEvent: (line) => events.push(line),
    });
    return { service, store, reads, events, committeeHex };
  };

  const liveParams = (
    committeeHex: string,
    committeeSignersHash: string,
  ): OnChainDaParams => ({
    outRef: "tx#0",
    committeeHex,
    committeeSignersHash,
    threshold: 2,
    ownerCount: 2,
    updateThreshold: 2,
    rawDatum: {} as never,
  });

  it("fails closed and quarantines when live DA params do not match config", async () => {
    const { service, store } = await startup(async (committeeSignersHash) => ({
      outRef: "tx#0",
      // Floor-compliant, but a different committee than the config names —
      // the mismatch is the whole point of the test.
      committeeHex: "fe".repeat(32) + "ff".repeat(32),
      committeeSignersHash,
      threshold: 2,
      ownerCount: 2,
      updateThreshold: 2,
      rawDatum: {} as never,
    }));
    await expect(service.initialize()).rejects.toThrow(/on-chain DA committee/);
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      status: "quarantined",
      quarantineReason:
        "l1_da_params_mismatch: on-chain DA committee does not match committee node config",
    });
  });

  it("fails startup without quarantining when live DA params cannot be read, after doubling delays up to the cap", async () => {
    const { service, store, reads, events } = await startup(async () => {
      throw new Error("Kupo request timed out");
    });
    await expect(service.initialize()).rejects.toThrow(
      "Kupo request timed out",
    );
    expect(reads.count).toBe(5);
    // Doubling, then capped at maxDelayMs: 1, 2, 4, 5 (linear would wait
    // 1, 2, 3, 4; uncapped, 1, 2, 4, 8).
    expect(
      events.map(
        (line) => (JSON.parse(line) as { readonly delayMs: number }).delayMs,
      ),
    ).toEqual([1, 2, 4, 5]);
    expect((await store.getL1SourceState())?.status).not.toBe("quarantined");
  });

  it("starts once a transient DA params read failure clears", async () => {
    let committeeHex = "";
    const { service, store, reads, events, ...rest } = await startup(
      async (committeeSignersHash) => {
        if (reads.count < 3) throw new Error("Kupo request timed out");
        return liveParams(committeeHex, committeeSignersHash);
      },
    );
    committeeHex = rest.committeeHex;
    await service.initialize();
    expect(reads.count).toBe(3);
    expect(events.map((line) => JSON.parse(line) as unknown)).toEqual([
      expect.objectContaining({
        event: "da_params_startup_read_retry",
        attempt: 1,
        delayMs: 1,
      }),
      expect.objectContaining({
        event: "da_params_startup_read_retry",
        attempt: 2,
        delayMs: 2,
      }),
    ]);
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      status: "healthy",
    });
  });

  it("never retries an integrity failure of the DA params read", async () => {
    const { service, store, reads } = await startup(async () => {
      throw new L1SourceIntegrityError("DA params chain point moved");
    });
    await expect(service.initialize()).rejects.toThrow(
      "DA params chain point moved",
    );
    expect(reads.count).toBe(1);
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      status: "quarantined",
    });
  });

  it("refuses stale deployment state before reading, and never retries it", async () => {
    const { service, store, reads, events } = await startup(async () => {
      throw new Error("Kupo request timed out");
    });
    await store.initDeployment({
      marker: makeDeploymentMarker("44".repeat(32)),
      manifestSha256: "55".repeat(32),
      contractDeploymentInfoSha256: "66".repeat(32),
      manifestRaw: "{}",
    });
    await expect(service.initialize()).rejects.toThrow(
      /stale_deployment_state_requires_fresh_redeploy/u,
    );
    expect(reads.count).toBe(0);
    expect(events).toEqual([]);
  });

  it("retries eight reads in production, the later waits at the 30 s cap", () => {
    // Waits of 1, 2, 4, 8, 16, then 30 and 30 s (the doubling passes the cap
    // at the sixth), about 90 s in all.
    expect(DA_PARAMS_STARTUP_RETRY).toEqual({
      attempts: 8,
      initialDelayMs: 1_000,
      maxDelayMs: 30_000,
    });
  });
});
