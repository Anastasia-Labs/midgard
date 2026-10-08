import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { CommitteeService } from "../src/committee-service.js";
import {
  L1_DA_PARAMS_MISMATCH,
  L1_DA_PARAMS_UNAVAILABLE,
} from "../src/committee-service.l1-tick.js";
import type {
  DaAttestationChainReader,
  OnChainDaParams,
} from "../src/l1/da-attestation-reader.js";
import { loadDaSigner, validateDaSignerMembership } from "../src/signer.js";
import { bytesToHex } from "../src/utils/hex.js";
import { minimalConfig, tempDir } from "./helpers.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";
import { fakeL1Source } from "./helpers/fake-l1-source.js";

describe("the on-chain DA params, checked every tick", () => {
  const committee = async () => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const cosigner = await loadDaSigner(`hex:${"00".repeat(31)}02`);
    // The governed thresholds are floored at two, so the smallest committee
    // has two sorted-unique members; the signer's index follows that order.
    const committeeKeys = [signer.publicKeyHex, cosigner.publicKeyHex].sort();
    const committeeHex = committeeKeys.join("");
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(committeeHex, "hex"), { dkLen: 32 }),
    );
    const config = {
      ...minimalConfig({
        manifestPath: `${dir}/manifest.json`,
        deploymentInfoPath: `${dir}/deployment.json`,
        signerSeed: seed,
        signerPublicKey: signer.publicKeyHex,
      }),
      daParams: { committeeHex, committeeSignersHash, threshold: 2 },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: config.daParams,
      signer,
      signerIndex: committeeKeys.indexOf(signer.publicKeyHex),
    });
    const live: OnChainDaParams = {
      outRef: "tx#0",
      committeeHex,
      committeeSignersHash,
      threshold: 2,
      ownerCount: 2,
      updateThreshold: 2,
      rawDatum: {} as never,
    };
    const chain: { read: () => Promise<OnChainDaParams>; reads: number } = {
      read: async () => live,
      reads: 0,
    };
    const store = await openTestCommitteeStore();
    const service = new CommitteeService({
      config,
      store,
      l1: fakeL1Source({ fetchStateQueueNodes: async () => [] }),
      payloadSource: {
        fetchPayloadCandidates: async () => {
          throw new Error("no header awaits a payload");
        },
      },
      signer,
      signerValidation,
      daChainReader: {
        fetchDaParams: async () => {
          chain.reads += 1;
          return chain.read();
        },
        fetchDaAttestationCandidates: async () => [],
      } satisfies DaAttestationChainReader,
      writeEvent: () => undefined,
    });
    return { service, store, chain, live };
  };

  it("holds every decision while the on-chain committee differs from the config, and resumes once it agrees", async () => {
    const { service, chain, live } = await committee();
    await service.initialize();
    expect(chain.reads).toBe(0);
    chain.read = async () => ({
      ...live,
      committeeHex: "fe".repeat(32) + "ff".repeat(32),
    });
    const reason = `${L1_DA_PARAMS_MISMATCH}: on-chain DA committee does not match committee node config`;
    await expect(service.tick()).resolves.toMatchObject({ held: [reason] });
    expect(service.latestL1View()).toBeUndefined();
    const held = await service.readinessSnapshot();
    expect(held.ready).toBe(false);
    expect(held.reasons).toContain(reason);

    chain.read = async () => live;
    const result = await service.tick();
    expect(result.held).toBeUndefined();
    expect(service.latestL1View()).toBeDefined();
    await expect(service.readinessSnapshot()).resolves.toMatchObject({
      ready: true,
    });
  });

  it("holds every decision while the DA params cannot be read, and resumes on the next read", async () => {
    const { service, chain, live } = await committee();
    await service.initialize();
    chain.read = async () => {
      throw new Error("DA params read refused: not_initialized");
    };
    const reason = `${L1_DA_PARAMS_UNAVAILABLE}: DA params read refused: not_initialized`;
    await expect(service.tick()).resolves.toMatchObject({ held: [reason] });
    expect((await service.readinessSnapshot()).reasons).toContain(reason);

    chain.read = async () => live;
    expect((await service.tick()).held).toBeUndefined();
    await expect(service.readinessSnapshot()).resolves.toMatchObject({
      ready: true,
    });
  });

  it("refuses stale deployment state at startup, before any read", async () => {
    const { service, store, chain } = await committee();
    await store.initDeployment({
      marker: makeDeploymentMarker("44".repeat(32)),
      manifestSha256: "55".repeat(32),
      contractDeploymentInfoSha256: "66".repeat(32),
      manifestRaw: "{}",
    });
    await expect(service.initialize()).rejects.toThrow(
      /stale_deployment_state_requires_fresh_redeploy/u,
    );
    expect(chain.reads).toBe(0);
  });
});
