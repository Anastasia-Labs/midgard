import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { CommitteeService } from "../src/committee-service.js";
import type { DaAttestationChainReader } from "../src/l1/da-attestation-reader.js";
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
        fetchDaParams: async () => fetchDaParams(committeeSignersHash),
        fetchDaAttestationCandidates: async () => [],
      } satisfies DaAttestationChainReader,
    });
    return { service, store };
  };

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

  it("fails startup without quarantining when live DA params cannot be read", async () => {
    const { service, store } = await startup(async () => {
      throw new Error("Kupo request timed out");
    });
    await expect(service.initialize()).rejects.toThrow(
      "Kupo request timed out",
    );
    expect((await store.getL1SourceState())?.status).not.toBe("quarantined");
  });
});
