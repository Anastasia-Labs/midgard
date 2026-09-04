import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import type { DaAttestationChainReader } from "../src/l1/da-attestation-reader.js";
import { loadDaSigner, validateDaSignerMembership } from "../src/signer.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { bytesToHex } from "../src/utils/hex.js";
import { WatcherService } from "../src/watcher.js";
import { minimalConfig, tempDir } from "./helpers.js";

describe("watcher startup DA params checks", () => {
  it("fails closed when live DA params do not match config", async () => {
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
    const service = new WatcherService({
      config,
      store: await JsonFileWatcherStore.open(dir),
      stateQueueProvider: { fetchStateQueueNodes: async () => [] },
      payloadSource: {
        fetchPayloadCandidates: async () => {
          throw new Error("payload source should not be used during startup");
        },
      },
      signer,
      signerValidation,
      daChainReader: {
        // Floor-compliant, but a different committee than the config names —
        // the mismatch is the whole point of the test.
        fetchDaParams: async () => ({
          outRef: "tx#0",
          committeeHex: "fe".repeat(32) + "ff".repeat(32),
          committeeSignersHash,
          threshold: 2,
          ownerCount: 2,
          updateThreshold: 2,
          rawDatum: {} as never,
        }),
        fetchDaAttestationCandidates: async () => [],
      } satisfies DaAttestationChainReader,
    });
    await expect(service.initialize()).rejects.toThrow(/on-chain DA committee/);
  });
});
