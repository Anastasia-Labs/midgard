import { describe, expect, it } from "vitest";

import type { DaAttestationChainReader } from "../src/l1/da-attestation-reader.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { WatcherService } from "../src/watcher.js";
import { DaPayloadClient } from "../src/da/client.js";
import { loadDaSigner, validateDaSignerMembership } from "../src/signer.js";
import { bytesToHex } from "../src/utils/hex.js";
import { blake2b } from "@noble/hashes/blake2.js";
import { minimalConfig, tempDir } from "./helpers.js";

describe("watcher startup DA params checks", () => {
  it("fails closed when live DA params do not match config", async () => {
    const dir = await tempDir();
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const committeeSignersHash = bytesToHex(
      blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
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
        committeeHex: signer.publicKeyHex,
        committeeSignersHash,
        threshold: 1,
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: config.daParams,
      signer,
      signerIndex: 0,
    });
    const service = new WatcherService({
      config,
      store: await JsonFileWatcherStore.open(dir),
      stateQueueProvider: { fetchStateQueueNodes: async () => [] },
      payloadClient: new DaPayloadClient({ endpoints: ["http://da.example"] }),
      signer,
      signerValidation,
      daChainReader: {
        fetchDaParams: async () => ({
          outRef: "tx#0",
          committeeHex: "ff".repeat(32),
          committeeSignersHash,
          threshold: 1,
          ownerCount: 0,
          updateThreshold: 1,
          rawDatum: {} as never,
        }),
        fetchDaAttestationCandidates: async () => [],
      } satisfies DaAttestationChainReader,
    });
    await expect(service.initialize()).rejects.toThrow(/on-chain DA committee/);
  });
});
