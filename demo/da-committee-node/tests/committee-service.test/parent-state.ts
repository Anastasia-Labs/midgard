import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { expect, it } from "vitest";

import { CommitteeService } from "../../src/committee-service.js";
import { loadDaSigner, validateDaSignerMembership } from "../../src/signer.js";
import { bytesToHex } from "../../src/utils/hex.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  tempDir,
} from ".././helpers.js";
import { fakeL1Source } from ".././helpers/fake-l1-source.js";
import {
  missingPayload,
  openTestCommitteeStore,
  payloadCandidates,
} from "./fixtures.js";

/**
 * Every event's inputs resolve against the state immediately before it, so
 * the committee replays a block from its parent's post-state and withholds
 * its attestation while that state is unavailable.
 */
export const registerParentStateTests = () => {
  it("does not attest a block until its parent's post-state is available, and then attests it", async () => {
    const dir = await tempDir();
    const parent = await makePayloadFixture(1);
    const { header, headerHash, payloadCbor } = await makePayloadFixture(3, {
      prevHeaderHash: parent.headerHash,
      prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    });
    const seed = "00".repeat(31) + "01";
    const signer = await loadDaSigner(`hex:${seed}`);
    const config = minimalConfig({
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
    let parentAvailable = false;
    const store = await openTestCommitteeStore();
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      l1: fakeL1Source({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
      payloadSource: {
        fetchPayloadCandidates: async (requested) =>
          requested === headerHash
            ? payloadCandidates([{ sourcePeerId: "da-peer", payloadCbor }])
            : parentAvailable && requested === parent.headerHash
              ? payloadCandidates([
                  { sourcePeerId: "da-peer", payloadCbor: parent.payloadCbor },
                ])
              : missingPayload("da-peer"),
      },
      signer,
      signerValidation,
    });

    await service.initialize();
    const withheld = await service.tick();
    expect(withheld).toMatchObject({ scannedHeaders: 1, signedHeaders: 0 });
    expect(withheld.errors.join("\n")).toMatch(
      /parent DA payload [0-9a-f]+ with utxos_root [0-9a-f]+ is not available/u,
    );
    const pending = await store.getDaPayload(headerHash);
    expect(pending?.validationStatus).not.toBe("malformed_da");
    expect(pending?.validationStatus).not.toBe("root_mismatch");

    parentAvailable = true;
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      errors: [],
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      validationStatus: "verified",
    });
  });
};
