import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { CommitteeService } from "../src/committee-service.js";
import type { DaSignatureRecord } from "../src/domain.js";
import { loadDaSigner, validateDaSignerMembership } from "../src/signer.js";
import type { PostgresCommitteeStore } from "../src/store/postgres.js";
import { bytesToHex } from "../src/utils/hex.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from "./helpers.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";
import { fakeL1Source } from "./helpers/fake-l1-source.js";

/**
 * Plan §8.4, §11: a deleted signed decision is never re-signed with
 * different content. Signing is deterministic in the header, the payload
 * bytes and the deployment's commitment authority (Ed25519 over the
 * availability commitment), and a payload decodes only from its canonical
 * CBOR, so a header observed again after its decision was deleted is signed
 * with the same decision bytes: from the retained payload, or from the
 * payload fetched again.
 */
describe("a header observed again after its signed decision was deleted", () => {
  it("is signed with the same decision bytes, from the retained or a refetched payload", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = `${"00".repeat(31)}21`;
    const signer = await loadDaSigner(`hex:${seed}`);
    const base = minimalConfig({
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: seed,
      signerPublicKey: signer.publicKeyHex,
    });
    const config = {
      ...base,
      daParams: {
        ...base.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
        ),
      },
    };
    const signerValidation = validateDaSignerMembership({
      daParams: config.daParams,
      signer,
      signerIndex: 0,
    });
    const serviceOn = async (store: PostgresCommitteeStore) => {
      const service = new CommitteeService({
        config,
        store,
        l1: fakeL1Source({
          fetchStateQueueNodes: async () => [
            makeObservedNode({ header, headerHash, depth: 10 }),
          ],
        }),
        payloadSource: payloadSourceFromBytes(payloadCbor),
        signer,
        signerValidation,
      });
      await service.initialize();
      return service;
    };
    const decision = async (
      store: PostgresCommitteeStore,
    ): Promise<DaSignatureRecord> => {
      const own = (await store.listDaSignatures(headerHash)).filter(
        ({ source }) => source === "local",
      );
      expect(own).toHaveLength(1);
      return own[0]!;
    };
    const bytesOf = ({
      availabilityCommitmentCbor,
      availabilityCommitmentDigest,
      signatureWitness,
      payloadHash,
    }: DaSignatureRecord) => ({
      availabilityCommitmentCbor,
      availabilityCommitmentDigest,
      signatureWitness,
      payloadHash,
    });

    const store = await openTestCommitteeStore();
    const service = await serviceOn(store);
    await expect(service.tick()).resolves.toMatchObject({ signedHeaders: 1 });
    const first = bytesOf(await decision(store));

    // The decision is deleted while the payload is retained, then the
    // header is read again: the same decision bytes.
    expect(await store.pruneSignedDecisions([headerHash])).toEqual([
      headerHash,
    ]);
    expect(await store.listDaSignatures(headerHash)).toEqual([]);
    await expect(service.tick()).resolves.toMatchObject({ signedHeaders: 1 });
    expect(bytesOf(await decision(store))).toEqual(first);

    // A member holding neither the decision nor the payload fetches the
    // payload again and signs the same decision bytes.
    const fresh = await openTestCommitteeStore();
    await expect((await serviceOn(fresh)).tick()).resolves.toMatchObject({
      signedHeaders: 1,
    });
    expect(bytesOf(await decision(fresh))).toEqual(first);
  });
});
