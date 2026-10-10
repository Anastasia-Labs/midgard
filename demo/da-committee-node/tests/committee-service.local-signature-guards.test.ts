import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { CommitteeService } from "../src/committee-service.js";
import type { DaSignatureRecord } from "../src/domain.js";
import { deriveExpectedDaAvailabilityCommitment } from "../src/peer/signatures.js";
import {
  loadDaSigner,
  signDaAttestation,
  validateDaSignerMembership,
} from "../src/signer.js";
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
 * The service's checks on this signer's stored signatures before it signs a
 * finalized, unattested header: one stored over another availability
 * commitment for the same header stops a second signature, and one under
 * the expected commitment whose witness does not verify fails the tick.
 * Neither changes the stored signatures.
 */
describe("the service's checks on this signer's stored signatures", () => {
  const setup = async () => {
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
    const store = await openTestCommitteeStore();
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
    // The first tick signs the header and retains its payload.
    await expect(service.tick()).resolves.toMatchObject({ signedHeaders: 1 });
    const [signed] = await store.listDaSignatures(headerHash);
    return { config, signer, store, service, headerHash, signed: signed! };
  };

  const local = async (store: PostgresCommitteeStore, headerHash: string) =>
    store.listDaSignatures(headerHash);

  it("refuses to sign a header this signer already signed under another availability commitment, keeping that signature", async () => {
    const { config, signer, store, service, headerHash, signed } =
      await setup();
    // The same signer over the same header and payload under another
    // commitment authority: a valid signature with another digest.
    const other = deriveExpectedDaAvailabilityCommitment({
      authority: {
        deploymentIdentity: "ff".repeat(28),
        responseGeometry: config.availabilityChallenge.responseGeometry,
      },
      headerHash,
      payloadCborHex: (await store.getDaPayload(headerHash))!.payloadCborHex,
    });
    expect(other.commitmentDigest).not.toBe(
      signed.availabilityCommitmentDigest,
    );
    const otherSignature: DaSignatureRecord = {
      ...signed,
      availabilityCommitmentCbor: other.commitmentCbor,
      availabilityCommitmentDigest: other.commitmentDigest,
      signatureWitness: signDaAttestation({
        signer,
        signerIndex: 0,
        availabilityCommitment: other.commitment,
      }),
    };
    // Only the other commitment's signature is stored; the payload stays.
    expect(await store.pruneSignedDecisions([headerHash])).toEqual([
      headerHash,
    ]);
    await store.saveDaSignature(otherSignature);
    const before = await local(store, headerHash);
    expect(before).toHaveLength(1);

    await expect(service.tick()).resolves.toMatchObject({
      signedHeaders: 0,
      errors: [
        `refusing to sign ${headerHash}: this signer already signed a different availability commitment`,
      ],
    });
    await expect(local(store, headerHash)).resolves.toEqual(before);
  });

  it("fails the tick on a stored signature under the expected commitment whose witness does not verify, keeping it", async () => {
    const { store, service, headerHash, signed } = await setup();
    const last = signed.signatureWitness.at(-1)!;
    await store.saveDaSignature({
      ...signed,
      signatureWitness: `${signed.signatureWitness.slice(0, -1)}${last === "0" ? "1" : "0"}`,
    });
    const before = await local(store, headerHash);
    expect(before).toHaveLength(1);
    expect(before[0]!.availabilityCommitmentDigest).toBe(
      signed.availabilityCommitmentDigest,
    );
    expect(before[0]!.signatureWitness).not.toBe(signed.signatureWitness);

    await expect(service.tick()).rejects.toThrow(
      "persisted local DA signature does not match the authenticated availability commitment",
    );
    await expect(local(store, headerHash)).resolves.toEqual(before);
  });
});
