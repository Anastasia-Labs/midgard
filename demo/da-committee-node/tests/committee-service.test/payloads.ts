import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { blake2b } from "@noble/hashes/blake2.js";
import { expect, it } from "vitest";

import { CommitteeService } from "../../src/committee-service.js";
import { daPayloadSha256 } from "../../src/da/payload.js";
import { type Header } from "../../src/domain.js";
import { hashBlockHeader } from "../../src/l1/state-queue-scanner.js";
import { loadDaSigner, validateDaSignerMembership } from "../../src/signer.js";
import { bytesToHex } from "../../src/utils/hex.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from ".././helpers.js";
import { withFinalSnapshot } from ".././helpers/final-snapshot.js";
import {
  expectedCommitment,
  failPayloadSource,
  missingPayload,
  openJsonCommitteeStore,
  payloadCandidates,
  payloadSourceFromCandidates,
} from "./fixtures.js";

export const registerPayloadsTests = () => {
  it("fetches payload bytes from the configured DA payload source", async () => {
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
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
      payloadSource: payloadSourceFromBytes(payloadCbor, "producer-peer"),
      signer,
      signerValidation,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [],
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      headerHash,
      sourcePeerId: "producer-peer",
      validationStatus: "verified",
    });
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
    ).resolves.toMatchObject({ headerHash, broadcastStatus: "local" });
  });

  it("verifies and signs a locally accepted libp2p payload without refetching", async () => {
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
    await store.saveDaPayload({
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      headerHash,
      payloadSchemaVersion: 1,
      payloadCborHex: payloadCbor.toString("hex"),
      payloadSha256: daPayloadSha256(payloadCbor),
      sourcePeerId: "libp2p:payload-submit",
      fetchedAt: new Date().toISOString(),
      validationStatus: "fetched",
    });
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
      payloadSource: failPayloadSource("payload source must not be used"),
      signer,
      signerValidation,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [],
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      headerHash,
      sourcePeerId: "libp2p:payload-submit",
      validationStatus: "verified",
      payloadSha256: daPayloadSha256(payloadCbor),
      conflictStatus: "none",
    });
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
    ).resolves.toMatchObject({ headerHash, broadcastStatus: "local" });
  });

  it("fails closed for malformed locally accepted payload bytes", async () => {
    const dir = await tempDir();
    const { header, headerHash } = await makePayloadFixture();
    // A payload-submit ACK can retain a canonical outer envelope whose inner
    // body is malformed.  The watcher remains the sole semantic gate.
    const invalidPayload = await wrapDaPayload(Buffer.from("deadbeef", "hex"), {
      mode: "identity",
    });
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
    await store.saveDaPayload({
      deploymentFingerprint: configWithDaHash.deploymentFingerprint,
      headerHash,
      payloadSchemaVersion: 1,
      payloadCborHex: invalidPayload.toString("hex"),
      payloadSha256: daPayloadSha256(invalidPayload),
      sourcePeerId: "libp2p:payload-submit",
      fetchedAt: new Date().toISOString(),
      validationStatus: "fetched",
    });
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
      payloadSource: failPayloadSource("payload source must not be used"),
      signer,
      signerValidation,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      headerHash,
      sourcePeerId: "libp2p:payload-submit",
      validationStatus: "malformed_da",
    });
    await expect(
      store.getDaSignature({
        headerHash,
        availabilityCommitmentDigest: expectedCommitment(
          configWithDaHash,
          headerHash,
          invalidPayload,
        ).commitmentDigest,
        signerIndex: 0,
      }),
    ).resolves.toBeUndefined();
  });

  it("re-verifies a cached rejection so a corrected build can attest the payload", async () => {
    const { header, payloadCbor } = await makePayloadFixture();
    const { service, store, headerHash } = await reverifyHarness(
      payloadCbor,
      header,
    );
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      validationStatus: "verified",
      payloadSha256: daPayloadSha256(payloadCbor),
    });
  });

  it("re-checks a cached rejection only once per process", async () => {
    const { header } = await makePayloadFixture();
    const invalidPayload = await wrapDaPayload(Buffer.from("deadbeef", "hex"), {
      mode: "identity",
    });
    const { service, store, headerHash } = await reverifyHarness(
      invalidPayload,
      header,
    );
    await expect(service.tick()).resolves.toMatchObject({ skippedHeaders: 1 });
    // The one re-check replaces the stale message with this build's cause.
    const rechecked = await store.getDaPayload(headerHash);
    expect(rechecked).toMatchObject({ validationStatus: "malformed_da" });
    expect(rechecked?.validationError).not.toBe(
      "stale verdict from an earlier build",
    );
    await store.saveDaPayload({
      ...rechecked!,
      validationError: "sentinel after the one re-check",
    });
    await expect(service.tick()).resolves.toMatchObject({ skippedHeaders: 1 });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      validationStatus: "malformed_da",
      validationError: "sentinel after the one re-check",
    });
  });

  it("detects conflicting payload bytes across DA endpoints and refuses to sign", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const conflictingPayload = Buffer.from(payloadCbor);
    conflictingPayload[conflictingPayload.length - 1] =
      (conflictingPayload[conflictingPayload.length - 1] ?? 0) ^ 0xff;
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
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
      payloadSource: payloadSourceFromCandidates([
        { sourcePeerId: "da-peer-a", payloadCbor },
        { sourcePeerId: "da-peer-b", payloadCbor: conflictingPayload },
      ]),
      signer,
      signerValidation,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
      errors: [`conflicting DA payload bytes for ${headerHash}`],
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      headerHash,
      validationStatus: "conflicted",
      conflictStatus: "conflicting_bytes",
      sourcePeerId: "da-peer-a,da-peer-b",
    });
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
    ).resolves.toBeUndefined();
  });

  it("signs after a transient missing DA payload becomes available", async () => {
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
    let payloadAvailable = false;
    const store = await openJsonCommitteeStore(dir);
    const service = new CommitteeService({
      config: configWithDaHash,
      store,
      stateQueueProvider: withFinalSnapshot({
        fetchStateQueueNodes: async () => [
          makeObservedNode({ header, headerHash, depth: 10 }),
        ],
      }),
      payloadSource: {
        fetchPayloadCandidates: async () =>
          payloadAvailable
            ? payloadCandidates([{ sourcePeerId: "da-peer", payloadCbor }])
            : missingPayload("da-peer"),
      },
      signer,
      signerValidation,
    });

    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 0,
      skippedHeaders: 1,
      payloadFetches: [
        {
          headerHash,
          status: "missing_da",
          sourcePeerIds: ["da-peer"],
          detail: "da-peer:not_found",
        },
      ],
      errors: [],
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      validationStatus: "missing_da",
      payloadSha256: "",
    });

    payloadAvailable = true;
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      skippedHeaders: 0,
      errors: [],
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      validationStatus: "verified",
      payloadSha256: daPayloadSha256(payloadCbor),
      conflictStatus: "none",
    });
  });
};

const reverifyHarness = async (payloadBytes: Buffer, header: Header) => {
  const dir = await tempDir();
  const headerHash = hashBlockHeader(header);
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
  const store = await openJsonCommitteeStore(dir);
  // A verdict an earlier build cached for these bytes.
  await store.saveDaPayload({
    deploymentFingerprint: configWithDaHash.deploymentFingerprint,
    headerHash,
    payloadSchemaVersion: 1,
    payloadCborHex: payloadBytes.toString("hex"),
    payloadSha256: daPayloadSha256(payloadBytes),
    sourcePeerId: "libp2p:payload-submit",
    fetchedAt: new Date().toISOString(),
    validationStatus: "malformed_da",
    validationError: "stale verdict from an earlier build",
  });
  const service = new CommitteeService({
    config: configWithDaHash,
    store,
    stateQueueProvider: withFinalSnapshot({
      fetchStateQueueNodes: async () => [
        makeObservedNode({ header, headerHash, depth: 10 }),
      ],
    }),
    payloadSource: failPayloadSource("payload source must not be used"),
    signer,
    signerValidation: validateDaSignerMembership({
      daParams: configWithDaHash.daParams,
      signer,
      signerIndex: 0,
    }),
  });
  await service.initialize();
  return { service, store, headerHash };
};
