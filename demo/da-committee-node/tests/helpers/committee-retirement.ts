import { createHash } from "node:crypto";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { verifyDaPayloadAgainstHeader } from "../../src/da/payload.js";
import type { StateQueueHeaderRecord } from "../../src/domain.js";
import { hashBlockHeader } from "../../src/l1/state-queue-scanner.js";
import { deriveExpectedDaAvailabilityCommitment } from "../../src/peer/signatures.js";
import { loadDaSigner, signDaAttestation } from "../../src/signer.js";
import {
  type CommitteeStore,
  decisionEffectId,
  type L1SourceState,
} from "../../src/store.js";
import {
  committeeRetirementSource,
  type CommitteeRetirementSourceDependencies,
} from "../../src/store/retirement-source.js";
import { makePayloadFixture, tempDir } from "../helpers.js";

export const retentionDays = 15,
  horizonMs = 15 * 86400000;
export const oldPoint = { slot: 100, blockHash: "12".repeat(32), blockNo: 100 };
export const expiryPoint = {
  slot: 2_000_000,
  blockHash: "34".repeat(32),
  blockNo: 3000,
};
export const descendantPoint = {
  slot: 2_100_000,
  blockHash: "56".repeat(32),
  blockNo: 5161,
};
export const retentionFixture = async (store: CommitteeStore) => {
  const dir = await tempDir(),
    journal = openAvailabilityOperationJournal(join(dir, "journal.sqlite"));
  const key = CML.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(7));
  const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
  const actorId = key.to_public().hash().to_hex(),
    contractManifestId = "89".repeat(32),
    deploymentFingerprint = "ab".repeat(32),
    committeeSignersHash = Buffer.from(
      blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
    ).toString("hex"),
    authority = "ef".repeat(32);
  const manifestRaw = JSON.stringify({
    da: { committeeSignersHash, transportProfile: { retentionDays } },
  });
  const binding = {
    deploymentFingerprint,
    manifestSha256: createHash("sha256").update(manifestRaw).digest("hex"),
    contractManifestId,
    committeeSignersHash,
    actorId,
    sourceAuthoritySha256: authority,
    peerIds: ["peer1"],
    retentionDays,
    recoveryDepth: 2160,
    maximumRecords: 512,
    maximumEncodedBytes: 8 * 1024 * 1024,
  };
  await store.initDeployment({
    marker: {
      schemaVersion: "midgard-deployment-marker-v1",
      manifestId: deploymentFingerprint,
    },
    manifestRaw,
    manifestSha256: binding.manifestSha256,
    contractDeploymentInfoSha256: "00".repeat(32),
  });
  const state: L1SourceState = {
    schemaVersion: 1,
    sourceMode: "local_node",
    network: "Preprod",
    authoritySha256: authority,
    status: "healthy",
    observations: [],
    observedAt: "2026-10-02T00:00:00.000Z",
    stateQueueReplayAnchor: {
      deploymentIdentityDigest: deploymentFingerprint,
      stateQueuePolicyId: "11".repeat(28),
      queue: [{ headerHash: null, outRef: `${"22".repeat(32)}#0` }],
      blockNo: "100",
      transactionIndex: "0",
    },
  };
  await store.saveL1SourceState(state);
  const deployment = {
    hubOraclePolicyId: "11".repeat(28),
    contracts: {
      stateQueue: { policyId: "22".repeat(28), spendingScriptAddress: "queue" },
      correctionLock: { spendingScriptAddress: "lock" },
      availabilityChallenge: {
        policyId: "33".repeat(28),
        spendingScriptAddress: "availability",
      },
    },
  } as SDK.DaAvailabilityDeployment;
  const output = (
    index: number,
    address: string,
    unit: string,
    datum: string,
  ): UTxO => ({
    txHash: "44".repeat(32),
    outputIndex: index,
    address,
    assets: { lovelace: 3000000n, [unit]: 1n },
    datum,
  });
  const root = output(
    0,
    "queue",
    deployment.contracts.stateQueue.policyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME,
    SDK.encodeLinkedListNodeView({
      key: "Empty",
      next: "Empty",
      data: Data.castTo(SDK.makeGenesisConfirmedState(0n), SDK.ConfirmedState),
    }),
  );
  const lock = output(
    1,
    "lock",
    SDK.correctionLockUnit(deployment.hubOraclePolicyId),
    Data.to("Idle", SDK.CorrectionLockDatum),
  );
  let current = { ...oldPoint };
  const proofs = new Map<string, typeof oldPoint>([
    [`${oldPoint.slot}:${oldPoint.blockHash}`, oldPoint],
    [`${expiryPoint.slot}:${expiryPoint.blockHash}`, expiryPoint],
    [`${descendantPoint.slot}:${descendantPoint.blockHash}`, descendantPoint],
  ]);
  const nativeTimes = new Map<number, number>([
    [oldPoint.slot, 100000],
    [expiryPoint.slot, horizonMs + 1_000_000],
    [descendantPoint.slot, horizonMs + 1_100_000],
  ]);
  const pinned = new Set<string>();
  let claimsCurrent = true,
    raw = {
      stateQueueUtxos: [root],
      availabilityUtxos: [] as UTxO[],
      correctionLockUtxos: [lock],
    };
  let submissionReader: CommitteeRetirementSourceDependencies["readSubmissionPoint"] =
    async (_tx, c) => ({ point: oldPoint, tip: c });
  const deps: CommitteeRetirementSourceDependencies = {
    binding,
    deployment,
    store,
    journal,
    readBoundary: async () => ({ ...current }),
    slotTimeMs: (slot) => nativeTimes.get(slot) ?? slot * 1000,
    readRawSnapshot: async () => raw,
    readCanonicalPoint: async (p, c) => {
      const point = proofs.get(`${p.slot}:${p.blockHash}`);
      return point ? { point, tip: c } : null;
    },
    readSubmissionPoint: (...args) => submissionReader(...args),
    readOperationalPins: async () => [...pinned],
    assertCurrent: async () => {},
    assertClaimsCurrent: async () => {
      if (!claimsCurrent) throw new Error("Real reconciliation unavailable");
    },
  };
  const source = committeeRetirementSource(deps);
  const base = await makePayloadFixture(1);
  const header = (end: number, variant = 0): StateQueueHeaderRecord => {
    const h = {
      ...base.header,
      startTime: BigInt(end - 1),
      endTime: BigInt(end),
      operatorVkey: variant
        ? variant.toString(16).padStart(56, "0")
        : base.header.operatorVkey,
    };
    const headerHash = hashBlockHeader(h);
    return {
      deploymentFingerprint,
      headerHash,
      stateQueueOutRef: `${"66".repeat(32)}#${end}`,
      blockAssetName: headerHash,
      header: h,
      computedHeaderHash: headerHash,
      daAttestation: SDK.NO_DA_ATTESTATION,
      observedChainPoint: {
        slot: oldPoint.slot,
        blockHash: oldPoint.blockHash,
        blockHeight: oldPoint.blockNo,
        depth: 5000,
        finalized: true,
        providerSource: "authenticated_state_queue_transition_v1",
      },
      finalized: true,
      status: "removed",
      validationErrors: [],
      updatedAt: "2026-10-02T00:00:00.000Z",
    };
  };
  const seed = async (end: number, variant = 0) => {
    const h = header(end, variant);
    await store.upsertStateQueueHeader(h);
    // Rebind the actual canonical payload to the header; verification supplies full V1 traces/counts.
    const payload = {
      ...base.payload,
      block_body: {
        ...base.payload.block_body,
        header: h.header,
        header_hash: h.headerHash,
      },
    };
    const { wrapDaPayload } = await import(
      "@al-ft/midgard-core/da-payload-envelope"
    );
    const bytes = await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    });
    const verified = await verifyDaPayloadAgainstHeader(
      bytes,
      h.headerHash,
      h.header,
      { payloadSchemaVersion: 1, stateQueueOutRef: h.stateQueueOutRef },
    );
    await store.saveDaPayload({
      deploymentFingerprint,
      headerHash: h.headerHash,
      payloadSchemaVersion: 1,
      payloadCborHex: bytes.toString("hex"),
      payloadSha256: verified.payloadSha256,
      sourcePeerId: "peer1",
      fetchedAt: "2026-10-02T00:00:00.000Z",
      validationStatus: "verified",
      rootSummary: verified.roots,
    });
    const expected = deriveExpectedDaAvailabilityCommitment({
      authority: {
        deploymentIdentity: deployment.hubOraclePolicyId,
        responseGeometry: {
          chunkByteLength: 14_020,
          trancheByteLength: 4 * 1024 * 1024,
          maxTrancheCount: 16,
        },
      },
      headerHash: h.headerHash,
      payloadCborHex: bytes.toString("hex"),
    });
    const signature = {
      deploymentFingerprint,
      headerHash: h.headerHash,
      signerIndex: 0,
      signatureWitness: signDaAttestation({
        signer,
        signerIndex: 0,
        availabilityCommitment: expected.commitment,
      }),
      availabilityCommitmentCbor: expected.commitmentCbor,
      availabilityCommitmentDigest: expected.commitmentDigest,
      payloadHash: verified.payloadSha256,
      committeeSignersHash,
      signedAt: "2026-10-02T00:00:00.000Z",
      broadcastStatus: "posted" as const,
      source: "local" as const,
      l1ChainPoint: h.observedChainPoint,
      validation: verified.validation,
    };
    const observation = {
      headerHash: h.headerHash,
      stateQueueOutRef: h.stateQueueOutRef,
      stateQueueStatus: h.status,
      slot: oldPoint.slot,
      blockHash: oldPoint.blockHash,
      finalized: true,
      hasPersistedDecision: true,
    };
    const prior = (await store.getL1SourceState())!;
    const sourceState = {
      ...prior,
      observations: [...prior.observations, observation],
    };
    const effect = {
      schemaVersion: 1 as const,
      effectId: decisionEffectId({
        deploymentFingerprint,
        headerHash: h.headerHash,
        stateQueueOutRef: h.stateQueueOutRef,
        effectKind: "signature_publish",
        signerIndex: 0,
      }),
      deploymentFingerprint,
      sourceMode: "local_node" as const,
      network: "Preprod",
      effectKind: "signature_publish" as const,
      headerHash: h.headerHash,
      stateQueueOutRef: h.stateQueueOutRef,
      signerIndex: 0,
      slot: oldPoint.slot,
      blockHash: oldPoint.blockHash,
      finalized: true as const,
      status: "pending" as const,
      attemptCount: 1,
      createdAt: "2026-10-02T00:00:00.000Z",
      updatedAt: "2026-10-02T00:00:00.000Z",
    };
    await store.beginDecisionEffect({ effect, sourceState, signature });
    await store.completeDecisionEffect({
      effectId: effect.effectId,
      expectedAttemptCount: 1,
      status: "published",
      updatedAt: "2026-10-02T00:00:00.000Z",
      signature,
    });
    await store.savePromiseCapacityEvidence({
      deploymentFingerprint,
      contractManifestId,
      actorId,
      headerHash: h.headerHash,
      commitmentDigest: expected.commitmentDigest,
      cutoffTimeMs: end + 720000,
      recoveryDepth: 2160,
      retirementKind: "terminal",
      point: oldPoint,
      certifiedAt: expiryPoint,
    });
    await store.saveDaAttestationCandidate({
      deploymentFingerprint,
      headerHash: h.headerHash,
      outRef: `${"77".repeat(32)}#0`,
      datumCbor: "80",
      attestationCount: 1,
      threshold: 1,
      committeeSignersHash,
      bitmap: "01",
      observedChainPoint: h.observedChainPoint,
      status: "burned",
    });
    await store.saveL1Submission({
      deploymentFingerprint,
      headerHash: h.headerHash,
      txKind: "apply",
      txHash: "88".repeat(32),
      inputsUsed: [],
      submittedAt: "2026-10-02T00:00:00.000Z",
      resultStatus: "confirmed",
    });
    await store.savePeerBroadcast({
      deploymentFingerprint,
      peerId: "peer1",
      headerHash: h.headerHash,
      availabilityCommitmentDigest: expected.commitmentDigest,
      signerIndex: 0,
      status: "posted",
      attempts: 1,
      updatedAt: "2026-10-02T00:00:00.000Z",
    });
    return { header: h, signature, effect, verified };
  };
  const persistFinancial = (headerHash: string) => {
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex("98".repeat(32)),
        0n,
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(
          credentialToAddress("Preprod", { type: "Key", hash: actorId }),
        ),
        CML.Value.from_coin(5_000_000n),
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, 100_000n);
    body.set_ttl(1000n);
    body.set_validity_interval_start(0n);
    const witnesses = CML.TransactionWitnessSet.new(),
      vkeys = CML.VkeywitnessList.new();
    vkeys.add(CML.make_vkey_witness(CML.hash_transaction(body), key));
    witnesses.set_vkeywitnesses(vkeys);
    const tx = CML.Transaction.new(body, witnesses, true);
    const intent = SDK.inspectDaAvailabilitySignedIntent({
      deploymentIdentity: contractManifestId,
      actor: actorId,
      headerHash,
      action: "prepare",
      signedCbor: tx.to_cbor_hex(),
    });
    const lease = journal.acquire(actorId, "fixture", 0, 10000);
    journal.persist(lease, intent, 0);
    journal.release(lease);
    return intent;
  };
  const scope = () =>
    SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 100000 });
  const compact = async () => {
    const s = scope();
    try {
      return await source.compact(s);
    } finally {
      s.close();
    }
  };
  return {
    store,
    signer,
    deployment,
    source,
    deps,
    persistFinancial,
    setSubmissionReader: (read: typeof submissionReader) => {
      submissionReader = read;
    },
    binding,
    journal,
    header,
    seed,
    proofs,
    pinned,
    scope,
    compact,
    setPoint: (p: typeof oldPoint, time?: number) => {
      current = { ...p };
      proofs.set(`${p.slot}:${p.blockHash}`, p);
      if (time !== undefined) nativeTimes.set(p.slot, time);
    },
    setClaims: (v: boolean) => {
      claimsCurrent = v;
    },
    setRaw: (v: typeof raw) => {
      raw = v;
    },
    root,
    lock,
  };
};
