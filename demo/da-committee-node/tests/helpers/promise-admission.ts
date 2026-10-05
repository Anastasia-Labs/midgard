import { type AvailabilityResponseAdmissionPolicy } from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { afterEach, vi } from "vitest";

import {
  committeePromiseAdmission,
  type CommitteePromiseAdmissionSnapshot,
} from "../../src/availability/promise-admission.js";
import { CommitteeService } from "../../src/committee-service.js";
import type { DaPayloadSource } from "../../src/da/source.js";
import { hashBlockHeader } from "../../src/l1/state-queue-scanner.js";
import { loadDaSigner, validateDaSignerMembership } from "../../src/signer.js";
import { JsonFileCommitteeStore } from "../../src/store.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  tempDir,
} from "../helpers.js";
import { withFinalSnapshot } from "./final-snapshot.js";
import { testPromiseRuntimePolicy } from "./promise-runtime-policy.js";

// An explicit bounded test actor model; production has no inferred policy.
const policy: AvailabilityResponseAdmissionPolicy = {
  id: "bounded-test-actor",
  envelopeId: "test-runtime-only",
  publishStepMs: 150_000,
  settleStepMs: 150_000,
  closeStepMs: 150_000,
  discoveryAndClockMarginMs: 0,
  supportedRecoveryMs: 0,
};
export const stores = new Set<JsonFileCommitteeStore>();
afterEach(async () => {
  for (const store of stores) await store.close();
  stores.clear();
});

export const promiseAdmissionFixture = async () => {
  const dir = await tempDir();
  const rebind = async (
    base: Awaited<ReturnType<typeof makePayloadFixture>>,
    header: SDK.Header,
  ) => {
    const headerHash = hashBlockHeader(header);
    const payload = {
      ...base.payload,
      block_body: {
        ...base.payload.block_body,
        header,
        header_hash: headerHash,
      },
    };
    const innerPayloadCbor = SDK.encodeDaPayload(payload);
    return {
      ...base,
      header,
      headerHash,
      payload,
      innerPayloadCbor,
      payloadCbor: await wrapDaPayload(innerPayloadCbor, { mode: "identity" }),
    };
  };
  const firstBase = await makePayloadFixture(1);
  const first = await rebind(firstBase, {
    ...firstBase.header,
    prevHeaderHash: "00".repeat(28),
  });
  const secondBase = await makePayloadFixture(2);
  const second = await rebind(secondBase, {
    ...secondBase.header,
    prevHeaderHash: first.headerHash,
    prevUtxosRoot: first.header.utxosRoot,
    startTime: first.header.endTime,
    endTime: first.header.endTime + 1n,
  });
  const signerBase = await loadDaSigner(`hex:${"00".repeat(31)}01`);
  const sign = vi.fn(signerBase.sign);
  const signer = { ...signerBase, sign };
  const rawConfig = minimalConfig({
    dir,
    manifestPath: `${dir}/manifest.json`,
    deploymentInfoPath: `${dir}/deployment.json`,
    signerSeed: "00".repeat(31) + "01",
    signerPublicKey: signer.publicKeyHex,
  });
  const config = {
    ...rawConfig,
    contractDeploymentInfo: { manifestId: "ab".repeat(32) },
    daParams: {
      ...rawConfig.daParams,
      committeeSignersHash: Buffer.from(
        blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
      ).toString("hex"),
    },
  };
  const signerValidation = validateDaSignerMembership({
    daParams: config.daParams,
    signer,
    signerIndex: 0,
  });
  const open = async () => {
    const store = await JsonFileCommitteeStore.open(dir);
    stores.add(store);
    return store;
  };
  const store = await open();
  const bytes = new Map([
    [first.headerHash, first.payloadCbor],
    [second.headerHash, second.payloadCbor],
  ]);
  const payloadSource: DaPayloadSource = {
    fetchPayloadCandidates: async (hash) => ({
      ok: true,
      attempts: [],
      candidates: [
        {
          sourcePeerId: "fixture",
          payloadSchemaVersion: 1,
          payloadCbor: bytes.get(hash)!,
        },
      ],
    }),
  };
  let snapshot: CommitteePromiseAdmissionSnapshot = {
    boundary: {
      pointId: "fixture-canonical-point",
      rollbackGeneration: 0,
      observedAtMs: 1_000,
    },
    canonicalTimeMs: 0,
    challenges: [],
    complete: true,
    blocking: { kind: "bounded", remainingMs: 0 },
    resourceWorkload: {
      walletInputs: 1,
      journalEntries: 0,
      challengeRecords: 0,
      storeRecords: 0,
      storeEncodedBytes: 0,
    },
  };
  const assertCurrent = vi.fn(async () => {});
  const source = {
    policyAuthority: testPromiseRuntimePolicy(policy, {
      deploymentFingerprint: config.deploymentFingerprint,
      contractManifestId: String(config.contractDeploymentInfo.manifestId),
    }).authority,
    readSnapshot: async () => snapshot,
    assertCurrent,
  };
  const admission = (activeStore = store) =>
    committeePromiseAdmission({
      config,
      store: activeStore,
      signerValidation,
      source,
      now: () => 1_000,
    });
  const provider = withFinalSnapshot({
    fetchStateQueueNodes: async () =>
      [first, second].map((item, index) =>
        makeObservedNode({
          ...item,
          depth: 10,
          outRef: `${"ab".repeat(32)}#${index}`,
        }),
      ),
  });
  const service = (activeStore = store, enabled = true) =>
    new CommitteeService({
      config,
      store: activeStore,
      stateQueueProvider: provider,
      payloadSource,
      signer,
      signerValidation,
      ...(enabled ? { promiseAdmission: admission(activeStore) } : {}),
      writeEvent: () => {},
    });
  return {
    dir,
    first,
    second,
    config,
    store,
    open,
    sign,
    signerValidation,
    admission,
    service,
    provider,
    payloadSource,
    source,
    assertCurrent,
    setSnapshot: (value: CommitteePromiseAdmissionSnapshot) => {
      snapshot = value;
    },
    getSnapshot: () => snapshot,
  };
};
