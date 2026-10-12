import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";
import { blake2b } from "@noble/hashes/blake2.js";
import { expect, it } from "vitest";

import { CommitteeService } from "../../src/committee-service.js";
import { obligations } from "../../src/l1/follower/obligations.js";
import { loadDaSigner, validateDaSignerMembership } from "../../src/signer.js";
import {
  retentionCycleOptions,
  runRetentionCycle,
} from "../../src/store/retention.js";
import { bytesToHex } from "../../src/utils/hex.js";
import {
  makeObservedNode,
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from ".././helpers.js";
import {
  FAKE_L1_PARAMETERS,
  fakeL1Source,
} from ".././helpers/fake-l1-source.js";
import { openTestCommitteeStore } from "./fixtures.js";

const ATTESTED: SDK.DaAvailabilityStateQueueStatus = {
  Attested: { commitment_hash: "33".repeat(32) },
};

export const registerRetentionFinalityTests = () => {
  it("keeps an attested payload whose commit is final: commit finality releases only the commit record", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = `${"00".repeat(31)}46`;
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
    // Committed far deeper than k, unattested: the member signs it.
    const committed = makeObservedNode({
      header,
      headerHash,
      depth: 10,
      slot: 10,
      blockHash: "11".repeat(32),
    });
    // Attested on chain since, still in the queue, its commit k-final.
    const attested = makeObservedNode({
      header,
      headerHash,
      daAttestation: ATTESTED,
      depth: FAKE_L1_PARAMETERS.securityParameter + 2,
      outRef: `${"ef".repeat(32)}#0`,
      slot: 14,
      blockHash: "12".repeat(32),
    });
    let nodes = [committed];
    const store = await openTestCommitteeStore();
    const service = new CommitteeService({
      config,
      store,
      l1: fakeL1Source({ fetchStateQueueNodes: async () => nodes }),
      payloadSource: payloadSourceFromBytes(payloadCbor),
      signer,
      signerValidation: validateDaSignerMembership({
        daParams: config.daParams,
        signer,
        signerIndex: 0,
      }),
      coordinator: { publishSignature: async () => "posted" },
    });
    await service.initialize();
    await expect(service.tick()).resolves.toMatchObject({
      signedHeaders: 1,
      errors: [],
    });
    expect(await store.getDaPayload(headerHash)).toBeDefined();

    nodes = [attested];
    await expect(service.tick()).resolves.toMatchObject({ errors: [] });
    expect(service.latestL1View()).toBeDefined();

    // The commit obligation is done with: final, its record released.
    const [obligation] = obligations({
      signed: [{ headerHash, endTimeMs: header.endTime }],
      presence: [
        {
          headerHash,
          firstCreatedHeight: 1_000 - 10 + 1,
          liveStatus: "Attested",
        },
      ],
      tipHeight: 1_000,
      finalBlockTimeMs: null,
      pruned: false,
      parameters: FAKE_L1_PARAMETERS,
    });
    expect(obligation).toMatchObject({
      state: "final",
      liveStatus: "Attested",
      retainCommitRecord: false,
      decisionDeletable: true,
    });

    // The payload is still pinned, both inside its challenge window and
    // past it: an attested node in the queue keeps it, whatever the commit.
    expect(service.readRetirementOperationalPins()).toContain(headerHash);
    const view = service.latestL1View()!;
    expect(view.liveQueueHeaderHashes.has(headerHash)).toBe(true);
    const endTimeMs = Number(header.endTime);
    for (const nowMs of [
      endTimeMs + 1,
      endTimeMs + MIDGARD_RETENTION_WINDOW.requiredRetentionMs + 1,
    ]) {
      const { deadlines, prune } = await runRetentionCycle(
        store,
        retentionCycleOptions(config, view, nowMs),
      );
      expect(prune.prunedHeaderHashes).toEqual([]);
      expect(
        deadlines.entries.find((entry) => entry.headerHash === headerHash),
      ).toMatchObject({ reasonCode: "live_queue_header" });
      expect(await store.getDaPayload(headerHash)).toBeDefined();
    }
  });
};
