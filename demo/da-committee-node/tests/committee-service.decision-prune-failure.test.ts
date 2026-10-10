import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { COMMITTEE_DECISION_PRUNE_FAILED } from "../src/committee-service.decision-pruning.js";
import { CommitteeService } from "../src/committee-service.js";
import { loadDaSigner, validateDaSignerMembership } from "../src/signer.js";
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
 * A tick prunes its deletable signed decisions after its signing and
 * reconcile work. A prune that throws (a lost instance lock, an SQL error)
 * leaves that work standing: the tick completes degraded with the named
 * reason `committee_decision_prune_failed` and its counters, never failed.
 * The backlog carries forward; the next tick retries, and a successful
 * prune clears the reason.
 */
describe("a failed signed-decision prune", () => {
  it("reports the tick degraded by name with its counters, and the next successful prune clears it", async () => {
    const dir = await tempDir();
    const { header, headerHash, payloadCbor } = await makePayloadFixture();
    const seed = `${"00".repeat(31)}31`;
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
    const store = await openTestCommitteeStore();
    const prune = store.pruneSignedDecisions;
    const pruneCalls: (readonly string[])[] = [];
    let failNext = true;
    store.pruneSignedDecisions = async (headerHashes) => {
      pruneCalls.push(headerHashes);
      if (failNext) {
        failNext = false;
        throw new Error("committee store instance lock is not held");
      }
      return prune(headerHashes);
    };
    const reconciled: string[] = [];
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
      signerValidation: validateDaSignerMembership({
        daParams: config.daParams,
        signer,
        signerIndex: 0,
      }),
      submitterReconciler: {
        reconcileHeader: async (record) => {
          reconciled.push(record.headerHash);
          return { status: "reconciled" };
        },
      },
    });
    await service.initialize();

    const named = `${COMMITTEE_DECISION_PRUNE_FAILED}: committee store instance lock is not held`;
    await expect(service.tick()).resolves.toMatchObject({
      scannedHeaders: 1,
      signedHeaders: 1,
      reconciledHeaders: 1,
      errors: [named],
    });
    expect(pruneCalls).toHaveLength(1);
    expect(reconciled).toEqual([headerHash]);
    // The signing work the prune followed stands.
    expect(
      (await store.listSignedDecisions()).map((signed) => signed.headerHash),
    ).toEqual([headerHash]);
    const degraded = await service.readinessSnapshot();
    expect(degraded.scanner).toMatchObject({
      status: "degraded",
      scannedHeaders: 1,
      signedHeaders: 1,
      reconciledHeaders: 1,
      errors: [named],
    });
    expect(degraded.reasons).toContain(
      "last committee node tick completed with errors",
    );
    expect(degraded.reasons).not.toContain(
      "last state queue scanner tick failed",
    );

    // The next tick retries the prune; its success clears the reason.
    await expect(service.tick()).resolves.toMatchObject({ errors: [] });
    expect(pruneCalls).toHaveLength(2);
    const recovered = await service.readinessSnapshot();
    expect(recovered.scanner).toMatchObject({ status: "ok", errors: [] });
    expect(recovered.reasons).not.toContainEqual(
      expect.stringMatching(/tick (failed|completed with errors)/u),
    );
  });
});
