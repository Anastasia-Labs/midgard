import type { AvailabilityOperationIntent } from "@al-ft/midgard-core/availability-operation-journal";
import type * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { createWatcherAvailabilityObservation } from "../../src/availability/observation.js";
import { unsafeAdmitWatcherStateQueueObservationForReplayTest } from "../../src/indexers/authenticated-state-queue-observation.js";
import { RELEASE_FINALITY_DEPTH } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import {
  type Fixture,
  fixture,
  pruneFixture,
  random,
} from "../support/l1-follower-raw-reads-fixture.js";

/**
 * Reconciling a signed availability intent against a follower store that has
 * pruned. The reads are the real follower raw reads over a pruned simulator
 * chain; only the deployment identity is a stand-in, since `operation` reads
 * nothing from it but the manifest id.
 */

const MANIFEST = "a1".repeat(32);

/** The tip of the fixture chain, admitted as a finalized observation. */
const tipObservation = (fx: Fixture) => {
  const tip = fx.tipPoint();
  const value = {
    schemaVersion: "midgard-watcher-production-state-queue-observation-v1",
    deploymentIdentityDigest: MANIFEST,
    protocolScriptAuthorityDigest: "45".repeat(32),
    stateQueuePolicyId: "46".repeat(28),
    hubOraclePolicyId: "47".repeat(28),
    nativePoint: {
      blockHash: tip.blockHash,
      parentBlockHash: null,
      slot: tip.slot,
      blockNo: tip.blockNo,
      chainPointId: "48".repeat(32),
      finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
    },
    sourceId: "follower:test",
    previousObservationDigest: null,
    checkpoints: [],
    finalizedQueue: [{ headerHash: null, outRef: `${"41".repeat(32)}#0` }],
    finalizedHeaders: [],
    finalizedCorrectionLock: null,
    correctionLockWitnesses: [],
  };
  return unsafeAdmitWatcherStateQueueObservationForReplayTest({
    ...value,
    observationDigest: watcherSha256CanonicalJson(value),
  });
};

/** A signed intent spending `spent`, valid from `validFrom`; it never lands. */
const intentSpending = (
  spent: string,
  validFrom: number,
): AvailabilityOperationIntent => {
  const [txHash, index] = spent.split("#") as [string, string];
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHash),
      BigInt(index),
    ),
  );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    200_000n,
  );
  body.set_validity_interval_start(BigInt(validFrom));
  body.set_ttl(BigInt(validFrom + 120));
  const signed = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  );
  return {
    id: "open",
    deploymentIdentity: MANIFEST,
    actor: "actor",
    headerHash: "header",
    action: "open",
    signedCbor: signed.to_cbor_hex(),
    txHash: CML.hash_transaction(body).to_hex(),
    spentOutRefs: [spent],
    collateralOutRefs: [],
    expectedOutRefs: [],
    validUntilSlot: validFrom + 120,
    completesWorkflow: false,
  };
};

const intake = (fx: Fixture) =>
  createWatcherAvailabilityObservation({
    identity: {
      manifestId: MANIFEST,
    } as unknown as VerifiedWatcherDeploymentIdentity,
    l1: { reads: fx.reads(), store: fx.store },
    confirmationDepth: 1,
    deployment: {} as SDK.DaAvailabilityDeployment,
  });

describe("availability operation reconcile on a pruned follower store", () => {
  it("reads a signed intent valid only above the pruned window as not landed", async () => {
    const fx = await fixture();
    const prunedThrough = await pruneFixture(fx);
    const found = await intake(fx).operation(
      tipObservation(fx),
      intentSpending(`${fx.hashes.b}#0`, prunedThrough + 1),
    );
    expect(found).toEqual({
      status: "unspent",
      currentSlot: Number(fx.tipPoint().slot),
    });
  });

  it("still refuses an intent that could have landed inside the pruned window", async () => {
    const fx = await fixture();
    const prunedThrough = await pruneFixture(fx);
    await expect(
      intake(fx).operation(
        tipObservation(fx),
        intentSpending(`${fx.hashes.b}#0`, prunedThrough),
      ),
    ).rejects.toThrow(/beyond_retention/u);
  });

  // A#0 was spent and its creating transaction pruned; the second input's
  // creating transaction never reached the store, as when a rollback drops
  // the parent intent whose output a child spends.
  it.each([
    ["spent and pruned", (fx: Fixture) => `${fx.hashes.a}#0`],
    ["never stored", () => `${random(0x77)}#0`],
  ])(
    "reads an input whose row is %s as missing, not as a refusal",
    async (_, spent) => {
      const fx = await fixture();
      const prunedThrough = await pruneFixture(fx);
      const found = await intake(fx).operation(
        tipObservation(fx),
        intentSpending(spent(fx), prunedThrough + 1),
      );
      expect(found).toEqual({
        status: "inputs_missing",
        currentSlot: Number(fx.tipPoint().slot),
        missingOutRefs: [spent(fx)],
      });
    },
  );
});
