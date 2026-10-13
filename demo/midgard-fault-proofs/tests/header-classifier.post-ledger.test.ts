import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { GENESIS_HEADER_HASH } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { type LedgerAt } from "../src/ledger-at.js";
import { prepareMinAdaTxFromCanonicalEvidence } from "../src/min-ada/prepare.js";
import { retainedPayloadReplaySource } from "../src/retained-payload-replay-source.js";
import { type RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import { MIN_ADA_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
  requireRunnableHeaderFault,
} from "../src/workflow/header-classifier.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthority,
} from "../src/workflow/release-finality-policy.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "./helpers/canonical-block-evidence-fixture.js";
import { fixtureLedgerAt } from "./support/fixture-ledger.js";

const fingerprint = "d1".repeat(32);
const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const finality: FraudProofReleaseFinalityAuthority = {
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: fingerprint,
    blueprintHash: "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  }),
};

const setup = async ({ failFirstRead = false } = {}) => {
  const tx = buildFixtureTransaction({
    spendInputs: [],
    outputs: [
      encodeMidgardTxOutput({
        address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x44)]),
        value: { lovelace: 1n, assets: new Map() },
      }),
    ],
    fee: 0n,
  });
  const block = await buildCanonicalBlockFixture({
    transactions: [tx],
    utxos: [],
    prevHeaderHash: GENESIS_HEADER_HASH,
  });
  const observation = authenticatedHeaderObservation(block);
  const provenance = {
    trustClass: "public_or_permissionless_da",
    sourceId: "one-shot/peer",
    grade: "security",
  } as const;
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation,
    payloadEnvelopeCbor: block.payloadEnvelopeCbor,
    daProvenance: provenance,
  });
  const prepared = await prepareMinAdaTxFromCanonicalEvidence({ evidence });
  expect(prepared.badTxId).toBe(tx.txId);
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: fingerprint,
    replayer: MIN_ADA_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority: finality,
  });
  const authenticatedObservationDigest =
    await authenticatedStateQueueObservationDigest({
      observation,
      minimumConfirmationDepth: policy.confirmationDepth,
    });
  let readCount = 0;
  const source: RetainedDaPayloadSource = {
    sourceId: "one-shot",
    fetchPayloadByHeaderHash: async (headerHash) => {
      expect(headerHash).toBe(block.headerHash);
      readCount++;
      if (
        (failFirstRead && readCount === 1) ||
        readCount > (failFirstRead ? 2 : 1)
      )
        return {
          ok: false,
          sourceId: "one-shot",
          attempts: [
            {
              sourceId: "one-shot",
              sourcePeerId: "peer",
              protocol: DaRequestResponseProtocol.payloadByHeader,
              status:
                failFirstRead && readCount === 1 ? "timeout" : "not_found",
              detail: "withheld request",
            },
          ],
        };
      return {
        ok: true,
        sourceId: "one-shot",
        sourcePeerId: "peer",
        payloadEnvelopeCbor: block.payloadEnvelopeCbor,
        provenance,
        attempts: [],
      };
    },
  };
  const classify = async (externalLedgerAt?: LedgerAt) => {
    // The production application creates this source for each classification.
    const replaySource = retainedPayloadReplaySource({ sources: [source] });
    return await classifyHeader({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: replaySource.sources,
      predecessorLedgerAt:
        externalLedgerAt ?? fixtureLedgerAt({ sources: replaySource.sources }),
    });
  };
  return { classify, readCount: () => readCount };
};

describe("classification shares its retained payload with the post ledger", () => {
  it("keeps a runnable transaction fault when a peer serves the current payload only once", async () => {
    const h = await setup();
    const decision = await h.classify();
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "minAda",
      violationId: "min-ada",
    });
    expect(requireRunnableHeaderFault(decision).category).toBe("minAda");
    expect(h.readCount()).toBe(1);
  });

  it("retries a failed read before capturing the successful payload", async () => {
    const h = await setup({ failFirstRead: true });
    expect(await h.classify()).toMatchObject({
      decision: "fault_detected",
      category: "minAda",
    });
    expect(h.readCount()).toBe(2);
  });

  it("starts a fresh retained snapshot for the next classification", async () => {
    const h = await setup();
    expect(await h.classify()).toMatchObject({
      decision: "fault_detected",
      category: "minAda",
    });
    await expect(h.classify()).rejects.toThrow(
      "Unable to fetch retained DA payload",
    );
    expect(h.readCount()).toBe(3);
  });

  it("still fails closed on an independently supplied unavailable post ledger", async () => {
    const h = await setup();
    expect(await h.classify(async () => undefined)).toMatchObject({
      decision: "unprovable",
      violationId: "authenticated-post-ledger-context-unavailable",
    });
  });
});
