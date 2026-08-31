import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import {
  COMPLETE_CANONICAL_REPLAY_V1,
  type CompleteCanonicalReplayV1,
  createCompleteCanonicalReplayUnionV1,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
  NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1,
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
} from "../src/workflow/complete-replay-v1.js";
import {
  authenticatedStateQueueObservationDigestV1,
  classifyProductionHeaderV1,
  createProductionHeaderClassifierV1,
  productionHeaderDecisionReplayContextV1,
  type ProductionHeaderDecisionV1,
  requireRunnableProductionHeaderFaultV1,
} from "../src/workflow/production-header-classifier-v1.js";
import {
  computeFraudProofReleaseFinalityPolicyDigestV1,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY_V1,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthorityV1,
} from "../src/workflow/release-finality-policy-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const DEPLOYMENT_FINGERPRINT = "d1".repeat(32);
const RELEASE_FINALITY_POLICY = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;

const finalityAuthority = (): FraudProofReleaseFinalityAuthorityV1 => ({
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY_V1,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
    deploymentIdentityDigest: DEPLOYMENT_FINGERPRINT,
    releaseIdentityDigest: "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigestV1(
      RELEASE_FINALITY_POLICY,
    ),
    policy: RELEASE_FINALITY_POLICY,
  }),
});

const retainedDaSource = ({
  payloadEnvelopeCbor,
  fetched,
}: {
  readonly payloadEnvelopeCbor: Buffer;
  readonly fetched: { count: number };
}): RetainedDaPayloadSource => ({
  sourceId: "libp2p",
  fetchPayloadByHeaderHash: async () => {
    fetched.count += 1;
    return {
      ok: true,
      provenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/peer-a",
        grade: "security",
      },
      sourceId: "libp2p",
      sourcePeerId: "peer-a",
      payloadEnvelopeCbor,
      attempts: [],
    };
  },
});

describe("production authenticated-header classifier V1", () => {
  it("admits only closed replay unions in canonical catalogue order", async () => {
    expect(() =>
      createCompleteCanonicalReplayUnionV1([
        NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1,
        DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
      ]),
    ).toThrow("canonical catalogue order");

    const forged: CompleteCanonicalReplayV1 = {
      replayVersion: COMPLETE_CANONICAL_REPLAY_V1,
      launchScope: ["doubleSpend"],
      replay: async () => {
        throw new Error("must not run");
      },
    };
    await expect(
      createProductionHeaderClassifierV1({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        replayer: forged,
        releaseFinalityAuthority: finalityAuthority(),
      }),
    ).rejects.toThrow("closed canonical replay bundle");
  });

  it("fetches public DA once and mints an opaque exact fault selection", async () => {
    const sharedInput = outRefCbor(71, 0n);
    const fixture = await buildCanonicalBlockFixtureV1({
      transactions: [
        buildFixtureTransactionV1({ spendInputs: [sharedInput], fee: 1n }),
        buildFixtureTransactionV1({ spendInputs: [sharedInput], fee: 2n }),
      ],
    });
    const observation = authenticatedHeaderObservationV1(fixture);
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigestV1({
        observation,
        minimumConfirmationDepth: 30,
      });
    const fetched = { count: 0 };
    const decision = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [
        retainedDaSource({
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          fetched,
        }),
      ],
    });
    expect(fetched.count).toBe(1);
    expect(decision).toMatchObject({
      decision: "fault_detected",
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      headerHash: fixture.headerHash,
      category: "doubleSpend",
      violationId: "double-spend",
      position: "1",
      authenticatedObservationDigest,
    });
    expect(decision.decisionDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(requireRunnableProductionHeaderFaultV1(decision)).toBe(decision);
  });

  it("keeps healthy decisions non-runnable and rejects structural forgeries", async () => {
    const fixture = await buildCanonicalBlockFixtureV1({ transactions: [] });
    const observation = authenticatedHeaderObservationV1(fixture);
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigestV1({
        observation,
        minimumConfirmationDepth: 30,
      });
    const decision = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [
        retainedDaSource({
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          fetched: { count: 0 },
        }),
      ],
    });
    expect(decision.decision).toBe("healthy");
    expect(() => requireRunnableProductionHeaderFaultV1(decision)).toThrow(
      "only fault_detected",
    );
    expect(() =>
      requireRunnableProductionHeaderFaultV1({
        ...decision,
        decision: "fault_detected",
        category: "doubleSpend",
        violationId: "double-spend",
        detectionId: "forged",
        position: "0",
      } as ProductionHeaderDecisionV1),
    ).toThrow("not module-admitted");
  });

  it("rejects an L1 source digest substitution before retained-DA I/O", async () => {
    const fixture = await buildCanonicalBlockFixtureV1({ transactions: [] });
    const observation = authenticatedHeaderObservationV1(fixture);
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const fetched = { count: 0 };
    await expect(
      classifyProductionHeaderV1({
        classifier,
        observation,
        authenticatedObservationDigest: "00".repeat(32),
        sources: [
          retainedDaSource({
            payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
            fetched,
          }),
        ],
      }),
    ).rejects.toThrow("observation digest differs");
    expect(fetched.count).toBe(0);
  });

  it("returns unprovable without predecessor authority and admits a freshly fetched predecessor", async () => {
    const predecessor = await buildCanonicalBlockFixtureV1({
      transactions: [],
      utxos: [
        {
          key: outRefCbor(65, 0n),
          value: encodeMidgardTxOutput({
            address: Buffer.concat([
              Buffer.from([0x60]),
              Buffer.alloc(28, 0x42),
            ]),
            value: { lovelace: 2_000_000n, assets: new Map() },
          }),
        },
      ],
    });
    const current = await buildCanonicalBlockFixtureV1({
      transactions: [],
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: predecessor.header.utxosRoot,
    });
    const observation = authenticatedHeaderObservationV1(current);
    const predecessorObservation = authenticatedHeaderObservationV1(
      predecessor,
      {
        chainPoint: { slot: 4241n, blockHash: "06".repeat(32) },
      },
    );
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigestV1({
        observation,
        minimumConfirmationDepth: 30,
      });
    const payloads = new Map([
      [current.headerHash, current.payloadEnvelopeCbor],
      [predecessor.headerHash, predecessor.payloadEnvelopeCbor],
    ]);
    const fetched = { count: 0 };
    const source: RetainedDaPayloadSource = {
      sourceId: "libp2p",
      fetchPayloadByHeaderHash: async (headerHash) => {
        fetched.count += 1;
        const payloadEnvelopeCbor = payloads.get(headerHash);
        if (payloadEnvelopeCbor === undefined) {
          return { ok: false, sourceId: "libp2p", attempts: [] };
        }
        return {
          ok: true,
          provenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "libp2p/peer-a",
            grade: "security",
          },
          sourceId: "libp2p",
          sourcePeerId: "peer-a",
          payloadEnvelopeCbor,
          attempts: [],
        };
      },
    };

    const unavailable = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [source],
    });
    expect(unavailable).toMatchObject({
      decision: "unprovable",
      reason: "predecessor_context_unavailable",
    });
    expect(
      productionHeaderDecisionReplayContextV1(unavailable),
    ).toBeUndefined();
    expect(fetched.count).toBe(1);

    const admitted = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [source],
      predecessorObservation,
    });
    expect(admitted.decision).toBe("healthy");
    expect(
      productionHeaderDecisionReplayContextV1(admitted)?.predecessor,
    ).toBeDefined();
    expect(fetched.count).toBe(3);
  });
});
