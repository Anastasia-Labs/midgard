import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import { INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1 } from "../src/workflow/complete-replay-v1.js";
import { completeCanonicalReplayDecisionDigestV1 } from "../src/workflow/complete-replay-v1.js";
import {
  admitProductionAuthenticatedReplayCaptureIdentityV1,
  admitProductionRawPredecessorContextV1,
  PRODUCTION_AUTHENTICATED_REPLAY_CAPTURE_PORT_V1,
  PRODUCTION_AUTHENTICATED_REPLAY_CAPTURE_V1,
  requireProductionAuthenticatedReplayCapturePortV1,
} from "../src/workflow/production-authenticated-replay-artifact-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const h32 = (byte: string): string => byte.repeat(32);
const context = async () => {
  const fixture = await buildCanonicalBlockFixtureV1({
    transactions: [
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(12, 0n)],
        fee: 1n,
        validityIntervalStart: 1n,
      }),
    ],
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/replay-capture-test",
      grade: "security",
    },
  });
  const replayDecision =
    await INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
  const detection = replayDecision.detections[0];
  if (detection === undefined) throw new Error("fixture did not detect fault");
  const identity = {
    schemaVersion: PRODUCTION_AUTHENTICATED_REPLAY_CAPTURE_V1,
    deploymentFingerprint: h32("44"),
    headerHash: evidence.headerHash,
    stateQueueObservationDigest: h32("55"),
    payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    payloadSha256: evidence.payloadSha256,
    replayVersion: replayDecision.replayVersion,
    replayDigest: completeCanonicalReplayDecisionDigestV1({
      evidence,
      replayer: INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1,
      decision: replayDecision,
    }),
    position: detection.position.toString(),
    detectionId: detection.detectionId,
  } as const;
  const admit = (value: unknown, selected = detection) =>
    admitProductionAuthenticatedReplayCaptureIdentityV1({
      value,
      evidence,
      replayer: INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1,
      replayDecision,
      detection: selected,
      deploymentFingerprint: identity.deploymentFingerprint,
      stateQueueObservationDigest: identity.stateQueueObservationDigest,
    });
  return { admit, detection, evidence, identity };
};

describe("production authenticated replay capture boundary V1", () => {
  it("re-admits exact predecessor bytes and both challenged-header links", async () => {
    const predecessorFixture = await buildCanonicalBlockFixtureV1({
      transactions: [],
    });
    const predecessor = await canonicalBlockEvidenceFromVerifiedPayloadV1({
      observation: authenticatedHeaderObservationV1(predecessorFixture),
      payloadEnvelopeCbor: predecessorFixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/predecessor-capture-test",
        grade: "security",
      },
    });
    const currentBase = (await context()).evidence;
    const current = {
      ...currentBase,
      header: {
        ...currentBase.header,
        prevHeaderHash: predecessor.headerHash,
        prevUtxosRoot: predecessor.header.utxosRoot,
      },
    };
    const raw = {
      observation: authenticatedHeaderObservationV1(predecessorFixture),
      payloadEnvelopeCborHex:
        predecessorFixture.payloadEnvelopeCbor.toString("hex"),
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/predecessor-capture-test",
        grade: "security",
      },
    } as const;
    await expect(
      admitProductionRawPredecessorContextV1({
        value: raw,
        currentEvidence: current,
        minimumConfirmationDepth: 30,
      }),
    ).resolves.toMatchObject({ headerHash: predecessor.headerHash });
    await expect(
      admitProductionRawPredecessorContextV1({
        value: raw,
        currentEvidence: {
          ...current,
          header: { ...current.header, prevHeaderHash: "ff".repeat(28) },
        },
        minimumConfirmationDepth: 30,
      }),
    ).rejects.toThrow("prev_header_hash and prev_utxos_root");
    await expect(
      admitProductionRawPredecessorContextV1({
        value: { ...raw, payloadEnvelopeCborHex: "0" },
        currentEvidence: current,
        minimumConfirmationDepth: 30,
      }),
    ).rejects.toThrow("canonical lowercase byte hex");
  });

  it("admits the exact pre-classification identity from a module-admitted replay", async () => {
    const { admit, identity } = await context();
    const admitted = admit(identity);
    expect(admitted).toEqual(identity);
    expect(Object.isFrozen(admitted)).toBe(true);
    expect("category" in admitted).toBe(false);
    expect("decisionDigest" in admitted).toBe(false);
  });

  it("rejects unknown fields and every fetched-evidence identity substitution", async () => {
    const { admit, identity } = await context();
    expect(() => admit({ ...identity, category: "mintAuthorization" })).toThrow(
      "missing or unknown fields",
    );
    for (const [field, value] of [
      ["deploymentFingerprint", h32("aa")],
      ["headerHash", "bb".repeat(28)],
      ["stateQueueObservationDigest", h32("cc")],
      ["payloadEnvelopeSha256", h32("dd")],
      ["payloadSha256", h32("ee")],
      ["replayVersion", "partial-replay"],
      ["replayDigest", h32("ff")],
      ["detectionId", "caller-route"],
      ["position", "8"],
    ] as const) {
      expect(() => admit({ ...identity, [field]: value })).toThrow();
    }
  });

  it("rejects a structural detection clone that was not emitted by the admitted replay", async () => {
    const { admit, detection, identity } = await context();
    expect(() => admit(identity, { ...detection })).toThrow(
      "was not selected from the admitted replay decision",
    );
  });

  it("rejects missing and category-substituted capture ports before I/O", () => {
    expect(() =>
      requireProductionAuthenticatedReplayCapturePortV1({
        category: "valueNotPreserved",
        port: null,
      }),
    ).toThrow("requires its exact authenticated replay capture port");
    expect(() =>
      requireProductionAuthenticatedReplayCapturePortV1({
        category: "valueNotPreserved",
        port: {
          portVersion: PRODUCTION_AUTHENTICATED_REPLAY_CAPTURE_PORT_V1,
          category: "mintAuthorization",
          capture: async () => ({}),
        } as never,
      }),
    ).toThrow("requires its exact authenticated replay capture port");
  });
});
