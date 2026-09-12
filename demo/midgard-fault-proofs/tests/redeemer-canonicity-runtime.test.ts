import { encodeMidgardRedeemerWitnessItem } from "@al-ft/midgard-core";
import { describe, expect, it, vi } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  admitRedeemerWorkflowArtifact,
  prepareRedeemerCanonicityWorkflowArtifact,
} from "../src/redeemer-canonicity/runtime.js";
import {
  createRedeemerCanonicityWorkflowRunnerSurface,
  type LoadedRedeemerCanonicityWorkflow,
} from "../src/redeemer-canonicity/runtime.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

const invocation = (category: string) =>
  ({
    mode: "run",
    category,
    deploymentFingerprint: "11".repeat(32),
    headerHash: "22".repeat(28),
    journalDirectory: "/tmp/redeemer-canonicity-test-journal",
    runtimeConfigPath: "/tmp/redeemer-canonicity-runtime.json",
    decisionDigest: "33".repeat(32),
    actuationPermit: {},
    fundingReservationPermit: {},
  }) as never;

describe("redeemerCanonicity production runner surface", () => {
  it("refuses another category before loading runtime state", async () => {
    const loadRuntimeConfig = vi.fn();
    const runner = createRedeemerCanonicityWorkflowRunnerSurface({
      loadRuntimeConfig,
    });
    await expect(
      runner.runOrResume(invocation("observerOrderInvalid")),
    ).rejects.toThrow(/category mismatch/u);
    expect(loadRuntimeConfig).not.toHaveBeenCalled();
  });

  it("requires retained public DA and always closes its runtime", async () => {
    const close = vi.fn(async () => undefined);
    const loaded = {
      schemaVersion: "midgard-production-fraud-proof-runtime-config-v1",
      workflow: {
        binding: {
          deploymentFingerprint: "11".repeat(32),
          definition: {
            category: "redeemerCanonicity",
            headerHash: "22".repeat(28),
          },
        },
        decisionDigest: "33".repeat(32),
      },
      retainedDaSources: [],
      close,
    } as unknown as LoadedRedeemerCanonicityWorkflow;
    const runner = createRedeemerCanonicityWorkflowRunnerSurface({
      loadRuntimeConfig: async () => loaded,
    });
    await expect(
      runner.runOrResume(invocation("redeemerCanonicity")),
    ).rejects.toThrow(/no public retained-DA source/u);
    expect(close).toHaveBeenCalledOnce();
  });
});

it("prepares and admits restart material from an actual accepted noncanonical redeemer", async () => {
  const item = encodeMidgardRedeemerWitnessItem({
    purpose: "Spend",
    index: 0n,
    redeemerCbor: Buffer.from("1800", "hex"),
    executionUnits: { memory: 1n, steps: 2n },
  });
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(7, 0n)],
        fee: 1n,
        redeemerWitnesses: [item],
      }),
    ],
  });
  const block = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "redeemer-material-fixture",
      grade: "security",
    },
  });
  const artifact = await prepareRedeemerCanonicityWorkflowArtifact(block);
  const admitted = admitRedeemerWorkflowArtifact(artifact);
  expect(admitted.evidence.canonical).toBe(false);
  expect(admitted.accepted?.nativeTxId).toBe(fixture.transactions[0]!.txId);
  expect(admitted.evidence.subject.transaction_id).toBe(
    fixture.transactions[0]!.txId,
  );
  expect(admitted.forced).toBeNull();
  expect(() =>
    admitRedeemerWorkflowArtifact({
      ...artifact,
      fieldCommitmentHex: "00".repeat(32),
    }),
  ).toThrow(/commitment/);
});
