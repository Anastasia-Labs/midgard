import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyDistinctAssetAccumulationFinding,
  createDistinctAssetAccumulationWorkflowRunnerSurface,
  type DistinctAssetAccumulationEvidence,
  distinctAssetAccumulationEvidenceCloses,
  type DistinctAssetAccumulationFinding,
  MIDGARD_MAX_DISTINCT_ASSETS,
  nextDistinctAssetAccumulationStage,
  prepareDistinctAssetAccumulationEvidence,
} from "../src/distinct-asset-accumulation-limit/index.js";

const h = (byte: string) => byte.repeat(64);
const accepted = (): VerdictSubject => ({
  version: 1n,
  direction: PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  source_kind: PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  transaction_id: h("1"),
  source_key: "",
  rejection_reason: null,
});
const forced = (
  reason: VerdictSubject["rejection_reason"],
): VerdictSubject => ({
  version: 1n,
  direction: PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  source_kind: PROOF_THREAD_SOURCE_KIND_FORCED,
  transaction_id: h("1"),
  source_key: "01",
  rejection_reason: reason,
});
const finding = (subject = accepted()): DistinctAssetAccumulationFinding => ({
  subject,
  coordinate: { kind: "input", inputIndex: 0, assetIndex: 16_384 },
});
const evidence = (
  subject = accepted(),
  crossing = true,
): DistinctAssetAccumulationEvidence => ({
  finding: finding(subject),
  traceStateHashHex: h("2"),
  workRootHex: h("3"),
  pre: {
    assetRootHex: h("4"),
    seenAssetCount: crossing ? 16_384 : 16_383,
    nonzeroAssetCount: 1,
    cursor: 16_384,
  },
  post: crossing
    ? null
    : {
        assetRootHex: h("5"),
        seenAssetCount: 16_384,
        nonzeroAssetCount: 2,
        cursor: 16_385,
      },
  mutationWasPresent: false,
});

describe("distinctAssetAccumulationLimit V1", () => {
  it("binds every typed coordinate arm", () => {
    expect(
      classifyDistinctAssetAccumulationFinding(finding()).coordinate.kind,
    ).toBe("input");
    expect(
      classifyDistinctAssetAccumulationFinding({
        subject: forced({
          OutputAssetAccumulationLimit: { output_index: 2n, asset_index: 3n },
        }),
        coordinate: { kind: "output", outputIndex: 2, assetIndex: 3 },
      }).coordinate.kind,
    ).toBe("output");
    expect(
      classifyDistinctAssetAccumulationFinding({
        subject: forced({ MintAssetAccumulationLimit: { mint_index: 4n } }),
        coordinate: { kind: "mint", mintIndex: 4 },
      }).coordinate.kind,
    ).toBe("mint");
  });

  it("refuses another constructor and coordinate mutation", () => {
    expect(() =>
      classifyDistinctAssetAccumulationFinding({
        subject: forced({
          OutputAssetAccumulationLimit: { output_index: 2n, asset_index: 4n },
        }),
        coordinate: { kind: "output", outputIndex: 2, assetIndex: 3 },
      }),
    ).toThrow(/coordinate changed/u);
    expect(() =>
      classifyDistinctAssetAccumulationFinding({
        subject: forced({ MintAssetAccumulationLimit: { mint_index: 4n } }),
        coordinate: { kind: "output", outputIndex: 2, assetIndex: 3 },
      }),
    ).toThrow(/coordinate changed/u);
  });

  it("accepts exactly 16,384 and identifies 16,385 as first crossing", () => {
    expect(
      prepareDistinctAssetAccumulationEvidence(evidence()).pre.seenAssetCount,
    ).toBe(MIDGARD_MAX_DISTINCT_ASSETS);
    expect(
      prepareDistinctAssetAccumulationEvidence(evidence(accepted(), false)).post
        ?.seenAssetCount,
    ).toBe(MIDGARD_MAX_DISTINCT_ASSETS);
  });

  it("applies opposite terminal polarity to accepted and forced verdicts", () => {
    expect(distinctAssetAccumulationEvidenceCloses(evidence())).toBe(true);
    const forcedBoundary = forced({
      InputAssetAccumulationLimit: { input_index: 0n, asset_index: 16_384n },
    });
    expect(
      distinctAssetAccumulationEvidenceCloses(evidence(forcedBoundary, false)),
    ).toBe(true);
    expect(
      distinctAssetAccumulationEvidenceCloses(evidence(forcedBoundary, true)),
    ).toBe(false);
  });

  it("refuses a forged crossing successor", () => {
    expect(() =>
      prepareDistinctAssetAccumulationEvidence({
        ...evidence(),
        pre: { ...evidence().pre, seenAssetCount: 16_383 },
      }),
    ).toThrow(/successor/u);
  });

  it("replays physical steps in canonical order", () => {
    const txId = h("a");
    expect(nextDistinctAssetAccumulationStage([])).toBe("step01");
    expect(
      nextDistinctAssetAccumulationStage([
        { sequence: 0, stage: "step01", txId, evidenceIdentity: h("b") },
      ]),
    ).toBe("step02");
    expect(() =>
      nextDistinctAssetAccumulationStage([
        { sequence: 1, stage: "step01", txId, evidenceIdentity: h("b") },
      ]),
    ).toThrow(/continuity/u);
    expect(() =>
      nextDistinctAssetAccumulationStage(
        [{ sequence: 0, stage: "step01", txId, evidenceIdentity: h("b") }],
        h("c"),
      ),
    ).toThrow(/identity/u);
  });

  it("exposes the strict shared-adapter runOrResume surface", () => {
    const runner = createDistinctAssetAccumulationWorkflowRunnerSurface({
      loadRuntimeConfig: async () => {
        throw new Error("loader should only run after adapter admission");
      },
    });
    expect(runner.runnerVersion).toBe(
      "midgard-production-fraud-proof-workflow-runner-v1",
    );
    expect(typeof runner.runOrResume).toBe("function");
  });
});
