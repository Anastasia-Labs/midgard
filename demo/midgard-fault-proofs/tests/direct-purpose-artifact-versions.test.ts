/** Every direct purpose family journals its artifact under the version it
 * has always written, and admits no other family's. */
import { describe, expect, it } from "vitest";

import { admitDirectPurposeArtifact } from "../src/direct-purpose-family/artifact.js";
import type { DirectPurposeReplay } from "../src/direct-purpose-family/artifact-family.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_ARTIFACT_FAMILY } from "../src/distinct-asset-accumulation-limit/replay.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_ARTIFACT_FAMILY } from "../src/execution-native-script-invalid/replay.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_ARTIFACT_FAMILY } from "../src/execution-source-script-decoding/replay.js";
import { MISSING_REDEEMER_ARTIFACT_FAMILY } from "../src/missing-redeemer/replay.js";
import { MISSING_SCRIPT_SOURCE_ARTIFACT_FAMILY } from "../src/missing-script-source/replay.js";
import { RECEIVE_PURPOSE_LANGUAGE_ARTIFACT_FAMILY } from "../src/receive-purpose-language/replay.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_ARTIFACT_FAMILY } from "../src/script-integrity-hash-mismatch/replay.js";
import { UNUSED_REDEEMER_ARTIFACT_FAMILY } from "../src/unused-redeemer/replay.js";
import { UNUSED_SCRIPT_WITNESS_ARTIFACT_FAMILY } from "../src/unused-script-witness/replay.js";

const FAMILIES = [
  [
    "missingRedeemer",
    "midgard-missing-redeemer-workflow-artifact-v1",
    MISSING_REDEEMER_ARTIFACT_FAMILY,
  ],
  [
    "missingScriptSource",
    "midgard-missing-script-source-workflow-artifact-v1",
    MISSING_SCRIPT_SOURCE_ARTIFACT_FAMILY,
  ],
  [
    "unusedRedeemer",
    "midgard-unused-redeemer-workflow-artifact-v1",
    UNUSED_REDEEMER_ARTIFACT_FAMILY,
  ],
  [
    "receivePurposeLanguage",
    "midgard-receive-purpose-language-workflow-artifact-v1",
    RECEIVE_PURPOSE_LANGUAGE_ARTIFACT_FAMILY,
  ],
  [
    "executionSourceScriptDecoding",
    "midgard-execution-source-script-decoding-workflow-artifact-v1",
    EXECUTION_SOURCE_SCRIPT_DECODING_ARTIFACT_FAMILY,
  ],
  [
    "distinctAssetAccumulationLimit",
    "midgard-distinct-asset-accumulation-limit-workflow-artifact-v1",
    DISTINCT_ASSET_ACCUMULATION_LIMIT_ARTIFACT_FAMILY,
  ],
  [
    "executionNativeScriptInvalid",
    "midgard-execution-native-script-invalid-workflow-artifact-v1",
    EXECUTION_NATIVE_SCRIPT_INVALID_ARTIFACT_FAMILY,
  ],
  [
    "scriptIntegrityHashMismatch",
    "midgard-script-integrity-hash-mismatch-workflow-artifact-v1",
    SCRIPT_INTEGRITY_HASH_MISMATCH_ARTIFACT_FAMILY,
  ],
  [
    "unusedScriptWitness",
    "midgard-unused-script-witness-workflow-artifact-v1",
    UNUSED_SCRIPT_WITNESS_ARTIFACT_FAMILY,
  ],
] as const;

describe.each(FAMILIES)("the %s artifact", (label, version, family) => {
  it("keeps its category and journaled version", () => {
    expect(family.label).toBe(label);
    expect(family.artifactVersion).toBe(version);
  });

  it("refuses an artifact journaled under another family's version", async () => {
    const other = FAMILIES.find(([, v]) => v !== version)![1];
    await expect(
      admitDirectPurposeArtifact<DirectPurposeReplay>(family, {
        schemaVersion: other,
        headerHash: "00",
        detectionId: "",
        coordinate: { sourceKind: 0, sourceIndex: 0 },
        payloadEnvelopeCbor: "00",
        predecessorLedger: null,
      }),
    ).rejects.toThrow(`${label} artifact shape changed`);
  });
});
