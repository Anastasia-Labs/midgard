import { encodeCbor, encodeMidgardTxOutput } from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/index.js";
import {
  deriveOutputReferenceScriptDecodingEvidenceFromCanonicalBlock,
  prepareOutputReferenceScriptDecodingRecoveryMaterial,
} from "../src/output-reference-script-decoding/authenticated-workflow.derive-output-reference-script-decoding-authenticated-source.js";
import { prepareWitnessScriptDecodingRecoveryMaterial } from "../src/witness-script-decoding/workflow.create-witness-script-decoding-recovery-ports.js";
import { deriveWitnessScriptDecodingEvidenceFromCanonicalBlock } from "../src/witness-script-decoding/workflow.derive-witness-script-decoding-authenticated-source.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import { completeReplayer } from "../src/workflow/complete-replay.detect-double-withdraws.js";
import {
  OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  classifyHeader,
  createHeaderClassifier,
} from "../src/workflow/header-classifier.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  replayBudget,
  setup,
  transport,
} from "./support/catalogue-retained-classification.setup.js";
import {
  outputWithPlutusScript,
  outputWithRawNativePayload,
} from "./support/output-reference-script-decoding-emulator.js";
import { buildRetainedPlutusIdentityFixture } from "./support/retained-reason-classifier.js";

afterEach(() => {
  transport.raw = undefined;
});

/** Tag-0 field-6 items whose payloads are not native scripts. */
const MALFORMED_NATIVE_ITEMS = [
  Buffer.from("820043820700", "hex"),
  Buffer.from("820043820800", "hex"),
];
/** Outputs whose tag-0 reference scripts are not native scripts. */
const malformedNativeOutputs = () => [
  outputWithRawNativePayload(Buffer.from("820700", "hex")),
  outputWithRawNativePayload(Buffer.from("820800", "hex")),
];

/** A tag-0 field-6 item carrying the native script `all []`. */
const HONEST_NATIVE_ITEM = Buffer.from("820043820180", "hex");
/** A PlutusV3 field-6 item whose bytes are not a program. */
const UNDECODABLE_PLUTUS_ITEM = Buffer.from("82034401020304", "hex");
/** A PlutusV3 reference script whose bytes are not a program. */
const undecodablePlutusOutput = () =>
  outputWithPlutusScript(Buffer.from("4401020304", "hex"));

const DA_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "libp2p/peer-a",
  grade: "security",
};

/** An honest identity program's Cardano bytes as a created output's reference script. */
const plutusReferenceOutput = () =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x11)]),
    value: { lovelace: 2_000_000n, assets: new Map() },
    script_ref: {
      language: "PlutusV3",
      scriptBytes: encodeCbor(Buffer.from("0101002324980041", "hex")),
    },
  });

/**
 * A family's classification of one accepted transaction built from
 * `transaction`, with the authenticated block evidence it read.
 */
const classifyTransaction = async (
  family:
    | typeof WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY
    | typeof OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  transaction: Omit<Parameters<typeof buildFixtureTransaction>[0], "fee">,
) => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [buildFixtureTransaction({ ...transaction, fee: 1n })],
  });
  const canonical = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: DA_PROVENANCE,
  });
  const classification = await classifyCanonicalBlockViolations({
    evidence: canonical,
    detections: (await family.replay(canonical)).detections,
  });
  if (classification.decision !== "fault_detected")
    throw new Error(`expected a fault, got ${classification.decision}`);
  return { selected: classification.selected, canonical };
};

/**
 * The output-reference family's classification of one accepted transaction
 * creating `outputs`, with the authenticated block evidence it read.
 */
const classifyOutputs = (outputs: readonly Buffer[]) =>
  classifyTransaction(
    OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    { spendInputs: [], outputs },
  );

type Fixture = Awaited<ReturnType<typeof buildRetainedPlutusIdentityFixture>>;

/**
 * The family's classifier decision over the fixture, with the authenticated
 * block evidence it replayed.
 */
const classifyFamily = async (
  fixture: Fixture,
  family:
    | typeof WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY
    | typeof OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
) => {
  const { classifierInput, classifyInput } = await setup(fixture, "normal");
  const { transitionTraceEventAuthority: _route, ...scoped } = classifierInput;
  let canonical: CanonicalBlockEvidence | undefined;
  const replayer = completeReplayer(
    family.launchScope,
    async (evidence, context) => {
      canonical = evidence;
      return (await family.replay(evidence, context)).detections;
    },
  );
  const decision = await classifyHeader({
    ...classifyInput,
    classifier: await createHeaderClassifier({ ...scoped, replayer }),
  });
  if (decision.decision !== "fault_detected" || canonical === undefined)
    throw new Error(`expected a fault, got ${decision.decision}`);
  return { decision, canonical };
};

describe(
  "the script-decoding workflows prove the classifier's first finding",
  replayBudget,
  () => {
    it("proves the native witness fault beside an honest Plutus witness", async () => {
      const fixture = await buildRetainedPlutusIdentityFixture(
        { verdict: "accepted" },
        { extraScriptWitnesses: MALFORMED_NATIVE_ITEMS.slice(0, 1) },
      );
      const { decision, canonical } = await classifyFamily(
        fixture,
        WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      );
      expect(decision.violationId).toBe("witness-native-script-malformed");
      const material = await prepareWitnessScriptDecodingRecoveryMaterial(
        canonical,
        decision.detectionId,
      );
      expect(material.evidence.finding.scriptIndex).toBe(1);
      expect(decision.detectionId.split(":")[3]).toBe("1");
    });

    it("proves the first of two native witness faults beside an honest Plutus witness", async () => {
      const fixture = await buildRetainedPlutusIdentityFixture(
        { verdict: "accepted" },
        { extraScriptWitnesses: MALFORMED_NATIVE_ITEMS },
      );
      const { decision, canonical } = await classifyFamily(
        fixture,
        WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      );
      const material = await prepareWitnessScriptDecodingRecoveryMaterial(
        canonical,
        decision.detectionId,
      );
      expect(material.evidence.finding.scriptIndex).toBe(1);
      expect(decision.detectionId.split(":")[3]).toBe("1");
    });

    it("proves the native reference-script fault beside an honest Plutus reference script", async () => {
      const { selected, canonical } = await classifyOutputs([
        plutusReferenceOutput(),
        ...malformedNativeOutputs().slice(0, 1),
      ]);
      expect(selected.violationId).toBe("output-reference-script-malformed");
      const material =
        await prepareOutputReferenceScriptDecodingRecoveryMaterial(
          canonical,
          selected.detectionId,
        );
      expect(material.evidence.outputIndex).toBe(1);
      expect(selected.detectionId.split(":")[4]).toBe("1");
    });

    it("proves the first of two native reference-script faults beside an honest Plutus reference script", async () => {
      const { selected, canonical } = await classifyOutputs([
        plutusReferenceOutput(),
        ...malformedNativeOutputs(),
      ]);
      const material =
        await prepareOutputReferenceScriptDecodingRecoveryMaterial(
          canonical,
          selected.detectionId,
        );
      expect(material.evidence.outputIndex).toBe(1);
      expect(selected.detectionId.split(":")[4]).toBe("1");
    });

    it("proves an undecodable Plutus witness beside an honest native witness", async () => {
      const { selected, canonical } = await classifyTransaction(
        WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
        {
          spendInputs: [],
          scriptWitnesses: [HONEST_NATIVE_ITEM, UNDECODABLE_PLUTUS_ITEM],
        },
      );
      expect(selected.violationId).toBe("witness-plutus-script-undecodable");
      expect(selected.detectionId.split(":")[3]).toBe("1");
      const material = await prepareWitnessScriptDecodingRecoveryMaterial(
        canonical,
        selected.detectionId,
      );
      expect(material.evidence.finding.scriptIndex).toBe(1);
      expect(
        deriveWitnessScriptDecodingEvidenceFromCanonicalBlock(canonical),
      ).toEqual(material.evidence);
    });

    it("proves an undecodable Plutus reference script beside an honest Plutus reference script", async () => {
      const { selected, canonical } = await classifyOutputs([
        plutusReferenceOutput(),
        undecodablePlutusOutput(),
      ]);
      expect(selected.violationId).toBe(
        "output-reference-plutus-script-undecodable",
      );
      expect(selected.detectionId.split(":")[4]).toBe("1");
      const material =
        await prepareOutputReferenceScriptDecodingRecoveryMaterial(
          canonical,
          selected.detectionId,
        );
      expect(material.evidence.outputIndex).toBe(1);
      expect(
        deriveOutputReferenceScriptDecodingEvidenceFromCanonicalBlock(
          canonical,
        ),
      ).toEqual(material.evidence);
    });
  },
);
