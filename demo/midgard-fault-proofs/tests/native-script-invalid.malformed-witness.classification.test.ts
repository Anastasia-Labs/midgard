import { afterEach, describe, expect, it } from "vitest";

import { NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  classifyHeader,
  createHeaderClassifier,
} from "../src/workflow/header-classifier.js";
import { INSTALLED_FRAUD_PROOF_CATEGORY_ORDER } from "../src/workflow/installed-categories.js";
import {
  replayBudget,
  setup,
  transport,
} from "./support/catalogue-retained-classification.setup.js";
import { buildRetainedPlutusIdentityFixture } from "./support/retained-reason-classifier.js";

afterEach(() => {
  transport.raw = undefined;
});

// A tag-0 item whose payload is not a native script, beside the transaction's
// Plutus witness.
const malformedNativeFixture = () =>
  buildRetainedPlutusIdentityFixture(
    { verdict: "accepted" },
    { extraScriptWitnesses: [Buffer.from("820043820700", "hex")] },
  );

describe(
  "a malformed native witness item through the installed catalogue",
  replayBudget,
  () => {
    it("classifies the block through all 42 installed categories as fault_detected", async () => {
      const fixture = await malformedNativeFixture();
      const { classifyInput } = await setup(fixture, "normal");
      const decision = await classifyHeader(classifyInput);
      expect(decision).toMatchObject({
        decision: "fault_detected",
        headerHash: fixture.block.headerHash,
        launchScope: INSTALLED_FRAUD_PROOF_CATEGORY_ORDER,
      });
    });

    it("leaves the item to witnessScriptDecoding when nativeScriptInvalid replays alone", async () => {
      const fixture = await malformedNativeFixture();
      const { classifierInput, classifyInput } = await setup(fixture, "normal");
      const { transitionTraceEventAuthority: _route, ...scoped } =
        classifierInput;
      const decision = await classifyHeader({
        ...classifyInput,
        classifier: await createHeaderClassifier({
          ...scoped,
          replayer: NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
        }),
      });
      expect(decision).toMatchObject({
        decision: "healthy",
        headerHash: fixture.block.headerHash,
        launchScope: ["nativeScriptInvalid"],
      });
    });
  },
);

// The same item named by a forced WitnessNativeScriptFalse claim.
const forcedClaimFixture = () =>
  buildRetainedPlutusIdentityFixture(
    {
      verdict: "rejected",
      reason: { WitnessNativeScriptFalse: { script_index: 1n } },
    },
    {
      sourceKind: "forced",
      extraScriptWitnesses: [Buffer.from("820043820700", "hex")],
    },
  );

describe(
  "a forced claim over a witness item that does not decode",
  replayBudget,
  () => {
    it("classifies the block through all 42 installed categories", async () => {
      const fixture = await forcedClaimFixture();
      const { classifyInput } = await setup(fixture, "forced");
      await expect(classifyHeader(classifyInput)).resolves.toMatchObject({
        headerHash: fixture.block.headerHash,
        launchScope: INSTALLED_FRAUD_PROOF_CATEGORY_ORDER,
      });
    });

    it("finds nothing for nativeScriptInvalid to evaluate when it replays alone", async () => {
      const fixture = await forcedClaimFixture();
      const { classifierInput, classifyInput } = await setup(fixture, "forced");
      const { transitionTraceEventAuthority: _route, ...scoped } =
        classifierInput;
      const decision = await classifyHeader({
        ...classifyInput,
        classifier: await createHeaderClassifier({
          ...scoped,
          replayer: NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
        }),
      });
      expect(decision).toMatchObject({
        decision: "healthy",
        headerHash: fixture.block.headerHash,
        launchScope: ["nativeScriptInvalid"],
      });
    });
  },
);
