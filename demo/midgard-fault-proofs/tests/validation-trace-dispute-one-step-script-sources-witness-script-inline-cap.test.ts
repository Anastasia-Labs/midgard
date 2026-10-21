/** The validation-trace dispute runner at the script-sources stage-zero
 * begin step, which reads the witness script of a failing transaction whose
 * one script is grown a byte of applied parameter at a time, a field the
 * prepare and semantic moves carry whole, inline: at, one byte over, and
 * far over the inline script-sources limit for a witness script
 * (support/validation-trace-dispute-one-step-inline-cap.ts). */
import { readCborBytes } from "@al-ft/midgard-core/codec";
import {
  encodeMidgardVersionedScriptListPreimage,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core/codec/versioned-script";
import { plutusV3ScriptWitness } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { fromHex } from "@lucid-evolution/lucid";

import { VALIDATION_TRACE_SCRIPT_SOURCES_WITNESS_SCRIPT_INLINE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-phase-evidence.js";
import { FAILING_SCRIPT } from "./helpers/plutus-execution-block-fixture.js";
import { applyExpectedScriptParams } from "./support/expected-parameter-application.js";
import { describeInlineCap } from "./support/validation-trace-dispute-one-step-inline-cap.js";

/** The most bytes one padding parameter carries, well within what a source
 * constant may. */
const PADDING_PARAMETER_BYTES = 4_096;

/** The failing script applied to byte strings of `bytes` bytes in all, each
 * of at most `PADDING_PARAMETER_BYTES`: it fails as the failing script does,
 * and its witness is `bytes` and some encoding bytes longer. */
const grown = (bytes: number): MidgardVersionedScript => {
  const params: string[] = [];
  for (let left = bytes; left > 0; left -= PADDING_PARAMETER_BYTES)
    params.push("70".repeat(Math.min(left, PADDING_PARAMETER_BYTES)));
  const applied = applyExpectedScriptParams(
    FAILING_SCRIPT.scriptBytes.toString("hex"),
    params,
  );
  return plutusV3ScriptWitness(
    Buffer.from(readCborBytes(fromHex(applied), 0, "padded script").value),
  );
};

describeInlineCap({
  field: "witness-script field",
  phase: "scriptSources",
  semantic: 5,
  at: "scriptSourcesStageZeroBeginSemantic",
  evidence: () => ({ fieldIndex: 6, itemIndex: 0 }),
  limit:
    VALIDATION_TRACE_SCRIPT_SOURCES_WITNESS_SCRIPT_INLINE_MAX_PREIMAGE_BYTES,
  unit: "padding byte",
  fieldBytes: (bytes) =>
    encodeMidgardVersionedScriptListPreimage([grown(bytes)]).length,
  stepOf: () => 31,
  installs: (bytes) => ({ failingScript: grown(bytes) }),
});
