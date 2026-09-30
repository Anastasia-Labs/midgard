import { type UTxO } from "@lucid-evolution/lucid";

import {
  type ExecutionSourceContext,
  type MeasurementRecorder,
} from "./execution-source-script-decoding-emulator.build-canonical-trace.js";
import { publishPlainReferenceScriptUtxo } from "./submit-init-emulator-shared.js";

// ## Stages

export const publishFamilyReferences = async (
  { harness, validators }: ExecutionSourceContext,
  recorder: MeasurementRecorder,
  label: string,
) => {
  const references: UTxO[] = [];
  for (const [index, step] of validators.entries()) {
    const published = await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: step.spendingScript,
      label: `${label}-${index.toString()}`,
    });
    recorder.recordPublication(index, published.publicationMeasurement);
    references.push(published.utxo);
  }
  return references;
};
