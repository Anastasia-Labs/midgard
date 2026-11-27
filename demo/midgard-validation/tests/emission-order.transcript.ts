import { computeHash32 } from "@al-ft/midgard-core/codec";

import type { DeterministicValidationMachineTrace } from "../src/index.js";
import { encodeValidationAuxiliaryWitnessCbor } from "../src/validation-machine-data.js";

const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

/** Each phase the trace runs, in order, with its consecutive step count. */
const phaseRuns = (trace: DeterministicValidationMachineTrace) => {
  const runs: { phase: string; steps: number }[] = [];
  for (const { phase } of trace.witnesses) {
    const last = runs.at(-1);
    if (last?.phase === phase) last.steps += 1;
    else runs.push({ phase, steps: 1 });
  }
  return runs.map(({ phase, steps }) => `${phase} ${steps.toString()}`);
};

/**
 * One row's whole trace in the validation trace transcript
 * (`tests/fixtures/validation-trace-transcript-v1.json`). The trace root
 * commits every state, and each state its work witness. The witness digest
 * commits every work witness and every auxiliary witness as the one-step
 * evidence encodes it, which the root does not reach. Together with the
 * context digest and the ledger operations they pin the whole trace byte for
 * byte; `phases` points a moved byte at the phase that moved it.
 */
export const transcriptRow = (
  name: string,
  trace: DeterministicValidationMachineTrace,
) => ({
  name,
  verdict: trace.verdict,
  rejectionCode: trace.rejectionCode,
  rejectionReason: JSON.parse(
    JSON.stringify(trace.rejectionReason, (_key, value: unknown) =>
      typeof value === "bigint" ? value.toString() : value,
    ),
  ) as unknown,
  stepCount: trace.tree.descriptor.stepCount,
  phases: phaseRuns(trace),
  traceRoot: hex(trace.tree.descriptor.traceRoot),
  witnessDigest: hex(
    computeHash32(
      Buffer.concat(
        trace.witnesses.flatMap((witness) => [
          witness.cbor,
          encodeValidationAuxiliaryWitnessCbor(witness.auxiliary),
        ]),
      ),
    ),
  ),
  validationContextDigest: hex(computeHash32(trace.validationContextCbor)),
  ledgerOps: trace.ledgerOps.map((op) =>
    op.type === "delete"
      ? { delete: hex(op.key) }
      : { insert: hex(op.key), value: hex(op.value) },
  ),
});
