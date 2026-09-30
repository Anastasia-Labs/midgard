import "./protected-output-signer-missing-lifecycle.registered-contracts.js";

import { type MidgardNativeTxFull } from "@al-ft/midgard-core";
import { type MidgardForcedTxFull } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";

import {
  type ProtectedOutputSignerMissingEvidence,
  submitProtectedOutputSignerMissingStep03,
} from "../src/protected-output-signer-missing/index.js";
import { makeScenario } from "./protected-output-signer-missing-lifecycle.make-scenario.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";

type Scenario = Awaited<ReturnType<typeof makeScenario>>;

export const scanToTerminal = async (
  s: Scenario,
  threadOutRef: string,
  evidence: ProtectedOutputSignerMissingEvidence,
  subject: MidgardNativeTxFull | MidgardForcedTxFull,
  carriage: Awaited<
    ReturnType<typeof submitProtectedOutputSignerMissingStep03>
  >,
) => {
  let cursor = threadOutRef;
  for (;;) {
    const result = await s.step04(cursor, evidence, subject, carriage);
    cursor = result.nextThreadOutRef;
    if (result.terminal) return cursor;
  }
};

/** Swaps the certificate slot with a chunk slot in a tier-3 opening. */
export const withSwappedCertificateSlot = (
  opening: SDK.FieldOpening,
): SDK.FieldOpening => {
  const copy = structuredClone(opening) as Record<string, unknown>;
  const variant = Object.values(copy)[0] as Record<string, unknown>;
  const carriage = variant.carriage as Record<string, unknown>;
  const certified = carriage.Certified as
    | { cert_ref_input_index: bigint; chunk_ref_input_indices: bigint[] }
    | undefined;
  if (certified === undefined)
    throw new Error("expected a Certified carriage to mutate");
  const [firstChunk, ...rest] = certified.chunk_ref_input_indices;
  if (firstChunk === undefined)
    throw new Error("certified carriage has no chunk");
  certified.chunk_ref_input_indices = [certified.cert_ref_input_index, ...rest];
  certified.cert_ref_input_index = firstChunk;
  return copy as SDK.FieldOpening;
};

export const measuredFit = createMeasuredFitRecorder(
  "protected-output-signer-missing",
  "lifecycle",
  "318 address witnesses in a three-chunk certified field 7; forced exact-reason and direct-terminal paths",
);
