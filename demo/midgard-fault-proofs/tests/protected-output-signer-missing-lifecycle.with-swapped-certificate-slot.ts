import "./protected-output-signer-missing-lifecycle.registered-contracts.js";

import { type MidgardNativeTxFull } from "@al-ft/midgard-core";
import { type MidgardForcedTxFull } from "@al-ft/midgard-core/codec/forced";
import type * as SDK from "@al-ft/midgard-sdk/fraud-proof";

import { type ProtectedOutputSignerMissingEvidence } from "../src/protected-output-signer-missing/index.js";
import { makeScenario } from "./protected-output-signer-missing-lifecycle.make-scenario.js";

type Scenario = Awaited<ReturnType<typeof makeScenario>>;

/**
 * Runs the shared scan from its opening turn to the terminal. The opening
 * turn publishes (and, for a certified field, certifies) the carriage; every
 * resumed turn reuses it. Returns the terminal thread and the turn count.
 */
export const scanToTerminal = async (
  s: Scenario,
  threadOutRef: string,
  evidence: ProtectedOutputSignerMissingEvidence,
  subject: MidgardNativeTxFull | MidgardForcedTxFull,
) => {
  const opening = await s.scan(threadOutRef, evidence, subject);
  let cursor = opening.nextThreadOutRef;
  let turns = 1;
  let terminal = opening.terminal;
  while (!terminal) {
    const result = await s.scan(cursor, evidence, subject, opening);
    cursor = result.nextThreadOutRef;
    terminal = result.terminal;
    turns += 1;
  }
  return { threadOutRef: cursor, turns };
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
