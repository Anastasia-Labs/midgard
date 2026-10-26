/** Plans a whole `executionSourceScriptDecoding` proof: step 01 binds the
 * leaf, the event selection hands its selected event to the terminal, and the
 * terminal convicts a forced rejection whose recorded reason is a native
 * script decoding arm. An honest leaf is refused before any transaction is
 * spent. */
import * as SDK from "@al-ft/midgard-sdk";

import type { DirectPurposeTransition } from "../direct-purpose-family/custody.js";
import { directPurposeInputDescriptorsPresent } from "../direct-purpose-family/script-purposes.js";
import { directPurposeBindTransitions } from "../direct-purpose-family/source-walk.js";
import { directBindingStates } from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import { CanonicalReplayPrerequisiteError } from "../workflow/replay-prerequisite.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_CATEGORY_LABEL } from "./contracts.js";

export class ExecutionSourceScriptDecodingHonestLeaf extends Error {
  constructor(message: string) {
    super(`${EXECUTION_SOURCE_SCRIPT_DECODING_CATEGORY_LABEL}: ${message}`);
    this.name = "ExecutionSourceScriptDecodingHonestLeaf";
  }
}

export type ExecutionSourceScriptDecodingProofPlan = {
  /** The one custody transition between step 01 and the terminal. */
  readonly transitions: readonly DirectPurposeTransition[];
  readonly final: SDK.ExecutionSourceScriptDecodingFinalState;
  readonly convicts: boolean;
};

export const prepareExecutionSourceScriptDecodingProof = ({
  material,
  prover,
}: {
  readonly material: DirectScriptMaterial;
  readonly prover: string;
}): ExecutionSourceScriptDecodingProofPlan => {
  const [bindEvent] = directPurposeBindTransitions(material, prover);
  const final = directBindingStates(material).selected;
  return {
    transitions: [{ ...bindEvent!, next: "terminal" }],
    final,
    convicts: SDK.executionSourceScriptDecodingConvicts(final),
  };
};

/** The plan, refused before spending when the terminal cannot convict. */
export const requireConvictingExecutionSourceScriptDecodingProof = (
  params: Parameters<typeof prepareExecutionSourceScriptDecodingProof>[0],
): ExecutionSourceScriptDecodingProofPlan => {
  const prepared = prepareExecutionSourceScriptDecodingProof(params);
  if (!prepared.convicts)
    throw new ExecutionSourceScriptDecodingHonestLeaf(
      "the leaf records no native script decoding reason",
    );
  return prepared;
};

/**
 * Whether the bound leaf convicts: `null` when it does not, when a spent or
 * referenced input has no predecessor descriptor (the input families own that
 * leaf), or when its subject is not in the shape the terminal reads.
 */
export const executionSourceScriptDecodingDetection = (
  material: DirectScriptMaterial,
): { readonly reason: SDK.RejectionReason } | null => {
  try {
    if (!directPurposeInputDescriptorsPresent(material)) return null;
    const { subject } = directBindingStates(material).selected;
    return subject.rejection_reason !== null &&
      SDK.executionSourceScriptDecodingSubjectConvicts(subject)
      ? { reason: subject.rejection_reason }
      : null;
  } catch (error) {
    if (error instanceof CanonicalReplayPrerequisiteError) throw error;
    return null;
  }
};
