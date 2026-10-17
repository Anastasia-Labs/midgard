import { type SpendingValidator } from "../../../common.js";
import { type ValidationTraceDisputeFaultProofContracts } from "./validation-trace-dispute.types.js";
import { VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES } from "./validation-trace-dispute.validation-trace-dispute-fault-proof-titles.js";

/** The published canonical route, using the same applied validators as the chain. */
export const canonicalValidationTraceReferenceScripts = (
  chain: ValidationTraceDisputeFaultProofContracts["validationTraceDispute"],
): readonly {
  readonly deploymentEntry: string;
  readonly validator: SpendingValidator;
}[] => {
  const semanticKeys = Object.keys(
    VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
  );
  return [
    {
      deploymentEntry: "validationTraceDisputeCanonicalDecodePrepare",
      validator: chain.canonicalDecodePrepare,
    },
    ...(["canonicalDecodeEmpty", "canonicalDecodeItem"] as const).map(
      (key) => ({
        deploymentEntry: `validationTraceDispute${key === "canonicalDecodeEmpty" ? "CanonicalDecodeEmpty" : "CanonicalDecodeItem"}Semantic`,
        validator: chain.semanticResolvers[semanticKeys.indexOf(key)]!,
      }),
    ),
    ...(["source", "observe", "proof", "settlement"] as const).map((key) => ({
      deploymentEntry: `validationTraceDisputeCanonicalDecodeItem${key[0]!.toUpperCase()}${key.slice(1)}`,
      validator: chain.canonicalDecodeItemStages[key],
    })),
    {
      deploymentEntry: "validationTraceDisputeProofItem",
      validator: chain.proofItem,
    },
  ];
};
