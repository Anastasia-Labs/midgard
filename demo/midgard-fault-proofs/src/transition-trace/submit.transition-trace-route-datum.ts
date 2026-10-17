import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  TransitionTraceProofCommitmentDatum,
  TransitionTraceStepDatum,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { initialTransitionTraceState } from "./phases.js";
import {
  readTransitionProof,
  transitionProofCbor,
  type TransitionProofInput,
} from "./proof-material.js";
import { transitionTraceFinalIndex } from "./submit.make-transition-trace-final-spend-redeemer.js";

export const transitionTraceRouteDatum = (
  proof: TransitionProofInput,
  prover: string,
): string => {
  const index = transitionTraceFinalIndex(proof);
  return index === 4 || index === 5
    ? Data.to(
        { fraud_prover: prover, data: initialTransitionTraceState(proof) },
        TransitionTraceProofCommitmentDatum,
      )
    : replacePlutusConstrFieldCbor(
        Data.to(
          { fraud_prover: prover, data: readTransitionProof(proof) },
          TransitionTraceStepDatum,
        ),
        [1, 0],
        transitionProofCbor(proof),
      );
};
