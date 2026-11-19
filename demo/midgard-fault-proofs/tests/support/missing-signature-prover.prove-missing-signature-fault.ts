import { Effect } from "effect";

import { type MissingSignatureFinding } from "../../src/missing-signature/finding.js";
import {
  type MissingSignatureProofOutcome,
  type MissingSignatureProverDeps,
  toError,
} from "./missing-signature-prover.assert-evidence-coherent.js";
import { runMissingSignatureProver } from "./missing-signature-prover.run-missing-signature-prover.js";

/** The proving core as an Effect for watcher/runtime composition. */
export const proveMissingSignatureFault = (
  finding: MissingSignatureFinding,
  deps: MissingSignatureProverDeps,
): Effect.Effect<MissingSignatureProofOutcome, Error> =>
  Effect.tryPromise({
    try: () => runMissingSignatureProver(finding, deps),
    catch: toError,
  });
