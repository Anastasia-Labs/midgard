/** The case history a runner on the Open scene reads its threads from: the
 * production strict-admission history over the scene's recorded raw L1. */
import {
  createValidationTraceCaseHistory,
  type ValidationTraceCaseHistory,
} from "../../src/validation-trace-dispute/case-history.js";
import {
  DEPLOYMENT_FINGERPRINT,
  releaseFinalityAuthority,
} from "../workflow.make-adapter.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

export const sceneCaseHistory = (
  scene: Scene,
  deploymentFingerprint: string = DEPLOYMENT_FINGERPRINT,
): ValidationTraceCaseHistory => {
  const history = releaseFinalityAuthority()
    .verifyForWorkflow({ deploymentFingerprint })
    .then((releaseFinality) =>
      createValidationTraceCaseHistory({
        authority: scene.l1.authority,
        releaseFinality,
      }),
    );
  return {
    transactions: async (input) => await (await history).transactions(input),
  };
};
