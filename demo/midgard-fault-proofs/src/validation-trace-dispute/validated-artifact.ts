/** The validation-trace adapter's check of its saved artifact against a
 * resumed run's replay (detected-claim.ts). A check that passed is kept for
 * its block, by the digest of its inputs (validation-inputs-digest.ts): the
 * evidence, the replay context, the classification and the artifact. The
 * check is a function of that content and of the predecessor authority's
 * admission, which is by identity, not content: inputs with the same content
 * pass again once their predecessor authority is itself admitted, without
 * running the check again, and the block's claim material, with the step
 * windows it has read since, is kept rather than rebuilt by a fresh
 * re-execution of the whole trace. Any other inputs, and inputs that are not
 * plain data, are checked in full; a check that fails forgets the block's
 * last pass. One pass is kept per block, beside the block's material. */
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";

import { requireReplayPredecessorLedger } from "../workflow/complete-replay.replay-context-identity.js";
import type { FraudProofFamilyWorkflowAdapter } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import { validateValidationTraceArtifact } from "./detected-claim.js";
import { validationInputsDigest } from "./validation-inputs-digest.js";

type ArtifactCheck = Parameters<
  NonNullable<FraudProofFamilyWorkflowAdapter["validatePreparedArtifact"]>
>[0];

export const validationTraceArtifactCheck = ({
  l1SlotConfig,
  materials,
  held,
}: {
  readonly l1SlotConfig: L1SlotConfig;
  /** Each block's claim material, as its saved artifact last passed. */
  readonly materials: Map<string, ValidationTraceClaimMaterial>;
  /** What a check that does not build becomes for the block. */
  readonly held: (headerHash: string) => (cause: unknown) => Promise<never>;
}) => {
  /** Each block's last check that passed, by the digest of its inputs. */
  const passed = new Map<string, string>();
  return {
    /** Forgets the block's last pass, once its material is prepared anew. */
    prepared: (headerHash: string) => passed.delete(headerHash),
    check: async ({
      evidence,
      replayContext,
      classification,
      artifact,
    }: ArtifactCheck) => {
      const { headerHash } = evidence;
      let inputs: string | undefined;
      try {
        inputs = validationInputsDigest({
          evidence,
          replayContext,
          classification,
          artifact,
        });
      } catch {
        inputs = undefined;
      }
      if (
        inputs !== undefined &&
        passed.get(headerHash) === inputs &&
        materials.has(headerHash)
      ) {
        // The digest sees the authority's fields, not which object was
        // admitted: a copy of an admitted authority is refused here.
        requireReplayPredecessorLedger({ evidence, context: replayContext });
        return;
      }
      passed.delete(headerHash);
      let material: ValidationTraceClaimMaterial | undefined;
      await validateValidationTraceArtifact({
        evidence,
        context: replayContext,
        classification,
        artifact,
        l1SlotConfig,
        onMaterial: (matched) => {
          material = matched;
        },
      }).catch(held(headerHash));
      if (material === undefined) return;
      materials.set(headerHash, material);
      if (inputs !== undefined) passed.set(headerHash, inputs);
    },
  };
};
