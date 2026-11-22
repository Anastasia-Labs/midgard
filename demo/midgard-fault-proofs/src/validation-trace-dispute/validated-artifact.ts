/** The validation-trace adapter's preparation of its artifact, and its check
 * of an artifact against a run's replay (detected-claim.ts). Every run
 * prepares the artifact from its evidence; one prepared from inputs of the
 * same content (the evidence, the replay context and the classification),
 * while the block's material stands, is returned again with that material,
 * and the step windows it has read since, rather than rebuilt by a fresh
 * re-execution of the whole trace; other inputs prepare it anew, so a
 * changed fault is prepared afresh. A check that passed is kept for
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
import type { WorkflowJsonObject } from "../workflow/identity.js";
import type { FraudProofFamilyWorkflowAdapter } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import { validateValidationTraceArtifact } from "./detected-claim.js";
import { validationInputsDigest } from "./validation-inputs-digest.js";

type ArtifactCheck = Parameters<
  NonNullable<FraudProofFamilyWorkflowAdapter["validatePreparedArtifact"]>
>[0];

type ArtifactInputs = Omit<ArtifactCheck, "artifact">;

/** The digest of `inputs`' content; undefined for inputs that are not plain
 * data. */
const digestOf = (inputs: object): string | undefined => {
  try {
    return validationInputsDigest(inputs);
  } catch {
    return undefined;
  }
};

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
  /** Each block's last preparation, by the digest of its inputs. */
  const preparedBy = new Map<
    string,
    Readonly<{ inputs: string; artifact: WorkflowJsonObject }>
  >();
  return {
    /** Forgets the block's last pass, once its material is prepared anew. */
    prepared: (headerHash: string) => passed.delete(headerHash),
    /** The block's artifact from `inputs`: the one last prepared from
     * inputs of the same content while its material stands, else `build`'s. */
    prepare: async (
      { evidence, replayContext, classification }: ArtifactInputs,
      build: () => Promise<WorkflowJsonObject>,
    ): Promise<WorkflowJsonObject> => {
      const { headerHash } = evidence;
      const inputs = digestOf({ evidence, replayContext, classification });
      const last = preparedBy.get(headerHash);
      if (
        inputs !== undefined &&
        last?.inputs === inputs &&
        materials.has(headerHash)
      ) {
        // As for a check that passed: the predecessor authority is admitted
        // by identity, not by content.
        requireReplayPredecessorLedger({ evidence, context: replayContext });
        return last.artifact;
      }
      preparedBy.delete(headerHash);
      const artifact = await build();
      if (inputs !== undefined)
        preparedBy.set(headerHash, Object.freeze({ inputs, artifact }));
      return artifact;
    },
    check: async ({
      evidence,
      replayContext,
      classification,
      artifact,
    }: ArtifactCheck) => {
      const { headerHash } = evidence;
      const inputs = digestOf({
        evidence,
        replayContext,
        classification,
        artifact,
      });
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
