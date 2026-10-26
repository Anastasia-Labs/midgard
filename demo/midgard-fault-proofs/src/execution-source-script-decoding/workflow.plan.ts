/** The admitted `executionSourceScriptDecoding` artifact with its custody
 * plan, and the planned transition an observed thread datum continues with. */
import type { DirectPurposeContracts } from "../direct-purpose-family/family.js";
import {
  type DirectPurposePaging,
  directPurposePaging,
} from "../direct-purpose-family/paging.js";
import { directPurposeStep01Datum } from "../direct-purpose-family/step01.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
} from "../workflow/journal.js";
import {
  admitExecutionSourceScriptDecodingWorkflowArtifact,
  type AdmittedExecutionSourceScriptDecodingArtifact,
} from "./artifact.js";
import {
  type ExecutionSourceScriptDecodingProofPlan,
  requireConvictingExecutionSourceScriptDecodingProof,
} from "./prover.js";

export type PlannedExecutionSourceScriptDecodingArtifact = Readonly<{
  admitted: AdmittedExecutionSourceScriptDecodingArtifact;
  proof: ExecutionSourceScriptDecodingProofPlan;
  paging: DirectPurposePaging;
  transitionAt: DirectPurposePaging["transitionAt"];
}>;

const plan = async (
  artifact: JournalJsonObject,
  prover: string,
): Promise<PlannedExecutionSourceScriptDecodingArtifact> => {
  const admitted =
    await admitExecutionSourceScriptDecodingWorkflowArtifact(artifact);
  const proof = requireConvictingExecutionSourceScriptDecodingProof({
    material: admitted.material,
    prover,
  });
  const paging = directPurposePaging({
    label: "executionSourceScriptDecoding",
    transitions: proof.transitions,
    first: directPurposeStep01Datum(admitted.material, prover),
  });
  return Object.freeze({
    admitted,
    proof,
    paging,
    transitionAt: paging.transitionAt,
  });
};

const CACHE_LIMIT = 4;
const cache = new Map<
  string,
  Promise<PlannedExecutionSourceScriptDecodingArtifact>
>();

/** Plans once per artifact, deployment and prover; a failed plan is not
 * kept, so a retry re-plans. */
export const planExecutionSourceScriptDecodingArtifact = ({
  artifact,
  contracts,
  prover,
}: {
  readonly artifact: JournalJsonObject;
  readonly contracts: DirectPurposeContracts;
  readonly prover: string;
}): Promise<PlannedExecutionSourceScriptDecodingArtifact> => {
  const key = [
    journalJsonDigest(artifact),
    prover,
    ...contracts.steps.map((step) => step.spendingScriptHash),
  ].join(":");
  const cached = cache.get(key);
  if (cached !== undefined) return cached;
  const planned = plan(artifact, prover);
  planned.catch(() => cache.delete(key));
  cache.set(key, planned);
  if (cache.size > CACHE_LIMIT) cache.delete(cache.keys().next().value!);
  return planned;
};
