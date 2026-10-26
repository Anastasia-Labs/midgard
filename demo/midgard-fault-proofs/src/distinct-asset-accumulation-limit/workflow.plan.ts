/** The admitted `distinctAssetAccumulationLimit` artifact with its whole custody
 * plan, and the planned transition an observed thread datum continues with. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
} from "../workflow/journal.js";
import {
  admitDistinctAssetAccumulationLimitWorkflowArtifact,
  type AdmittedDistinctAssetAccumulationLimitArtifact,
} from "./artifact.js";
import type {
  DistinctAssetAccumulationLimitContracts,
  DistinctAssetAccumulationLimitRole,
} from "./contracts.js";
import type { DistinctAssetAccumulationLimitTransition } from "./custody.js";
import {
  type DistinctAssetAccumulationLimitProofPlan,
  requireConvictingDistinctAssetAccumulationLimitProof,
} from "./prover.js";
import { distinctAssetAccumulationLimitStep01Datum } from "./step01.js";
import { isDistinctAssetAccumulationLimitPagedRole } from "./walk.js";

export type PlannedDistinctAssetAccumulationLimitArtifact = Readonly<{
  admitted: AdmittedDistinctAssetAccumulationLimitArtifact;
  proof: DistinctAssetAccumulationLimitProofPlan;
  /** Every field a transition spending `role` opens. */
  fieldsOf: (role: DistinctAssetAccumulationLimitRole) => readonly number[];
  /** The one planned transition spending `role` from a thread holding `datum`. */
  transitionAt: (
    role: DistinctAssetAccumulationLimitRole,
    datum: string,
  ) => DistinctAssetAccumulationLimitTransition;
}>;

/** The transition continuing a thread at `role` that holds `datum`: a paged
 * fold role continues from the fold state its datum holds, at the configured
 * page size; every other role continues as planned after the transition that
 * wrote `datum` (`step01Datum` before the first), which no page size
 * changes. */
export const distinctAssetAccumulationLimitTransitionAt =
  (proof: DistinctAssetAccumulationLimitProofPlan, step01Datum: string) =>
  (
    role: DistinctAssetAccumulationLimitRole,
    datum: string,
  ): DistinctAssetAccumulationLimitTransition => {
    if (
      isDistinctAssetAccumulationLimitPagedRole(role) ||
      role === "inputsResolve"
    )
      return proof.walk.transitionAt(role, datum);
    const matches = proof.transitions.filter(
      (transition, index) =>
        transition.role === role &&
        (index === 0
          ? step01Datum
          : proof.transitions[index - 1]!.datum
        ).toLowerCase() === datum.toLowerCase(),
    );
    if (matches.length !== 1)
      throw new Error(
        `distinctAssetAccumulationLimit ${role} thread datum matches ${matches.length.toString()} planned transitions`,
      );
    return matches[0]!;
  };

const plan = async (
  artifact: JournalJsonObject,
  contracts: DistinctAssetAccumulationLimitContracts,
  prover: string,
): Promise<PlannedDistinctAssetAccumulationLimitArtifact> => {
  const admitted =
    await admitDistinctAssetAccumulationLimitWorkflowArtifact(artifact);
  const proof = await requireConvictingDistinctAssetAccumulationLimitProof({
    material: admitted.material,
    contracts,
    prover,
  });
  const fields = new Map<DistinctAssetAccumulationLimitRole, Set<number>>();
  for (const transition of proof.transitions) {
    const opened = fields.get(transition.role) ?? new Set<number>();
    for (const field of transition.fields) opened.add(field);
    fields.set(transition.role, opened);
  }
  const step01Datum = distinctAssetAccumulationLimitStep01Datum(
    admitted.material,
    prover,
  );
  return Object.freeze({
    admitted,
    proof,
    fieldsOf: (role) => [...(fields.get(role) ?? [])].sort((l, r) => l - r),
    transitionAt: distinctAssetAccumulationLimitTransitionAt(
      proof,
      step01Datum,
    ),
  });
};

const CACHE_LIMIT = 4;
const cache = new Map<
  string,
  Promise<PlannedDistinctAssetAccumulationLimitArtifact>
>();

/** Plans once per artifact, deployment and prover; a failed plan is not
 * kept, so a retry re-plans. */
export const planDistinctAssetAccumulationLimitArtifact = ({
  artifact,
  contracts,
  prover,
}: {
  readonly artifact: JournalJsonObject;
  readonly contracts: DistinctAssetAccumulationLimitContracts;
  readonly prover: string;
}): Promise<PlannedDistinctAssetAccumulationLimitArtifact> => {
  const key = [
    journalJsonDigest(artifact),
    prover,
    ...contracts.steps.map((step) => step.spendingScriptHash),
  ].join(":");
  const cached = cache.get(key);
  if (cached !== undefined) return cached;
  const planned = plan(artifact, contracts, prover);
  planned.catch(() => cache.delete(key));
  cache.set(key, planned);
  if (cache.size > CACHE_LIMIT) cache.delete(cache.keys().next().value!);
  return planned;
};

/** The accepted source's inclusion under the challenged block. */
export const distinctAssetAccumulationLimitSourceInclusion = async (
  admitted: AdmittedDistinctAssetAccumulationLimitArtifact,
) => {
  const compact = admitted.material.material.compact;
  if (admitted.coordinate.sourceKind !== 0 || !("validity" in compact))
    throw new Error(
      "distinctAssetAccumulationLimit inclusion requires an accepted source",
    );
  const { current } = admitted;
  const entry = current.transactions[admitted.coordinate.sourceIndex]!;
  const transactions = current.rootData.transactions;
  return parseSubmitStep01TxInclusion({
    nativeTxId: entry.txId,
    nativeTx: nativeTxFromCoreCompact(compact),
    nativeTxCompactCbor: admitted.material.bound.compact_cbor,
    l2TransactionSourceCbor: entry.valueBytes.toString("hex"),
    transactionsPhasRoot: transactions.phasRoot,
    txMembershipProofCbor: Data.to(
      await keyValuePhasProof(
        { ...transactions, root: transactions.phasRoot },
        entry.keyBytes,
        entry.valueBytes,
      ),
      SDK.Proof,
    ),
  });
};
