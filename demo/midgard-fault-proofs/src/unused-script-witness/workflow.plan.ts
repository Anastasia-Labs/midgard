/** The admitted `unusedScriptWitness` artifact with its whole custody
 * plan, and the planned transition an observed thread datum continues with. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { DirectPurposeRole } from "../direct-purpose-family/family.js";
import {
  type DirectPurposePaging,
  directPurposePaging,
} from "../direct-purpose-family/paging.js";
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
  type AdmittedUnusedScriptWitnessArtifact,
  admitUnusedScriptWitnessWorkflowArtifact,
} from "./artifact.js";
import type {
  UnusedScriptWitnessContracts,
  UnusedScriptWitnessRole,
} from "./contracts.js";
import {
  unusedScriptWitnessSharedTransition,
  type UnusedScriptWitnessTransition,
} from "./custody.js";
import {
  requireConvictingUnusedScriptWitnessProof,
  type UnusedScriptWitnessProofPlan,
} from "./prover.js";
import { unusedScriptWitnessStep01Datum } from "./step01.js";
import { isUnusedScriptWitnessPagedRole } from "./walk.js";

export type PlannedUnusedScriptWitnessArtifact = Readonly<{
  admitted: AdmittedUnusedScriptWitnessArtifact;
  proof: UnusedScriptWitnessProofPlan;
  /** The shared source custody, re-pageable at any page size. */
  paging: DirectPurposePaging;
  /** Whether `role` is a role of the shared bind and source custody. */
  isSourceRole: (
    role: UnusedScriptWitnessRole,
  ) => role is UnusedScriptWitnessRole & DirectPurposeRole;
  /** Every field a transition spending `role` opens. */
  fieldsOf: (role: UnusedScriptWitnessRole) => readonly number[];
  /** The one planned transition spending `role` from a thread holding `datum`. */
  transitionAt: (
    role: UnusedScriptWitnessRole,
    datum: string,
  ) => UnusedScriptWitnessTransition;
}>;

/** The transition continuing a thread at `role` that holds `datum`: a
 * shared source role continues through the kit's paging; a paged walk role
 * continues from the walk checkpoint its datum holds, at the configured page
 * size; every other role continues as planned after the transition that
 * wrote `datum` (`step01Datum` before the first). */
export const unusedScriptWitnessTransitionAt =
  (
    proof: UnusedScriptWitnessProofPlan,
    step01Datum: string,
    paging: DirectPurposePaging,
    isSourceRole: (
      role: UnusedScriptWitnessRole,
    ) => role is UnusedScriptWitnessRole & DirectPurposeRole,
  ) =>
  (
    role: UnusedScriptWitnessRole,
    datum: string,
  ): UnusedScriptWitnessTransition => {
    if (isSourceRole(role))
      return unusedScriptWitnessSharedTransition(
        paging.transitionAt(role, datum),
      );
    if (
      isUnusedScriptWitnessPagedRole(role) ||
      role === "allPurposesSpendResolve"
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
        `unusedScriptWitness ${role} thread datum matches ${matches.length.toString()} planned transitions`,
      );
    return matches[0]!;
  };

const plan = async (
  artifact: JournalJsonObject,
  contracts: UnusedScriptWitnessContracts,
  prover: string,
): Promise<PlannedUnusedScriptWitnessArtifact> => {
  const admitted = await admitUnusedScriptWitnessWorkflowArtifact(artifact);
  const proof = await requireConvictingUnusedScriptWitnessProof({
    material: admitted.material,
    contracts,
    prover,
    scriptIndex: admitted.scriptIndex,
  });
  const fields = new Map<UnusedScriptWitnessRole, Set<number>>();
  for (const transition of proof.transitions) {
    const opened = fields.get(transition.role) ?? new Set<number>();
    for (const field of transition.fields) opened.add(field);
    fields.set(transition.role, opened);
  }
  const step01Datum = unusedScriptWitnessStep01Datum(admitted.material, prover);
  const paging = directPurposePaging({
    label: "unusedScriptWitness",
    transitions: proof.source,
    first: step01Datum,
  });
  const sourceRoles = new Set<string>(proof.source.map(({ role }) => role));
  const isSourceRole = (
    role: UnusedScriptWitnessRole,
  ): role is UnusedScriptWitnessRole & DirectPurposeRole =>
    sourceRoles.has(role);
  return Object.freeze({
    admitted,
    proof,
    paging,
    isSourceRole,
    fieldsOf: (role) => [...(fields.get(role) ?? [])].sort((l, r) => l - r),
    transitionAt: unusedScriptWitnessTransitionAt(
      proof,
      step01Datum,
      paging,
      isSourceRole,
    ),
  });
};

const CACHE_LIMIT = 4;
const cache = new Map<string, Promise<PlannedUnusedScriptWitnessArtifact>>();

/** Plans once per artifact, deployment and prover; a failed plan is not
 * kept, so a retry re-plans. */
export const planUnusedScriptWitnessArtifact = ({
  artifact,
  contracts,
  prover,
}: {
  readonly artifact: JournalJsonObject;
  readonly contracts: UnusedScriptWitnessContracts;
  readonly prover: string;
}): Promise<PlannedUnusedScriptWitnessArtifact> => {
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
export const unusedScriptWitnessSourceInclusion = async (
  admitted: AdmittedUnusedScriptWitnessArtifact,
) => {
  const compact = admitted.material.material.compact;
  if (admitted.coordinate.sourceKind !== 0 || !("validity" in compact))
    throw new Error(
      "unusedScriptWitness inclusion requires an accepted source",
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
