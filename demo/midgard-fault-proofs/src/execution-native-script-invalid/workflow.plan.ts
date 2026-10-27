/** The admitted `executionNativeScriptInvalid` artifact with its whole
 * custody plan, and the planned transition an observed thread datum
 * continues with. */
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
  admitExecutionNativeScriptInvalidWorkflowArtifact,
  type AdmittedExecutionNativeScriptInvalidArtifact,
} from "./artifact.js";
import type {
  ExecutionNativeScriptInvalidContracts,
  ExecutionNativeScriptInvalidRole,
} from "./contracts.js";
import {
  executionNativeScriptInvalidSharedTransition,
  type ExecutionNativeScriptInvalidTransition,
} from "./custody.js";
import {
  type ExecutionNativeScriptInvalidProofPlan,
  requireConvictingExecutionNativeScriptInvalidProof,
} from "./prover.js";
import { executionNativeScriptInvalidStep01Datum } from "./step01.js";
import { isExecutionNativeScriptInvalidWalkRole } from "./walk.js";

type Role = ExecutionNativeScriptInvalidRole;

export type PlannedExecutionNativeScriptInvalidArtifact = Readonly<{
  admitted: AdmittedExecutionNativeScriptInvalidArtifact;
  proof: ExecutionNativeScriptInvalidProofPlan;
  /** The shared source and purpose custody, re-pageable at any page size. */
  paging: DirectPurposePaging;
  /** Whether `role` is a role of the shared bind, source and purpose
   * custody. */
  isSourceRole: (role: Role) => role is Role & DirectPurposeRole;
  /** Every field a transition spending `role` opens. */
  fieldsOf: (role: Role) => readonly number[];
  /** The one planned transition spending `role` from a thread holding
   * `datum`. */
  transitionAt: (
    role: Role,
    datum: string,
  ) => ExecutionNativeScriptInvalidTransition;
}>;

/** The transition continuing a thread at `role` that holds `datum`: a
 * shared role continues through the kit's paging; a signer or evaluation
 * role continues from the walk state its datum holds, at the configured page
 * size; every other role continues as planned after the transition that
 * wrote `datum` (`step01Datum` before the first). */
export const executionNativeScriptInvalidTransitionAt =
  (
    proof: ExecutionNativeScriptInvalidProofPlan,
    step01Datum: string,
    paging: DirectPurposePaging,
    isSourceRole: (role: Role) => role is Role & DirectPurposeRole,
  ) =>
  (role: Role, datum: string): ExecutionNativeScriptInvalidTransition => {
    if (isExecutionNativeScriptInvalidWalkRole(role)) {
      if (proof.walk === null)
        throw new Error(
          `executionNativeScriptInvalid ${role}: the route evaluates no native source`,
        );
      return proof.walk.transitionAt(role, datum);
    }
    const matches = proof.transitions.filter(
      (transition, index) =>
        transition.role === role &&
        (index === 0
          ? step01Datum
          : proof.transitions[index - 1]!.datum
        ).toLowerCase() === datum.toLowerCase(),
    );
    if (matches.length === 1) return matches[0]!;
    if (isSourceRole(role))
      return executionNativeScriptInvalidSharedTransition(
        paging.transitionAt(role, datum),
      );
    throw new Error(
      `executionNativeScriptInvalid ${role} thread datum matches ${matches.length.toString()} planned transitions`,
    );
  };

const plan = async (
  artifact: JournalJsonObject,
  contracts: ExecutionNativeScriptInvalidContracts,
  prover: string,
): Promise<PlannedExecutionNativeScriptInvalidArtifact> => {
  const admitted =
    await admitExecutionNativeScriptInvalidWorkflowArtifact(artifact);
  const proof = await requireConvictingExecutionNativeScriptInvalidProof({
    material: admitted.material,
    contracts,
    prover,
    entry: admitted.entry,
  });
  const fields = new Map<Role, Set<number>>();
  for (const transition of proof.transitions) {
    const opened = fields.get(transition.role) ?? new Set<number>();
    for (const field of transition.fields) opened.add(field);
    fields.set(transition.role, opened);
  }
  const step01Datum = executionNativeScriptInvalidStep01Datum(
    admitted.material,
    prover,
  );
  const paging = directPurposePaging({
    label: "executionNativeScriptInvalid",
    transitions: proof.source,
    first: step01Datum,
  });
  const sourceRoles = new Set<string>(proof.source.map(({ role }) => role));
  const isSourceRole = (role: Role): role is Role & DirectPurposeRole =>
    sourceRoles.has(role);
  return Object.freeze({
    admitted,
    proof,
    paging,
    isSourceRole,
    fieldsOf: (role) => [...(fields.get(role) ?? [])].sort((l, r) => l - r),
    transitionAt: executionNativeScriptInvalidTransitionAt(
      proof,
      step01Datum,
      paging,
      isSourceRole,
    ),
  });
};

const CACHE_LIMIT = 4;
const cache = new Map<
  string,
  Promise<PlannedExecutionNativeScriptInvalidArtifact>
>();

/** Plans once per artifact, deployment and prover; a failed plan is not
 * kept, so a retry re-plans. */
export const planExecutionNativeScriptInvalidArtifact = ({
  artifact,
  contracts,
  prover,
}: {
  readonly artifact: JournalJsonObject;
  readonly contracts: ExecutionNativeScriptInvalidContracts;
  readonly prover: string;
}): Promise<PlannedExecutionNativeScriptInvalidArtifact> => {
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
export const executionNativeScriptInvalidSourceInclusion = async (
  admitted: AdmittedExecutionNativeScriptInvalidArtifact,
) => {
  const compact = admitted.material.material.compact;
  if (admitted.coordinate.sourceKind !== 0 || !("validity" in compact))
    throw new Error(
      "executionNativeScriptInvalid inclusion requires an accepted source",
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
