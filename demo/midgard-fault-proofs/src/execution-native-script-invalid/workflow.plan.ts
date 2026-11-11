/** The admitted `executionNativeScriptInvalid` artifact with its whole
 * custody plan, and the planned transition an observed thread datum
 * continues with. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { DirectPurposeTransition } from "../direct-purpose-family/custody.js";
import type {
  DirectPurposeContracts,
  DirectPurposeRole,
} from "../direct-purpose-family/family.js";
import {
  type DirectPurposePaging,
  directPurposePaging,
  withDirectPurposeWalkPages,
} from "../direct-purpose-family/paging.js";
import { directPurposeStep01Datum } from "../direct-purpose-family/step01.js";
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
import {
  type ExecutionNativeScriptInvalidProofPlan,
  requireConvictingExecutionNativeScriptInvalidProof,
} from "./prover.js";
import { isExecutionNativeScriptInvalidWalkRole } from "./walk.js";

type Role = DirectPurposeRole;

export type PlannedExecutionNativeScriptInvalidArtifact = Readonly<{
  admitted: AdmittedExecutionNativeScriptInvalidArtifact;
  proof: ExecutionNativeScriptInvalidProofPlan;
  /** The custody, its shared source and purpose runs and its signer and
   * evaluation pages re-pageable at any page size. */
  paging: DirectPurposePaging;
  /** Every field a transition spending `role` opens. */
  fieldsOf: (role: Role) => readonly number[];
}>;

/** The custody as the kit fits it. A shared role continues through the
 * kit's paging; a signer or evaluation role starts a page at every
 * transition and continues from the walk state its datum holds, at the
 * configured page size; every other role continues as planned after the
 * transition that wrote `datum` (`step01Datum` before the first). */
export const executionNativeScriptInvalidPaging = (
  proof: ExecutionNativeScriptInvalidProofPlan,
  step01Datum: string,
): DirectPurposePaging => {
  const source = directPurposePaging({
    label: "executionNativeScriptInvalid",
    transitions: proof.source,
    first: step01Datum,
  });
  const sourceRoles = new Set<string>(proof.source.map(({ role }) => role));
  const transitionAt = (role: Role, datum: string): DirectPurposeTransition => {
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
    if (sourceRoles.has(role)) return source.transitionAt(role, datum);
    throw new Error(
      `executionNativeScriptInvalid ${role} thread datum matches ${matches.length.toString()} planned transitions`,
    );
  };
  return withDirectPurposeWalkPages({
    source,
    walk:
      proof.walk === null
        ? null
        : {
            isPaged: isExecutionNativeScriptInvalidWalkRole,
            page: proof.walk.page,
          },
    transitionAt,
  });
};

const plan = async (
  artifact: JournalJsonObject,
  contracts: DirectPurposeContracts,
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
  return Object.freeze({
    admitted,
    proof,
    paging: executionNativeScriptInvalidPaging(
      proof,
      directPurposeStep01Datum(admitted.material, prover),
    ),
    fieldsOf: (role) => [...(fields.get(role) ?? [])].sort((l, r) => l - r),
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
  readonly contracts: DirectPurposeContracts;
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
