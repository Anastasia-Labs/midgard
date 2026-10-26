/** The admitted `unusedRedeemer` artifact with its whole custody plan, and
 * the planned transition an observed thread datum continues with. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type {
  DirectPurposeContracts,
  DirectPurposeRole,
} from "../direct-purpose-family/family.js";
import {
  type DirectPurposePaging,
  directPurposePaging,
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
  type AdmittedUnusedRedeemerArtifact,
  admitUnusedRedeemerWorkflowArtifact,
} from "./artifact.js";
import {
  requireConvictingUnusedRedeemerProof,
  type UnusedRedeemerProofPlan,
} from "./prover.js";

export type PlannedUnusedRedeemerArtifact = Readonly<{
  admitted: AdmittedUnusedRedeemerArtifact;
  proof: UnusedRedeemerProofPlan;
  /** Every field a transition spending `role` opens. */
  fieldsOf: (role: DirectPurposeRole) => readonly number[];
  /** The custody re-paged at any page size; `transitionAt` names the one
   * transition spending `role` from a thread holding `datum`. */
  paging: DirectPurposePaging;
  transitionAt: DirectPurposePaging["transitionAt"];
}>;

const plan = async (
  artifact: JournalJsonObject,
  contracts: DirectPurposeContracts,
  prover: string,
): Promise<PlannedUnusedRedeemerArtifact> => {
  const admitted = await admitUnusedRedeemerWorkflowArtifact(artifact);
  const proof = await requireConvictingUnusedRedeemerProof({
    material: admitted.material,
    contracts,
    prover,
    redeemerIndex: admitted.redeemerIndex,
  });
  const fields = new Map<DirectPurposeRole, Set<number>>();
  for (const transition of proof.transitions) {
    const opened = fields.get(transition.role) ?? new Set<number>();
    for (const field of transition.fields) opened.add(field);
    fields.set(transition.role, opened);
  }
  const paging = directPurposePaging({
    label: "unusedRedeemer",
    transitions: proof.transitions,
    first: directPurposeStep01Datum(admitted.material, prover),
  });
  return Object.freeze({
    admitted,
    proof,
    fieldsOf: (role) => [...(fields.get(role) ?? [])].sort((l, r) => l - r),
    paging,
    transitionAt: paging.transitionAt,
  });
};

const CACHE_LIMIT = 4;
const cache = new Map<string, Promise<PlannedUnusedRedeemerArtifact>>();

/** Plans once per artifact, deployment and prover; a failed plan is not
 * kept, so a retry re-plans. */
export const planUnusedRedeemerArtifact = ({
  artifact,
  contracts,
  prover,
}: {
  readonly artifact: JournalJsonObject;
  readonly contracts: DirectPurposeContracts;
  readonly prover: string;
}): Promise<PlannedUnusedRedeemerArtifact> => {
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
export const unusedRedeemerSourceInclusion = async (
  admitted: AdmittedUnusedRedeemerArtifact,
) => {
  const compact = admitted.material.material.compact;
  if (admitted.coordinate.sourceKind !== 0 || !("validity" in compact))
    throw new Error("unusedRedeemer inclusion requires an accepted source");
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
