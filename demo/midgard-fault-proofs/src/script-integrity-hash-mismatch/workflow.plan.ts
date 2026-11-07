/** The admitted `scriptIntegrityHashMismatch` artifact with its whole custody
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
  admitScriptIntegrityHashMismatchWorkflowArtifact,
  type AdmittedScriptIntegrityHashMismatchArtifact,
} from "./artifact.js";
import type {
  ScriptIntegrityHashMismatchContracts,
  ScriptIntegrityHashMismatchRole,
} from "./contracts.js";
import type { ScriptIntegrityHashMismatchTransition } from "./custody.js";
import {
  requireConvictingScriptIntegrityHashMismatchProof,
  type ScriptIntegrityHashMismatchProofPlan,
} from "./prover.js";
import { scriptIntegrityHashMismatchStep01Datum } from "./step01.js";
import { isScriptIntegrityHashMismatchPagedRole } from "./walk.js";

export type PlannedScriptIntegrityHashMismatchArtifact = Readonly<{
  admitted: AdmittedScriptIntegrityHashMismatchArtifact;
  proof: ScriptIntegrityHashMismatchProofPlan;
  /** Every field a transition spending `role` opens. */
  fieldsOf: (role: ScriptIntegrityHashMismatchRole) => readonly number[];
  /** The one planned transition spending `role` from a thread holding `datum`. */
  transitionAt: (
    role: ScriptIntegrityHashMismatchRole,
    datum: string,
  ) => ScriptIntegrityHashMismatchTransition;
}>;

/** The transition continuing a thread at `role` that holds `datum`: a paged
 * walk role continues from the walk checkpoint its datum holds, at the
 * configured page size; every other role continues as planned after the
 * transition that wrote `datum` (`step01Datum` before the first). */
export const scriptIntegrityHashMismatchTransitionAt =
  (proof: ScriptIntegrityHashMismatchProofPlan, step01Datum: string) =>
  (
    role: ScriptIntegrityHashMismatchRole,
    datum: string,
  ): ScriptIntegrityHashMismatchTransition => {
    if (
      isScriptIntegrityHashMismatchPagedRole(role) ||
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
        `scriptIntegrityHashMismatch ${role} thread datum matches ${matches.length.toString()} planned transitions`,
      );
    return matches[0]!;
  };

const plan = async (
  artifact: JournalJsonObject,
  contracts: ScriptIntegrityHashMismatchContracts,
  prover: string,
): Promise<PlannedScriptIntegrityHashMismatchArtifact> => {
  const admitted =
    await admitScriptIntegrityHashMismatchWorkflowArtifact(artifact);
  const proof = await requireConvictingScriptIntegrityHashMismatchProof({
    material: admitted.material,
    contracts,
    prover,
  });
  const fields = new Map<ScriptIntegrityHashMismatchRole, Set<number>>();
  for (const transition of proof.transitions) {
    const opened = fields.get(transition.role) ?? new Set<number>();
    for (const field of transition.fields) opened.add(field);
    fields.set(transition.role, opened);
  }
  const step01Datum = scriptIntegrityHashMismatchStep01Datum(
    admitted.material,
    prover,
  );
  return Object.freeze({
    admitted,
    proof,
    fieldsOf: (role) => [...(fields.get(role) ?? [])].sort((l, r) => l - r),
    transitionAt: scriptIntegrityHashMismatchTransitionAt(proof, step01Datum),
  });
};

const CACHE_LIMIT = 4;
const cache = new Map<
  string,
  Promise<PlannedScriptIntegrityHashMismatchArtifact>
>();

/** Plans once per artifact, deployment and prover; a failed plan is not
 * kept, so a retry re-plans. */
export const planScriptIntegrityHashMismatchArtifact = ({
  artifact,
  contracts,
  prover,
}: {
  readonly artifact: JournalJsonObject;
  readonly contracts: ScriptIntegrityHashMismatchContracts;
  readonly prover: string;
}): Promise<PlannedScriptIntegrityHashMismatchArtifact> => {
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
export const scriptIntegrityHashMismatchSourceInclusion = async (
  admitted: AdmittedScriptIntegrityHashMismatchArtifact,
) => {
  const compact = admitted.material.material.compact;
  if (admitted.coordinate.sourceKind !== 0 || !("validity" in compact))
    throw new Error(
      "scriptIntegrityHashMismatch inclusion requires an accepted source",
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
