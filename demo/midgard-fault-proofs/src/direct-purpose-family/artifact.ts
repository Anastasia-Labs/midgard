/** The direct purpose workflow artifact: the retained payload, the
 * predecessor ledger witness and the coordinate, re-planned on every admission; and
 * the admitted artifact's whole custody plan, kept per family, artifact,
 * deployment and prover. */
import * as SDK from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { ledgerFromWitness, recordWitness } from "../ledger-store/witness.js";
import {
  ledgerWitnessJson,
  parseLedgerWitnessJson,
} from "../ledger-store/witness-json.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";
import { reconstructDaPayload } from "../transition-trace/reconstruct.reconstruct-da-payload.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import { completeCanonicalReplayPredecessorStoreLedger } from "../workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.replay-context-identity.js";
import {
  normalizeWorkflowJson,
  workflowJsonDigest,
  type WorkflowJsonObject,
} from "../workflow/identity.js";
import type {
  DirectPurposeAdmitted,
  DirectPurposeArtifactAdmission,
  DirectPurposeArtifactFamily,
  DirectPurposeProof,
  DirectPurposeReplay,
} from "./artifact-family.js";
import type { DirectPurposeContracts, DirectPurposeRole } from "./family.js";
import { type DirectPurposePaging, directPurposePaging } from "./paging.js";
import type { DirectPurposeSourceCoordinate } from "./replay-support.js";
import { directPurposeStep01Datum } from "./step01.js";

const HEX = /^(?:[0-9a-f]{2})+$/u;

const admitCoordinate = (
  label: string,
  value: unknown,
): DirectPurposeSourceCoordinate => {
  const c = value as WorkflowJsonObject;
  if (
    typeof c !== "object" ||
    c === null ||
    Array.isArray(c) ||
    Object.keys(c).sort().join(",") !== "sourceIndex,sourceKind" ||
    (c.sourceKind !== 0 && c.sourceKind !== 1) ||
    typeof c.sourceIndex !== "number" ||
    !Number.isSafeInteger(c.sourceIndex) ||
    c.sourceIndex < 0
  )
    throw new Error(`${label} invalid coordinate`);
  return { sourceKind: c.sourceKind, sourceIndex: c.sourceIndex };
};

export const admitDirectPurposeArtifact = async <
  Prepared extends DirectPurposeReplay,
>(
  family: DirectPurposeArtifactAdmission<Prepared>,
  value: WorkflowJsonObject,
): Promise<DirectPurposeAdmitted<Prepared>> => {
  const { label } = family;
  if (
    Object.keys(value).sort().join(",") !==
      "coordinate,detectionId,headerHash,payloadEnvelopeCbor,predecessorWitness,schemaVersion" ||
    value.schemaVersion !== family.artifactVersion
  )
    throw new Error(`${label} artifact shape changed`);
  for (const key of ["headerHash", "payloadEnvelopeCbor"] as const)
    if (typeof value[key] !== "string" || !HEX.test(value[key]))
      throw new Error(`${label} invalid ${key}`);
  const current = await reconstructDaPayload({
    payloadEnvelopeCbor: Buffer.from(
      value.payloadEnvelopeCbor as string,
      "hex",
    ),
    expectedHeaderHash: value.headerHash as string,
  });
  // The witness opens the root the authenticated header commits, never one
  // the artifact names.
  const predecessor =
    value.predecessorWitness === null
      ? undefined
      : ledgerFromWitness(
          current.header.prevUtxosRoot,
          parseLedgerWitnessJson(
            value.predecessorWitness,
            `${label} predecessor witness`,
          ),
        );
  const prepared = await family.prepare({
    current,
    coordinate: admitCoordinate(label, value.coordinate),
    ...(predecessor === undefined ? {} : { predecessor }),
  });
  if (prepared === null)
    throw new Error(`${label} artifact does not prove a fault`);
  if (prepared.detectionId !== value.detectionId)
    throw new Error(`${label} detection identity changed`);
  return { ...prepared, current };
};

export const prepareDirectPurposeArtifact = async <
  Prepared extends DirectPurposeReplay,
>(
  family: DirectPurposeArtifactAdmission<Prepared>,
  {
    evidence,
    classification,
    replayContext,
  }: {
    readonly replayContext?: CompleteCanonicalReplayContext;
    readonly evidence: CanonicalBlockEvidence;
    readonly classification: Extract<
      CanonicalBlockClassification,
      { decision: "fault_detected" }
    >;
  },
): Promise<WorkflowJsonObject> => {
  const predecessor = completeCanonicalReplayPredecessorStoreLedger({
    evidence,
    context: replayContext,
  });
  const findings = await family.detect({
    block: evidence,
    ...(predecessor === undefined ? {} : { predecessor }),
  });
  const finding = findings.find(
    (item) => item.detectionId === classification.selected.detectionId,
  );
  if (
    finding === undefined ||
    classification.category !== family.label ||
    classification.headerHash !== evidence.headerHash
  )
    throw new Error(`${family.label} classification mismatch`);
  // Prepare the selected finding again over a recording ledger, and prove
  // every input it resolved, the only ledger reads a family's convicting
  // proof makes: the witness holds what admission and capture read. A node
  // the witness lacks fails capture, so the proofs are taken here even where
  // the reads of preparation already loaded every node they need.
  const recording =
    predecessor === undefined ? undefined : recordWitness(predecessor);
  const prepared = await family.prepare({
    current: evidence.reconstruction,
    coordinate: finding.prepared.coordinate,
    ...(recording === undefined ? {} : { predecessor: recording.ledger }),
  });
  if (recording !== undefined && prepared !== null)
    for (const key of prepared.material.resolvedInputs.byKey.keys())
      await prepared.material.resolvedInputs.proof(Buffer.from(key, "hex"));
  return normalizeWorkflowJson({
    schemaVersion: family.artifactVersion,
    headerHash: evidence.headerHash,
    detectionId: finding.detectionId,
    coordinate: finding.prepared.coordinate,
    payloadEnvelopeCbor:
      evidence.reconstruction.payloadEnvelopeCbor.toString("hex"),
    predecessorWitness:
      recording === undefined ? null : ledgerWitnessJson(recording.witness()),
  }) as WorkflowJsonObject;
};

export type DirectPurposePlannedArtifact<
  Prepared extends DirectPurposeReplay,
  Proof extends DirectPurposeProof,
> = Readonly<{
  admitted: DirectPurposeAdmitted<Prepared>;
  proof: Proof;
  /** The custody re-paged at any page size; its `transitionAt` names the
   * one transition spending a role from a thread holding a datum. */
  paging: DirectPurposePaging;
  /** Every field a transition spending `role` opens. */
  fieldsOf: (role: DirectPurposeRole) => readonly number[];
}>;

/** The custody of `proof` as the kit fits it, `first` the step-01 datum:
 * the family's own paging, or its planned transitions paged by the kit. */
export const directPurposeArtifactPaging = <
  Prepared extends DirectPurposeReplay,
  Proof extends DirectPurposeProof,
>(
  family: DirectPurposeArtifactFamily<Prepared, Proof>,
  proof: Proof,
  first: string,
): DirectPurposePaging =>
  family.paging?.(proof, first) ??
  directPurposePaging({
    label: family.label,
    transitions: proof.transitions,
    first,
  });

const plan = async <
  Prepared extends DirectPurposeReplay,
  Proof extends DirectPurposeProof,
>(
  family: DirectPurposeArtifactFamily<Prepared, Proof>,
  artifact: WorkflowJsonObject,
  contracts: DirectPurposeContracts,
  prover: string,
): Promise<DirectPurposePlannedArtifact<Prepared, Proof>> => {
  const admitted = await admitDirectPurposeArtifact(family, artifact);
  const proof = await family.prove(admitted, contracts, prover);
  const fields = new Map<DirectPurposeRole, Set<number>>();
  for (const transition of proof.transitions) {
    const opened = fields.get(transition.role) ?? new Set<number>();
    for (const field of transition.fields) opened.add(field);
    fields.set(transition.role, opened);
  }
  const first = directPurposeStep01Datum(admitted.material, prover);
  return Object.freeze({
    admitted,
    proof,
    paging: directPurposeArtifactPaging(family, proof, first),
    fieldsOf: (role) => [...(fields.get(role) ?? [])].sort((l, r) => l - r),
  });
};

const CACHE_LIMIT = 4;
const caches = new WeakMap<object, Map<string, Promise<unknown>>>();

/** Plans once per family, artifact, deployment and prover; a failed plan is
 * not kept, so a retry re-plans. */
export const planDirectPurposeArtifact = <
  Prepared extends DirectPurposeReplay,
  Proof extends DirectPurposeProof,
>(
  family: DirectPurposeArtifactFamily<Prepared, Proof>,
  {
    artifact,
    contracts,
    prover,
  }: {
    readonly artifact: WorkflowJsonObject;
    readonly contracts: DirectPurposeContracts;
    readonly prover: string;
  },
): Promise<DirectPurposePlannedArtifact<Prepared, Proof>> => {
  const cache = caches.get(family) ?? new Map<string, Promise<unknown>>();
  caches.set(family, cache);
  const key = [
    workflowJsonDigest(artifact),
    prover,
    ...contracts.steps.map((step) => step.spendingScriptHash),
  ].join(":");
  const cached = cache.get(key);
  if (cached !== undefined)
    return cached as Promise<DirectPurposePlannedArtifact<Prepared, Proof>>;
  const planned = plan(family, artifact, contracts, prover);
  planned.catch(() => cache.delete(key));
  cache.set(key, planned);
  if (cache.size > CACHE_LIMIT) cache.delete(cache.keys().next().value!);
  return planned;
};

/** The accepted source's inclusion under the challenged block. */
export const directPurposeSourceInclusion = async <
  Prepared extends DirectPurposeReplay,
>(
  family: DirectPurposeArtifactAdmission<Prepared>,
  admitted: DirectPurposeAdmitted<Prepared>,
) => {
  const compact = admitted.material.material.compact;
  if (admitted.coordinate.sourceKind !== 0 || !("validity" in compact))
    throw new Error(`${family.label} inclusion requires an accepted source`);
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
