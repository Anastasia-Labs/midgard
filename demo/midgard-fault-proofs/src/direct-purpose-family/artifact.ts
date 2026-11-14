/** The journaled direct purpose workflow artifact: the retained payload, the
 * predecessor ledger and the coordinate, re-planned on every admission; and
 * the admitted artifact's whole custody plan, kept per family, artifact,
 * deployment and prover. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { checkLedgerJson, ledgerJson } from "../ledger-at.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";
import { reconstructDaPayload } from "../transition-trace/reconstruct.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import {
  type CompleteCanonicalReplayContext,
  completeCanonicalReplayPredecessorLedger,
} from "../workflow/complete-replay.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
  normalizeJournalJson,
} from "../workflow/journal.js";
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
  const c = value as JournalJsonObject;
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
  value: JournalJsonObject,
): Promise<DirectPurposeAdmitted<Prepared>> => {
  const { label } = family;
  if (
    Object.keys(value).sort().join(",") !==
      "coordinate,detectionId,headerHash,payloadEnvelopeCbor,predecessorLedger,schemaVersion" ||
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
  const predecessor =
    value.predecessorLedger === null
      ? undefined
      : await checkLedgerJson({
          value: value.predecessorLedger,
          utxosRoot: current.header.prevUtxosRoot,
          label: `${label} predecessor ledger`,
        });
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
): Promise<JournalJsonObject> => {
  const predecessor = completeCanonicalReplayPredecessorLedger({
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
  const artifact = normalizeJournalJson({
    schemaVersion: family.artifactVersion,
    headerHash: evidence.headerHash,
    detectionId: finding.detectionId,
    coordinate: finding.prepared.coordinate,
    payloadEnvelopeCbor:
      evidence.reconstruction.payloadEnvelopeCbor.toString("hex"),
    predecessorLedger:
      predecessor === undefined ? null : ledgerJson(predecessor),
  }) as JournalJsonObject;
  await admitDirectPurposeArtifact(family, artifact);
  return artifact;
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
  artifact: JournalJsonObject,
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
    readonly artifact: JournalJsonObject;
    readonly contracts: DirectPurposeContracts;
    readonly prover: string;
  },
): Promise<DirectPurposePlannedArtifact<Prepared, Proof>> => {
  const cache = caches.get(family) ?? new Map<string, Promise<unknown>>();
  caches.set(family, cache);
  const key = [
    journalJsonDigest(artifact),
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
