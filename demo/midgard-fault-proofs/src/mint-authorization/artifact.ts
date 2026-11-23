import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { ledgerFromWitness, recordWitness } from "../ledger-store/witness.js";
import {
  ledgerWitnessJson,
  parseLedgerWitnessJson,
} from "../ledger-store/witness-json.js";
import { reconstructDaPayload } from "../transition-trace/reconstruct.reconstruct-da-payload.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import { completeCanonicalReplayPredecessorStoreLedger } from "../workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.replay-context-identity.js";
import {
  normalizeWorkflowJson,
  workflowJsonDigest,
  type WorkflowJsonObject,
} from "../workflow/identity.js";
import {
  canonicalHex,
  canonicalNaturalString,
  EVEN_HEX,
  exactArtifactRecord,
  HEX_28,
  safeNaturalNumber,
} from "../workflow/native-index-artifact.js";
import {
  detectMintAuthorizationReplay,
  mintAuthorizationDetectionId,
  mintAuthorizationInputProof,
  prepareMintAuthorizationReplay,
} from "./replay.js";

export const MINT_AUTHORIZATION_ARTIFACT =
  "midgard-mint-authorization-workflow-artifact-v3";

/** Serialized material is only a preimage; replay rederives the entire claim. */
const deriveMintAuthorizationWorkflowArtifact = async (
  value: WorkflowJsonObject,
) => {
  const record = exactArtifactRecord(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "coordinate",
      "payloadEnvelopeCbor",
      "predecessorWitness",
    ],
    "mint authorization artifact",
  );
  if (record.schemaVersion !== MINT_AUTHORIZATION_ARTIFACT)
    throw new Error("mint authorization: artifact schema changed");
  const headerHash = canonicalHex(
    record.headerHash,
    HEX_28,
    "mint authorization header",
  );
  const current = await reconstructDaPayload({
    payloadEnvelopeCbor: Buffer.from(
      canonicalHex(
        record.payloadEnvelopeCbor,
        EVEN_HEX,
        "mint authorization payload",
      ),
      "hex",
    ),
    expectedHeaderHash: headerHash,
  });
  // The witness opens the root the authenticated header commits, never one
  // the artifact names.
  const predecessor =
    record.predecessorWitness === null
      ? undefined
      : ledgerFromWitness(
          current.header.prevUtxosRoot,
          parseLedgerWitnessJson(
            record.predecessorWitness,
            "mint authorization predecessor witness",
          ),
        );
  const raw = exactArtifactRecord(
    record.coordinate,
    ["sourceIndex", "policyIndex"],
    "mint authorization coordinate",
  );
  const coordinate = {
    sourceIndex: safeNaturalNumber(
      raw.sourceIndex,
      "mint authorization source index",
    ),
    policyIndex: canonicalNaturalString(
      raw.policyIndex,
      "mint authorization policy index",
    ),
  };
  if (record.detectionId !== mintAuthorizationDetectionId(coordinate))
    throw new Error("mint authorization: detection identity changed");
  const prepared = (
    await prepareMintAuthorizationReplay({
      current,
      predecessor,
      sourceIndex: coordinate.sourceIndex,
    })
  ).find((item) => item.coordinate.policyIndex === coordinate.policyIndex);
  if (prepared === undefined)
    throw new Error("mint authorization: artifact does not prove a fault");
  return prepared;
};

// Admission is a pure function of the artifact record, and a running workflow
// admits the same artifact several times per cursor action (capture, both
// field-carriage requirements, the proof chunk). Re-deriving the DA payload
// and the claim scan each time dominated long native-policy workflows, so the
// derivation is retained for the few artifacts one prover process works on.
const ADMITTED_ARTIFACT_LIMIT = 4;
const admittedArtifacts = new Map<
  string,
  ReturnType<typeof deriveMintAuthorizationWorkflowArtifact>
>();

export const admitMintAuthorizationWorkflowArtifact = (
  value: WorkflowJsonObject,
): ReturnType<typeof deriveMintAuthorizationWorkflowArtifact> => {
  const key = workflowJsonDigest(normalizeWorkflowJson(value));
  const known = admittedArtifacts.get(key);
  if (known !== undefined) {
    admittedArtifacts.delete(key);
    admittedArtifacts.set(key, known);
    return known;
  }
  const derived = deriveMintAuthorizationWorkflowArtifact(value);
  admittedArtifacts.set(key, derived);
  derived.catch(() => {
    // A failed admission is not retained: the next caller re-derives and
    // observes the failure itself, as it would have without the cache.
    if (admittedArtifacts.get(key) === derived) admittedArtifacts.delete(key);
  });
  while (admittedArtifacts.size > ADMITTED_ARTIFACT_LIMIT) {
    const oldest = admittedArtifacts.keys().next().value;
    if (oldest === undefined) break;
    admittedArtifacts.delete(oldest);
  }
  return derived;
};

export const prepareMintAuthorizationWorkflowArtifact = async ({
  evidence,
  classification,
  replayContext,
}: {
  evidence: CanonicalBlockEvidence;
  classification: Extract<
    CanonicalBlockClassification,
    { decision: "fault_detected" }
  >;
  replayContext?: CompleteCanonicalReplayContext;
}) => {
  const predecessor = completeCanonicalReplayPredecessorStoreLedger({
    evidence,
    context: replayContext,
  });
  const finding = (
    await detectMintAuthorizationReplay({ block: evidence, predecessor })
  ).find((item) => item.detectionId === classification.selected.detectionId);
  if (
    finding === undefined ||
    classification.category !== "mintAuthorization" ||
    classification.headerHash !== evidence.headerHash ||
    classification.selected.violationId !== finding.violationId ||
    classification.selected.position !== finding.position
  )
    throw new Error("mint authorization: classification changed");
  // Prepare the selected source again over a recording ledger, and prove
  // every resolved input as step 04 does: the witness holds what admission
  // and capture read.
  const recording =
    predecessor === undefined ? undefined : recordWitness(predecessor);
  const prepared = (
    await prepareMintAuthorizationReplay({
      current: evidence.reconstruction,
      predecessor: recording?.ledger,
      sourceIndex: finding.prepared.coordinate.sourceIndex,
    })
  ).find(
    (item) =>
      item.coordinate.policyIndex === finding.prepared.coordinate.policyIndex,
  );
  if (recording !== undefined && prepared !== undefined)
    for (const input of [...prepared.references, ...prepared.spends])
      await mintAuthorizationInputProof(prepared, input);
  return normalizeWorkflowJson({
    schemaVersion: MINT_AUTHORIZATION_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId: finding.detectionId,
    coordinate: finding.prepared.coordinate,
    payloadEnvelopeCbor:
      evidence.reconstruction.payloadEnvelopeCbor.toString("hex"),
    predecessorWitness:
      recording === undefined ? null : ledgerWitnessJson(recording.witness()),
  }) as WorkflowJsonObject;
};
