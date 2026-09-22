import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { reconstructDaPayload } from "../transition-trace/reconstruct.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import {
  type CompleteCanonicalReplayContext,
  completeCanonicalReplayPredecessorEvidence,
} from "../workflow/complete-replay.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
  normalizeJournalJson,
} from "../workflow/journal.js";
import {
  canonicalHex,
  canonicalNaturalString,
  EVEN_HEX,
  exactJournalRecord,
  HEX_28,
  safeNaturalNumber,
} from "../workflow/native-index-artifact.js";
import {
  detectMintAuthorizationReplay,
  mintAuthorizationDetectionId,
  prepareMintAuthorizationReplay,
} from "./replay.js";

export const MINT_AUTHORIZATION_ARTIFACT =
  "midgard-mint-authorization-workflow-artifact-v1";

/** Serialized material is only a preimage; replay rederives the entire claim. */
const deriveMintAuthorizationWorkflowArtifact = async (
  value: JournalJsonObject,
) => {
  const record = exactJournalRecord(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "coordinate",
      "payloadEnvelopeCbor",
      "predecessorEnvelopeCbor",
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
  const predecessor =
    record.predecessorEnvelopeCbor === null
      ? undefined
      : await reconstructDaPayload({
          payloadEnvelopeCbor: Buffer.from(
            canonicalHex(
              record.predecessorEnvelopeCbor,
              EVEN_HEX,
              "mint authorization predecessor",
            ),
            "hex",
          ),
          expectedHeaderHash: current.header.prevHeaderHash,
        });
  const raw = exactJournalRecord(
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
  value: JournalJsonObject,
): ReturnType<typeof deriveMintAuthorizationWorkflowArtifact> => {
  const key = journalJsonDigest(normalizeJournalJson(value));
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
  const predecessor = completeCanonicalReplayPredecessorEvidence({
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
  const artifact = normalizeJournalJson({
    schemaVersion: MINT_AUTHORIZATION_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId: finding.detectionId,
    coordinate: finding.prepared.coordinate,
    payloadEnvelopeCbor:
      evidence.reconstruction.payloadEnvelopeCbor.toString("hex"),
    predecessorEnvelopeCbor:
      predecessor?.reconstruction.payloadEnvelopeCbor.toString("hex") ?? null,
  }) as JournalJsonObject;
  await admitMintAuthorizationWorkflowArtifact(artifact);
  return artifact;
};
