/** The prepared artifact of a validation-trace dispute workflow: the claim
 * the challenger commits at Open and the terminal and initial states it
 * presents at VerifySource, all built from the DA replay of the detected
 * event, in a journal-safe encoding. */
import {
  decodeMidgardValidationMachineState,
  encodeMidgardValidationMachineState,
  type MidgardValidationMachineState,
  type MidgardValidationTraceProof,
} from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { exact } from "../workflow/field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";

export const VALIDATION_TRACE_DISPUTE_ARTIFACT =
  "midgard-validation-trace-dispute-artifact-v1" as const;

const HASH_32 = /^[0-9a-f]{64}$/u;
/** A block header hash: blake2b-224. */
const HEADER_HASH = /^[0-9a-f]{56}$/u;
const HEX = /^(?:[0-9a-f]{2})+$/u;
const DECIMAL = /^(?:0|[1-9][0-9]*)$/u;

/** What the open step needs from the replay, decoded. */
export type ValidationTraceDisputeArtifact = Readonly<{
  headerHash: string;
  /** The detected event's key, as the claim witness binds it. */
  eventKeyCbor: string;
  claim: SDK.ValidationDisputeClaim;
  terminal: MidgardValidationMachineState;
  terminalProof: MidgardValidationTraceProof;
  initialState: MidgardValidationMachineState;
}>;

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

export const encodeValidationTraceDisputeArtifact = ({
  headerHash,
  material,
}: {
  readonly headerHash: string;
  readonly material: ValidationTraceClaimMaterial;
}): JournalJsonObject =>
  Object.freeze({
    schemaVersion: VALIDATION_TRACE_DISPUTE_ARTIFACT,
    headerHash,
    eventKeyCbor: Data.to(
      material.claimWitness.event_to_step_membership.key,
      SDK.EventKey,
    ),
    claimWitnessCbor: Data.to(
      material.claimWitness,
      SDK.ValidationClaimWitness,
    ),
    challengerRoot: material.challengerRoot,
    stepCount: material.stepCount.toString(),
    terminalStateCbor: hex(
      encodeMidgardValidationMachineState(material.terminal),
    ),
    terminalProof: Object.freeze({
      stateIndex: material.terminalLeafProof.stateIndex,
      stateHash: hex(material.terminalLeafProof.stateHash),
      siblings: Object.freeze(material.terminalLeafProof.siblings.map(hex)),
    }),
    initialStateCbor: hex(
      encodeMidgardValidationMachineState(material.initialState),
    ),
  });

const requireHex = (value: unknown, pattern: RegExp, label: string): string => {
  if (typeof value !== "string" || !pattern.test(value))
    throw new Error(`validation-trace artifact ${label} is malformed`);
  return value;
};

/** The artifact `value` encodes, or throws. */
export const parseValidationTraceDisputeArtifact = (
  value: JournalJsonObject,
): ValidationTraceDisputeArtifact => {
  const fields = exact(
    value,
    [
      "schemaVersion",
      "headerHash",
      "eventKeyCbor",
      "claimWitnessCbor",
      "challengerRoot",
      "stepCount",
      "terminalStateCbor",
      "terminalProof",
      "initialStateCbor",
    ],
    "validation-trace artifact",
  );
  if (fields.schemaVersion !== VALIDATION_TRACE_DISPUTE_ARTIFACT)
    throw new Error("validation-trace artifact has another schema");
  const proof = exact(
    fields.terminalProof,
    ["stateIndex", "stateHash", "siblings"],
    "validation-trace artifact terminal proof",
  );
  if (
    typeof proof.stateIndex !== "number" ||
    !Number.isSafeInteger(proof.stateIndex) ||
    proof.stateIndex < 0 ||
    !Array.isArray(proof.siblings)
  )
    throw new Error("validation-trace artifact terminal proof is malformed");
  const stepCount = requireHex(fields.stepCount, DECIMAL, "step count");
  const eventKeyCbor = requireHex(fields.eventKeyCbor, HEX, "event key");
  const claimWitness = Data.from(
    requireHex(fields.claimWitnessCbor, HEX, "claim witness"),
    SDK.ValidationClaimWitness,
  );
  if (
    Data.to(claimWitness.event_to_step_membership.key, SDK.EventKey) !==
    eventKeyCbor
  )
    throw new Error("validation-trace artifact names two events");
  const state = (label: string, raw: unknown) =>
    decodeMidgardValidationMachineState(
      Buffer.from(requireHex(raw, HEX, label), "hex"),
    );
  return Object.freeze({
    headerHash: requireHex(fields.headerHash, HEADER_HASH, "header hash"),
    eventKeyCbor,
    claim: Object.freeze({
      claimWitness,
      challengerRoot: requireHex(
        fields.challengerRoot,
        HASH_32,
        "challenger root",
      ),
      stepCount: BigInt(stepCount),
    }),
    terminal: state("terminal state", fields.terminalStateCbor),
    terminalProof: Object.freeze({
      stateIndex: proof.stateIndex,
      stateHash: Buffer.from(
        requireHex(proof.stateHash, HASH_32, "terminal hash"),
        "hex",
      ),
      siblings: Object.freeze(
        (proof.siblings as readonly unknown[]).map((sibling) =>
          Buffer.from(requireHex(sibling, HASH_32, "proof sibling"), "hex"),
        ),
      ),
    }),
    initialState: state("initial state", fields.initialStateCbor),
  });
};

/** The artifact of the validation-trace workflow `context` runs, or throws
 * for another workflow's, or for one naming another block. */
export const validationTraceArtifactOf = (
  context: Pick<FraudProofWorkflowAdapterContext, "identity" | "artifact">,
): ValidationTraceDisputeArtifact => {
  if (
    context.identity.category !== "validationTraceDispute" ||
    context.identity.target.kind !== "state_queue_header"
  )
    throw new Error("validation-trace adapter received another workflow");
  const artifact = parseValidationTraceDisputeArtifact(context.artifact);
  if (artifact.headerHash !== context.identity.target.headerHash)
    throw new Error("validation-trace artifact names another block");
  return artifact;
};
