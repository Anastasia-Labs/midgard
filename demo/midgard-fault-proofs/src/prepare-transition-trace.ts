import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { stringifyJson } from "./json-file.js";
import {
  detectTransitionTraceFaults,
  reconstructDaPayload,
  type TransitionTraceDetection,
  transitionTraceFinalIndex,
} from "./transition-trace/index.js";
import { readValidationDisputeCborFile } from "./validation-dispute/from-files.js";

export const TRANSITION_TRACE_PREPARE_SCHEMA_VERSION =
  "midgard-transition-trace-prepare-v1" as const;

export type PrepareTransitionTraceFromDaEnvelopeConfig = {
  readonly daPayloadEnvelopePath: string;
  readonly headerHash: string;
  readonly outputDir?: string;
};

export type PreparedTransitionTraceDetection = {
  readonly kind: TransitionTraceDetection["kind"];
  readonly invariant: string;
  readonly diagnostic: string;
  readonly buildable: boolean;
  readonly finalIndex: number | null;
  readonly reason?: string;
  readonly proofPath?: string;
};

export type PreparedTransitionTraceGuidance = {
  readonly evidence:
    | "omittedDueL1Events"
    | "outOfWindowSourceEvents"
    | "acceptedTransactionTransitionMismatches"
    | "l2TransactionTransitions";
  readonly kind:
    | "omittedDueL1Event"
    | "outOfWindowSourceEvent"
    | "acceptedTransactionTransitionMismatch"
    | "invalidOneStepTransition";
  readonly buildableFromRetainedDaOnly: false;
  readonly reason: string;
};

export type PreparedTransitionTraceOutput = {
  readonly schemaVersion: typeof TRANSITION_TRACE_PREPARE_SCHEMA_VERSION;
  readonly headerHash: string;
  readonly outputDir: string;
  readonly planPath: string;
  readonly proofPaths: readonly string[];
  readonly detections: readonly PreparedTransitionTraceDetection[];
  readonly guidance: readonly PreparedTransitionTraceGuidance[];
};

const RETAINED_DA_ONLY_GUIDANCE = [
  {
    evidence: "omittedDueL1Events",
    kind: "omittedDueL1Event",
    buildableFromRetainedDaOnly: false,
    reason:
      "Requires an authentic surviving L1 event NFT reference input and its ledger-sorted event_ref_input_index.",
  },
  {
    evidence: "outOfWindowSourceEvents",
    kind: "outOfWindowSourceEvent",
    buildableFromRetainedDaOnly: false,
    reason:
      "Requires an authentic surviving L1 event NFT reference input and its ledger-sorted event_ref_input_index.",
  },
  {
    evidence: "acceptedTransactionTransitionMismatches",
    kind: "acceptedTransactionTransitionMismatch",
    buildableFromRetainedDaOnly: false,
    reason:
      "Requires authenticated validation-claim and terminal-ledger evidence that retained DA alone does not supply.",
  },
  {
    evidence: "l2TransactionTransitions",
    kind: "invalidOneStepTransition",
    buildableFromRetainedDaOnly: false,
    reason:
      "Requires authenticated predecessor-ledger mutation witnesses that retained DA alone does not supply.",
  },
] as const satisfies readonly PreparedTransitionTraceGuidance[];

const proofFileName = (
  index: number,
  kind: TransitionTraceDetection["kind"],
): string =>
  `proof-${index.toString().padStart(3, "0")}-${kind
    .replace(/[A-Z]/gu, (letter) => `-${letter.toLowerCase()}`)
    .replace(/^-+/u, "")}.cbor`;

/**
 * Authenticates retained-DA envelope bytes against a caller-pinned committed
 * header hash, detects every header-derivable transition-trace fault, and
 * writes canonical Data CBOR proofs plus an auditable plan.
 */
export const prepareTransitionTraceFromDaEnvelope = async ({
  daPayloadEnvelopePath,
  headerHash,
  outputDir,
}: PrepareTransitionTraceFromDaEnvelopeConfig): Promise<PreparedTransitionTraceOutput> => {
  const payloadEnvelopeHex = await readValidationDisputeCborFile(
    daPayloadEnvelopePath,
    "--da-payload-envelope",
  );
  const reconstruction = await reconstructDaPayload({
    payloadEnvelopeCbor: Buffer.from(payloadEnvelopeHex, "hex"),
    expectedHeaderHash: headerHash,
  });
  const resolvedOutputDir =
    outputDir ?? `transition-trace-${reconstruction.headerHash}`;
  await mkdir(resolvedOutputDir, { recursive: true });

  const detections = await detectTransitionTraceFaults(reconstruction);
  const preparedDetections: PreparedTransitionTraceDetection[] = [];
  const proofWrites: Promise<void>[] = [];
  const proofPaths: string[] = [];

  for (const [index, detection] of detections.entries()) {
    if (!detection.buildable) {
      preparedDetections.push({
        kind: detection.kind,
        invariant: detection.invariant,
        diagnostic: detection.diagnostic,
        buildable: false,
        finalIndex: null,
        reason: detection.reason,
      });
      continue;
    }
    const fileName = proofFileName(index, detection.kind);
    const proofPath = join(resolvedOutputDir, fileName);
    const proofCbor = Data.to(detection.proof, SDK.TransitionFaultProof);
    proofPaths.push(proofPath);
    proofWrites.push(writeFile(proofPath, `${proofCbor}\n`, "utf8"));
    preparedDetections.push({
      kind: detection.kind,
      invariant: detection.invariant,
      diagnostic: detection.diagnostic,
      buildable: true,
      finalIndex: transitionTraceFinalIndex(detection.proof),
      proofPath: fileName,
    });
  }

  const planPath = join(resolvedOutputDir, "plan.json");
  await Promise.all([
    ...proofWrites,
    writeFile(
      planPath,
      stringifyJson({
        schemaVersion: TRANSITION_TRACE_PREPARE_SCHEMA_VERSION,
        headerHash: reconstruction.headerHash,
        evidence: {
          kind: "retained-da-envelope",
          envelopePath: daPayloadEnvelopePath,
          authenticatedByteForByte: true,
        },
        proofCount: proofPaths.length,
        detections: preparedDetections,
        guidance: RETAINED_DA_ONLY_GUIDANCE,
      }),
      "utf8",
    ),
  ]);

  return {
    schemaVersion: TRANSITION_TRACE_PREPARE_SCHEMA_VERSION,
    headerHash: reconstruction.headerHash,
    outputDir: resolvedOutputDir,
    planPath,
    proofPaths,
    detections: preparedDetections,
    guidance: RETAINED_DA_ONLY_GUIDANCE,
  };
};
