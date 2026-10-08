import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
  decodePayloadStrict,
  TransitionTraceChallengerError,
} from "@al-ft/midgard-fault-proofs";
import type { DaPayload } from "@al-ft/midgard-sdk";

import {
  makeWatcherDurablePayload,
  type WatcherReconstructedState,
} from "../storage/durable-store.js";
import {
  type Classification,
  classifyFailure,
  countSetFromCanonical,
  declaredCountFailures,
  digestResult,
  type EvaluateWatcherHeaderRootReconstructionInput,
  orderReasonCodes,
  rootSetFromCanonical,
} from "./header-root-reconstruction.classify-failure.js";
import {
  countSetFromHeader,
  fail,
  rootSetFromHeader,
  sha256Hex,
  WATCHER_HEADER_COUNT_FIELDS,
  WATCHER_HEADER_ROOT_FIELDS,
  WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION,
  type WatcherHeaderCountSet,
  type WatcherHeaderRootReconstructionResult,
  type WatcherHeaderRootSet,
} from "./header-root-reconstruction.make-watcher-authenticated-header-observation.js";

export const evaluateWatcherHeaderRootReconstruction = async (
  input: EvaluateWatcherHeaderRootReconstructionInput,
): Promise<WatcherHeaderRootReconstructionResult> => {
  const payloadEnvelopeSha256 = sha256Hex(input.payloadEnvelopeCbor);
  // Digest-only unwrap. The authoritative decode happens inside the canonical
  // reconstruction; this exists so a rejected block still records which inner
  // byte string was examined.
  let payloadSha256: string | null = null;
  let innerBytes: Uint8Array | null = null;
  try {
    const unwrapped = await unwrapDaPayload(input.payloadEnvelopeCbor, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    });
    innerBytes = unwrapped.innerBytes;
    payloadSha256 = sha256Hex(innerBytes);
  } catch {
    payloadSha256 = null;
  }

  const header = input.observation.header;
  const headerRoots = rootSetFromHeader(header);
  const headerCounts = countSetFromHeader(header);
  const common = {
    schemaVersion: WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION,
    headerRoots,
    headerCounts,
    headerHash: input.observation.headerHash,
    headerPrevUtxosRoot: header.prevUtxosRoot,
    payloadEnvelopeSha256,
    payloadSha256,
  } as const;

  const reject = (classification: Classification) =>
    digestResult({
      ...common,
      action: "reject",
      reasonCodes: orderReasonCodes(classification.reasonCodes),
      rootMismatches: classification.rootMismatches,
      countMismatches: classification.countMismatches,
      reconstructedRoots: null,
      reconstructedCounts: null,
    });

  let evidence: CanonicalBlockEvidence;
  try {
    evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: input.observation,
      payloadEnvelopeCbor: input.payloadEnvelopeCbor,
      daProvenance: input.daProvenance,
      ...(input.minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth: input.minimumConfirmationDepth }),
    });
  } catch (error) {
    return reject(
      declaredCountFailureBefore(error, innerBytes, headerCounts) ??
        classifyFailure(error),
    );
  }
  const declared = declaredCountFailures(
    evidence.reconstruction.payload,
    headerCounts,
  );
  const declaredFailure = declared.before ?? declared.after;
  if (declaredFailure !== null) return reject(declaredFailure);
  return digestResult({
    ...common,
    action: "accept",
    reasonCodes: [],
    rootMismatches: [],
    countMismatches: [],
    reconstructedRoots: rootSetFromCanonical(evidence.reconstruction.roots),
    reconstructedCounts: countSetFromCanonical(evidence.reconstruction.counts),
    payloadSha256: sha256Hex(evidence.reconstruction.payloadCbor),
  });
};

/**
 * A reconstruction failure a declared-count comparison would have preceded.
 * Every canonical reconstruction error is raised after the payload decodes,
 * so a payload that decodes had its declared counts checked first; the
 * header comparison additionally preceded every error raised after root
 * authentication, that is every error but a header or root mismatch.
 */
const declaredCountFailureBefore = (
  error: unknown,
  innerBytes: Uint8Array | null,
  headerCounts: WatcherHeaderCountSet,
): Classification | null => {
  if (!(error instanceof TransitionTraceChallengerError) || innerBytes === null)
    return null;
  let payload: DaPayload;
  try {
    payload = decodePayloadStrict(innerBytes);
  } catch {
    return null;
  }
  const declared = declaredCountFailures(payload, headerCounts);
  const beforeRoots =
    error.code === "headerMismatch" || error.code === "rootMismatch";
  return declared.before ?? (beforeRoots ? null : declared.after);
};

// ---------------------------------------------------------------------------
// Durable record
// ---------------------------------------------------------------------------

/**
 * Canonical CBOR commitment persisted as the reconstructed state:
 * `[ header_hash, [8 roots], [7 counts] ]`, all in the module's fixed field
 * order. It is derived only from values the reconstruction and the L1 header
 * agree on, so the record is reproducible from the same bytes.
 */
const reconstructedStateBytes = (
  result: WatcherHeaderRootReconstructionResult,
  roots: WatcherHeaderRootSet,
  counts: WatcherHeaderCountSet,
): Buffer =>
  encodeCborArrayRaw([
    encodeCborBytes(Buffer.from(result.headerHash, "hex")),
    encodeCborArrayRaw(
      WATCHER_HEADER_ROOT_FIELDS.map((field) =>
        encodeCborBytes(Buffer.from(roots[field], "hex")),
      ),
    ),
    encodeCborArrayRaw(
      WATCHER_HEADER_COUNT_FIELDS.map((field) =>
        encodeCborUnsigned(BigInt(counts[field])),
      ),
    ),
  ]);

/**
 * Builds the W03-reserved `WatcherReconstructedState` record for an accepted
 * reconstruction. `inputIds` must be the exact canonical evidence input ids
 * whose bytes were reconstructed; nothing else is accepted as a source.
 */
export const makeWatcherHeaderRootReconstructedState = (input: {
  readonly result: WatcherHeaderRootReconstructionResult;
  readonly chainPointId: string;
  readonly inputIds: readonly string[];
}): WatcherReconstructedState => {
  const result = input.result;
  if (
    result.schemaVersion !== WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION
  ) {
    fail("unsupported_schema", "$.result.schemaVersion");
  }
  if (
    result.action !== "accept" ||
    result.reconstructedRoots === null ||
    result.reconstructedCounts === null
  ) {
    fail("result_not_accepted", "$.result.action");
  }
  const roots = result.reconstructedRoots as WatcherHeaderRootSet;
  const counts = result.reconstructedCounts as WatcherHeaderCountSet;
  if (typeof input.chainPointId !== "string" || input.chainPointId === "") {
    fail("invalid_input_ids", "$.chainPointId");
  }
  if (input.inputIds.length === 0) {
    fail("invalid_input_ids", "$.inputIds");
  }
  const seen = new Set<string>();
  for (const [index, inputId] of input.inputIds.entries()) {
    if (typeof inputId !== "string" || inputId === "" || seen.has(inputId)) {
      fail("invalid_input_ids", `$.inputIds[${index.toString()}]`);
    }
    seen.add(inputId);
  }
  return Object.freeze({
    blockHash: result.headerHash,
    chainPointId: input.chainPointId,
    priorStateRoot: result.headerPrevUtxosRoot,
    postStateRoot: roots.utxos_root,
    inputIds: Object.freeze([...input.inputIds]),
    state: makeWatcherDurablePayload(
      reconstructedStateBytes(result, roots, counts).toString("hex"),
    ),
  });
};
