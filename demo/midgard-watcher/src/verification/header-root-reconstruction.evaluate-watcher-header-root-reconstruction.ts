import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { canonicalBlockEvidenceFromVerifiedPayload } from "@al-ft/midgard-fault-proofs";

import {
  makeWatcherDurablePayload,
  type WatcherReconstructedState,
} from "../storage/durable-store.js";
import {
  classifyFailure,
  countSetFromCanonical,
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
  try {
    const unwrapped = await unwrapDaPayload(input.payloadEnvelopeCbor, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    });
    payloadSha256 = sha256Hex(unwrapped.innerBytes);
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

  try {
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: input.observation,
      payloadEnvelopeCbor: input.payloadEnvelopeCbor,
      daProvenance: input.daProvenance,
      ...(input.minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth: input.minimumConfirmationDepth }),
    });
    const reconstructedRoots = rootSetFromCanonical(
      evidence.reconstruction.roots,
    );
    const reconstructedCounts = countSetFromCanonical(
      evidence.reconstruction.counts,
    );
    return digestResult({
      ...common,
      action: "accept",
      reasonCodes: [],
      rootMismatches: [],
      countMismatches: [],
      reconstructedRoots,
      reconstructedCounts,
      payloadSha256: sha256Hex(evidence.reconstruction.payloadCbor),
    });
  } catch (error) {
    const classification = classifyFailure(error);
    return digestResult({
      ...common,
      action: "reject",
      reasonCodes: orderReasonCodes(classification.reasonCodes),
      rootMismatches: classification.rootMismatches,
      countMismatches: classification.countMismatches,
      reconstructedRoots: null,
      reconstructedCounts: null,
    });
  }
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
 * reconstruction. `inputIds` must be the exact W21 canonical-store input ids
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
