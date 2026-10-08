import { createHash } from "node:crypto";

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  assertSecurityGradeEvidence,
  DA_PAYLOAD_VERSION,
  type DaPayload,
  EMPTY_MERKLE_TREE_ROOT,
} from "@al-ft/midgard-sdk";

import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import {
  makeWatcherDurablePayload,
  type WatcherDaProofInput,
} from "../../src/storage/durable-store.js";
import {
  WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION,
  type WatcherPublicDaEventToStep,
  type WatcherPublicDaPayload,
  type WatcherPublicDaProofBundle,
  type WatcherPublicDaTraceStep,
} from "../../src/storage/public-da-client.js";

// ---------------------------------------------------------------------------
// Fixtures: verified public DA records as a successful fetch from PEER
// ---------------------------------------------------------------------------

export const repeatHex = (value: number, length: number): string =>
  value.toString(16).padStart(2, "0").repeat(length);

export const FINGERPRINT = repeatHex(0x1a, 32);

export const HEADER_HASH = repeatHex(0xab, 28);

export const OTHER_MANIFEST_ID = repeatHex(0x7e, 32);

export const PEER = "da-peer-a";

/** The libp2p peer ID a parsed config derives from PEER's multiaddress. */
const PEER_ID = "12D3KooWAbcdefghijkmnopqrstuvwxyz1234A";

export const EVENT_KEY = "0a1b2c3d";

export const MARKER = makeDeploymentMarker(FINGERPRINT);

export const sha256Hex = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

export const identityOf = (
  manifestId: string = FINGERPRINT,
): VerifiedWatcherDeploymentIdentity => ({
  manifestId,
  network: "Preprod",
  trustRootId: "trust-root-a",
  blueprintHash: repeatHex(0x33, 32),
  fundingProfileBundleDigest: "ab".repeat(32),
  ruleBundleCommitment: repeatHex(0x44, 32),
  programCommitments: {},
  durableMarker: makeDeploymentMarker(manifestId),
});

export const daPayload = (headerHash: string): DaPayload => {
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 1n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  return {
    version: DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header: {
        prevUtxosRoot: repeatHex(0x01, 32),
        utxosRoot: repeatHex(0x02, 32),
        withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
        forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: repeatHex(0x03, 32),
        depositsRoot: EMPTY_MERKLE_TREE_ROOT,
        transitionTraceRoot: repeatHex(0x04, 32),
        eventToStepRoot: repeatHex(0x05, 32),
        validationTracesRoot: repeatHex(0x06, 32),
        ...counts,
        startTime: 1_000n,
        endTime: 1_999n,
        blockSlot: 42n,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        prevHeaderHash: repeatHex(0x07, 28),
        operatorVkey: repeatHex(0x08, 28),
        protocolVersion: 1n,
      },
      utxos: [],
      withdrawals: [],
      forced_transactions: [],
      transactions: [[repeatHex(0x09, 32), repeatHex(0x0a, 40)]],
      transaction_preimages: [[repeatHex(0x09, 32), repeatHex(0x0b, 64)]],
      forced_transaction_preimages: [],
      cek_program_material: [],
      deposits: [],
      transition_trace: [[repeatHex(0x0c, 32), repeatHex(0x0d, 48)]],
      event_to_step: [[repeatHex(0x0e, 32), repeatHex(0x0f, 8)]],
      validation_traces: [[repeatHex(0x10, 32), repeatHex(0x11, 24)]],
      validation_trace_witnesses: [],
      counts,
    },
  };
};

export const PROOF_BUNDLE_BYTES = Buffer.alloc(96, 0x5a);

export const TRACE_STEP_BYTES = Buffer.alloc(64, 0x6b);

export const TRACE_PROOF_BYTES = Buffer.alloc(48, 0x7c);

export const EVENT_ENTRY_BYTES = Buffer.alloc(32, 0x8d);

export const EVENT_PROOF_BYTES = Buffer.alloc(24, 0x9e);

const sourceOf = (protocol: DaRequestResponseProtocol) =>
  ({
    schemaVersion: WATCHER_PUBLIC_DA_CLIENT_SCHEMA_VERSION,
    deploymentFingerprint: FINGERPRINT,
    headerHash: HEADER_HASH,
    sourcePeerIdentity: PEER,
    sourcePeerId: PEER_ID,
    provenance: assertSecurityGradeEvidence({
      trustClass: "public_or_permissionless_da",
      sourceId: PEER_ID,
      grade: "security",
    }),
    attempts: Object.freeze([
      Object.freeze({
        peerIdentity: PEER,
        protocol,
        status: "success" as const,
      }),
    ]),
  }) as const;

const durableInput = (
  kind: WatcherDaProofInput["kind"],
  bytes: Buffer,
): WatcherDaProofInput =>
  Object.freeze({
    inputId: sha256Hex(bytes),
    kind,
    payload: makeWatcherDurablePayload(bytes.toString("hex")),
  });

export const publicDaPayload = async (
  envelope: Buffer,
): Promise<WatcherPublicDaPayload> =>
  Object.freeze({
    ...sourceOf(DaRequestResponseProtocol.payloadByHeader),
    payloadHash: sha256Hex(envelope),
    payloadEnvelopeCbor: envelope,
    innerPayloadCbor: Buffer.from(
      (await unwrapDaPayload(envelope, { maxPayloadBytes: 1_000_000 }))
        .innerBytes,
    ),
    durableInput: durableInput("da_payload", envelope),
  });

export const publicDaProofBundle = (): WatcherPublicDaProofBundle =>
  Object.freeze({
    ...sourceOf(DaRequestResponseProtocol.proofBundleByHeader),
    proofBundleHash: sha256Hex(PROOF_BUNDLE_BYTES),
    proofBundleBytes: PROOF_BUNDLE_BYTES,
    durableInput: durableInput("proof_input", PROOF_BUNDLE_BYTES),
  });

export const publicDaTraceStep = (): WatcherPublicDaTraceStep =>
  Object.freeze({
    ...sourceOf(DaRequestResponseProtocol.traceStepByIndex),
    stepIndex: 3,
    transitionStepBytes: TRACE_STEP_BYTES,
    transitionStepSha256: sha256Hex(TRACE_STEP_BYTES),
    membershipProofBytes: TRACE_PROOF_BYTES,
    membershipProofSha256: sha256Hex(TRACE_PROOF_BYTES),
  });

/** `entry === null` is a nonmembership answer for EVENT_KEY. */
export const publicDaEventToStep = (
  entry: Buffer | null = EVENT_ENTRY_BYTES,
): WatcherPublicDaEventToStep =>
  Object.freeze({
    ...sourceOf(DaRequestResponseProtocol.eventToStepByEvent),
    eventKey: Buffer.from(EVENT_KEY, "hex"),
    eventToStepEntryBytes: entry,
    eventToStepEntrySha256: entry === null ? null : sha256Hex(entry),
    membershipOrNonmembershipProofBytes: EVENT_PROOF_BYTES,
    membershipOrNonmembershipProofSha256: sha256Hex(EVENT_PROOF_BYTES),
  });
