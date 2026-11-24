/**
 * The defense material build itself, as the material worker runs it: the
 * block's evidence from its retained DA payload, checked against the header
 * the landed queue names; its predecessor ledger, read from the node's store
 * ledger at the parent's root over the job's read channel
 * (`ledger-port.ts`); then our trace of the case's event at the case's step
 * count. Its replay runs the event's scripts to the end, so it runs off the
 * node's main thread: a job is plain bytes and numbers with its read
 * channel's port, and its outcome is plain bytes and numbers, which cross
 * the thread boundary intact.
 */
import type { MessagePort } from "node:worker_threads";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import { canonicalBlockEvidenceFromVerifiedPayload } from "@al-ft/midgard-fault-proofs/evidence";
import {
  buildPortableValidationTraceDefenseMaterial,
  type PortableValidationTraceDefense,
} from "@al-ft/midgard-fault-proofs/validation-trace-dispute/defender";
import type * as SDKCore from "@al-ft/midgard-sdk/core";
import * as SDK from "@al-ft/midgard-sdk/fraud-proof";

import { decodeStoredPayload } from "../da/stored-payload.js";
import { workerLedger } from "../services/mpf-native-owner/worker-ledger.js";
import { ledgerReadClient } from "./ledger-port.js";

/** One build: the landed block, the read channel to its parent's ledger,
 * and the case. */
export type DefenseMaterialJob = Readonly<{
  payloadEnvelopeCbor: Uint8Array;
  schemaVersion: number;
  /** Where the node observed the landed block: the follower's cursor. */
  point: Readonly<{ slot: number; hash: string }>;
  headerHash: string;
  parentHeaderHash: string;
  parentUtxosRoot: string;
  /**
   * The read channel to the ledger at `parentUtxosRoot` (`serveLedgerReads`
   * on the other end); transferred with the job, closed when the build ends.
   */
  ledgerPort: MessagePort;
  eventKey: SDKCore.EventKey;
  stepCount: number;
  l1SlotConfig: L1SlotConfig;
}>;

/** A build's outcome; `unavailable` is a transient lack, retried. */
export type PortableDefenseOutcome =
  | Readonly<{ kind: "defend"; defense: PortableValidationTraceDefense }>
  | Readonly<{ kind: "operator_fault"; detail: string }>
  | Readonly<{ kind: "trace_disagrees"; detail: string }>
  | Readonly<{ kind: "unavailable"; detail: string }>;

const bytes = (view: Uint8Array): Buffer =>
  Buffer.from(view.buffer, view.byteOffset, view.byteLength);

export const buildPortableDefenseOutcome = async (
  job: DefenseMaterialJob,
): Promise<PortableDefenseOutcome> => {
  const reads = ledgerReadClient(job.ledgerPort);
  try {
    const payloadEnvelopeCbor = bytes(job.payloadEnvelopeCbor);
    const payload = await decodeStoredPayload({
      payloadCbor: payloadEnvelopeCbor,
      schemaVersion: job.schemaVersion,
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: {
        schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
        sourceMode: "local_node",
        provenance: {
          trustClass: "authenticated_cardano_l1",
          sourceId: "node-l1-follower",
          grade: "security",
        },
        chainPoint: {
          slot: BigInt(job.point.slot),
          blockHash: job.point.hash,
        },
        confirmationDepth: 1,
        headerHash: job.headerHash,
        header: payload.block_body.header,
      },
      payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "node-retained-da",
        grade: "security",
      },
    });
    const material = await buildPortableValidationTraceDefenseMaterial({
      evidence,
      predecessor: workerLedger(reads, job.parentUtxosRoot, reads.outputs),
      eventKey: job.eventKey,
      stepCount: job.stepCount,
      l1SlotConfig: job.l1SlotConfig,
    });
    switch (material.kind) {
      case "defend":
        return { kind: "defend", defense: material };
      case "operator_fault":
        return {
          kind: "operator_fault",
          detail: `the replay rejects the event the block accepted${material.rejection === undefined ? "" : `: ${material.rejection.code}`}`,
        };
      case "trace_disagrees":
        return { kind: "trace_disagrees", detail: material.detail };
    }
  } catch (cause) {
    return { kind: "unavailable", detail: formatUnknownError(cause) };
  } finally {
    reads.close();
  }
};
