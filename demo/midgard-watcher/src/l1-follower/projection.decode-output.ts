import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import {
  lockOutput,
  queueOutput,
} from "../indexers/authenticated-state-queue-observation.queue-output.js";
import type { WatcherProjectionDeployment } from "./projection.js";

/**
 * Decoding one created output for the watcher projection (`projection.ts`):
 * a queue root or node, a CorrectionLock, or a `malformed` row the view
 * reports while it is live.
 */

const hexBytes = (hex: string): Buffer => Buffer.from(hex, "hex");

export type DecodedRow =
  | Readonly<{
      kind: "root" | "node";
      headerHash: Buffer | null;
      nextHeaderHash: Buffer | null;
      headerCbor: Buffer | null;
      stateQueueNodeCbor: Buffer | null;
      datumCbor: Buffer;
    }>
  | Readonly<{ kind: "lock"; datumCbor: Buffer }>
  | Readonly<{ kind: "malformed"; reason: string }>;

const describe = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/**
 * Decodes one created output. Null: neither a queue nor a lock output (for
 * example a payment to the queue address without the policy, or the
 * hub-oracle output).
 */
export const decodeOutput = (
  output: CML.TransactionOutput,
  outRef: string,
  addresses: Readonly<{ stateQueue: string; correctionLock: string }>,
  deployment: WatcherProjectionDeployment,
): DecodedRow | null => {
  try {
    const queue = queueOutput({
      output,
      outRef,
      stateQueueAddress: addresses.stateQueue,
      stateQueuePolicyId: deployment.stateQueueMint,
    });
    if (queue !== null) {
      if (queue.node.headerHash === null) {
        const datum = output.datum()?.as_datum();
        if (datum === undefined)
          return { kind: "malformed", reason: "root has no inline datum" };
        return {
          kind: "root",
          headerHash: null,
          nextHeaderHash:
            queue.nextHeaderHash === null
              ? null
              : hexBytes(queue.nextHeaderHash),
          headerCbor: null,
          stateQueueNodeCbor: null,
          datumCbor: hexBytes(datum.to_canonical_cbor_hex()),
        };
      }
      const header = queue.header;
      if (header === null)
        return { kind: "malformed", reason: "node has no decoded header" };
      return {
        kind: "node",
        headerHash: hexBytes(header.headerHash),
        nextHeaderHash:
          queue.nextHeaderHash === null ? null : hexBytes(queue.nextHeaderHash),
        headerCbor: hexBytes(header.headerCborHex),
        stateQueueNodeCbor: hexBytes(header.stateQueueNodeCborHex),
        datumCbor: hexBytes(header.linkedListDatumCborHex),
      };
    }
    const lock = lockOutput({
      output,
      outRef,
      correctionLockAddress: addresses.correctionLock,
      hubOraclePolicyId: deployment.hubOracleMint,
    });
    if (lock === null) return null;
    return {
      kind: "lock",
      datumCbor: hexBytes(Data.to(lock.datum, SDK.CorrectionLockDatum)),
    };
  } catch (error) {
    return { kind: "malformed", reason: describe(error) };
  }
};

/** The CML outputs a tx created on chain: its outputs, or a failed tx's collateral return. */
export const createdCmlOutput = (
  body: CML.TransactionBody,
  isValid: boolean,
  index: number,
): CML.TransactionOutput | undefined => {
  if (isValid) {
    const outputs = body.outputs();
    return index < outputs.len() ? outputs.get(index) : undefined;
  }
  return body.collateral_return();
};
