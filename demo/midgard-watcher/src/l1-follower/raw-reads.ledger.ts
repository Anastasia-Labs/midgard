import { chainPoint, TransportRequestError } from "@al-ft/l1-node-transport";
import type { FraudProofRawL1Utxo } from "@al-ft/midgard-fault-proofs";
import {
  decodeLedgerUtxos,
  type OutRef,
  type Point,
  type WalletLedger,
} from "@al-ft/midgard-l1-follower";
import { CML } from "@lucid-evolution/lucid";

import { isWatcherL1TransientFailure } from "../l1/transient-failure.js";
import type { LedgerOutputsAt } from "./raw-reads.types.js";
import { outRefLabel, rawUtxo } from "./reads.js";

/**
 * One `utxo_by_txin` answer at an acquired point, or why there is none:
 * `too_old` (the point is more than k blocks below the node's tip; it never
 * becomes acquirable again), `not_on_chain` (a fork the follower has not
 * rolled back yet), `unavailable` (the node or its transport did not answer:
 * `isWatcherL1TransientFailure`) or `failed` (any other failure, such as an
 * answer that does not decode). `not_on_chain` and `unavailable` are
 * transient; `failed` is not.
 */
export type LedgerOutputsAnswer =
  | Readonly<{ kind: "ok"; outputs: ReadonlyMap<string, FraudProofRawL1Utxo> }>
  | Readonly<{
      kind: "too_old" | "not_on_chain" | "unavailable" | "failed";
      detail: string;
    }>;

export type LedgerOutputsQuery = (
  point: Point,
  outRefs: readonly OutRef[],
) => Promise<LedgerOutputsAnswer>;

const failure = (error: unknown): LedgerOutputsAnswer => {
  const detail = error instanceof Error ? error.message : String(error);
  const code = error instanceof TransportRequestError ? error.code : null;
  return {
    kind:
      code === "acquire_point_too_old"
        ? "too_old"
        : code === "acquire_point_not_on_chain"
          ? "not_on_chain"
          : isWatcherL1TransientFailure(error)
            ? "unavailable"
            : "failed",
    detail,
  };
};

/**
 * The ledger-state input query over the node's LocalStateQuery: the
 * `utxo_by_txin` answer at an acquired point, each output canonicalised as
 * the stored bodies are.
 */
export const ledgerOutputsQueryFromTransport =
  (ledger: WalletLedger): LedgerOutputsQuery =>
  async (point, outRefs) => {
    let answer: Uint8Array;
    try {
      answer = await ledger.withLedgerState(
        chainPoint(BigInt(point.slot), point.hash.toString("hex")),
        (session) =>
          session.query({
            query: "utxo_by_txin",
            txIns: outRefs.map((outRef) => ({
              txId: outRef.txHash.toString("hex"),
              index: outRef.index,
            })),
          }),
      );
    } catch (error) {
      return failure(error);
    }
    const outputs = new Map<string, FraudProofRawL1Utxo>();
    try {
      for (const utxo of decodeLedgerUtxos(answer)) {
        const output = CML.TransactionOutput.from_cbor_bytes(utxo.outputCbor);
        try {
          const label = outRefLabel(utxo.outRef);
          outputs.set(label, rawUtxo(label, output));
        } finally {
          output.free();
        }
      }
    } catch (error) {
      return failure(error);
    }
    return { kind: "ok", outputs };
  };

/**
 * The ledger-state input resolver the raw reads use: a point the node
 * cannot acquire (or any transport failure) is null, and the read then
 * refuses with a named reason.
 */
export const ledgerOutputsFromTransport = (
  ledger: WalletLedger,
): LedgerOutputsAt => {
  const query = ledgerOutputsQueryFromTransport(ledger);
  return async (point, outRefs) => {
    const answer = await query(point, outRefs);
    return answer.kind === "ok" ? answer.outputs : null;
  };
};
