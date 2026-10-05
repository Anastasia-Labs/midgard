import { transactionConsumesOutRef } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import { readAdmittedLocalKupmiosBoundary } from "./local-kupmios-http-ogmios-source.admit-local-kupmios-raw-block-at-point.js";
import { rawPoint } from "./local-kupmios-http-ogmios-source.fetch-json.js";
import {
  openOgmiosSession,
  sameKupoPoint,
  sameRawPoint,
} from "./local-kupmios-http-ogmios-source.open-ogmios-session.js";
import {
  exactKeys,
  naturalNumber,
  record,
} from "./local-kupmios-http-ogmios-source.parse-ogmios-block.js";
import type {
  KupoMatch,
  KupoPoint,
  LocalKupmiosHttpOgmiosSourceConfig,
  OgmiosRawTransactionAtPoint,
  OgmiosTip,
} from "./local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";
import {
  LocalKupmiosCheckpointChangedError,
  type LocalKupmiosFraudProofRawSource,
} from "./local-kupmios-raw-l1-authority.js";
import {
  admitFraudProofRawL1Point,
  type FraudProofRawL1Point,
  type FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";
import {
  inspectSignedWorkflowTransaction,
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "./signed-transaction-reconciliation.js";

/** Exact signed recovery under the concrete source's capture and transport scope. */
export const createLocalKupmiosSignedRecoveryReader =
  ({
    source,
    queryTip,
    getKupoCheckpoint,
    readRawTransaction,
    fetchMatches,
    utxoFromMatch,
    ogmiosWebSocketUrl,
    timeoutMs,
    webSocketFactory,
    signal,
    maxResponseBytes,
  }: {
    readonly source: LocalKupmiosFraudProofRawSource;
    readonly queryTip: () => Promise<OgmiosTip>;
    readonly getKupoCheckpoint: (slot: number) => Promise<KupoPoint>;
    readonly readRawTransaction: (input: {
      txHash: string;
      point: KupoPoint;
    }) => Promise<OgmiosRawTransactionAtPoint>;
    readonly fetchMatches: (pattern: string) => Promise<readonly KupoMatch[]>;
    readonly utxoFromMatch: (match: KupoMatch) => Promise<FraudProofRawL1Utxo>;
    readonly ogmiosWebSocketUrl: string;
    readonly timeoutMs: number;
    readonly webSocketFactory: NonNullable<
      LocalKupmiosHttpOgmiosSourceConfig["webSocketFactory"]
    >;
    readonly signal?: AbortSignal;
    readonly maxResponseBytes: number | undefined;
  }) =>
  async (
    input: SignedWorkflowTransaction,
  ): Promise<SignedTransactionRecoveryObservation> => {
    const signed = inspectSignedWorkflowTransaction(input);
    // Replacement authorization still requires stable expiry/spend evidence,
    // even when this source normally observes action prerequisites at inclusion.
    const boundary = await readAdmittedLocalKupmiosBoundary({
      source,
      observationDepth: "inclusion",
    });
    const canonicalPoint = boundary.ogmiosTip;
    let releaseFinalPoint = boundary.kupoCheckpoint;
    let retirementBoundaryRead = false;
    const readRetirementBoundary = async () => {
      if (retirementBoundaryRead) return;
      const stable = await readAdmittedLocalKupmiosBoundary({
        source,
        observationDepth: "recovery_finality",
      });
      if (!sameRawPoint(stable.ogmiosTip, canonicalPoint))
        throw new LocalKupmiosCheckpointChangedError(
          "Tip changed while establishing signed attempt retirement",
        );
      releaseFinalPoint = stable.kupoCheckpoint;
      retirementBoundaryRead = true;
    };
    let inclusionPoint: FraudProofRawL1Point | undefined;
    const inputs: { outRef: string; outputCbor: string }[] = [];
    const result = (
      status: SignedTransactionRecoveryObservation["status"],
      reason: string,
    ): SignedTransactionRecoveryObservation =>
      Object.freeze({
        transactionHash: input.transactionHash,
        signedTransactionCborHex: input.signedTransactionCborHex,
        status,
        reason,
        canonicalPoint,
        releaseFinalPoint,
        inputs: Object.freeze(inputs),
        ...(inclusionPoint === undefined ? {} : { inclusionPoint }),
      });
    const finish = async (
      status: SignedTransactionRecoveryObservation["status"],
      reason: string,
    ) => {
      const confirmation = exactKeys(
        await source.confirmCanonicalPoint({ point: releaseFinalPoint }),
        ["canonical", "point"],
        [],
        "signed recovery canonical confirmation",
      );
      if (
        confirmation.canonical !== true ||
        !sameRawPoint(
          admitFraudProofRawL1Point(
            confirmation.point,
            "signed recovery confirmed point",
          ),
          releaseFinalPoint,
        )
      )
        throw new LocalKupmiosCheckpointChangedError(
          "Signed recovery release-final boundary rolled back",
        );
      const after = await queryTip();
      if (!sameRawPoint(rawPoint(after), canonicalPoint))
        throw new LocalKupmiosCheckpointChangedError(
          "Canonical tip changed during signed transaction recovery",
        );
      return result(status, reason);
    };
    const tipCheckpoint = await getKupoCheckpoint(Number(canonicalPoint.slot));
    if (
      !sameKupoPoint(tipCheckpoint, {
        slot: Number(canonicalPoint.slot),
        blockHash: canonicalPoint.blockHash,
      })
    )
      return result("unknown", "Kupo has not indexed the exact canonical tip");
    const inclusion = await source.resolveTransactionInclusion!({
      txHash: input.transactionHash,
    });
    if (inclusion !== null) {
      const point = admitFraudProofRawL1Point(
        inclusion,
        "signed recovery inclusion",
      );
      const raw = await readRawTransaction({
        txHash: input.transactionHash,
        point: { slot: Number(point.slot), blockHash: point.blockHash },
      });
      const included = CML.Transaction.from_cbor_hex(raw.transactionCbor);
      if (
        !included.is_valid() ||
        included.body().to_cbor_hex() !== signed.body.to_cbor_hex() ||
        included.witness_set().to_canonical_cbor_hex() !==
          signed.transaction.witness_set().to_canonical_cbor_hex()
      )
        throw new Error(
          "Canonical transaction differs from the recorded signed body",
        );
      const confirmedInclusion = await getKupoCheckpoint(Number(point.slot));
      if (
        !sameKupoPoint(confirmedInclusion, {
          slot: Number(point.slot),
          blockHash: point.blockHash,
        })
      )
        throw new LocalKupmiosCheckpointChangedError(
          "Signed recovery inclusion rolled back",
        );
      inclusionPoint = point;
      return finish(
        "included",
        "Exact recorded transaction body is on the canonical chain",
      );
    }
    // Stable expiry and exact canonical absence retire the attempt even when a
    // rollback erased its parent's output creation. Input history is required
    // for rebroadcast or spend-based invalidation, not for proving elapsed TTL.
    if (
      signed.expiresAtSlot !== undefined &&
      BigInt(canonicalPoint.slot) >= signed.expiresAtSlot
    ) {
      await readRetirementBoundary();
      if (BigInt(releaseFinalPoint.slot) >= signed.expiresAtSlot)
        return finish(
          "expired",
          "Recorded TTL passed beyond the canonical recovery horizon and the exact transaction is absent",
        );
      return finish(
        "pending",
        "Recorded TTL passed at the tip but remains inside the canonical recovery horizon",
      );
    }
    let status: SignedTransactionRecoveryObservation["status"] = "rebroadcast";
    let reason =
      "Canonical transaction absent and every recorded input remains unspent";
    for (const outRef of signed.inputOutRefs) {
      const [transactionHash, index] = outRef.split("#");
      const matches = await fetchMatches(`${index}@${transactionHash}`);
      if (
        matches.length !== 1 ||
        matches[0]!.txHash !== transactionHash ||
        matches[0]!.outputIndex.toString() !== index
      ) {
        status = "unknown";
        reason = "A recorded input lacks exact canonical creation history";
        break;
      }
      const match = matches[0]!;
      const output = await utxoFromMatch(match);
      inputs.push({ outRef, outputCbor: output.outputCbor });
      if (match.spentAt !== null) {
        const spending = await readRawTransaction({
          txHash: match.spentAt.txHash,
          point: match.spentAt,
        });
        if (
          !transactionConsumesOutRef({
            transactionCbor: spending.transactionCbor,
            transactionId: match.spentAt.txHash,
            outRef,
          })
        )
          throw new Error(
            "Kupo input spend lacks its exact canonical consuming transaction",
          );
        await readRetirementBoundary();
        const stableSpend =
          BigInt(match.spentAt.slot) <= BigInt(releaseFinalPoint.slot);
        // An exact stable spend makes this signed body impossible regardless of
        // input role. This retires only the attempt: funding is re-observed and
        // reserved separately after every recorded signed attempt is resolved.
        // Keep scanning so missing canonical history still prevents retirement.
        if (stableSpend) {
          status = "invalidated";
          reason =
            "A recorded input is stably spent by another canonical transaction";
        } else if (status !== "invalidated") {
          status = "pending";
          reason = "A recorded input spend is not yet release-final";
        }
      }
    }
    if (status !== "rebroadcast") return finish(status, reason);
    // Missing TTL prevents expiry-based replacement, but does not prevent
    // observing the mempool or replaying the exact still-valid signed body.
    if (
      signed.expiresAtSlot !== undefined &&
      BigInt(canonicalPoint.slot) >= signed.expiresAtSlot
    )
      return finish(
        "pending",
        "Recorded TTL passed at the tip; release-final expiry proof is not yet available",
      );
    if (
      signed.validFromSlot !== undefined &&
      BigInt(canonicalPoint.slot) < signed.validFromSlot
    )
      return finish(
        "pending",
        "Recorded lower validity bound has not reached the canonical tip",
      );
    const mempool = await openOgmiosSession({
      url: ogmiosWebSocketUrl,
      timeoutMs,
      webSocketFactory,
      signal,
      maxResponseBytes,
    });
    try {
      const acquired = record(
        await mempool.request("acquireMempool", {}),
        "signed recovery mempool snapshot",
      );
      if (
        acquired.acquired !== "mempool" ||
        naturalNumber(acquired.slot, "mempool snapshot slot") <
          Number(canonicalPoint.slot)
      )
        return finish(
          "unknown",
          "Mempool snapshot predates the observed canonical tip",
        );
      const present = await mempool.request("hasTransaction", {
        id: input.transactionHash,
      });
      if (typeof present !== "boolean")
        throw new Error("Invalid mempool transaction verdict");
      if (present)
        return finish(
          "pending",
          "Recorded transaction remains in the node mempool",
        );
    } finally {
      await mempool.close();
    }
    return finish(status, reason);
  };
