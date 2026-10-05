import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import { WATCHER_CARDANO_SECURITY_PARAMETER_K } from "midgard-watcher";

import type {
  HistoryChildActor,
  HistoryWindowOffer,
} from "../../src/devnet-stack/history-child-evidence.js";
import { digest } from "../../src/devnet-stack/history-window-canonical.js";

/** A well-framed hostile declaration, never a native receipt or readiness proof. */
export const untrustedHistoryOffer = (
  owner: HistoryChildActor,
): HistoryWindowOffer => {
  const point = { blockHash: "0".repeat(64), blockNo: "0", slot: "0" };
  const exact = { ...point, pointId: computeFraudProofRawL1PointId(point) };
  const fields = {
    actor: {
      role: "history-recorder" as const,
      runId: owner.runId,
      deploymentFingerprint: owner.deploymentFingerprint,
      codeStamp: owner.codeStamp,
      serviceSpecsDigest: owner.serviceSpecsDigest,
      attemptId: owner.attemptId,
    },
    sourceEpoch: owner.attemptId,
    authorityDigest: "0".repeat(64),
    startupDigest: "0".repeat(64),
    sealId: owner.attemptId,
    promotionDigest: "0".repeat(64),
    first: exact,
    last: exact,
    rowCount: 1,
    recoveryWindow: WATCHER_CARDANO_SECURITY_PARAMETER_K,
    rangeDigest: "0".repeat(64),
  };
  return {
    window: {
      ...fields,
      generation: digest(
        JSON.stringify(["history-full-window-seal-v1", fields]),
      ),
    },
    predecessor: null,
  };
};
