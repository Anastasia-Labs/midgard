import {
  type LaterFaultPosition,
  leafAdmitsLaterFault,
} from "../src/fraud-proof/first-fault-later-phases.js";
import { forcedVerdictSubject } from "../src/fraud-proof/proof-thread-substrate.js";
import type { RejectionReason } from "../src/rejection-reason.js";

export const context = { body_network_mismatch: false };
export const rejected = (reason: RejectionReason | null) =>
  forcedVerdictSubject({
    transactionId: "11".repeat(32),
    sourceKey: { transactionId: "77".repeat(32), outputIndex: 0n },
    rejectionReason: reason,
  });
export const admits = (reason: RejectionReason, observed: LaterFaultPosition) =>
  leafAdmitsLaterFault(rejected(reason), context, observed);

export const witness = (scriptIndex: bigint): LaterFaultPosition => ({
  phase: "unusedAudit",
  position: { kind: "witness", scriptIndex },
});
export const redeemer = (redeemerIndex: bigint): LaterFaultPosition => ({
  phase: "unusedAudit",
  position: { kind: "redeemer", redeemerIndex },
});
export const scan = (kind: bigint, index: bigint): LaterFaultPosition => ({
  phase: "execution",
  position: { coordinate: { kind, index }, subcheck: "NativeScriptScan" },
});
export const evaluation = (
  kind: bigint,
  index: bigint,
): LaterFaultPosition => ({
  phase: "execution",
  position: { coordinate: { kind, index }, subcheck: "NativeScriptEvaluation" },
});
export const integrity: LaterFaultPosition = { phase: "scriptIntegrity" };
export const receive = (kind: bigint, index: bigint): LaterFaultPosition => ({
  phase: "cek",
  position: { coordinate: { kind, index }, subcheck: "ReceiveLanguage" },
});
export const run = (kind: bigint, index: bigint): LaterFaultPosition => ({
  phase: "cek",
  position: { coordinate: { kind, index }, subcheck: "ScriptRun" },
});
export const inputAsset = (
  inputIndex: bigint,
  assetIndex: bigint,
): LaterFaultPosition => ({
  phase: "value",
  position: { kind: "input", inputIndex, assetIndex },
});
export const outputAsset = (
  outputIndex: bigint,
  assetIndex: bigint,
): LaterFaultPosition => ({
  phase: "value",
  position: { kind: "output", outputIndex, assetIndex },
});
export const mintAsset = (mintIndex: bigint): LaterFaultPosition => ({
  phase: "value",
  position: { kind: "mint", mintIndex },
});

export const purpose = (purpose_kind: bigint, purpose_index: bigint) => ({
  purpose_kind,
  purpose_index,
});
export const everyLaterPosition = [
  witness(0n),
  redeemer(0n),
  scan(0n, 0n),
  evaluation(0n, 0n),
  integrity,
  receive(3n, 0n),
  run(0n, 0n),
  inputAsset(0n, 0n),
  outputAsset(0n, 0n),
  mintAsset(0n),
];
