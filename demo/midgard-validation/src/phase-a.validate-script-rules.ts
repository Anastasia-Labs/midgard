import {
  decodeMidgardVersionedScript,
  EMPTY_NULL_ROOT,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import { type MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";

import { carriedPlutusWitnessScriptFault } from "./carried-plutus-script-decoding.js";
import { type MidgardRawEnvelopePhaseAProjection } from "./ledger-tx/codec.js";
import type { MidgardLedgerTx } from "./ledger-tx/types.js";
import {
  codecErrorDetail,
  hashHexes,
  reject,
} from "./phase-a.validate-input-sets.js";
import { RejectCodes, RejectedTx } from "./types.js";

/**
 * The witness-script phase, in witness item order: a native witness must parse
 * and hold, and a Plutus witness's stored program must decode.
 */
export const validateWitnessScripts = (
  tx: MidgardLedgerTx | MidgardRawEnvelopePhaseAProjection["ledgerTx"],
): RejectedTx | null => {
  let witnessSigners: ReadonlySet<string> | undefined;
  for (const witness of tx.scriptWitnesses) {
    let script;
    try {
      script =
        "script" in witness
          ? witness.script
          : decodeMidgardVersionedScript(witness.versionedItemBytes);
    } catch (cause) {
      return reject(
        tx.txId,
        RejectCodes.InvalidFieldType,
        codecErrorDetail(cause),
        "phaseANativeScripts",
        { arm: "WitnessNativeScriptMalformed", index: BigInt(witness.index) },
      );
    }
    if (script.language !== "NativeCardano") {
      const plutusFault = carriedPlutusWitnessScriptFault(
        tx.txId,
        script,
        witness.index,
      );
      if (plutusFault !== null) return plutusFault;
      continue;
    }
    witnessSigners ??= new Set(hashHexes(tx.witnessKeyHashes));
    if (
      !verifyMidgardNativeScript(script.nativeScript, {
        validityIntervalStart: tx.validityIntervalStart,
        validityIntervalEnd: tx.validityIntervalEnd,
        witnessSigners,
      })
    ) {
      return reject(
        tx.txId,
        RejectCodes.NativeScriptInvalid,
        `native script verification failed for script index ${witness.index}`,
        "phaseANativeScripts",
        { arm: "WitnessNativeScriptFalse", index: BigInt(witness.index) },
      );
    }
  }
  return null;
};

export const validateRequiredObservers = (
  tx: Pick<MidgardLedgerTx, "txId" | "requiredObserverHashes">,
): RejectedTx | null => {
  if (tx.requiredObserverHashes.length < 2) {
    return null;
  }
  for (let index = 1; index < tx.requiredObserverHashes.length; index += 1) {
    if (
      Buffer.compare(
        tx.requiredObserverHashes[index - 1]!,
        tx.requiredObserverHashes[index]!,
      ) >= 0
    ) {
      return reject(
        tx.txId,
        RejectCodes.InvalidFieldType,
        `required observers must be strictly ordered and unique at index ${index}`,
        "phaseAScriptPreconditions",
        { arm: "ObserverOrderInvalid", index: BigInt(index) },
      );
    }
  }
  return null;
};

export const validateScriptEvaluationPreconditions = (
  tx: Pick<
    MidgardLedgerTx,
    | "txId"
    | "requiresPlutusEvaluation"
    | "scriptIntegrityHash"
    | "requiredObserverHashes"
    | "networkId"
  >,
): RejectedTx | null => {
  if (!tx.requiresPlutusEvaluation) {
    return null;
  }
  if (Buffer.from(tx.scriptIntegrityHash).equals(EMPTY_NULL_ROOT)) {
    return reject(
      tx.txId,
      RejectCodes.InvalidFieldType,
      "missing script_integrity_hash for plutus witness bundle",
      "phaseAScriptPreconditions",
      { arm: "ScriptIntegrityHashMissing" },
    );
  }

  if (tx.requiredObserverHashes.length > 0 && tx.networkId === undefined) {
    return reject(
      tx.txId,
      RejectCodes.InvalidFieldType,
      "network_id is required when plutus witness bundles use required observers",
      "phaseAScriptPreconditions",
      { arm: "ObserversForbiddenOnUntaggedNetwork" },
    );
  }

  return null;
};

/**
 * A normal transaction's declared execution units, summed over its redeemers,
 * must fit the per-transaction execution capability of the consensus profile
 * (Cardano's maxTxExUnits) in both components. Forced transactions carry no
 * redeemers in this release.
 */
export const validateDeclaredExUnits = (
  tx: Pick<MidgardLedgerTx, "txId" | "redeemers">,
  limits: MidgardConsensusProfile["limits"],
): RejectedTx | null => {
  let memory = 0n;
  let steps = 0n;
  for (const redeemer of tx.redeemers) {
    memory += redeemer.exUnits.memory;
    steps += redeemer.exUnits.steps;
  }
  const maxMemory = BigInt(limits.minSupportedTransactionExecutionMemoryUnits);
  const maxSteps = BigInt(limits.minSupportedTransactionExecutionCpuUnits);
  return memory > maxMemory || steps > maxSteps
    ? reject(
        tx.txId,
        RejectCodes.TxExUnits,
        `declared ex-units mem=${memory} steps=${steps} exceed the per-tx cap mem=${maxMemory} steps=${maxSteps}`,
        "phaseAScriptPreconditions",
      )
    : null;
};
