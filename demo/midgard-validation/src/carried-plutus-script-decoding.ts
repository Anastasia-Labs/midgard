import type { MidgardVersionedScript } from "@al-ft/midgard-core/codec";

import type { MidgardLedgerTx } from "./ledger-tx/types.js";
import { reject } from "./phase-a.validate-input-sets.js";
import { scriptDecodeRecordOfScript } from "./script-decode.step-walk.js";
import { RejectCodes, type RejectedTx } from "./types.js";

const ITEM_KIND_WITNESS = 0n;
const ITEM_KIND_OUTPUT = 1n;

const refusedPlutusScript = (
  txId: Buffer,
  script: Exclude<MidgardVersionedScript, { language: "NativeCardano" }>,
  itemKind: bigint,
  itemIndex: bigint,
): RejectedTx | null => {
  const result = scriptDecodeRecordOfScript(
    script.language === "PlutusV3" ? 3 : 128,
    script.scriptBytes,
  );
  return result.k === "refused"
    ? reject(
        txId,
        RejectCodes.ScriptProgramEncoding,
        `Plutus script decode refused class ${result.class} at bit ${result.bitOffset}`,
        itemKind === ITEM_KIND_WITNESS
          ? "phaseANativeScripts"
          : "scriptSources",
        {
          arm: "PlutusScriptUndecodable",
          itemKind,
          index: itemIndex,
          refusalClass: BigInt(result.class),
        },
      )
    : null;
};

/**
 * The witness item check, run at the witness-script phase in item order with
 * the native witness checks: a Plutus witness whose stored program does not
 * decode is the transaction's fault at that item.
 */
export const carriedPlutusWitnessScriptFault = (
  txId: Buffer,
  script: MidgardVersionedScript,
  witnessIndex: number,
): RejectedTx | null =>
  script.language === "NativeCardano"
    ? null
    : refusedPlutusScript(
        txId,
        script,
        ITEM_KIND_WITNESS,
        BigInt(witnessIndex),
      );

/**
 * The output item check, run at the script-source phase: an output reference
 * script whose stored Plutus program does not decode is the transaction's
 * fault at the lowest such output. Forced and normal sources run it alike.
 */
export const validateCarriedPlutusOutputScripts = (
  tx: Pick<MidgardLedgerTx, "txId" | "outputs">,
): RejectedTx | null => {
  for (const [index, output] of tx.outputs.entries()) {
    if (
      output.scriptRef === undefined ||
      output.scriptRef.language === "NativeCardano"
    )
      continue;
    const fault = refusedPlutusScript(
      tx.txId,
      output.scriptRef,
      ITEM_KIND_OUTPUT,
      BigInt(index),
    );
    if (fault !== null) return fault;
  }
  return null;
};
