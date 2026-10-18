/**
 * Emulator support for the script-decode validator.
 *
 * The validator's compiled script is larger than one transaction may be
 * (32 KB against 16,384 B), so no transaction can publish it as a reference
 * script; the deployment publishes it out of band. Here the reference UTxO is
 * placed straight into the emulator ledger at an address no key or script can
 * spend, which is what such a publication leaves behind.
 */
import { buildScriptDecodeValidator } from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  credentialToAddress,
  type Emulator,
  type LucidEvolution,
  scriptHashToCredential,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  plutusItemScriptDecode,
  recordScriptDecode,
  type ScriptDecodeDeployment,
} from "../../src/script-decode/index.js";
import {
  network,
  readBlueprint,
  realBlueprintPath,
} from "./emulator/blueprints.js";

const REFERENCE_TX_HASH = "5d".repeat(32);

/** Places the validator as a reference script in the emulator ledger. */
export const injectScriptDecodeDeployment = (
  emulator: Emulator,
): ScriptDecodeDeployment => {
  const validator = buildScriptDecodeValidator(
    readBlueprint(realBlueprintPath) as never,
    network,
  );
  const reference: UTxO = {
    txHash: REFERENCE_TX_HASH,
    outputIndex: 0,
    address: credentialToAddress(
      network,
      scriptHashToCredential("2f".repeat(28)),
    ),
    assets: { lovelace: 150_000_000n },
    datum: null,
    datumHash: null,
    // As a ledger output carries it: the CBOR bytes of the script's CBOR.
    scriptRef: {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(validator.spendingScript.script),
    },
  };
  emulator.ledger[`${reference.txHash}${reference.outputIndex}`] = {
    utxo: reference,
    spent: false,
  };
  return { validator, reference };
};

const deployments = new WeakMap<Emulator, ScriptDecodeDeployment>();

/** The emulator's script-decode deployment, placed on first use. */
export const scriptDecodeDeploymentOf = (
  emulator: Emulator,
): ScriptDecodeDeployment => {
  const existing = deployments.get(emulator);
  if (existing !== undefined) return existing;
  const deployment = injectScriptDecodeDeployment(emulator);
  deployments.set(emulator, deployment);
  return deployment;
};

/**
 * The decode record of the script a Plutus item carries, minted on first use;
 * undefined for a native or malformed item, which takes no record.
 */
export const recordPlutusItem = async (
  emulator: Emulator,
  lucid: LucidEvolution,
  item: Uint8Array,
): Promise<UTxO | undefined> => {
  const decoded = plutusItemScriptDecode(item);
  if (decoded === undefined) return undefined;
  return (
    await recordScriptDecode({
      lucid,
      deployment: scriptDecodeDeploymentOf(emulator),
      language: decoded.language,
      scriptBytes: decoded.scriptBytes,
    })
  ).recordUtxo;
};
