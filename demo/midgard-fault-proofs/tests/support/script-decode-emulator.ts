/**
 * Emulator support for the script-decode validator.
 *
 * Initialization publishes the validator and its class stakers as reference
 * scripts and registers the stakers' reward accounts (a node test runs that
 * from a genuine Init). Here the reference UTxOs are placed straight into the
 * emulator ledger, at an address no key or script can spend, and the reward
 * accounts are registered in the emulator's chain state, which is what that
 * deployment leaves behind.
 */
import {
  buildScriptDecodeValidator,
  SCRIPT_DECODE_STAKER_CLASS_NAMES,
  type ScriptDecodeStakerClassName,
} from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  credentialToAddress,
  type Emulator,
  type LucidEvolution,
  type Script,
  scriptHashToCredential,
  type UTxO,
  validatorToRewardAddress,
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

/** Places `script` as a reference script at output `index` of the ledger. */
const placeReference = (
  emulator: Emulator,
  outputIndex: number,
  script: Script,
): UTxO => {
  const reference: UTxO = {
    txHash: REFERENCE_TX_HASH,
    outputIndex,
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
      script: applyDoubleCborEncoding(script.script),
    },
  };
  emulator.ledger[`${reference.txHash}${reference.outputIndex}`] = {
    utxo: reference,
    spent: false,
  };
  return reference;
};

/** Registers the reward account of the withdrawal script `script`. */
export const registerEmulatorRewardAccount = (
  emulator: Emulator,
  script: Script,
): void => {
  emulator.chain[validatorToRewardAddress(network, script)] = {
    registeredStake: true,
    delegation: { poolId: null, rewards: 0n },
  };
};

/**
 * Places the validator and its stakers as reference scripts in the emulator
 * ledger and registers the stakers' reward accounts.
 */
export const injectScriptDecodeDeployment = (
  emulator: Emulator,
): ScriptDecodeDeployment => {
  const validator = buildScriptDecodeValidator(
    readBlueprint(realBlueprintPath) as never,
    network,
  );
  const reference = placeReference(emulator, 0, validator.spendingScript);
  const stakerReferences = Object.fromEntries(
    SCRIPT_DECODE_STAKER_CLASS_NAMES.map((name, index) => {
      const { withdrawalScript } = validator.stakers[name];
      registerEmulatorRewardAccount(emulator, withdrawalScript);
      return [name, placeReference(emulator, index + 1, withdrawalScript)];
    }),
  ) as Record<ScriptDecodeStakerClassName, UTxO>;
  return { validator, reference, stakerReferences };
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
