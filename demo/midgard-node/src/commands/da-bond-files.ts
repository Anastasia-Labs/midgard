/**
 * The files the pool withdrawal's multi-signer flow passes between machines,
 * and the offline `da-bond witness` step that reads one and writes the other.
 *
 * `da-bond withdraw begin|cancel|complete --build-unsigned <file>` writes an
 * unsigned-transaction file; each DA params owner (and the fee payer) runs
 * `da-bond witness --key-env <ENV> <file>` with only their own key, on any
 * machine, and hands the witness file back; `da-bond assemble` checks the
 * owner quorum and submits. Nothing in this module reads the chain, a
 * manifest or any environment variable other than the one named key.
 */
import { writeFileSync } from "node:fs";
import { readFile } from "node:fs/promises";

import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";

export const DA_BOND_UNSIGNED_FORMAT = "midgard-da-bond-unsigned-v1";
export const DA_BOND_WITNESS_FORMAT = "midgard-da-bond-witness-v1";

/** The pool redeemer an unsigned file's transaction carries. */
export type DaBondWithdrawAction =
  | "BeginWithdraw"
  | "CancelWithdraw"
  | "CompleteWithdraw";

const WITHDRAW_ACTIONS: readonly DaBondWithdrawAction[] = [
  "BeginWithdraw",
  "CancelWithdraw",
  "CompleteWithdraw",
];

export type DaBondUnsignedFile = Readonly<{
  format: typeof DA_BOND_UNSIGNED_FORMAT;
  network: string;
  manifestId: string;
  action: DaBondWithdrawAction;
  txCbor: string;
  txBodyHash: string;
  /** The body's `required_signers`: the owners who must witness. */
  requiredSigners: readonly string[];
  /** The payment key hash of `--fee-address`, whose inputs pay the fee. */
  feePayerKeyHash: string;
  poolOutRef: string;
  daParamsOutRef: string;
  /** POSIX milliseconds, as decimal strings. */
  validFrom?: string;
  validTo?: string;
}>;

export type DaBondWitnessFile = Readonly<{
  format: typeof DA_BOND_WITNESS_FORMAT;
  txBodyHash: string;
  keyHash: string;
  witnessSetCbor: string;
}>;

const KEY_HASH = /^[0-9a-f]{56}$/u;
const TX_HASH = /^[0-9a-f]{64}$/u;
const HEX = /^(?:[0-9a-f]{2})+$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;
const MILLIS = /^(?:0|[1-9][0-9]*)$/u;
const ENV_NAME = /^[A-Za-z_][A-Za-z0-9_]*$/u;

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const requireString = (
  record: Record<string, unknown>,
  field: string,
  pattern: RegExp,
  label: string,
): string => {
  const value = record[field];
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label}: field ${field} is missing or malformed`);
  }
  return value;
};

const readJsonFile = async (path: string, label: string): Promise<unknown> => {
  let text: string;
  try {
    text = await readFile(path, "utf8");
  } catch (error) {
    throw new Error(
      `${label} ${path} cannot be read: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  try {
    return JSON.parse(text) as unknown;
  } catch {
    throw new Error(`${label} ${path} is not JSON`);
  }
};

/** What an unsigned pool transaction does, read from its CBOR alone. */
export type DaBondUnsignedTxView = Readonly<{
  /** The arm of the transaction's one spend redeemer, the pool's. */
  action: DaBondWithdrawAction;
  /** `CompleteWithdraw`'s `amount`, in lovelace. */
  amount?: string;
  /** Every output, in order: bech32 address and lovelace. */
  outputs: readonly Readonly<{ address: string; lovelace: string }>[];
}>;

type Freeable = { free: () => void };

/**
 * Decodes the transaction's one spend redeemer as a pool withdrawal arm and
 * lists its outputs, so a witness signs what it has been shown rather than
 * what the file claims. The quorum transactions spend the pool as their only
 * script input (fee inputs are key-locked), so exactly one spend redeemer is
 * expected.
 */
export const daBondUnsignedTxView = (
  txCbor: string,
  label = "DA bond unsigned transaction",
): DaBondUnsignedTxView => {
  const owned: Freeable[] = [];
  const own = <T extends Freeable>(value: T): T => {
    owned.push(value);
    return value;
  };
  try {
    const tx = own(CML.Transaction.from_cbor_hex(txCbor));
    const spendData: string[] = [];
    const redeemers = own(tx.witness_set()).redeemers();
    if (redeemers !== undefined) {
      own(redeemers);
      const legacy = redeemers.as_arr_legacy_redeemer();
      const map = redeemers.as_map_redeemer_key_to_redeemer_val();
      if (legacy !== undefined) {
        own(legacy);
        for (let index = 0; index < legacy.len(); index += 1) {
          const redeemer = own(legacy.get(index));
          if (redeemer.tag() === CML.RedeemerTag.Spend) {
            spendData.push(own(redeemer.data()).to_cbor_hex());
          }
        }
      }
      if (map !== undefined) {
        own(map);
        const keys = own(map.keys());
        for (let index = 0; index < keys.len(); index += 1) {
          const key = own(keys.get(index));
          if (key.tag() === CML.RedeemerTag.Spend) {
            const value = map.get(key);
            if (value !== undefined) {
              spendData.push(own(own(value).data()).to_cbor_hex());
            }
          }
        }
      }
    }
    if (spendData.length !== 1) {
      throw new Error(
        `${label}: expected exactly one spend redeemer (the pool's), found ${spendData.length.toString()}`,
      );
    }
    let redeemer: SDK.DaBondPoolSpendRedeemer;
    try {
      redeemer = Data.from(spendData[0]!, SDK.DaBondPoolSpendRedeemer);
    } catch {
      throw new Error(
        `${label}: the spend redeemer is not a DA bond pool redeemer`,
      );
    }
    const arm = Object.keys(redeemer)[0];
    if (!WITHDRAW_ACTIONS.includes(arm as DaBondWithdrawAction)) {
      throw new Error(
        `${label}: the pool redeemer is ${String(arm)}, not a withdrawal step`,
      );
    }
    const outputs: { address: string; lovelace: string }[] = [];
    const body = own(tx.body());
    const bodyOutputs = own(body.outputs());
    for (let index = 0; index < bodyOutputs.len(); index += 1) {
      const output = own(bodyOutputs.get(index));
      outputs.push({
        address: own(output.address()).to_bech32(undefined),
        lovelace: own(output.amount()).coin().toString(),
      });
    }
    return {
      action: arm as DaBondWithdrawAction,
      ...("CompleteWithdraw" in redeemer
        ? { amount: redeemer.CompleteWithdraw.amount.toString() }
        : {}),
      outputs,
    };
  } finally {
    for (const value of owned.reverse()) {
      value.free();
    }
  }
};

/**
 * Parses an unsigned-transaction file and checks it against its own
 * transaction: the recorded body hash, required signers and action must be
 * the ones the CBOR carries, so a file cannot describe one transaction and
 * hold another.
 */
export const parseDaBondUnsignedFile = (
  value: unknown,
  label = "DA bond unsigned file",
): DaBondUnsignedFile => {
  if (!isRecord(value) || value.format !== DA_BOND_UNSIGNED_FORMAT) {
    throw new Error(`${label} is not a ${DA_BOND_UNSIGNED_FORMAT} file`);
  }
  const action = value.action;
  if (
    typeof action !== "string" ||
    !WITHDRAW_ACTIONS.includes(action as DaBondWithdrawAction)
  ) {
    throw new Error(`${label}: field action is missing or malformed`);
  }
  const network = value.network;
  const manifestId = value.manifestId;
  if (typeof network !== "string" || network.length === 0) {
    throw new Error(`${label}: field network is missing or malformed`);
  }
  if (typeof manifestId !== "string" || manifestId.length === 0) {
    throw new Error(`${label}: field manifestId is missing or malformed`);
  }
  const txCbor = requireString(value, "txCbor", HEX, label);
  const txBodyHash = requireString(value, "txBodyHash", TX_HASH, label);
  const feePayerKeyHash = requireString(
    value,
    "feePayerKeyHash",
    KEY_HASH,
    label,
  );
  const poolOutRef = requireString(value, "poolOutRef", OUT_REF, label);
  const daParamsOutRef = requireString(value, "daParamsOutRef", OUT_REF, label);
  const requiredSigners = value.requiredSigners;
  if (
    !Array.isArray(requiredSigners) ||
    !requiredSigners.every(
      (signer) => typeof signer === "string" && KEY_HASH.test(signer),
    )
  ) {
    throw new Error(`${label}: field requiredSigners is missing or malformed`);
  }
  const bounds: { validFrom?: string; validTo?: string } = {};
  for (const field of ["validFrom", "validTo"] as const) {
    if (value[field] !== undefined) {
      bounds[field] = requireString(value, field, MILLIS, label);
    }
  }
  const actualBodyHash = SDK.daBondPoolTxBodyHash(txCbor);
  if (actualBodyHash !== txBodyHash) {
    throw new Error(
      `${label}: txBodyHash ${txBodyHash} is not the hash of its transaction (${actualBodyHash})`,
    );
  }
  const view = daBondUnsignedTxView(txCbor, label);
  if (view.action !== action) {
    throw new Error(
      `${label}: action ${action} is not the transaction's pool redeemer ${view.action}`,
    );
  }
  const actualSigners = SDK.daBondPoolTxRequiredSigners(txCbor);
  if (
    actualSigners.length !== requiredSigners.length ||
    actualSigners.some((signer, index) => signer !== requiredSigners[index])
  ) {
    throw new Error(
      `${label}: requiredSigners differ from the transaction's required signers (${actualSigners.join(",")})`,
    );
  }
  return {
    format: DA_BOND_UNSIGNED_FORMAT,
    network,
    manifestId,
    action: action as DaBondWithdrawAction,
    txCbor,
    txBodyHash,
    requiredSigners: actualSigners,
    feePayerKeyHash,
    poolOutRef,
    daParamsOutRef,
    ...bounds,
  };
};

export const readDaBondUnsignedFile = async (
  path: string,
): Promise<DaBondUnsignedFile> =>
  parseDaBondUnsignedFile(
    await readJsonFile(path, "DA bond unsigned file"),
    `DA bond unsigned file ${path}`,
  );

export const parseDaBondWitnessFile = (
  value: unknown,
  label = "DA bond witness file",
): DaBondWitnessFile => {
  if (!isRecord(value) || value.format !== DA_BOND_WITNESS_FORMAT) {
    throw new Error(`${label} is not a ${DA_BOND_WITNESS_FORMAT} file`);
  }
  return {
    format: DA_BOND_WITNESS_FORMAT,
    txBodyHash: requireString(value, "txBodyHash", TX_HASH, label),
    keyHash: requireString(value, "keyHash", KEY_HASH, label),
    witnessSetCbor: requireString(value, "witnessSetCbor", HEX, label),
  };
};

export const readDaBondWitnessFile = async (
  path: string,
): Promise<DaBondWitnessFile> =>
  parseDaBondWitnessFile(
    await readJsonFile(path, "DA bond witness file"),
    `DA bond witness file ${path}`,
  );

/**
 * Writes `value` as JSON to a file that must not exist yet: a file another
 * signer may still be reading is never overwritten.
 */
export const writeNewJsonFile = (path: string, value: unknown): void => {
  try {
    writeFileSync(path, `${JSON.stringify(value, null, 2)}\n`, {
      encoding: "utf8",
      flag: "wx",
    });
  } catch (error) {
    throw new Error(
      `Cannot write ${path}: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
};

/**
 * The bech32 payment signing key a secret names: an `ed25519_sk` /
 * `ed25519e_sk` key as is, or a mnemonic's enterprise payment key (account
 * 0, the key `selectWallet.fromSeed(seed, { addressType: "Enterprise" })`
 * signs with). Error messages never carry the secret.
 */
export const daBondSigningKeyFromSecret = (secret: string): string => {
  const trimmed = secret.trim();
  if (trimmed.startsWith("ed25519_sk1") || trimmed.startsWith("ed25519e_sk1")) {
    return trimmed;
  }
  if (trimmed.split(/\s+/u).length < 12) {
    throw new Error(
      "The signing secret is neither a bech32 ed25519_sk/ed25519e_sk key nor a mnemonic",
    );
  }
  try {
    return walletFromSeed(trimmed, {
      addressType: "Enterprise",
      accountIndex: 0,
      network: "Mainnet",
    }).paymentKey;
  } catch {
    throw new Error("The signing mnemonic does not derive a payment key");
  }
};

/** Reads the one named secret; never echoes its value. */
export const readDaBondSecretEnv = (
  env: NodeJS.ProcessEnv,
  name: string,
  flag: string,
): string => {
  if (!ENV_NAME.test(name)) {
    throw new Error(`${flag} must name one environment variable`);
  }
  const value = env[name]?.trim();
  if (!value) {
    throw new Error(`Environment variable ${name} (${flag}) is empty or unset`);
  }
  return value;
};

export type DaBondWitnessOptions = Readonly<{
  keyEnv: string;
  out?: string;
}>;

/**
 * `da-bond witness`: signs the unsigned file's transaction body with the key
 * in `--key-env`, offline. Refuses a key that is neither one of the
 * transaction's required signers nor the fee payer the file records. Writes
 * the witness to `--out` (a new file) and returns a summary, or returns the
 * witness itself for the caller to print.
 */
export const runDaBondWitnessCommand = async (
  unsignedPath: string,
  options: DaBondWitnessOptions,
  env: NodeJS.ProcessEnv = process.env,
): Promise<unknown> => {
  const unsigned = await readDaBondUnsignedFile(unsignedPath);
  const key = daBondSigningKeyFromSecret(
    readDaBondSecretEnv(env, options.keyEnv, "--key-env"),
  );
  const { keyHash, witnessSetCbor } = SDK.witnessDaBondPoolTx(
    unsigned.txCbor,
    key,
  );
  if (
    !unsigned.requiredSigners.includes(keyHash) &&
    keyHash !== unsigned.feePayerKeyHash
  ) {
    throw new Error(
      `Refusing to witness: key ${keyHash} is neither a required signer (${unsigned.requiredSigners.join(", ")}) nor the fee payer ${unsigned.feePayerKeyHash} of this ${unsigned.action} transaction`,
    );
  }
  const witness: DaBondWitnessFile = {
    format: DA_BOND_WITNESS_FORMAT,
    txBodyHash: unsigned.txBodyHash,
    keyHash,
    witnessSetCbor,
  };
  if (options.out === undefined) {
    return witness;
  }
  writeNewJsonFile(options.out, witness);
  const view = daBondUnsignedTxView(unsigned.txCbor);
  return {
    witnessFile: options.out,
    action: view.action,
    ...(view.amount === undefined ? {} : { amount: view.amount }),
    outputs: view.outputs,
    txBodyHash: unsigned.txBodyHash,
    keyHash,
    roles: [
      ...(unsigned.requiredSigners.includes(keyHash)
        ? ["required-signer"]
        : []),
      ...(keyHash === unsigned.feePayerKeyHash ? ["fee-payer"] : []),
    ],
  };
};
