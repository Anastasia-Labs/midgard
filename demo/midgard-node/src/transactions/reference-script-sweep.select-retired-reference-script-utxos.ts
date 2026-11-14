import {
  type Assets,
  calculateMinLovelaceFromUTxO,
  CML,
  mintingPolicyToId,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import { compareOutRefs, outRefLabel } from "../tx-context.js";
import {
  assertRetiredPolicyIsNotLive,
  type LiveReferenceScriptDeployment,
  positiveUnitsUnderPolicy,
  QUARANTINE_OUTPUT_PLACEHOLDER_LOVELACE,
  REFERENCE_SCRIPT_SWEEP_BURN_VALIDITY_SLOTS,
  ReferenceScriptSweepRefusal,
  type RetiredAuthPolicyDisposition,
  unitPolicyId,
} from "./reference-script-sweep.reference-script-fee.js";
import {
  acceptsReferenceScriptUtxo,
  resolveReferenceScriptUtxo,
} from "./reference-scripts.js";

/**
 * Selects the UTxOs that carry a reference script and a token of the retired
 * policy, and refuses the whole sweep if spending them could break the live
 * deployment's reference-script resolution or would drop an asset the sweep
 * cannot account for. `utxos` is the whole wallet snapshot the plan uses.
 */
export const selectRetiredReferenceScriptUtxos = ({
  utxos,
  referenceScriptsAddress,
  retiredAuthPolicyId,
  live,
}: {
  readonly utxos: readonly UTxO[];
  readonly referenceScriptsAddress: string;
  readonly retiredAuthPolicyId: string;
  readonly live: LiveReferenceScriptDeployment;
}): readonly UTxO[] => {
  const retired = assertRetiredPolicyIsNotLive(retiredAuthPolicyId, live);
  const livePolicy = live.authPolicyId.toLowerCase();
  const liveAuthPolicy = { policyId: livePolicy };
  const selected = utxos
    .filter(
      (utxo) =>
        utxo.scriptRef !== undefined &&
        utxo.scriptRef !== null &&
        positiveUnitsUnderPolicy(utxo.assets, retired).length > 0,
    )
    .sort(compareOutRefs);

  // No selected UTxO may be one the live resolution accepts for any target.
  const liveAccepted = selected.flatMap((utxo) => {
    const targets = live.targets.filter((target) =>
      acceptsReferenceScriptUtxo(
        utxo,
        referenceScriptsAddress,
        target,
        liveAuthPolicy,
      ),
    );
    return targets.length === 0
      ? []
      : [
          `${outRefLabel(utxo)}(${targets.map((target) => target.name).join("|")})`,
        ];
  });
  if (liveAccepted.length > 0) {
    throw new ReferenceScriptSweepRefusal(
      "live-resolved-outref",
      `selected UTxOs are ones the live deployment resolves: ${liveAccepted.join(",")}`,
    );
  }
  const liveTokenHolders = selected.filter(
    (utxo) => positiveUnitsUnderPolicy(utxo.assets, livePolicy).length > 0,
  );
  if (liveTokenHolders.length > 0) {
    throw new ReferenceScriptSweepRefusal(
      "live-auth-token",
      `selected UTxOs also hold live auth tokens: ${liveTokenHolders.map(outRefLabel).join(",")}`,
    );
  }
  // Every live target must still resolve from what the sweep leaves behind.
  const selectedOutRefs = new Set(selected.map(outRefLabel));
  const remaining = utxos.filter(
    (utxo) => !selectedOutRefs.has(outRefLabel(utxo)),
  );
  const stranded = live.targets.filter(
    (target) =>
      resolveReferenceScriptUtxo(
        remaining,
        referenceScriptsAddress,
        target,
        liveAuthPolicy,
      ) === undefined,
  );
  if (stranded.length > 0) {
    throw new ReferenceScriptSweepRefusal(
      "live-target-stranded",
      `live targets would have no resolvable reference script after the sweep: ${stranded.map((target) => target.name).join(",")}`,
    );
  }
  const foreignAssetHolders = selected.filter((utxo) =>
    Object.entries(utxo.assets).some(
      ([unit, amount]) =>
        unit !== "lovelace" && amount !== 0n && unitPolicyId(unit) !== retired,
    ),
  );
  if (foreignAssetHolders.length > 0) {
    throw new ReferenceScriptSweepRefusal(
      "foreign-asset",
      `selected UTxOs hold assets outside the retired policy: ${foreignAssetHolders.map(outRefLabel).join(",")}`,
    );
  }
  return selected;
};

type NativeAuthPolicyShape = {
  readonly signerKeyHash: string;
  readonly invalidHereafterSlot: bigint;
};

/** Reads the `all [sig, invalid-hereafter]` shape the deployment creates. */
const readNativeAuthPolicyShape = (
  script: Script,
): NativeAuthPolicyShape | undefined => {
  const native = CML.NativeScript.from_cbor_hex(script.script);
  try {
    const all = native.as_script_all();
    const conditions = all?.native_scripts();
    if (conditions === undefined || conditions.len() !== 2) {
      return undefined;
    }
    const signer = conditions.get(0).as_script_pubkey()?.ed25519_key_hash();
    const hereafter = conditions.get(1).as_script_invalid_hereafter()?.after();
    if (signer === undefined || hereafter === undefined) {
      return undefined;
    }
    return {
      signerKeyHash: signer.to_hex(),
      invalidHereafterSlot: hereafter,
    };
  } finally {
    native.free();
  }
};

/**
 * Burns the retired tokens when the policy can still be satisfied by the
 * reference wallet within a fresh validity window; otherwise quarantines them.
 */
export const decideRetiredAuthPolicyDisposition = ({
  retiredAuthPolicyId,
  policyScript,
  signerKeyHash,
  currentSlot,
}: {
  readonly retiredAuthPolicyId: string;
  readonly policyScript?: Script;
  readonly signerKeyHash: string;
  readonly currentSlot: number;
}): RetiredAuthPolicyDisposition => {
  if (policyScript === undefined) {
    return {
      kind: "quarantine",
      reason:
        "retired policy script not supplied (--retired-auth-policy-script); it cannot be witnessed for a burn",
    };
  }
  if (
    policyScript.type !== "Native" ||
    mintingPolicyToId(policyScript) !== retiredAuthPolicyId
  ) {
    throw new ReferenceScriptSweepRefusal(
      "policy-script-mismatch",
      `supplied policy script does not hash to ${retiredAuthPolicyId}`,
    );
  }
  const shape = readNativeAuthPolicyShape(policyScript);
  if (shape === undefined) {
    return {
      kind: "quarantine",
      reason:
        "retired policy is not the all[signature, invalid-hereafter] auth shape",
    };
  }
  if (shape.signerKeyHash !== signerKeyHash) {
    return {
      kind: "quarantine",
      reason: `retired policy signer ${shape.signerKeyHash} is not the reference wallet key ${signerKeyHash}`,
    };
  }
  const validToSlot = currentSlot + REFERENCE_SCRIPT_SWEEP_BURN_VALIDITY_SLOTS;
  if (BigInt(validToSlot) > shape.invalidHereafterSlot) {
    return {
      kind: "quarantine",
      reason: `retired policy expired: invalid hereafter slot ${shape.invalidHereafterSlot.toString()}, current slot ${currentSlot.toString()}`,
    };
  }
  return {
    kind: "burn",
    policyScript,
    validToSlot,
    reason: `retired policy is satisfiable until slot ${shape.invalidHereafterSlot.toString()}`,
  };
};

const cborHeaderBytes = (argument: bigint): number =>
  argument < 24n
    ? 1
    : argument < 0x100n
      ? 2
      : argument < 0x10000n
        ? 3
        : argument < 0x100000000n
          ? 5
          : 9;

/** Canonical CBOR size of a multi-asset value (`[coin, multiasset]`). */
export const valueCborBytes = (assets: Readonly<Assets>): number => {
  const byPolicy = new Map<string, (readonly [string, bigint])[]>();
  for (const [unit, amount] of Object.entries(assets)) {
    if (unit !== "lovelace" && amount > 0n) {
      const policy = unitPolicyId(unit);
      byPolicy.set(policy, [
        ...(byPolicy.get(policy) ?? []),
        [unit.slice(56), amount],
      ]);
    }
  }
  const coinBytes = cborHeaderBytes(assets.lovelace ?? 0n);
  if (byPolicy.size === 0) {
    return coinBytes;
  }
  let bytes = 1 + coinBytes + cborHeaderBytes(BigInt(byPolicy.size));
  for (const entries of byPolicy.values()) {
    bytes +=
      cborHeaderBytes(28n) + 28 + cborHeaderBytes(BigInt(entries.length));
    for (const [assetNameHex, amount] of entries) {
      const nameBytes = assetNameHex.length / 2;
      bytes +=
        cborHeaderBytes(BigInt(nameBytes)) +
        nameBytes +
        cborHeaderBytes(amount);
    }
  }
  return bytes;
};

export const sortedTokenUnits = (utxos: readonly UTxO[]): readonly string[] =>
  utxos
    .flatMap((utxo) =>
      Object.entries(utxo.assets)
        .filter(([unit, amount]) => unit !== "lovelace" && amount > 0n)
        .map(([unit]) => unit),
    )
    .sort();

export const sumTokens = (utxos: readonly UTxO[]): Assets => {
  const totals: Assets = {};
  for (const utxo of utxos) {
    for (const [unit, amount] of Object.entries(utxo.assets)) {
      if (unit !== "lovelace" && amount > 0n) {
        totals[unit] = (totals[unit] ?? 0n) + amount;
      }
    }
  }
  return totals;
};

/**
 * Packs the batch's tokens into as few quarantine outputs as the value-size
 * budget allows, each carrying only its minimum ADA.
 */
export const packQuarantineOutputs = ({
  tokens,
  quarantineAddress,
  valueBytesPerOutput,
  coinsPerUtxoByte,
}: {
  readonly tokens: Readonly<Assets>;
  readonly quarantineAddress: string;
  readonly valueBytesPerOutput: number;
  readonly coinsPerUtxoByte: bigint;
}): readonly Assets[] => {
  const outputs: Assets[] = [];
  let current: Assets = { lovelace: QUARANTINE_OUTPUT_PLACEHOLDER_LOVELACE };
  let currentCount = 0;
  for (const unit of Object.keys(tokens).sort()) {
    const candidate = { ...current, [unit]: tokens[unit]! };
    if (currentCount > 0 && valueCborBytes(candidate) > valueBytesPerOutput) {
      outputs.push(current);
      current = {
        lovelace: QUARANTINE_OUTPUT_PLACEHOLDER_LOVELACE,
        [unit]: tokens[unit]!,
      };
      currentCount = 1;
    } else {
      current = candidate;
      currentCount += 1;
    }
  }
  if (currentCount > 0) {
    outputs.push(current);
  }
  return outputs.map((assets) => ({
    ...assets,
    lovelace: calculateMinLovelaceFromUTxO(coinsPerUtxoByte, {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: quarantineAddress,
      assets,
    }),
  }));
};
