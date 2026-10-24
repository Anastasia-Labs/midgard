import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  CML,
  coreToTxOutput,
  Data,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  type TxSignBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import {
  findRedeemerDataCbor,
  type RedeemerPointer,
  resolveMintPolicyContextIndex,
} from "./helpers/redeemer-inspection.js";

export const mkUtxo = (
  txHashByte: string,
  outputIndex: number,
  assets: Assets = { lovelace: 5_000_000n },
): UTxO => ({
  txHash: txHashByte.repeat(32),
  outputIndex,
  address: "addr_test1qpz4js6k2c6un3h8y8sh2nmkg7u9s8w7up0psd4w6zv6r9u9gq3h",
  assets,
});

export const scriptRef = {
  type: "PlutusV3",
  script: "5900",
} as const;

export const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  maxCollateralInputs: 3,
} as const;

const hashHexBlake2b256 = (hex: string): Promise<string> =>
  Effect.runPromise(SDK.hashHexWithBlake2b(hex, 32));

export const canonicalDatumCbor = (cbor: string): string =>
  CML.PlutusData.from_cbor_hex(cbor).to_canonical_cbor_hex();

export const expectLeft = <E, A>(
  result:
    | { readonly _tag: "Left"; readonly left: E }
    | { readonly _tag: "Right"; readonly right: A },
): E => {
  expect(result._tag).toBe("Left");
  if (result._tag !== "Left") {
    throw new Error("Expected Left");
  }
  return result.left;
};

const singletonMembershipRoot = async (
  keyCbor: string,
  valueCbor: string,
): Promise<string> => {
  const [keyHash, valueHash] = await Promise.all([
    hashHexBlake2b256(keyCbor),
    hashHexBlake2b256(valueCbor),
  ]);
  return hashHexBlake2b256(`ff${keyHash}${valueHash}`);
};

export const countedSingletonMembershipRoot = async (
  domain: SDK.RootDomain,
  keyCbor: string,
  valueCbor: string,
): Promise<{ readonly root: string; readonly phasRoot: string }> => {
  const phasRoot = await singletonMembershipRoot(keyCbor, valueCbor);
  const root = await Effect.runPromise(
    SDK.commitCountedRootProgram({
      domain,
      phasRoot,
      count: 1n,
    }),
  );
  return { root, phasRoot };
};

export const deploymentRecords = new Map<string, unknown>();

export const loadRealContracts: typeof loadRealMidgardContractsForTest = async (
  ...args
) => {
  const contracts = await loadRealMidgardContractsForTest(...args);
  const pair = SDK.requireEventHistoryContracts(contracts);
  const historyIdentity = (history: SDK.EventHistoryContracts) => ({
    recipe: history.recipe,
    listPolicyId: history.list.policyId,
    listAddress: history.list.spendingScriptAddress,
    retirementHash: validatorToScriptHash(history.retirement.withdrawalScript),
    retentionHash: history.retention.spendingScriptHash,
  });
  deploymentRecords.set(contracts.payout.policyId, {
    provenance:
      "Real applied list/retirement/retention/payout scripts; seeded fixture hub, confirmed-state and settlement UTxOs. This is not real frontier establishment or live acceptance.",
    hubPolicyId: contracts.hubOracle.policyId,
    confirmedPolicyId: contracts.stateQueue.policyId,
    settlementPolicyId: contracts.settlement.policyId,
    payoutPolicyId: contracts.payout.policyId,
    reserveHash: contracts.reserve.spendingScriptHash,
    deposit: historyIdentity(pair.deposit),
    withdrawal: historyIdentity(pair.withdrawal),
  });
  return contracts;
};

export const findUtxoWithUnit = (
  utxos: readonly UTxO[],
  unit: string,
  quantity = 1n,
): UTxO => {
  const utxo = utxos.find((candidate) => candidate.assets[unit] === quantity);
  if (utxo === undefined) {
    throw new Error(
      `Missing UTxO with ${unit} quantity ${quantity.toString()}`,
    );
  }
  return utxo;
};

export const findReferenceScriptUtxo = (
  utxos: readonly UTxO[],
  script: Script,
): UTxO => {
  const expectedHash = validatorToScriptHash(script);
  const utxo = utxos.find(
    (candidate) =>
      candidate.scriptRef != null &&
      validatorToScriptHash(candidate.scriptRef) === expectedHash,
  );
  if (utxo === undefined) {
    throw new Error(`Missing reference script UTxO for ${expectedHash}`);
  }
  return utxo;
};

export const findReferenceScriptUtxoBefore = (
  utxos: readonly UTxO[],
  script: Script,
  later: UTxO,
): UTxO => {
  const expectedHash = validatorToScriptHash(script);
  const utxo = utxos.find(
    (candidate) =>
      candidate.scriptRef != null &&
      validatorToScriptHash(candidate.scriptRef) === expectedHash &&
      compareOutRefs(candidate, later) < 0,
  );
  if (utxo === undefined) {
    throw new Error(
      `Missing reference script UTxO for ${expectedHash} that sorts before ${later.txHash}#${later.outputIndex.toString()}`,
    );
  }
  return utxo;
};

export const findPureAdaUtxo = (
  utxos: readonly UTxO[],
  lovelace: bigint,
): UTxO => {
  const utxo = utxos.find(
    (candidate) =>
      candidate.scriptRef === undefined &&
      Object.keys(candidate.assets).length === 1 &&
      candidate.assets.lovelace === lovelace,
  );
  if (utxo === undefined) {
    throw new Error(
      `Missing pure ADA UTxO with ${lovelace.toString()} lovelace`,
    );
  }
  return utxo;
};

export const signedMeasurements: {
  txHash: string;
  testName: string | undefined;
  transactionCbor: string;
  bytes: number;
  memory: string;
  steps: string;
  fee: string;
}[] = [];

export const validityWindowSlots = (tx: TxSignBuilder): bigint => {
  const body = CML.Transaction.from_cbor_hex(tx.toCBOR()).body();
  return body.ttl()! - body.validity_interval_start()!;
};

export const submitWithWallet = async (tx: TxSignBuilder): Promise<string> => {
  try {
    const signed = await tx.sign.withWallet().complete();
    expect(signed.toCBOR().length / 2).toBeLessThanOrEqual(
      PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
    );
    const transaction = CML.Transaction.from_cbor_hex(signed.toCBOR());
    const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
    let memory = 0n;
    let steps = 0n;
    for (let index = 0; index < (redeemers?.len() ?? 0); index++) {
      memory += redeemers!.get(index).ex_units().mem();
      steps += redeemers!.get(index).ex_units().steps();
    }
    expect(memory).toBeLessThanOrEqual(PROTOCOL_PARAMETERS_DEFAULT.maxTxExMem);
    expect(steps).toBeLessThanOrEqual(PROTOCOL_PARAMETERS_DEFAULT.maxTxExSteps);
    const txHash = await signed.submit();
    signedMeasurements.push({
      txHash,
      testName: expect.getState().currentTestName,
      transactionCbor: signed.toCBOR(),
      bytes: signed.toCBOR().length / 2,
      memory: memory.toString(),
      steps: steps.toString(),
      fee: transaction.body().fee().toString(),
    });
    return txHash;
  } catch (cause) {
    const message =
      cause instanceof Error && cause.message.length > 0
        ? cause.message
        : String(cause);
    throw new Error(
      `Failed to sign or submit reserve/payout test tx: ${message}`,
      {
        cause,
      },
    );
  }
};

export const decodeRedeemer = <T>(
  tx: CML.Transaction,
  pointer: RedeemerPointer,
  schema: unknown,
): T => {
  const cbor = findRedeemerDataCbor(tx, pointer);
  if (cbor === undefined) {
    throw new Error(
      `Missing redeemer tag=${pointer.tag.toString()} index=${pointer.index.toString()}`,
    );
  }
  return Data.from(cbor, schema as never) as T;
};

export const mintPointer = (
  policyIds: readonly string[],
  targetPolicyId: string,
): RedeemerPointer => ({
  tag: CML.RedeemerTag.Mint,
  index: resolveMintPolicyContextIndex({ policyIds, targetPolicyId }),
});

type CmlInputSet = {
  len(): number;
  get(index: number): CML.TransactionInput;
};

export const requireTxInputIndex = (
  inputs: CmlInputSet | undefined,
  target: Pick<UTxO, "txHash" | "outputIndex">,
  label: string,
): bigint => {
  if (inputs === undefined) {
    throw new Error(`${label} inputs are missing from final tx`);
  }
  const outRefs = Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  }).sort(compareOutRefs);
  for (let index = 0; index < outRefs.length; index += 1) {
    const input = outRefs[index]!;
    if (
      input.txHash === target.txHash &&
      input.outputIndex === target.outputIndex
    ) {
      return BigInt(index);
    }
  }
  throw new Error(
    `${label} input ${target.txHash}#${target.outputIndex.toString()} is missing from final tx`,
  );
};

export const requireEventOutputIndex = (
  tx: CML.Transaction,
  eventAddress: string,
  eventUnit: string,
): bigint => {
  const outputs = tx.body().outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address === eventAddress &&
      output.datum != null &&
      output.assets[eventUnit] === 1n
    ) {
      return BigInt(index);
    }
  }
  throw new Error(`Missing event output for ${eventUnit} at ${eventAddress}`);
};
