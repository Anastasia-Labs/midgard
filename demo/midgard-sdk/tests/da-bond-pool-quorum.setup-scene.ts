import { readFileSync } from "node:fs";

import {
  applyDoubleCborEncoding,
  CML,
  Data,
  Emulator,
  generateEmulatorAccountFromPrivateKey,
  Lucid,
  type LucidEvolution,
  mintingPolicyToId,
  type Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  availabilityResponseGeometry,
  DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  daAvailabilityParameters,
} from "../src/availability-challenge.js";
import type { AuthenticatedValidator } from "../src/common.js";
import { DaParamsDatum } from "../src/da-attestation.js";
import { daBondPoolUnit } from "../src/da-bond-pool.js";
import {
  buildBeginDaBondPoolWithdrawTxProgram,
  buildInitDaBondPoolTxProgram,
  DaBondPoolBuildError,
  type DaBondPoolBuildFailureReason,
} from "../src/da-bond-pool-transactions.js";

const ADA = 1_000_000n;

const NETWORK: Network = "Custom";

const WITHDRAW_DELAY_MS = 60_000n;

const parameters = daAvailabilityParameters({
  responseGeometry: availabilityResponseGeometry(
    DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  ),
  ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  challengerBondLovelace:
    DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  maxOpenFeeLovelace: 500_000n,
  maxPublicationFeeLovelace: 500_000n,
  maxSettlementFeeLovelace: 500_000n,
  maxCloseFeeLovelace: 1_000_000n,
  maxTimeoutFeeLovelace: 1_200_000n,
});

export const alwaysSucceedsBlueprint = JSON.parse(
  readFileSync(
    new URL(
      "../../midgard-node/blueprints/always-succeeds/plutus.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
  }[];
};

const alwaysSucceedsScript = (title: string): Script => {
  const validator = alwaysSucceedsBlueprint.validators.find(
    (entry) => entry.title === title,
  );
  if (validator === undefined) {
    throw new Error(`always-succeeds blueprint has no ${title}`);
  }
  return {
    type: "PlutusV3",
    script: applyDoubleCborEncoding(validator.compiledCode),
  };
};

const standInPool = (): AuthenticatedValidator => {
  const script = alwaysSucceedsScript(
    alwaysSucceedsBlueprint.validators[0]!.title,
  );
  return {
    policyId: mintingPolicyToId(script),
    spendingScriptAddress: validatorToAddress(NETWORK, script),
    spendingScriptHash: validatorToScriptHash(script),
    spendingScriptCBOR: script.script,
    mintingScriptCBOR: script.script,
    spendingScript: script,
    mintingScript: script,
  };
};

type Key = { readonly bech32: string; readonly keyHash: string };

const keyOf = (privateKey: CML.PrivateKey): Key => {
  const key = {
    bech32: privateKey.to_bech32(),
    keyHash: privateKey.to_public().hash().to_hex(),
  };
  privateKey.free();
  return key;
};

export type Scene = {
  readonly emulator: Emulator;
  readonly lucid: LucidEvolution;
  readonly unit: string;
  readonly poolAddress: string;
  readonly feePayer: Key;
  readonly ownerA: Key;
  readonly ownerB: Key;
  readonly stranger: Key;
  /** Unsigned `BeginWithdraw`, required signers `[ownerA, ownerB]`. */
  readonly txCbor: string;
  readonly txHash: string;
};

export const daParams = (
  owners: readonly Key[],
  updateThreshold: bigint,
): Pick<DaParamsDatum, "owners" | "update_threshold"> => ({
  owners: owners.map((owner) => owner.keyHash),
  update_threshold: updateThreshold,
});

export const setupScene = async (): Promise<Scene> => {
  const account = generateEmulatorAccountFromPrivateKey({
    lovelace: 100_000n * ADA,
  });
  const emulator = new Emulator([account], {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxCollateralInputs: 3,
  });
  const lucid = await Lucid(emulator, NETWORK);
  lucid.selectWallet.fromPrivateKey(account.privateKey);
  const feePayer: Key = {
    bech32: account.privateKey,
    keyHash: CML.PrivateKey.from_bech32(account.privateKey)
      .to_public()
      .hash()
      .to_hex(),
  };
  // One extended and one normal key, so both bech32 forms are exercised.
  const ownerA = keyOf(CML.PrivateKey.generate_ed25519extended());
  const ownerB = keyOf(CML.PrivateKey.generate_ed25519());
  const stranger = keyOf(CML.PrivateKey.generate_ed25519());
  const pool = standInPool();
  const unit = daBondPoolUnit(pool.policyId);

  const paramsDatum: DaParamsDatum = {
    committee: "",
    committee_signers_hash: "00".repeat(32),
    da_threshold: 1n,
    owners: [ownerA.keyHash, ownerB.keyHash],
    update_threshold: 2n,
  };
  const submit = async (signed: { submit: () => Promise<string> }) => {
    const hash = await signed.submit();
    await lucid.awaitTx(hash);
    emulator.awaitBlock(1);
    return hash;
  };
  const paramsHash = await submit(
    await (
      await lucid
        .newTx()
        .pay.ToContract(
          validatorToAddress(
            NETWORK,
            alwaysSucceedsScript("midgard.always_fail.else"),
          ),
          { kind: "inline", value: Data.to(paramsDatum, DaParamsDatum) },
          { lovelace: 5n * ADA },
        )
        .complete()
    ).sign
      .withWallet()
      .complete(),
  );
  const [daParamsUtxo] = await lucid.utxosByOutRef([
    { txHash: paramsHash, outputIndex: 0 },
  ]);
  const [initUtxo] = await lucid.wallet().getUtxos();
  const init = await Effect.runPromise(
    buildInitDaBondPoolTxProgram(lucid, {
      poolValidator: pool,
      parameters,
      initUtxo: initUtxo!,
      lovelace: parameters.da_bond_pool_floor_lovelace + 600n * ADA,
    }),
  );
  await submit(await (await init.complete()).sign.withWallet().complete());
  const [poolUtxo] = await lucid.utxosAtWithUnit(
    pool.spendingScriptAddress,
    unit,
  );
  const now = BigInt(emulator.now());
  const begin = await Effect.runPromise(
    buildBeginDaBondPoolWithdrawTxProgram(lucid, {
      poolValidator: pool,
      parameters,
      pool: { utxo: poolUtxo! },
      daParamsUtxo: daParamsUtxo!,
      signerKeyHashes: [ownerA.keyHash, ownerB.keyHash],
      withdrawDelayMs: WITHDRAW_DELAY_MS,
      validity: { validFrom: now, validTo: now + 300_000n },
    }),
  );
  const unsigned = await begin.complete();
  return {
    emulator,
    lucid,
    unit,
    poolAddress: pool.spendingScriptAddress,
    feePayer,
    ownerA,
    ownerB,
    stranger,
    txCbor: unsigned.toCBOR(),
    txHash: unsigned.toHash(),
  };
};

export const reasonOf = (run: () => unknown): DaBondPoolBuildFailureReason => {
  try {
    run();
  } catch (error) {
    expect(error).toBeInstanceOf(DaBondPoolBuildError);
    return (error as DaBondPoolBuildError).reason;
  }
  throw new Error("expected a DaBondPoolBuildError, nothing was thrown");
};

export const errorOf = (run: () => unknown): DaBondPoolBuildError => {
  try {
    run();
  } catch (error) {
    expect(error).toBeInstanceOf(DaBondPoolBuildError);
    return error as DaBondPoolBuildError;
  }
  throw new Error("expected a DaBondPoolBuildError, nothing was thrown");
};

/** The one vkey witness in a witness set, as [vkey hex, signature hex]. */
export const vkeyWitnesses = (witnessSetCbor: string): readonly string[][] => {
  const witnessSet = CML.TransactionWitnessSet.from_cbor_hex(witnessSetCbor);
  const list = witnessSet.vkeywitnesses();
  const out: string[][] = [];
  for (let index = 0; index < (list?.len() ?? 0); index += 1) {
    const witness = list!.get(index);
    out.push([
      Buffer.from(witness.vkey().to_raw_bytes()).toString("hex"),
      witness.ed25519_signature().to_hex(),
    ]);
  }
  return out;
};

/** Rebuilds a one-witness set with the signature's last byte flipped. */
export const tamperedSignature = (witnessSetCbor: string): string => {
  const witnessSet = CML.TransactionWitnessSet.from_cbor_hex(witnessSetCbor);
  const witness = witnessSet.vkeywitnesses()!.get(0);
  const signature = witness.ed25519_signature().to_raw_bytes();
  signature[signature.length - 1] ^= 0x01;
  const list = CML.VkeywitnessList.new();
  list.add(
    CML.Vkeywitness.new(
      witness.vkey(),
      CML.Ed25519Signature.from_raw_bytes(signature),
    ),
  );
  const tampered = CML.TransactionWitnessSet.new();
  tampered.set_vkeywitnesses(list);
  return tampered.to_cbor_hex();
};

/** Every witness-set field except the vkeys. */
export const nonVkeyWitnessFields = (
  txCbor: string,
): Record<string, unknown> => {
  const fields = JSON.parse(
    CML.Transaction.from_cbor_hex(txCbor).witness_set().to_json(),
  ) as Record<string, unknown>;
  delete fields.vkeywitnesses;
  return fields;
};
