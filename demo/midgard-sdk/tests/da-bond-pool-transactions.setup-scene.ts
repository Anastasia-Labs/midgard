import { readFileSync } from "node:fs";

import {
  applyDoubleCborEncoding,
  CML,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  mintingPolicyToId,
  type Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  type TxSigned,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
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
import { daBondPoolUnit, encodeDaBondPoolDatum } from "../src/da-bond-pool.js";
import {
  DaBondPoolBuildError,
  type DaBondPoolBuildFailureReason,
} from "../src/da-bond-pool-transactions.js";
import * as Sdk from "../src/index.js";

export const ADA = 1_000_000n;

export const NETWORK: Network = "Custom";

const PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxCollateralInputs: 3,
} as const;

// Any positive delay: the always-succeeds stand-in does not compile one in.
export const WITHDRAW_DELAY_MS = 60_000n;

export const parameters = daAvailabilityParameters({
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

export const FLOOR = parameters.da_bond_pool_floor_lovelace;

export const MIN_TOP_UP = parameters.da_bond_min_top_up_lovelace;

const alwaysSucceedsBlueprint = JSON.parse(
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

/** The pool's shape (one script, `Script(policy)`), always succeeding. */
export const standInPool = (): AuthenticatedValidator => {
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

/** A never-spent address for the DA params and reference-script UTxOs. */
export const parkingAddress = (): string =>
  validatorToAddress(NETWORK, alwaysSucceedsScript("midgard.always_fail.else"));

const run = <A>(program: Effect.Effect<A, DaBondPoolBuildError>) =>
  Effect.runPromise(Effect.either(program));

export const expectRefusal = async (
  program: Effect.Effect<unknown, DaBondPoolBuildError>,
  reason: DaBondPoolBuildFailureReason,
): Promise<void> => {
  const result = await run(program);
  if (Either.isRight(result)) {
    throw new Error(`expected a ${reason} refusal, the build succeeded`);
  }
  expect(result.left).toBeInstanceOf(DaBondPoolBuildError);
  expect(result.left.reason).toBe(reason);
};

export const expectBuilds = async (
  program: Effect.Effect<unknown, DaBondPoolBuildError>,
): Promise<void> => {
  const result = await run(program);
  if (Either.isLeft(result)) {
    throw new Error(
      `expected the build to succeed, refused ${result.left.reason}: ${result.left.message}`,
    );
  }
};

type CmlOutRefs = {
  readonly len: () => number;
  readonly get: (index: number) => {
    readonly transaction_id: () => { readonly to_hex: () => string };
    readonly index: () => bigint | number;
  };
};

export const outRefKey = (outRef: {
  readonly txHash: string;
  readonly outputIndex: number;
}): string => `${outRef.txHash}#${outRef.outputIndex.toString()}`;

/**
 * Reference inputs as a script sees them. The body keeps the order Lucid
 * inserted them in; the ledger reads them as a set, sorted by transaction id
 * and then output index, and that sorted list is what redeemer indices count.
 */
const sortedOutRefKeys = (list: CmlOutRefs | undefined): readonly string[] => {
  const refs: { txHash: string; outputIndex: bigint }[] = [];
  for (let index = 0; index < (list?.len() ?? 0); index += 1) {
    const input = list!.get(index);
    refs.push({
      txHash: input.transaction_id().to_hex(),
      outputIndex: BigInt(input.index()),
    });
  }
  return refs
    .sort((left, right) =>
      left.txHash === right.txHash
        ? Number(left.outputIndex - right.outputIndex)
        : left.txHash < right.txHash
          ? -1
          : 1,
    )
    .map((ref) => `${ref.txHash}#${ref.outputIndex.toString()}`);
};

/** What the ledger sees: redeemers, reference inputs, bounds, signers. */
export const assembled = (signed: TxSigned) => {
  const tx = CML.Transaction.from_cbor_hex(signed.toCBOR());
  const body = tx.body();
  const redeemers = tx.witness_set().redeemers()?.to_flat_format();
  const flat: { tag: number; index: bigint; data: unknown }[] = [];
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const redeemer = redeemers!.get(index);
    flat.push({
      tag: redeemer.tag(),
      index: redeemer.index(),
      data: Data.from(redeemer.data().to_cbor_hex()),
    });
  }
  const signers: string[] = [];
  const required = body.required_signers();
  for (let index = 0; index < (required?.len() ?? 0); index += 1) {
    signers.push(required!.get(index).to_hex());
  }
  return {
    bytes: signed.toCBOR().length / 2,
    redeemers: flat,
    referenceInputs: sortedOutRefKeys(
      body.reference_inputs() as unknown as CmlOutRefs,
    ),
    validityStartSlot: body.validity_interval_start(),
    ttlSlot: body.ttl(),
    signers,
    inlineScripts: tx.witness_set().plutus_v3_scripts()?.len() ?? 0,
  };
};

export const SPEND = CML.RedeemerTag.Spend;

export const MINT = CML.RedeemerTag.Mint;

export const redeemerOf = (
  view: ReturnType<typeof assembled>,
  tag: number,
): unknown => {
  const matches = view.redeemers.filter((redeemer) => redeemer.tag === tag);
  expect(matches).toHaveLength(1);
  return matches[0]!.data;
};

export type Scene = {
  readonly emulator: Emulator;
  readonly lucid: LucidEvolution;
  readonly pool: AuthenticatedValidator;
  readonly unit: string;
  readonly walletKeyHash: string;
  readonly otherOwner: string;
  readonly daParamsUtxo: UTxO;
  readonly spendingReference: UTxO;
};

export const submit = async (
  scene: Pick<Scene, "emulator" | "lucid">,
  signed: TxSigned,
): Promise<string> => {
  const txHash = await signed.submit();
  await scene.lucid.awaitTx(txHash);
  scene.emulator.awaitBlock(1);
  return txHash;
};

export const daParamsDatum = (
  owners: readonly string[],
  updateThreshold: bigint,
): DaParamsDatum => ({
  committee: "",
  committee_signers_hash: "00".repeat(32),
  da_threshold: 1n,
  owners: [...owners],
  update_threshold: updateThreshold,
});

export const setupScene = async (): Promise<Scene> => {
  const account = generateEmulatorAccount({ lovelace: 100_000n * ADA });
  const emulator = new Emulator([account], PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, NETWORK);
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const payment = getAddressDetails(account.address).paymentCredential;
  if (payment?.type !== "Key") {
    throw new Error("expected a key-payment emulator wallet");
  }
  const pool = standInPool();
  const otherOwner = "ab".repeat(28);
  const parking = parkingAddress();
  const paramsCbor = Data.to(
    daParamsDatum([payment.hash, otherOwner], 1n),
    DaParamsDatum,
  );
  const setup = await (
    await lucid
      .newTx()
      // The script reference first, so the params UTxO sorts second among
      // reference inputs while the builder hands it to readFrom first.
      .pay.ToContract(
        parking,
        { kind: "inline", value: Data.void() },
        { lovelace: 30n * ADA },
        pool.spendingScript,
      )
      .pay.ToContract(
        parking,
        { kind: "inline", value: paramsCbor },
        { lovelace: 5n * ADA },
      )
      .complete()
  ).sign
    .withWallet()
    .complete();
  const setupHash = await submit({ emulator, lucid }, setup);
  const [spendingReference, daParamsUtxo] = await lucid.utxosByOutRef([
    { txHash: setupHash, outputIndex: 0 },
    { txHash: setupHash, outputIndex: 1 },
  ]);
  return {
    emulator,
    lucid,
    pool,
    unit: daBondPoolUnit(pool.policyId),
    walletKeyHash: payment.hash,
    otherOwner,
    daParamsUtxo: daParamsUtxo!,
    spendingReference: spendingReference!,
  };
};

export const syntheticPool = (
  scene: Pick<Scene, "pool" | "unit">,
  datum: Sdk.DaBondPoolDatum,
  lovelace: bigint,
): UTxO => ({
  txHash: "cd".repeat(32),
  outputIndex: 0,
  address: scene.pool.spendingScriptAddress,
  assets: { lovelace, [scene.unit]: 1n },
  datum: encodeDaBondPoolDatum(datum),
});

export const poolAt = async (scene: Scene): Promise<UTxO> => {
  const utxos = await scene.lucid.utxosAtWithUnit(
    scene.pool.spendingScriptAddress,
    scene.unit,
  );
  expect(utxos).toHaveLength(1);
  return utxos[0]!;
};
