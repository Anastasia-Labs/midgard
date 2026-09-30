import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  Lucid,
  paymentCredentialOf,
  type UTxO,
  utxoToTransactionInput,
  utxoToTransactionOutput,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import * as UPLC from "@lucid-evolution/uplc";
import { expect } from "vitest";

import { TEST_AVAILABILITY_PARAMETERS } from "./availability-challenge.js";
import {
  MAINNET_PROTOCOL_PARAMETERS,
  MAINNET_PROTOCOL_PARAMETERS_SOURCE,
} from "./mainnet-protocol-parameters.js";

export const AVAILABILITY_EMULATOR_PARAMETERS = {
  ...MAINNET_PROTOCOL_PARAMETERS,
} as const;

/**
 * The deployment profile the blueprint under test is compiled for. Every
 * window below is the generated profile's value, which the validators compile
 * in; none is an SDK constant.
 */
export const AVAILABILITY_PROFILE = DEPLOYMENT_PROFILES["preprod-testing"];

export const AVAILABILITY_TIMING = Object.freeze({
  /** `OpenChallenge` needs `validTo - 1 < end_time + da_challenge_window_ms`. */
  daChallengeWindowMs: BigInt(
    AVAILABILITY_PROFILE.timing.da_challenge_window_ms,
  ),
  daSlashGraceMs: BigInt(AVAILABILITY_PROFILE.timing.da_slash_grace_ms),
  /** `BeginWithdraw` writes `unlock_at = validTo - 1 + delay`. */
  daBondWithdrawDelayMs: BigInt(
    AVAILABILITY_PROFILE.timing.da_bond_withdraw_delay_ms,
  ),
});

/**
 * The largest exact fee a DA availability builder sets is the timeout's:
 * `min(penalty, taken) + c <= da_slash_penalty + max_timeout_fee`. The ledger
 * holds `collateralPercentage` of it as collateral (G9, H1).
 */
export const AVAILABILITY_REQUIRED_COLLATERAL_LOVELACE =
  ((TEST_AVAILABILITY_PARAMETERS.da_slash_penalty_lovelace +
    TEST_AVAILABILITY_PARAMETERS.max_timeout_fee_lovelace) *
    BigInt(AVAILABILITY_EMULATOR_PARAMETERS.collateralPercentage) +
    99n) /
  100n;

/**
 * One plain-ADA collateral coin covers the largest collateral alone and
 * leaves a collateral return above min-UTxO.
 */
export const AVAILABILITY_COLLATERAL_COIN_LOVELACE =
  AVAILABILITY_REQUIRED_COLLATERAL_LOVELACE + 5_000_000n;

/**
 * P3: queue nodes carry at least this much, so an Apply or Open output that
 * grows the node datum stays above min-UTxO.
 */
export const AVAILABILITY_QUEUE_NODE_LOVELACE = 5_000_000n;

/**
 * Any amount at or above the attestation output's min-UTxO; Apply refunds it
 * whole to the rescue beneficiary. Covers the 16-tranche commitment datum.
 */
export const AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE = 25_000_000n;

/** `floor + 2 * da_bond`: backs two attestations and one full slash. */
export const AVAILABILITY_DEFAULT_POOL_LOVELACE =
  TEST_AVAILABILITY_PARAMETERS.da_bond_pool_floor_lovelace +
  2n * TEST_AVAILABILITY_PARAMETERS.da_bond_lovelace;

export type AvailabilityMeasurement = {
  name: string;
  signedBytes: number;
  memory: bigint;
  steps: bigint;
  outputs: number;
  fee: bigint;
  referenceInputCount: number;
  referencedScriptBytes: number;
  uniqueReferencedScriptBytes: number;
};

export const measureAvailabilityTransaction = (
  name: string,
  cbor: string,
  references: readonly UTxO[] = [],
): AvailabilityMeasurement => {
  const transaction = CML.Transaction.from_cbor_hex(cbor);
  const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
  let memory = 0n;
  let steps = 0n;
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const units = redeemers!.get(index).ex_units();
    memory += units.mem();
    steps += units.steps();
  }
  const scripts = references.flatMap((input) =>
    input.scriptRef ? [input.scriptRef] : [],
  );
  const uniqueScripts = new Map(
    scripts.map((script) => [validatorToScriptHash(script), script]),
  );
  const result = {
    name,
    signedBytes: cbor.length / 2,
    memory,
    steps,
    outputs: transaction.body().outputs().len(),
    fee: transaction.body().fee(),
    referenceInputCount: transaction.body().reference_inputs()?.len() ?? 0,
    referencedScriptBytes: scripts.reduce(
      (total, script) => total + script.script.length / 2,
      0,
    ),
    uniqueReferencedScriptBytes: [...uniqueScripts.values()].reduce(
      (total, script) => total + script.script.length / 2,
      0,
    ),
  };
  expect(result.signedBytes, name).toBeLessThanOrEqual(16_384);
  expect(
    memory,
    `${name}: aggregate memory with 20% reserve`,
  ).toBeLessThanOrEqual(13_200_000n);
  expect(steps, `${name}: aggregate CPU with 20% reserve`).toBeLessThanOrEqual(
    8_000_000_000n,
  );
  return result;
};

export type AvailabilityLayout = {
  inputs: readonly UTxO[];
  references: readonly UTxO[];
  policies: readonly string[];
};

const compare = (a: UTxO, b: UTxO) =>
  a.txHash < b.txHash
    ? -1
    : a.txHash > b.txHash
      ? 1
      : a.outputIndex - b.outputIndex;

export const position = (inputs: readonly UTxO[], utxo: UTxO) => {
  const found = [...inputs]
    .sort(compare)
    .findIndex(
      (input) =>
        input.txHash === utxo.txHash && input.outputIndex === utxo.outputIndex,
    );
  if (found < 0) throw new Error("Missing authored availability input");
  return BigInt(found);
};

export const index = (layout: AvailabilityLayout, utxo: UTxO) =>
  position(layout.inputs, utxo);

export const refIndex = (layout: AvailabilityLayout, utxo: UTxO) =>
  position(layout.references, utxo);

export const spendingInputs = (layout: AvailabilityLayout) =>
  layout.inputs.filter(
    (input) => paymentCredentialOf(input.address).type === "Script",
  );

export const mintIndex = (layout: AvailabilityLayout, policy: string) =>
  BigInt(
    spendingInputs(layout).length + [...layout.policies].sort().indexOf(policy),
  );

export const inline = (value: string) => ({ kind: "inline" as const, value });

export const outRef = SDK.outputReferenceFromUTxO;

export const sameOutRef = (a: UTxO, b: UTxO) =>
  a.txHash === b.txHash && a.outputIndex === b.outputIndex;

// ---------------------------------------------------------------------------
// Evaluation capture (H9). The fixture's Lucid evaluates with Scalus through
// this wrapper, which keeps the transaction and resolved inputs of the last
// failed evaluation so a refusal can be attributed to one redeemer and script.
// ---------------------------------------------------------------------------

type EvaluationFailure = {
  readonly sequence: number;
  readonly tx: string;
  readonly utxos: readonly UTxO[];
  /** The Scalus evaluator's message (it names no redeemer). */
  readonly message: string;
  /**
   * The same transaction re-evaluated by the Aiken machine, which names the
   * failing redeemer (`Spend[i]`, `Mint[i]`, `Reward[i]`, ...) and its trace.
   * Diagnosis only: budgets always come from Scalus.
   */
  readonly diagnosis: string;
};

export let evaluationSequence = 0;

export let lastEvaluationFailure: EvaluationFailure | undefined;

type LucidEvaluator = NonNullable<
  NonNullable<Parameters<typeof Lucid>[2]>["evaluator"]
>;

type EvaluatorInput = Parameters<LucidEvaluator["evaluate"]>[0];

const diagnoseWithAiken = ({
  tx,
  additionalUTxOs,
  context,
}: EvaluatorInput): string => {
  try {
    UPLC.eval_phase_two_raw(
      CML.Transaction.from_cbor_hex(tx).to_cbor_bytes(),
      additionalUTxOs.map((utxo) =>
        utxoToTransactionInput(utxo).to_cbor_bytes(),
      ),
      additionalUTxOs.map((utxo) =>
        utxoToTransactionOutput(utxo).to_cbor_bytes(),
      ),
      context.costModels.to_cbor_bytes(),
      context.protocolParameters.maxTxExSteps,
      context.protocolParameters.maxTxExMem,
      BigInt(context.slotConfig.zeroTime),
      BigInt(context.slotConfig.zeroSlot),
      context.slotConfig.slotLength,
    );
    return "aiken evaluation succeeded";
  } catch (cause) {
    return cause instanceof Error ? cause.message : String(cause);
  }
};

const capturingEvaluator = (inner: LucidEvaluator): LucidEvaluator => ({
  name: inner.name,
  evaluate: async (input) => {
    try {
      return await inner.evaluate(input);
    } catch (cause) {
      evaluationSequence += 1;
      lastEvaluationFailure = {
        sequence: evaluationSequence,
        tx: input.tx,
        utxos: input.additionalUTxOs,
        message: cause instanceof Error ? cause.message : String(cause),
        diagnosis: diagnoseWithAiken(input),
      };
      throw cause;
    }
  },
});

/** The last evaluation failure the fixture evaluator saw (diagnosis). */
export const lastAvailabilityEvaluationFailure = () => lastEvaluationFailure;

/** Mainnet (Van Rossem) costing through the capturing Scalus evaluator. */
export const createAvailabilityEmulatorLucid = (emulator: Emulator) =>
  Lucid(emulator, "Preprod", {
    evaluator: capturingEvaluator(
      createScalusEvaluator({
        protocolMajorVersion: MAINNET_PROTOCOL_PARAMETERS_SOURCE.protocolMajor,
      }),
    ),
  });

export type AvailabilityRefusalPurpose = "spend" | "mint" | "withdraw";

export type AvailabilityRefusalExpectation =
  | {
      readonly purpose: AvailabilityRefusalPurpose;
      /** A script hash or a contract name (`AvailabilityScriptNames`). */
      readonly script: string;
      /** The failing redeemer's ledger index, when the test pins it. */
      readonly index?: number;
    }
  | { readonly trace: RegExp };

export type AvailabilityRefusal = {
  readonly purpose: AvailabilityRefusalPurpose | "other";
  readonly index: number;
  readonly scriptHash: string | undefined;
  readonly message: string;
};

export const AIKEN_TAG_PURPOSE: Readonly<
  Record<string, AvailabilityRefusalPurpose | "other">
> = {
  Spend: "spend",
  Mint: "mint",
  Withdraw: "withdraw",
  Publish: "other",
  Vote: "other",
  Propose: "other",
};

/**
 * The failing redeemer as the Aiken machine reports it
 * (`failed script execution\n  Mint[0] ...`): its tag and ledger index.
 */
export const parseAvailabilityEvaluationFailure = (
  diagnosis: string,
): { purpose: AvailabilityRefusalPurpose | "other"; index: number } => {
  const match = /\b(Spend|Mint|Withdraw|Publish|Vote|Propose)\[(\d+)\]/.exec(
    diagnosis,
  );
  if (!match)
    throw new Error(
      `Cannot attribute the evaluation failure to a redeemer: ${diagnosis}`,
    );
  return {
    purpose: AIKEN_TAG_PURPOSE[match[1]!] ?? "other",
    index: Number(match[2]),
  };
};
