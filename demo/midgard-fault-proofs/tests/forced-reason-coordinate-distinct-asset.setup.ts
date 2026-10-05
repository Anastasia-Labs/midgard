import {
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxId,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardNativeTxFull,
  MidgardValidationPhase,
} from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardTxOutput,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  retainedValidationAuxiliaryWitnessData,
} from "@al-ft/midgard-validation";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx as makeFixtureNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  signedKeyOutput,
  signNativeTx,
} from "./support/forced-reason-signed-native-tx.js";

/**
 * Forced transactions that cross the distinct-asset bound, and the validation
 * machine's own trace of them.
 *
 * Sixteen spend inputs at the signing key carry 1,024 distinct assets each,
 * so the input fold ends with exactly the bound (16,384) seen. Unit n is name
 * `n mod 1024` (two bytes) under policy `n div 1024`, so canonical value
 * order is numeric order.
 */

export const LIMIT = MIDGARD_CONSENSUS_LIMITS.maxDistinctAssetCount;
const PER_INPUT = 1_024;
const INPUTS = LIMIT / PER_INPUT;

const policyHex = (policy: number): string =>
  Buffer.concat([Buffer.alloc(27, 0xc0), Buffer.from([policy])]).toString(
    "hex",
  );
const nameHex = (name: number): string =>
  Buffer.from([name >> 8, name & 0xff]).toString("hex");

/** The given units, each with quantity 1. */
const units = (
  unitIds: readonly number[],
): Map<string, Map<string, bigint>> => {
  const assets = new Map<string, Map<string, bigint>>();
  for (const unit of unitIds) {
    const policy = policyHex(Math.floor(unit / PER_INPUT));
    const names = assets.get(policy) ?? new Map<string, bigint>();
    names.set(nameHex(unit % PER_INPUT), 1n);
    assets.set(policy, names);
  }
  return assets;
};
const range = (start: number, count: number): number[] =>
  Array.from({ length: count }, (_, offset) => start + offset);

const keyAddress = decodeMidgardTxOutput(signedKeyOutput()).address;
/** An output at the signing key holding `unitIds`. */
const keyOutput = (
  unitIds: readonly number[],
  lovelace = 10_000_000n,
): Buffer =>
  encodeMidgardTxOutput({
    address: keyAddress,
    value: { lovelace, assets: units(unitIds) },
  });

const spendInput = (byte: number): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, byte),
    outputIndex: 0,
  });

const spendInputs = Array.from({ length: INPUTS }, (_, index) =>
  spendInput(0x20 + index),
);

/** The sixteen saturating spend inputs and the outputs they resolve to. */
const saturatingLedger: (readonly [Buffer, Buffer])[] = spendInputs.map(
  (input, index) =>
    [input, keyOutput(range(index * PER_INPUT, PER_INPUT))] as const,
);

export type CrossingScenario = Readonly<{
  nativeTx: MidgardNativeTxFull;
  transactionId: Buffer;
  /** The node's forced canonical bytes of `nativeTx`. */
  forcedCanonicalCbor: Buffer;
  /** Ledger entries as `[out-ref item bytes, output bytes]`. */
  ledger: readonly (readonly [Buffer, Buffer])[];
}>;

const scenario = (
  nativeTx: MidgardNativeTxFull,
  ledger: readonly (readonly [Buffer, Buffer])[],
): CrossingScenario => ({
  nativeTx,
  transactionId: computeMidgardNativeTxId(nativeTx),
  forcedCanonicalCbor: encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical(nativeTx),
  ),
  ledger,
});

/**
 * Output 1 holds unit 0, already seen in the input fold, and then unit
 * 16,385, which is new: the crossing is output 1, asset 1. Value is not
 * preserved either; that rule comes later in the machine.
 */
export const outputCrossing = (): CrossingScenario =>
  scenario(
    signNativeTx(
      makeNativeTx({
        spendInputCbors: spendInputs,
        fee: 1n,
        outputCbors: [signedKeyOutput(10_000_000n), keyOutput([0, LIMIT + 1])],
      }),
    ),
    saturatingLedger,
  );

/** The written coordinate of {@link outputCrossing}. */
export const OUTPUT_CROSSING = { outputIndex: 1, assetIndex: 1 } as const;

/**
 * A seventeenth spend input, sorting after the others, carries one unit seen
 * nowhere else, and a reference input sorts before every spend. The reference
 * input takes schedule position 0 and folds nothing, so the crossing is
 * schedule position 17, asset 0. The one output pays none of the assets, so
 * value is not preserved either: the machine reaches the input fold first.
 */
export const inputCrossing = (): CrossingScenario => {
  const crossingInput = spendInput(0x40);
  const referenceInput = encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x10),
    outputIndex: 0,
  });
  return scenario(
    signNativeTx(
      makeNativeTx({
        spendInputCbors: [...spendInputs, crossingInput],
        referenceByte: "10",
        fee: 1n,
        outputCbors: [signedKeyOutput(10_000_000n)],
      }),
    ),
    [
      [referenceInput, signedKeyOutput(10_000_000n)],
      ...saturatingLedger,
      [crossingInput, keyOutput([LIMIT])],
    ],
  );
};

/** The written coordinate of {@link inputCrossing}. */
export const INPUT_CROSSING = { inputIndex: 17, assetIndex: 0 } as const;

/** The empty name and every one-byte name: 257 units per policy. */
const policyUnits = (): Map<string, bigint> =>
  new Map([
    ["", 1n],
    ...Array.from(
      { length: 256 },
      (_, name) => [Buffer.from([name]).toString("hex"), 1n] as const,
    ),
  ]);

/**
 * Outputs and mint that alone carry more than the bound, with no asset in the
 * inputs. Six outputs hold six policies of 257 units each (9,252 units), and
 * 28 always-true native policies mint 257 units each, so the walk reaches the
 * bound inside the mint and crosses at mint entry 16,384 - 9,252 = 7,132.
 */
export const outputsAndMintCrossing = (): CrossingScenario => {
  const outputs = Array.from({ length: 6 }, (_, output) =>
    makeOutput(
      100_000_000n,
      undefined,
      new Map(
        Array.from({ length: 6 }, (_, policy) => [
          Buffer.concat([
            Buffer.alloc(27, 0xc1),
            Buffer.from([output * 6 + policy]),
          ]).toString("hex"),
          policyUnits(),
        ]),
      ),
    ),
  );
  // `all` over k always-true children is always true, and each k is a
  // distinct script, so a distinct policy.
  const scripts = Array.from({ length: 28 }, (_, children) =>
    nativeScriptWitness({
      type: "all",
      scripts: Array.from({ length: children }, () => ({
        type: "all" as const,
        scripts: [],
      })),
    }),
  );
  const mintNames = new Map(
    [...policyUnits()].map(([name, quantity]) => [
      Buffer.from(name, "hex"),
      quantity,
    ]),
  );
  const spent = outRefFromByte(0x75);
  const transaction = makeFixtureNativeTx({
    spendInputs: [spent],
    outputs,
    scriptWitnesses: scripts,
    mintPreimageCbor: makeMintPreimageCbor(
      new Map(
        scripts.map((script) => [
          Buffer.from(hashScriptWitness(script), "hex"),
          mintNames,
        ]),
      ),
    ),
  });
  return scenario(transaction.tx, [
    [spent, makeOutput(FUNDED_OUTPUT_LOVELACE)],
  ]);
};

/** The written coordinate of {@link outputsAndMintCrossing}. */
export const MINT_CROSSING = { mintIndex: 7_132 } as const;

/**
 * The validation machine's trace of a forced `crossing` under `orderKey`,
 * built the way block production builds it, with the retained witnesses of
 * its last ValueAndMint asset steps and its two endpoints.
 */
export const buildForcedCrossingTrace = async (
  crossing: CrossingScenario,
  orderKey: SDK.OutputReference,
) => {
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: orderKey },
  } as const;
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, SDK.EventKeySchema as never),
    "hex",
  );
  const trace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      eventKeyCbor,
      sourceKind: "forced",
      blockEndTimeMs: 1_800_000_000_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 100n,
      transactionId: crossing.transactionId,
      canonicalTransactionCbor: crossing.forcedCanonicalCbor,
      programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
      priorUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      postUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      ledgerWitnessEntries: crossing.ledger.map(([outRef, output]) => ({
        outRef,
        output,
      })),
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: "E_ASSET_COUNT",
    }),
  );
  const tree = buildMidgardValidationTraceTree(
    trace.states.map(hashMidgardValidationMachineState),
    "rejected",
    hashMidgardValidationRejectionCode("E_ASSET_COUNT"),
  );
  const descriptor: SDK.ValidationTraceDescriptor = {
    schema_version: BigInt(tree.descriptor.schemaVersion),
    machine_version: BigInt(tree.descriptor.machineVersion),
    trace_root: tree.descriptor.traceRoot.toString("hex"),
    step_count: BigInt(tree.descriptor.stepCount),
    initial_state_hash: tree.descriptor.initialStateHash.toString("hex"),
    terminal_state_hash: tree.descriptor.terminalStateHash.toString("hex"),
    verdict: "Rejected",
    rejection_code_hash: tree.descriptor.rejectionCodeHash.toString("hex"),
  };
  const descriptorCbor = Buffer.from(
    Data.to(descriptor as never, SDK.ValidationTraceDescriptorSchema as never),
    "hex",
  );
  // The fold stops at the crossing, so the last asset steps are the crossing
  // and the steps just before it. Retaining every one of the 16,384 earlier
  // input-fold steps would add nothing a proof opens.
  const assetKinds = ["valueInputAsset", "valueOutputAsset", "valueMintAsset"];
  const assetSteps = trace.witnesses.flatMap((witness, index) =>
    witness.phase === "valueAndMint" &&
    witness.auxiliary !== null &&
    assetKinds.includes(witness.auxiliary.kind)
      ? [index]
      : [],
  );
  const retainedWitnesses: [Buffer, Buffer][] = assetSteps
    .slice(-4)
    .map((index) => {
      const witness = trace.witnesses[index]!;
      return [
        Buffer.from(
          SDK.encodeRetainedValidationWitnessKey({
            event_key: eventKey,
            execution_index: SDK.retainedValidationStateCoordinate(
              descriptor.step_count,
              BigInt(index),
            ),
          }),
        ),
        Buffer.from(
          SDK.encodeRetainedValidationWitness({
            machine_state: SDK.validationMachineStateDataFromCore(
              trace.states[index]!,
            ),
            trace_proof: SDK.validationTraceProofDataFromCore(
              tree.proofs[index]!,
            ),
            phase: BigInt(MidgardValidationPhase[witness.phase]),
            program_counter: BigInt(witness.programCounter),
            witness_cbor: witness.cbor.toString("hex"),
            auxiliary: Data.from(
              Data.to(
                retainedValidationAuxiliaryWitnessData(
                  witness.auxiliary,
                ) as never,
              ),
              SDK.RetainedValidationAuxiliaryWitnessSchema,
            ) as unknown as SDK.RetainedValidationAuxiliaryWitness,
          }),
        ),
      ];
    });
  return {
    trace,
    eventKey,
    eventKeyCbor,
    descriptorCbor,
    retainedWitnesses,
  };
};

/** The trace and retained witnesses {@link buildForcedCrossingTrace} returns. */
export type CrossingTrace = Awaited<
  ReturnType<typeof buildForcedCrossingTrace>
>;

/** The forced-order key every suite block commits its crossing under. */
export const ORDER_KEY = {
  transactionId: "71".repeat(32),
  outputIndex: 0n,
} as const;

/** The forced reason naming output `outputIndex`, asset `assetIndex`. */
export const outputReason = (coordinate: {
  readonly outputIndex: number;
  readonly assetIndex: number;
}): SDK.RejectionReason => ({
  OutputAssetAccumulationLimit: {
    output_index: BigInt(coordinate.outputIndex),
    asset_index: BigInt(coordinate.assetIndex),
  },
});
