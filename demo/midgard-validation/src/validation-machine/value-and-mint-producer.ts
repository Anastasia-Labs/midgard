/**
 * The ValueAndMint producer: the value equation over the spent inputs, the
 * outputs and the mint, one authenticated asset mutation at a time.
 */
import {
  buildMidgardLedgerOutputAssetFrontier,
  buildMidgardValidationMerkleMembership,
  encodeCbor,
  hashMidgardMintAssetLeaf,
  hashMidgardResolvedContextItemLeaf,
} from "@al-ft/midgard-core";
import {
  decodeMidgardTxOutput,
  type MidgardValue,
} from "@al-ft/midgard-core/codec";
import { Effect } from "effect";

import { UNSET_REASON_COORDINATE } from "../rejection-reason.js";
import { outputCborMeetsMinAda } from "../value-accounting.js";
import {
  emptyMidgardInputResolutionSchedule,
  hash32,
} from "./input-resolution.js";
import type { PreparedValidationTransaction } from "./prepared-validation-transaction.js";
import type { ValidationTraceWalkState } from "./trace-builder-types.js";
import {
  phaseContinues,
  type PhaseOutcome,
  phaseStops,
  type TraceWalk,
} from "./trace-walk.js";
import type { ValidationMachineWorkWitness } from "./types.js";
import {
  applyValidationValueMutationStep,
  buildValidationValueMutationSteps,
  emptyValidationValueAccumulator,
  encodeValidationValueAccumulator,
  midgardValueAssets,
  midgardValueContributions,
  type ValidationValueContribution,
} from "./value-mutation.js";

type Auxiliary = NonNullable<ValidationMachineWorkWitness["auxiliary"]>;
type AssetFields =
  | "assetIndex"
  | "policyId"
  | "assetName"
  | "quantity"
  | "assetFrontier"
  | "assetSiblings"
  | "mutationStep";
/** An asset step's witness, less the asset and its mutation. */
type AssetWitnessHead =
  | Omit<Extract<Auxiliary, { kind: "valueInputAsset" }>, AssetFields>
  | Omit<Extract<Auxiliary, { kind: "valueOutputAsset" }>, AssetFields>;

/** A ValueAndMint replay carries no ledger proof: the empty CBOR list. */
const emptyReplayProofCbor = Buffer.from([0x80]);

export const produceValueAndMint = (
  prepared: Pick<
    PreparedValidationTransaction,
    | "phaseALedgerTx"
    | "resolutionScheduleNodes"
    | "resolutionScheduleHash"
    | "ledgerState"
    | "ledgerDescriptorState"
    | "outputCbors"
    | "input"
  >,
  state: Pick<
    ValidationTraceWalkState,
    "admittedOutputDescriptorCbors" | "admittedOutputDescriptorLeafHashes"
  >,
  nativeControlCbor: Buffer,
  { push }: Pick<TraceWalk, "push">,
): Effect.Effect<PhaseOutcome, Error> =>
  Effect.gen(function* () {
    const {
      resolutionScheduleNodes: nodes,
      resolutionScheduleHash: scheduleHash,
      ledgerState,
      ledgerDescriptorState,
      outputCbors,
    } = prepared;
    const maxAssets =
      prepared.input.consensusProfile.limits.maxDistinctAssetCount;
    const mintAssets = [...prepared.phaseALedgerTx!.mint.assets];
    const contributions: ValidationValueContribution[] = [];
    for (const node of nodes) {
      if (node.sourceKind !== "spend") continue;
      const value = ledgerState.get(node.key.toString("hex"));
      if (value === undefined) {
        return yield* Effect.fail(
          new Error(
            "value mutation planning lost a previously authenticated ledger value",
          ),
        );
      }
      contributions.push(
        ...midgardValueContributions(decodeMidgardTxOutput(value).value, 1n),
      );
    }
    for (const outputCbor of outputCbors) {
      contributions.push(
        ...midgardValueContributions(
          decodeMidgardTxOutput(outputCbor).value,
          -1n,
        ),
      );
    }
    for (const asset of mintAssets) {
      contributions.push({
        unit: Buffer.concat([
          Buffer.from(asset.policyId),
          Buffer.from(asset.assetName),
        ]),
        quantityDelta: asset.quantity,
      });
    }
    const mutationSteps = yield* Effect.tryPromise({
      try: () => buildValidationValueMutationSteps(contributions),
      catch: (cause) =>
        cause instanceof Error
          ? cause
          : new Error("failed to build authenticated value mutations"),
    });
    const accumulator = emptyValidationValueAccumulator();
    const cursor = {
      replay: 0,
      replayAsset: 0,
      valueHash: Buffer.alloc(32),
      remainingScheduleHash: emptyMidgardInputResolutionSchedule(),
      output: 0,
      outputAsset: 0,
      mint: 0,
      mutation: 0,
    };
    const pushAt = (
      stage: number,
      auxiliary?: Auxiliary,
      replayScheduleHash = scheduleHash,
    ): void =>
      push(
        "valueAndMint",
        encodeCbor([
          nativeControlCbor,
          BigInt(stage),
          replayScheduleHash,
          BigInt(cursor.replay),
          BigInt(cursor.replayAsset),
          cursor.valueHash,
          cursor.remainingScheduleHash,
          BigInt(cursor.output),
          BigInt(cursor.outputAsset),
          BigInt(cursor.mint),
          encodeValidationValueAccumulator(accumulator),
        ]),
        auxiliary,
      );
    /** Folds one input's or output's assets in; a stop past the bound. */
    const foldAssets = (
      value: MidgardValue,
      head: AssetWitnessHead,
      assetCursor: "replayAsset" | "outputAsset",
      exhausted: string,
      overLimit: (assetIndex: number) => PhaseOutcome,
    ) =>
      Effect.gen(function* () {
        const assets = midgardValueAssets(value);
        if (assets.length === 0) return null;
        const material = buildMidgardLedgerOutputAssetFrontier(assets);
        cursor[assetCursor] = 1;
        cursor.valueHash = hash32(head.descriptorCbor);
        for (const [assetIndex, asset] of assets.entries()) {
          const mutationStep = mutationSteps[cursor.mutation];
          if (mutationStep === undefined) {
            return yield* Effect.fail(new Error(exhausted));
          }
          pushAt(head.kind === "valueInputAsset" ? 2 : 3, {
            ...head,
            assetIndex,
            policyId: asset.policyId,
            assetName: asset.assetName,
            quantity: asset.quantity,
            assetFrontier: material.frontier,
            assetSiblings: buildMidgardValidationMerkleMembership(
              material.leaves,
              assetIndex,
            ).siblings,
            mutationStep,
          });
          if (mutationStep.postAssetEntryCount > maxAssets) {
            return overLimit(assetIndex);
          }
          applyValidationValueMutationStep(accumulator, mutationStep);
          cursor.mutation += 1;
          cursor[assetCursor] += 1;
        }
        return null;
      });

    pushAt(0, undefined, emptyMidgardInputResolutionSchedule());
    cursor.remainingScheduleHash = scheduleHash;
    pushAt(1);

    // The resolved-item frontier ScriptSources stage 3 built: each replay
    // opens its own leaf in it.
    const replayLeaves: Uint8Array[] = [];
    for (const [itemIndex, node] of nodes.entries()) {
      const descriptorCbor = ledgerDescriptorState.get(
        node.key.toString("hex"),
      );
      if (descriptorCbor === undefined) {
        return yield* Effect.fail(
          new Error(
            "value replay lost a previously authenticated ledger descriptor",
          ),
        );
      }
      replayLeaves.push(
        hashMidgardResolvedContextItemLeaf({
          sourceKind: node.sourceKind,
          itemIndex,
          key: node.key,
          outputCbor: descriptorCbor,
        }),
      );
    }
    for (const [replayIndex, node] of nodes.entries()) {
      const outRefHex = node.key.toString("hex");
      const outputCbor = ledgerState.get(outRefHex);
      const descriptorCbor = ledgerDescriptorState.get(outRefHex);
      if (outputCbor === undefined || descriptorCbor === undefined) {
        return yield* Effect.fail(
          new Error(
            "value replay lost a previously authenticated ledger descriptor",
          ),
        );
      }
      const { sourceKind, itemIndex, key, nextScheduleHash } = node;
      pushAt(2, {
        kind: "resolvedInputReplay",
        sourceKind,
        itemIndex,
        key,
        nextScheduleHash,
        value: descriptorCbor,
        proofCbor: emptyReplayProofCbor,
        siblings: buildMidgardValidationMerkleMembership(
          replayLeaves,
          replayIndex,
        ).siblings,
      });
      const value = decodeMidgardTxOutput(outputCbor).value;
      if (sourceKind === "spend") {
        accumulator.lovelaceDelta += value.lovelace;
        const stop = yield* foldAssets(
          value,
          {
            kind: "valueInputAsset",
            sourceKind,
            itemIndex,
            key,
            nextScheduleHash,
            descriptorCbor,
          },
          "replayAsset",
          "value replay exhausted authenticated mutation steps",
          (assetIndex) =>
            phaseStops(
              {
                InputAssetAccumulationLimit: {
                  input_index: UNSET_REASON_COORDINATE,
                  asset_index: BigInt(assetIndex),
                },
              },
              "V1 spend-value replay exceeds the asset bound",
            ),
        );
        if (stop !== null) return stop;
      }
      cursor.replayAsset = 0;
      cursor.valueHash = Buffer.alloc(32);
      cursor.remainingScheduleHash = nextScheduleHash;
      cursor.replay += 1;
    }

    pushAt(2);
    for (const [outputIndex, outputCbor] of outputCbors.entries()) {
      const descriptorCbor = state.admittedOutputDescriptorCbors[outputIndex];
      if (descriptorCbor === undefined) {
        return yield* Effect.fail(
          new Error(
            "value replay lost an authenticated transaction-output descriptor",
          ),
        );
      }
      pushAt(3, {
        kind: "valueOutputDescriptor",
        outputIndex,
        descriptorCbor,
        siblings: buildMidgardValidationMerkleMembership(
          state.admittedOutputDescriptorLeafHashes,
          outputIndex,
        ).siblings,
      });
      const value = decodeMidgardTxOutput(outputCbor).value;
      // E_MIN_ADA / MIN-ADA-TX (#618 ruling 1; R8 of decision 0005). The
      // mirror of the ValueAndMint stage-3 output-descriptor conjunct in
      // onchain/aiken/lib/midgard/validation-machine/, evaluated in the same
      // place: after the descriptor step's witness is committed, before this
      // output's Ada is folded into the accumulator and before the asset
      // cursor opens. `outputCbor` is the canonical output preimage the
      // descriptor's `total_length` binds, so both halves price the same
      // bytes.
      if (!outputCborMeetsMinAda(outputCbor, value.lovelace)) {
        return phaseStops(
          { OutputBelowMinAda: { output_index: BigInt(outputIndex) } },
          `V1 output[${outputIndex.toString()}] is below the minimum-Ada floor`,
        );
      }
      accumulator.lovelaceDelta -= value.lovelace;
      const stop = yield* foldAssets(
        value,
        { kind: "valueOutputAsset", outputIndex, descriptorCbor },
        "outputAsset",
        "output replay exhausted authenticated value mutations",
        (assetIndex) =>
          phaseStops(
            {
              OutputAssetAccumulationLimit: {
                output_index: BigInt(outputIndex),
                asset_index: BigInt(assetIndex),
              },
            },
            "V1 output-value replay exceeds the asset bound",
          ),
      );
      if (stop !== null) return stop;
      cursor.outputAsset = 0;
      cursor.valueHash = Buffer.alloc(32);
      cursor.output += 1;
    }

    pushAt(3);
    const mintLeaves = mintAssets.map((asset) =>
      hashMidgardMintAssetLeaf(asset),
    );
    for (const [mintIndex, asset] of mintAssets.entries()) {
      pushAt(4, {
        kind: "valueMintAsset",
        mintIndex,
        policyId: Buffer.from(asset.policyId),
        assetName: Buffer.from(asset.assetName),
        quantity: asset.quantity,
        siblings: buildMidgardValidationMerkleMembership(mintLeaves, mintIndex)
          .siblings,
        mutationStep: mutationSteps[cursor.mutation]!,
      });
      const mutationStep = mutationSteps[cursor.mutation];
      if (mutationStep === undefined) {
        return yield* Effect.fail(
          new Error("mint replay exhausted authenticated value mutations"),
        );
      }
      if (mutationStep.postAssetEntryCount > maxAssets) {
        return phaseStops(
          { MintAssetAccumulationLimit: { mint_index: BigInt(mintIndex) } },
          "V1 mint replay exceeds the asset bound",
        );
      }
      applyValidationValueMutationStep(accumulator, mutationStep);
      cursor.mutation += 1;
      cursor.mint += 1;
    }

    pushAt(4);
    const valueIsPreserved =
      accumulator.lovelaceDelta - prepared.phaseALedgerTx!.fee === 0n &&
      accumulator.nonzeroAssetCount === 0;
    pushAt(5);
    return valueIsPreserved
      ? phaseContinues
      : phaseStops("ValueNotPreserved", "V1 value equation does not balance");
  });
