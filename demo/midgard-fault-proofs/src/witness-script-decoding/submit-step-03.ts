import {
  MidgardNativeScriptDecodingDirections,
  type MidgardNativeScriptDecodingRefusalClass,
} from "@al-ft/midgard-core";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type WitnessScriptDecodingScanState,
  WitnessScriptDecodingStep03DatumSchema,
  WitnessScriptDecodingStep03RedeemerSchema,
  WitnessScriptDecodingStep04DatumSchema,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import {
  nativeScriptDecodingFrameData,
  nativeScriptDecodingScanArgsEvidence,
  nativeScriptDecodingWindowProofs,
} from "../native-script-decoding/evidence.js";
import {
  buildNativeScriptDecodingScanPlan,
  NativeScriptDecodingPlanRoutes,
} from "../native-script-decoding/scan-plan.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  WITNESS_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type WitnessScriptDecodingContracts,
} from "./contracts.js";
import {
  witnessScriptDecodingCheckpoint,
  type WitnessScriptDecodingEvidence,
  WitnessScriptDecodingResultClasses,
} from "./witness-script-decoding.js";

const mappedRefusal = (
  refusalClass: MidgardNativeScriptDecodingRefusalClass,
): bigint => BigInt(refusalClass + 1);

export const submitWitnessScriptDecodingStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  preSubmitBoundary,
  preSubmitBoundaryForResult,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WitnessScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: WitnessScriptDecodingEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundaryForResult?: (
    closed: boolean,
  ) => Promise<FraudProofPreSubmitBoundary | undefined>;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<WitnessScriptDecodingScanState>({
    threadUtxo,
    signer,
    schema: WitnessScriptDecodingStep03DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    state.item_commitment !== evidence.itemCommitmentHex ||
    state.total_length !== BigInt(evidence.itemLength) ||
    state.checkpoint_hash !==
      witnessScriptDecodingCheckpoint({
        evidence,
        controlCbor: state.control_cbor,
        nextExpectedScriptHash: state.next_expected_script_hash,
      })
  ) {
    throw new Error(
      `${FAMILY}: authenticated scan state differs from evidence`,
    );
  }

  let args: {
    readonly control_cbor: string;
    readonly chunk_proof: ReturnType<
      typeof nativeScriptDecodingWindowProofs
    >["chunk_proof"];
    readonly next_chunk_proof: ReturnType<
      typeof nativeScriptDecodingWindowProofs
    >["next_chunk_proof"];
    readonly frames: readonly ReturnType<
      typeof nativeScriptDecodingFrameData
    >[];
    readonly step_budget: bigint;
  };
  let nextControl = state.control_cbor;
  let nextClass = state.result_class;
  let closes =
    state.result_class !== BigInt(WitnessScriptDecodingResultClasses.Pending);
  let route: "closed" | "segment" | "verdict" = "closed";

  if (!closes) {
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: Buffer.from(evidence.itemHex, "hex"),
      direction: Number(evidence.finding.subject.direction) as 0 | 1,
    });
    if (plan.route !== NativeScriptDecodingPlanRoutes.Machine) {
      throw new Error(`${FAMILY}: pending state has a non-machine plan`);
    }
    const segment = plan.segments.find(
      ({ controlBefore }) => controlBefore.cborHex === state.control_cbor,
    );
    if (segment !== undefined) {
      args = nativeScriptDecodingScanArgsEvidence({
        segment,
        fieldIndex: 6,
        itemIndex: evidence.finding.scriptIndex,
        itemBytes: Buffer.from(evidence.itemHex, "hex"),
      });
      const isLast = plan.segments.at(-1) === segment;
      closes =
        isLast &&
        plan.direction ===
          MidgardNativeScriptDecodingDirections.WrongfulRejection;
      // `closed_state_v1` preserves the authenticated pre-fold control; only
      // a resumable advance commits the fold's returned control.
      nextControl = closes ? state.control_cbor : segment.controlAfter.cborHex;
      nextClass = closes
        ? BigInt(WitnessScriptDecodingResultClasses.NoFault)
        : BigInt(WitnessScriptDecodingResultClasses.Pending);
      route = "segment";
    } else if (plan.verdict.control?.cborHex === state.control_cbor) {
      const refusal = plan.verdict.refusalClass;
      if (refusal === null) {
        throw new Error(
          `${FAMILY}: terminal plan was not closed by its segment`,
        );
      }
      args = {
        control_cbor: state.control_cbor,
        ...nativeScriptDecodingWindowProofs({
          window: plan.verdict.window,
          fieldIndex: 6,
          itemIndex: evidence.finding.scriptIndex,
          itemBytes: Buffer.from(evidence.itemHex, "hex"),
        }),
        frames: [],
        step_budget: 1n,
      };
      nextClass = mappedRefusal(refusal);
      closes = true;
      route = "verdict";
    } else {
      throw new Error(
        `${FAMILY}: checkpoint is absent from the exact scan plan`,
      );
    }
  } else {
    args = {
      control_cbor: "",
      chunk_proof: null,
      next_chunk_proof: null,
      frames: [],
      step_budget: 0n,
    };
  }

  const nextStepIndex = closes ? 3 : 2;
  const nextExpectedScriptHash =
    contracts.steps[nextStepIndex].spendingScriptHash;
  const nextState: WitnessScriptDecodingScanState = {
    ...state,
    control_cbor: nextControl,
    next_expected_script_hash: nextExpectedScriptHash,
    checkpoint_hash: witnessScriptDecodingCheckpoint({
      evidence,
      controlCbor: nextControl,
      nextExpectedScriptHash,
    }),
    result_class: nextClass,
  };
  const nextSchema = closes
    ? WitnessScriptDecodingStep04DatumSchema
    : WitnessScriptDecodingStep03DatumSchema;
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    nextSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 03`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 03`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step 03`,
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, ...args },
        ],
      } as never,
      WitnessScriptDecodingStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const resolvedPreSubmitBoundary =
    preSubmitBoundaryForResult === undefined
      ? preSubmitBoundary
      : await preSubmitBoundaryForResult(closes);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: `${FAMILY} step 03`,
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary: resolvedPreSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return {
    txHash,
    route,
    closed: closes,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    scanState: nextState,
  };
};
