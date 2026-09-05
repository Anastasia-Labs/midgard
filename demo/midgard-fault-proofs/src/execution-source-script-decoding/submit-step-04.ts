import {
  type MidgardNativeScriptDecodingDirection,
  MidgardNativeScriptDecodingDirections,
  type MidgardNativeScriptDecodingRefusalClass,
} from "@al-ft/midgard-core";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
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
import type { ExecutionSourceScriptDecodingContracts } from "./contracts.js";
import {
  executionSourceScriptDecodingCheckpoint,
  type ExecutionSourceScriptDecodingEvidence,
  ExecutionSourceScriptDecodingResultClasses,
} from "./family.js";
import {
  ExecutionSourceScanStateSchema,
  ExecutionSourceStep04DatumSchema,
  ExecutionSourceStep04RedeemerSchema,
  ExecutionSourceStep05DatumSchema,
} from "./schemas.js";

const FAMILY = "execution-source-script-decoding";
const mappedRefusal = (
  value: MidgardNativeScriptDecodingRefusalClass,
): bigint => BigInt(value);

export type ExecutionSourceScanState = Data.Static<
  typeof ExecutionSourceScanStateSchema
>;
export type ExecutionSourceScanArgs = Omit<
  Extract<
    Data.Static<typeof ExecutionSourceStep04RedeemerSchema>,
    { Continue: unknown }
  >["Continue"][0],
  "input_index" | "output_index"
>;

/** The thread as step 04 holds it: its UTxO, token, and authenticated state. */
export const readExecutionSourceScanState = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  stepIndex = 3,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ExecutionSourceScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Step 04 (`3`, the default) or the closed thread waiting at step 05 (`4`). */
  readonly stepIndex?: 3 | 4;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<ExecutionSourceScanState>({
    threadUtxo,
    signer,
    schema: (stepIndex === 3
      ? ExecutionSourceStep04DatumSchema
      : ExecutionSourceStep05DatumSchema) as never,
    family: FAMILY,
    stepIndex,
  });
  return { threadUtxo, threadToken, state };
};

/**
 * Derives the next step-04 transaction from the authenticated state and the
 * retained evidence: the exact planned segment (or verdict) the shared planner
 * cuts, the successor state, and whether it closes to step 05.
 *
 * `direction` folds the canonical scan under an explicit polarity instead of
 * the subject's own — a diagnostic for driving an honest verdict's exact
 * terminal through the real validators so step 05 can refuse it on chain.
 */
export const planExecutionSourceScriptDecodingStep04 = ({
  contracts,
  state,
  evidence,
  direction = Number(evidence.finding.subject.direction) as 0 | 1,
}: {
  readonly contracts: ExecutionSourceScriptDecodingContracts;
  readonly state: ExecutionSourceScanState;
  readonly evidence: ExecutionSourceScriptDecodingEvidence;
  readonly direction?: MidgardNativeScriptDecodingDirection;
}): {
  readonly args: ExecutionSourceScanArgs;
  readonly nextState: ExecutionSourceScanState;
  readonly nextStepIndex: 3 | 4;
  readonly closes: boolean;
} => {
  if (
    state.source.item_commitment !== evidence.itemCommitmentHex ||
    state.checkpoint_hash !==
      executionSourceScriptDecodingCheckpoint({
        evidence,
        controlCbor: state.control_cbor,
        nextExpectedScriptHash: state.next_expected_script_hash,
      })
  )
    throw new Error(
      `${FAMILY}: scan checkpoint differs from retained evidence`,
    );
  const item = Buffer.from(evidence.descriptor.scriptItemHex, "hex");
  const fieldIndex = evidence.descriptor.originKind === 0 ? 6 : 2;
  const itemIndex =
    state.source.origin_kind === 0n
      ? Number(state.source.source_index)
      : Number(Buffer.from(state.source.source_key, "hex").readUInt16BE(36));
  let args: ExecutionSourceScanArgs;
  let nextControl = state.control_cbor;
  let nextClass = state.result_class;
  let closes =
    state.result_class !==
    BigInt(ExecutionSourceScriptDecodingResultClasses.Pending);
  if (!closes) {
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction,
    });
    if (plan.route !== NativeScriptDecodingPlanRoutes.Machine)
      throw new Error(`${FAMILY}: pending state has no machine plan`);
    const segment = plan.segments.find(
      ({ controlBefore }) => controlBefore.cborHex === state.control_cbor,
    );
    if (segment !== undefined) {
      args = nativeScriptDecodingScanArgsEvidence({
        segment,
        fieldIndex,
        itemIndex,
        itemBytes: item,
      }) as ExecutionSourceScanArgs;
      const isLast = plan.segments.at(-1) === segment;
      closes =
        isLast &&
        plan.direction ===
          MidgardNativeScriptDecodingDirections.WrongfulRejection;
      nextControl = closes ? state.control_cbor : segment.controlAfter.cborHex;
      nextClass = closes
        ? BigInt(ExecutionSourceScriptDecodingResultClasses.NoFault)
        : BigInt(ExecutionSourceScriptDecodingResultClasses.Pending);
    } else if (plan.verdict.control?.cborHex === state.control_cbor) {
      if (plan.verdict.refusalClass === null)
        throw new Error(`${FAMILY}: terminal plan was not closed by segment`);
      args = {
        control_cbor: state.control_cbor,
        ...nativeScriptDecodingWindowProofs({
          window: plan.verdict.window,
          fieldIndex,
          itemIndex,
          itemBytes: item,
        }),
        frames: [],
        step_budget: 1n,
      } as ExecutionSourceScanArgs;
      nextClass = mappedRefusal(plan.verdict.refusalClass);
      closes = true;
    } else
      throw new Error(`${FAMILY}: checkpoint is absent from exact scan plan`);
  } else {
    args = {
      control_cbor: "",
      chunk_proof: null,
      next_chunk_proof: null,
      frames: [],
      step_budget: 0n,
    } as ExecutionSourceScanArgs;
  }
  const nextStepIndex = closes ? 4 : 3;
  const nextExpectedScriptHash =
    contracts.steps[nextStepIndex].spendingScriptHash;
  const nextState: ExecutionSourceScanState = {
    ...state,
    control_cbor: nextControl,
    next_expected_script_hash: nextExpectedScriptHash,
    checkpoint_hash: executionSourceScriptDecodingCheckpoint({
      evidence,
      controlCbor: nextControl,
      nextExpectedScriptHash,
    }),
    result_class: nextClass,
  };
  return { args, nextState, nextStepIndex, closes };
};

/**
 * Submits exactly the scan arguments and successor state the caller names, so
 * a lifecycle suite can drive a substituted chunk window, a tampered
 * checkpoint, a wrong successor or a premature close through the real step-04
 * validator. The classified builder below derives them from the plan.
 */
export const submitExecutionSourceScriptDecodingStep04Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  args,
  nextState,
  nextStepIndex,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ExecutionSourceScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly args: ExecutionSourceScanArgs;
  readonly nextState: ExecutionSourceScanState;
  readonly nextStepIndex: 3 | 4;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await readExecutionSourceScanState({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
  });
  const closes = nextStepIndex === 4;
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    (closes
      ? ExecutionSourceStep05DatumSchema
      : ExecutionSourceStep04DatumSchema) as never,
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
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 04`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 04`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step 04`,
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, ...args },
        ],
      } as never,
      ExecutionSourceStep04RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: `${FAMILY} step 04`,
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    closed: closes,
  };
};

export const submitExecutionSourceScriptDecodingStep04 = async ({
  evidence,
  direction,
  ...rest
}: Omit<
  Parameters<typeof submitExecutionSourceScriptDecodingStep04Raw>[0],
  "args" | "nextState" | "nextStepIndex"
> & {
  readonly evidence: ExecutionSourceScriptDecodingEvidence;
  /** See `planExecutionSourceScriptDecodingStep04`. */
  readonly direction?: MidgardNativeScriptDecodingDirection;
}) => {
  const { state } = await readExecutionSourceScanState(rest);
  const planned = planExecutionSourceScriptDecodingStep04({
    contracts: rest.contracts,
    state,
    evidence,
    ...(direction === undefined ? {} : { direction }),
  });
  return await submitExecutionSourceScriptDecodingStep04Raw({
    ...rest,
    ...planned,
  });
};
