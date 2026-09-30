import type {
  FieldOpening,
  NativeScriptDecodingScanThreadState,
} from "@al-ft/midgard-sdk";
import {
  encodeMidgardTxInputCanonical,
  type MidgardTxInput,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
  nativeScriptDecodingOpenedSubjectState,
  NativeScriptDecodingStep03OpenSubjectSpendRedeemer,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { type FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { NativeScriptDecodingContracts } from "./contracts.js";
import {
  classifyNativeScriptDecodingOutOfDomainFace,
  NativeScriptDecodingOutOfDomainFaces,
  nativeScriptDecodingSubjectFieldIndex,
} from "./evidence.js";
import {
  nativeScriptDecodingSubmitError,
  requireNativeScriptDecodingReferenceScript,
  requireNativeScriptDecodingThreadUtxo,
} from "./submit-common.js";
import {
  advanceStep03Thread,
  BIND_DESCRIPTOR_INDEX,
  OPEN_SUBJECT_INDEX,
  OPEN_SUBJECT_LABEL,
  requirePreOpenState,
  requireStep03State,
  STEP_04_INDEX,
  step03Result,
  type SubmitNativeScriptDecodingStep03Result,
} from "./submit-native-script-decoding-step-03.advance-step03-thread.js";

// ## OpenSubject

export const submitNativeScriptDecodingStep03OpenSubject = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  subjectFieldInputs,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Required whenever the accusation names a real field and non-negative ordinal. */
  readonly nativeTxCompactCbor?: string;
  readonly subjectFieldInputs?: readonly MidgardTxInput[];
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: OPEN_SUBJECT_INDEX,
      threadOutRef,
    });
  const state = requireStep03State({
    threadUtxo,
    signer,
    stepIndex: OPEN_SUBJECT_INDEX,
  });
  requirePreOpenState(state);

  const face = classifyNativeScriptDecodingOutOfDomainFace({
    outpointSourceKind: state.outpoint_source_kind,
    outpointCursor: state.outpoint_cursor,
    itemCount:
      subjectFieldInputs === undefined
        ? null
        : BigInt(subjectFieldInputs.length),
  });
  if (
    face !== null &&
    state.direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION
  ) {
    throw nativeScriptDecodingSubmitError(
      "an out-of-domain accusation can close only for direction B.",
    );
  }

  const needsOpening =
    face === null || face === NativeScriptDecodingOutOfDomainFaces.CountFace;
  let subjectFieldOpening: FieldOpening | null = null;
  let carriageUtxos: readonly UTxO[] = [];
  if (needsOpening) {
    if (nativeTxCompactCbor === undefined || subjectFieldInputs === undefined) {
      throw nativeScriptDecodingSubmitError(
        "the accused pair names a field and non-negative ordinal; supply its compact transaction and complete field items.",
      );
    }
    const fieldIndex = nativeScriptDecodingSubjectFieldIndex(
      state.outpoint_source_kind,
    );
    const planned = planFaultProofFieldOpening({
      anchorSourceKind: state.source_kind === 1n ? 1n : 0n,
      fieldIndex,
      anchorTxId: state.verified_tx_id,
      nativeTxCompactCbor,
      itemCbors: subjectFieldInputs.map(encodeMidgardTxInputCanonical),
      owner: signer.paymentKeyHash,
      publish: publishCarriage,
      label: `${OPEN_SUBJECT_LABEL} subject field`,
    });
    signer.selectWallet(lucid);
    const published =
      publishedCarriageUtxos ??
      (await publishFaultProofFieldCarriage({
        lucid,
        signer,
        planned,
        publisherAddress: signer.address,
        label: `${OPEN_SUBJECT_LABEL} subject field`,
        preSubmitBoundary: publicationPreSubmitBoundary,
      }));
    const stepReference = requireNativeScriptDecodingReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash:
        contracts.steps[OPEN_SUBJECT_INDEX].spendingScriptHash,
      stepIndex: OPEN_SUBJECT_INDEX,
    });
    subjectFieldOpening = faultProofFieldOpening({
      planned,
      referenceInputs: [
        ...published,
        ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
        stepReference,
      ],
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${OPEN_SUBJECT_LABEL} subject field`,
    });
    carriageUtxos = [
      ...published,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ];
  }

  let nextState: NativeScriptDecodingScanThreadState;
  let destinationAddress: string;
  if (face === null) {
    if (subjectFieldInputs === undefined) {
      throw nativeScriptDecodingSubmitError(
        "an in-domain subject requires the complete field item list.",
      );
    }
    const subjectOutpoint = subjectFieldInputs[Number(state.outpoint_cursor)];
    if (subjectOutpoint === undefined) {
      throw nativeScriptDecodingSubmitError(
        "the accused ordinal is not present in the supplied field.",
      );
    }
    const outpointKeyCbor = Buffer.from(
      encodeMidgardTxInputCanonical(subjectOutpoint),
    ).toString("hex");
    nextState = await Effect.runPromise(
      nativeScriptDecodingOpenedSubjectState({
        state,
        outpointKeyBytes: outpointKeyCbor,
        outputIndex: subjectOutpoint.output_index,
      }),
    );
    destinationAddress =
      contracts.steps[BIND_DESCRIPTOR_INDEX].spendingScriptAddress;
  } else {
    nextState = {
      ...state,
      refusal_class: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
    };
    destinationAddress = contracts.steps[STEP_04_INDEX].spendingScriptAddress;
  }

  const opening = subjectFieldOpening;
  const { txHash, layout } = await advanceStep03Thread({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    destinationAddress,
    nextState,
    spendingStepIndex: OPEN_SUBJECT_INDEX,
    buildRedeemer: (resolved) =>
      Data.to(
        {
          Continue: [
            {
              input_index: resolved.inputIndex,
              output_index: resolved.outputIndex,
              subject_field_opening: opening,
            },
          ],
        },
        NativeScriptDecodingStep03OpenSubjectSpendRedeemer,
      ),
    carriageUtxos,
    referenceScriptUtxo,
    preSubmitBoundary,
    awaitConfirmation,
  });
  return step03Result({
    txHash,
    layout,
    signer,
    threadOutRef,
    threadToken,
    destinationAddress,
    scanState: nextState,
    awaitConfirmation,
  });
};
