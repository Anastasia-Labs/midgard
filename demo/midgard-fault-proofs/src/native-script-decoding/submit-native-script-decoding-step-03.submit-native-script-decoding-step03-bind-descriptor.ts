import {
  decodeMidgardLedgerOutputCommitment,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
  type MidgardLedgerOutputCommitment,
} from "@al-ft/midgard-core";
import type {
  BoundedItemChunkProof,
  NativeScriptDecodingScanThreadState,
  Proof,
} from "@al-ft/midgard-sdk";
import {
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
  nativeScriptDecodingBoundDescriptorState,
  nativeScriptDecodingOpenedSubjectState,
  NativeScriptDecodingStep03BindDescriptorSpendRedeemer,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type ResolvedProverSigner } from "../runtime.js";
import { type FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { NativeScriptDecodingContracts } from "./contracts.js";
import {
  buildNativeScriptDecodingChunkProof,
  buildNativeScriptDecodingLedgerMembership,
  type NativeScriptDecodingLedgerTrieHandle,
} from "./evidence.js";
import {
  NativeScriptDecodingPlanRoutes,
  type NativeScriptDecodingScanPlan,
} from "./scan-plan.js";
import {
  nativeScriptDecodingSubmitError,
  requireNativeScriptDecodingThreadUtxo,
} from "./submit-common.js";
import {
  ADVANCE_OR_CLOSE_INDEX,
  advanceStep03Thread,
  BIND_DESCRIPTOR_INDEX,
  requireAnchoredItemBytes,
  requireOpenedState,
  requireStep03State,
  STEP_04_INDEX,
  step03Result,
  type SubmitNativeScriptDecodingStep03Result,
} from "./submit-native-script-decoding-step-03.advance-step03-thread.js";

// ## BindDescriptor

export const submitNativeScriptDecodingStep03BindDescriptor = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  outpointKeyCbor,
  descriptorCbor,
  ledgerTrie,
  plan,
  referenceScriptItemBytes,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly outpointKeyCbor: string;
  readonly descriptorCbor: string;
  readonly ledgerTrie: NativeScriptDecodingLedgerTrieHandle;
  readonly plan?: NativeScriptDecodingScanPlan;
  readonly referenceScriptItemBytes?: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: BIND_DESCRIPTOR_INDEX,
      threadOutRef,
    });
  const state = requireStep03State({
    threadUtxo,
    signer,
    stepIndex: BIND_DESCRIPTOR_INDEX,
  });
  requireOpenedState(state);

  const reopened = await Effect.runPromise(
    nativeScriptDecodingOpenedSubjectState({
      state,
      outpointKeyBytes: outpointKeyCbor,
      outputIndex: state.output_index,
    }),
  );
  if (reopened.outpoint_key_hash !== state.outpoint_key_hash) {
    throw nativeScriptDecodingSubmitError(
      "the supplied outpoint key is not the key committed by OpenSubject.",
    );
  }

  const descriptor: MidgardLedgerOutputCommitment =
    decodeMidgardLedgerOutputCommitment(Buffer.from(descriptorCbor, "hex"));
  if (BigInt(descriptor.outputIndex) !== state.output_index) {
    throw nativeScriptDecodingSubmitError(
      `the descriptor resolves output index ${descriptor.outputIndex.toString()}, but OpenSubject fixed ${state.output_index.toString()}.`,
    );
  }
  if (descriptor.totalLength <= 0) {
    throw nativeScriptDecodingSubmitError(
      "the descriptor commits a non-positive output item length.",
    );
  }
  const ledgerMembershipProof: Proof =
    await buildNativeScriptDecodingLedgerMembership({
      trie: ledgerTrie,
      outpointKey: Buffer.from(outpointKeyCbor, "hex"),
      priorLedgerRootHex: state.prior_ledger_root,
    });
  const bound = nativeScriptDecodingBoundDescriptorState({
    state,
    referenceScriptLanguage: BigInt(descriptor.referenceScriptLanguage),
    referenceScriptTotalLength: BigInt(descriptor.referenceScriptTotalLength),
    referenceScriptItemCommitment:
      descriptor.referenceScriptItemCommitment.toString("hex"),
  });

  let nextState: NativeScriptDecodingScanThreadState;
  let destinationAddress: string;
  let firstChunkProof: BoundedItemChunkProof | null;
  if (descriptor.referenceScriptLanguage === 0) {
    if (plan === undefined || referenceScriptItemBytes === undefined) {
      throw nativeScriptDecodingSubmitError(
        "a tag-0 descriptor needs the scan plan and reference-script item bytes.",
      );
    }
    requireAnchoredItemBytes({
      itemBytes: referenceScriptItemBytes,
      itemIndex: descriptor.outputIndex,
      totalLength: BigInt(descriptor.referenceScriptTotalLength),
      itemCommitmentHex:
        descriptor.referenceScriptItemCommitment.toString("hex"),
    });
    firstChunkProof = buildNativeScriptDecodingChunkProof({
      fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
      itemIndex: descriptor.outputIndex,
      itemBytes: referenceScriptItemBytes,
      chunkIndex: 0,
    });
    if (plan.route === NativeScriptDecodingPlanRoutes.Machine) {
      const bindControl =
        plan.segments[0]?.controlBefore ?? plan.verdict.control;
      if (bindControl === null) {
        throw nativeScriptDecodingSubmitError(
          "the machine-route plan carries no bind control.",
        );
      }
      nextState = { ...bound, machine_state_hash: bindControl.hashHex };
      destinationAddress =
        contracts.steps[ADVANCE_OR_CLOSE_INDEX].spendingScriptAddress;
    } else if (plan.route === NativeScriptDecodingPlanRoutes.BindMalformed) {
      if (
        state.direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE
      ) {
        throw nativeScriptDecodingSubmitError(
          "a malformed wrapper closes only a wrongful-acceptance claim.",
        );
      }
      nextState = {
        ...bound,
        refusal_class: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
      };
      destinationAddress = contracts.steps[STEP_04_INDEX].spendingScriptAddress;
    } else {
      throw nativeScriptDecodingSubmitError(
        "the plan claims a descriptor contradiction for a tag-0 descriptor.",
      );
    }
  } else {
    if (
      state.direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION
    ) {
      throw nativeScriptDecodingSubmitError(
        "a non-tag-0 descriptor closes only a wrongful-rejection contradiction.",
      );
    }
    firstChunkProof = null;
    nextState = {
      ...bound,
      refusal_class: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED,
    };
    destinationAddress = contracts.steps[STEP_04_INDEX].spendingScriptAddress;
  }

  const proof = firstChunkProof;
  const { txHash, layout } = await advanceStep03Thread({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    destinationAddress,
    nextState,
    spendingStepIndex: BIND_DESCRIPTOR_INDEX,
    buildRedeemer: (resolved) =>
      Data.to(
        {
          Continue: [
            {
              input_index: resolved.inputIndex,
              output_index: resolved.outputIndex,
              outpoint_key_cbor: outpointKeyCbor,
              descriptor_cbor: descriptorCbor,
              ledger_membership_proof: ledgerMembershipProof,
              first_chunk_proof: proof,
            },
          ],
        },
        NativeScriptDecodingStep03BindDescriptorSpendRedeemer,
      ),
    carriageUtxos: [],
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
