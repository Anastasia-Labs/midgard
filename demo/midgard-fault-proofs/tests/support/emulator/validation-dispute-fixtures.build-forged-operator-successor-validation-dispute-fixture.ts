import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardValidationPhaseName,
} from "@al-ft/midgard-core";
import {
  decodeCekContextCborArray,
  validationTraceDescriptorDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildValidationDisputeEvidenceBundle,
  type DeterministicValidationMachineTrace,
  encodeValidationAuxiliaryWitnessCbor,
  validationSemanticResolverIndex,
  valueAndMintKind,
  type ValueAndMintStepKind,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import { redeemerItemExecutor } from "../../../src/redeemer-item-plan.js";
import { scriptSourcesMiddleYieldIndex } from "../../../src/validation-dispute/script-sources-yields.js";
import { forgeCekCoreSuccessor } from "./cek-builtin-failure-forgery.js";
import { type ForcedValidationDisputeFixture } from "./validation-dispute-fixtures.build-accepted-claim-over-rejecting-transaction-fixture.js";
import { buildForcedValidationDisputeCommitments } from "./validation-dispute-fixtures.build-forced-validation-dispute-commitments.js";
import { buildNativeTransactionTrace } from "./validation-dispute-fixtures.build-native-transaction-trace.js";
import {
  forgeLedgerOutputProofSuccessor,
  type LedgerOutputProofSuccessorForgery,
} from "./validation-dispute-fixtures.forge-ledger-output-proof-successor.js";
import { forgeRedeemerFoldTrace } from "./validation-dispute-fixtures.forge-redeemer-select.js";
import { withMaximumValueAssetProof } from "./value-asset-maximum.js";

/**
 * R5 item 1 (#617) journey fixture for the cek and ValueAndMint prepare +
 * semantic decomposition. The transaction is the honest, valid, signed
 * native transaction above and the challenger's trace is its honest
 * accepted trace; the operator commits the same trace up to the first state
 * of `disputedPhase` and a forged successor from there on (the honest
 * `Accepted` terminal with a fabricated work root, repeated to the honest
 * length so the descriptor and every midpoint after the boundary disagree,
 * which pins the bisection at exactly that boundary). The one-step the challenger then
 * proves on L1 is the honest trace's first step of that phase:
 *
 * - `cek`: this pure key-witness spend has `execution_count == 0` (no script
 *   execution of any language), so the cek phase is the single stand-alone
 *   ValueAndMint hand-off (`cek_v1` prepare,
 *   then `cek_finish_semantic_v1`, resolver 11 / semantic 0);
 * - `valueAndMint`: the stage-0 `begin` step (`value_and_mint_v1` prepare,
 *   then `value_and_mint_begin_semantic_v1`, resolver 12 / semantic 0).
 */
export const buildForgedOperatorSuccessorValidationDisputeFixture = async ({
  operatorVkey,
  now,
  addressWitnessCount,
  disputedPhase,
  disputedValueKind,
  cekSelection = false,
  cekCoreArm,
  cekContextStage,
  cekContextMintCursor,
  cekContextItemAction,
  cekObserverCount,
  plutusSelection = false,
  cekPlutusMint = false,
  cekRedeemerSelectOrdinal,
  cekRedeemerSelectDonorOrdinal,
  cekRedeemerSkipOrdinal,
  cekRedeemerSkipForgery = false,
  cekRedeemerSelectLengthDelta,
  cekProgramLambdaCount = 1,
  cekDataGraph = false,
  redeemerDataCbor,
  cekDirectBuiltin = false,
  cekBlsFinal = false,
  cekMaximumDirect = false,
  cekSemanticTag,
  cekBuiltinFailureTag,
  cekBuiltinFailureBudgetForgery,
  cekDirectMapConversion = false,
  assetCount = 0,
  dishonestChallenger = false,
  ledgerOutputProofForgery,
  maximumAssetProof = false,
  lateNativeItem = false,
  nativeItemWidth = 0,
  observerCount = 0,
  preconditionsRejection,
  rejectAfterPreconditions = false,
  preconditionsItemIndex,
  resolveInputsKind,
  scriptSourcesSemanticIndex,
  scriptSourcesItemExecutor,
  scriptSourcesMiddleKind,
  scriptSourcesDescriptorAction,
  scriptSourcesRejection,
  descriptorMaximum = false,
  scriptSourcesItemIndex,
  disputedMatchOrdinal,
  worstCaseWitness = false,
  ledgerOutputValueOpening = false,
  ledgerOutputDatumAction,
  permutationWitnessMutation,
  outputDatumCbor,
  outputLovelace,
  prepareFieldCarriage,
}: {
  readonly operatorVkey: string;
  readonly addressWitnessCount?: number;
  readonly now: number;
  readonly disputedPhase: Exclude<MidgardValidationPhaseName, "terminal">;
  readonly disputedValueKind?: ValueAndMintStepKind;
  readonly cekSelection?: boolean;
  readonly cekCoreArm?: string;
  readonly cekContextStage?: number;
  readonly cekContextMintCursor?: number;
  readonly cekContextItemAction?: string;
  readonly cekObserverCount?: number;
  readonly plutusSelection?: boolean;
  /** See {@link buildNativeTransactionTrace}. */
  readonly cekPlutusMint?: boolean;
  /**
   * Dispute the nth (0-based) CEK context redeemer select step of the
   * honest trace, counted across every execution.
   */
  readonly cekRedeemerSelectOrdinal?: number;
  /**
   * Forge the challenger's disputed redeemer select: from the honest low
   * state it selects what the honest select step at this ordinal selected,
   * and its successor is exactly the one that selection yields. Every
   * membership proof is genuine, so only the execution leaf at the purpose
   * bound can refuse it. The operator's claim is the honest trace.
   */
  readonly cekRedeemerSelectDonorOrdinal?: number;
  /**
   * Dispute the nth (0-based) CEK context redeemer skip step of the honest
   * trace, counted across every execution.
   */
  readonly cekRedeemerSkipOrdinal?: number;
  /**
   * Forge the challenger's disputed select as a skip of the same purpose.
   * The operator's claim is the honest trace.
   */
  readonly cekRedeemerSkipForgery?: boolean;
  /** See {@link forgeRedeemerFoldTrace}. */
  readonly cekRedeemerSelectLengthDelta?: number;
  readonly cekProgramLambdaCount?: number;
  readonly cekDataGraph?: boolean;
  readonly redeemerDataCbor?: Uint8Array;
  readonly cekDirectBuiltin?: boolean;
  readonly cekBlsFinal?: boolean;
  readonly cekMaximumDirect?: boolean;
  readonly cekSemanticTag?: number;
  readonly cekBuiltinFailureTag?: 12 | 21 | 52 | 82 | 83;
  readonly cekBuiltinFailureBudgetForgery?: "cpu" | "memory";
  /** With `dishonestChallenger`: see {@link forgeDirectMapConversionSuccessor}. */
  readonly cekDirectMapConversion?: boolean;
  readonly assetCount?: number;
  readonly dishonestChallenger?: boolean;
  /** With `dishonestChallenger`: see {@link LedgerOutputProofSuccessorForgery}. */
  readonly ledgerOutputProofForgery?: LedgerOutputProofSuccessorForgery;
  readonly maximumAssetProof?: boolean;
  readonly lateNativeItem?: boolean;
  readonly nativeItemWidth?: number;
  readonly observerCount?: number;
  readonly rejectAfterPreconditions?: boolean;
  readonly preconditionsRejection?:
    | "missingIntegrity"
    | "untaggedObservers"
    | "observerOrder";
  readonly preconditionsItemIndex?: number;
  readonly resolveInputsKind?:
    | "initial"
    | "finish"
    | "membershipBegin"
    | "membershipStep"
    | "membershipFinalize"
    | "nonMembership";
  readonly scriptSourcesSemanticIndex?: number;
  readonly scriptSourcesItemExecutor?: number;
  readonly scriptSourcesMiddleKind?: number;
  readonly descriptorMaximum?: boolean;
  readonly scriptSourcesRejection?:
    | "missingRedeemer"
    | "missingObserver"
    | "missingReceive"
    | "unusedRedeemer";
  readonly scriptSourcesItemIndex?: number;
  /**
   * Selects the nth (0-based) state matching every other selector as the
   * disputed low state, instead of the first. Lets a battery reach later
   * occurrences of a repeating step kind — e.g. a ledger-output-proof step
   * whose stage role carries attestation yields.
   */
  readonly disputedMatchOrdinal?: number;
  /**
   * Adjudicate the most expensive matching step instead of the first: the
   * matching state whose encoded auxiliary witness is largest. A maximum-shape
   * claim has to be measured at the worst step the honest trace reaches, not
   * merely at the first one. Mutually exclusive with
   * {@link disputedMatchOrdinal}, which selects by position instead.
   */
  readonly worstCaseWitness?: boolean;
  /**
   * Narrow the disputed step to a ledger-output-proof value step that carries
   * a previous map-head opening — the permutation half of the mixed-width
   * output value fold (`ledger_output_value_v1.asset_step`). Combine with
   * `assetCount: 1304` so the native canonical key order and the lexical
   * execution order are genuinely different permutations.
   */
  readonly ledgerOutputValueOpening?: boolean;
  /**
   * Narrow the disputed step to a ledger-output-proof datum step taking this
   * traversal action (combine with {@link disputedMatchOrdinal} to reach a
   * later one, e.g. a nested head).
   */
  readonly ledgerOutputDatumAction?:
    | "headScalar"
    | "headSequence"
    | "headMap"
    | "headLargeConstructor";
  /**
   * Forge the disputed step's permutation witness (a mixed-width mint context
   * item or a ledger-output value step) in the challenger's own trace, so the
   * refusal reaches the on-chain membership / head-opening clause rather than
   * any local builder gate: `foreignIndex` claims another frontier slot,
   * `forgedHead` misquotes the previous head's quantity, and `omittedHead`
   * drops a required previous-head opening.
   */
  readonly permutationWitnessMutation?:
    | "foreignIndex"
    | "forgedHead"
    | "omittedHead";
  /** Inline datum for the forced transaction's produced output; see
   * {@link buildNativeTransactionTrace}. */
  readonly outputDatumCbor?: Buffer;
  readonly outputLovelace?: bigint;
  readonly scriptSourcesDescriptorAction?: "begin" | "header" | "tail";
  readonly prepareFieldCarriage?: (input: {
    trace: DeterministicValidationMachineTrace;
    stateIndex: number;
    source: {
      compact_cbor: string;
      witness_set_compact_cbor: string;
      field_preimage_lengths_cbor: string;
    };
  }) => Promise<
    Parameters<
      typeof buildValidationDisputeEvidenceBundle
    >[0]["resolveFieldCarriage"]
  >;
}): Promise<
  ForcedValidationDisputeFixture & {
    readonly disputedPhase: Exclude<MidgardValidationPhaseName, "terminal">;
    readonly disputedLowIndex: number;
  }
> => {
  const {
    txOrderId,
    eventKey,
    forcedTransaction,
    honestTrace: originalTrace,
    preUtxosRoot,
    postUtxosRoot,
  } = await buildNativeTransactionTrace({
    now,
    addressWitnessCount,
    txOrderSeed: disputedPhase === "cek" ? "e4" : "e5",
    assetCount:
      cekSelection ||
      cekPlutusMint ||
      disputedPhase === "phaseANativeScripts" ||
      (scriptSourcesMiddleKind !== undefined && scriptSourcesMiddleKind >= 5)
        ? Math.max(1, assetCount)
        : assetCount,
    mintAsset:
      cekSelection ||
      disputedValueKind === "mintAsset" ||
      (disputedPhase === "phaseANativeScripts" && !plutusSelection) ||
      (scriptSourcesMiddleKind !== undefined && scriptSourcesMiddleKind >= 5),
    plutusSelection:
      plutusSelection ||
      preconditionsRejection === "missingIntegrity" ||
      preconditionsRejection === "untaggedObservers",
    cekPlutusMint,
    cekProgramLambdaCount,
    cekDataGraph,
    redeemerDataCbor,
    nativeItemWidth,
    cekDirectBuiltin,
    cekBlsFinal,
    cekMaximumDirect,
    cekSemanticTag,
    cekBuiltinFailureTag,
    observerCount,
    preconditionsRejection,
    rejectAfterPreconditions,
    resolveMissingInput: resolveInputsKind === "nonMembership",
    scriptSourcesRejection,
    descriptorMaximum,
    cekObserverCount,
    ...(outputDatumCbor === undefined ? {} : { outputDatumCbor }),
    ...(outputLovelace === undefined ? {} : { outputLovelace }),
  });
  let challengerTrace = originalTrace;
  let disputedMatchesSeen = 0;
  const redeemerSelectIndices = originalTrace.witnesses.flatMap(
    (witness, index) =>
      witness.auxiliary?.kind === "cekRedeemerContextSelect" ? [index] : [],
  );
  const redeemerSkipIndices = originalTrace.witnesses.flatMap(
    (witness, index) =>
      witness.auxiliary?.kind === "cekRedeemerContextSkip" ? [index] : [],
  );
  if (worstCaseWitness && disputedMatchOrdinal !== undefined) {
    throw new Error(
      "worstCaseWitness and disputedMatchOrdinal select the disputed step by incompatible rules",
    );
  }
  const isDisputedState = (
    state: (typeof challengerTrace.states)[number],
    index: number,
  ): boolean => {
    const auxiliary = challengerTrace.witnesses[index]?.auxiliary;
    const contextStage = (() => {
      if (cekContextStage === undefined || state.phase !== "cek")
        return undefined;
      const work = decodeCekContextCborArray(
        Buffer.from(challengerTrace.witnesses[index]!.cbor).toString("hex"),
        9,
      );
      if (!Array.isArray(work) || typeof work[1] !== "string" || work[1] === "")
        return undefined;
      const context = decodeCekContextCborArray(work[1], 25);
      return context;
    })();
    return (
      state.phase === disputedPhase &&
      (scriptSourcesMiddleKind === undefined ||
        (validationSemanticResolverIndex(challengerTrace.witnesses[index]!) ===
          0 &&
          scriptSourcesMiddleYieldIndex(
            challengerTrace.witnesses[index]!.cbor.toString("hex"),
            Data.from(
              encodeValidationAuxiliaryWitnessCbor(
                challengerTrace.witnesses[index]!.auxiliary,
              ).toString("hex"),
            ),
          ) === scriptSourcesMiddleKind)) &&
      (scriptSourcesItemIndex === undefined ||
        (() => {
          const auxiliary = challengerTrace.witnesses[index]!.auxiliary;
          return (
            auxiliary?.kind === "transactionFieldChunk" &&
            auxiliary.itemIndex === scriptSourcesItemIndex
          );
        })()) &&
      (scriptSourcesDescriptorAction === undefined ||
        (() => {
          const auxiliary = challengerTrace.witnesses[index]!.auxiliary;
          if (scriptSourcesDescriptorAction === "begin")
            return auxiliary?.kind === "redeemerScanBegin";
          return (
            auxiliary?.kind === "redeemerItemStep" &&
            auxiliary.witness.action.kind ===
              (scriptSourcesDescriptorAction === "header"
                ? "openHeader"
                : "openTail")
          );
        })()) &&
      (scriptSourcesSemanticIndex === undefined ||
        validationSemanticResolverIndex(challengerTrace.witnesses[index]!) ===
          scriptSourcesSemanticIndex) &&
      (scriptSourcesItemExecutor === undefined ||
        (() => {
          const auxiliary = challengerTrace.witnesses[index]!.auxiliary;
          return (
            auxiliary?.kind === "redeemerItemStep" &&
            redeemerItemExecutor(auxiliary.control, auxiliary.witness).index ===
              scriptSourcesItemExecutor
          );
        })()) &&
      (resolveInputsKind === undefined ||
        (() => {
          const auxiliary = challengerTrace.witnesses[index]!.auxiliary;
          switch (resolveInputsKind) {
            case "initial":
              return (
                auxiliary === null &&
                challengerTrace.states[index - 1]?.phase !== "resolveInputs"
              );
            case "finish":
              return (
                auxiliary === null &&
                challengerTrace.states[index - 1]?.phase === "resolveInputs"
              );
            case "membershipBegin":
              return (
                auxiliary?.kind === "scheduledLedgerLookup" &&
                auxiliary.value !== null
              );
            case "nonMembership":
              return (
                auxiliary?.kind === "scheduledLedgerLookup" &&
                auxiliary.value === null
              );
            case "membershipStep":
              return auxiliary?.kind === "ledgerOutputProofStep";
            case "membershipFinalize":
              return auxiliary?.kind === "ledgerOutputProofFinalize";
          }
        })()) &&
      (!ledgerOutputValueOpening ||
        (() => {
          const auxiliary = challengerTrace.witnesses[index]!.auxiliary;
          return (
            auxiliary?.kind === "ledgerOutputProofStep" &&
            auxiliary.witness?.kind === "value" &&
            auxiliary.witness.previous !== null
          );
        })()) &&
      (ledgerOutputDatumAction === undefined ||
        (() => {
          const auxiliary = challengerTrace.witnesses[index]!.auxiliary;
          return (
            auxiliary?.kind === "ledgerOutputProofStep" &&
            auxiliary.witness?.kind === "datum" &&
            auxiliary.witness.action?.kind === ledgerOutputDatumAction
          );
        })()) &&
      (disputedPhase !== "phaseAScriptPreconditions" ||
        (preconditionsItemIndex === undefined
          ? challengerTrace.witnesses[index]!.auxiliary === null
          : challengerTrace.witnesses[index]!.auxiliary?.kind ===
              "transactionFieldChunk" &&
            challengerTrace.witnesses[index]!.auxiliary.itemIndex ===
              preconditionsItemIndex)) &&
      (!lateNativeItem ||
        challengerTrace.states[index - 1]?.phase === "nativeScripts") &&
      (cekContextStage === undefined ||
        contextStage?.[0] === BigInt(cekContextStage)) &&
      (cekContextMintCursor === undefined ||
        contextStage?.[20] === BigInt(cekContextMintCursor)) &&
      (cekContextItemAction === undefined ||
        (auxiliary?.kind === "redeemerItemStep" &&
          auxiliary.witness.action.kind === cekContextItemAction)) &&
      (cekRedeemerSelectOrdinal === undefined ||
        index === redeemerSelectIndices[cekRedeemerSelectOrdinal]) &&
      (cekRedeemerSkipOrdinal === undefined ||
        index === redeemerSkipIndices[cekRedeemerSkipOrdinal]) &&
      (cekCoreArm === undefined ||
        (auxiliary?.kind === "cekCoreStep" &&
          auxiliary.step.witness.kind === cekCoreArm)) &&
      (cekSemanticTag === undefined ||
        (auxiliary?.kind === "cekCoreStep" &&
          ("tag" in auxiliary.step.witness
            ? auxiliary.step.witness.tag === BigInt(cekSemanticTag)
            : cekCoreArm !== undefined))) &&
      (disputedValueKind === undefined ||
        valueAndMintKind(challengerTrace.witnesses[index]!) ===
          disputedValueKind) &&
      (disputedMatchOrdinal === undefined ||
        disputedMatchesSeen++ === disputedMatchOrdinal)
    );
  };
  // Adjudicate the largest matching auxiliary when a maximum is requested.
  const disputedLowIndex = worstCaseWitness
    ? challengerTrace.states.reduce(
        (best, state, index) => {
          if (!isDisputedState(state, index)) return best;
          const cost = encodeValidationAuxiliaryWitnessCbor(
            challengerTrace.witnesses[index]!.auxiliary,
          ).length;
          return cost > best.cost ? { index, cost } : best;
        },
        { index: -1, cost: -1 },
      ).index
    : challengerTrace.states.findIndex(isDisputedState);
  if (disputedLowIndex < 0) {
    throw new Error(
      `honest accepted validation trace is missing its ${disputedPhase} phase`,
    );
  }
  if (maximumAssetProof)
    challengerTrace = withMaximumValueAssetProof(
      challengerTrace,
      disputedLowIndex,
    );
  if (permutationWitnessMutation !== undefined) {
    // Forge the challenger's own permutation witness for the disputed step.
    // The adjudicated one-step argument reads the auxiliary of
    // witnesses[disputedLowIndex] (the witness of the transition FROM the
    // disputed low state), and the auxiliary is not part of any work root,
    // so only the on-chain membership / head-opening clause of the stage-8
    // mint item or the output value fold can notice the exchange - every
    // local builder gate still passes on the honest work witnesses.
    const adjacent = challengerTrace.witnesses[disputedLowIndex]!;
    const auxiliary = adjacent.auxiliary;
    const requireHead = <T>(head: T | null): T => {
      if (head === null)
        throw new Error(
          "disputed permutation step carries no previous-head opening to mutate",
        );
      return head;
    };
    const mutatedAuxiliary = (() => {
      if (auxiliary?.kind === "cekMintContextItem") {
        switch (permutationWitnessMutation) {
          case "foreignIndex":
            return {
              ...auxiliary,
              mintIndex:
                auxiliary.mintIndex === 0
                  ? auxiliary.mintIndex + 1
                  : auxiliary.mintIndex - 1,
            };
          case "forgedHead": {
            const previous = requireHead(auxiliary.previous);
            return {
              ...auxiliary,
              previous: { ...previous, quantity: previous.quantity + 1n },
            };
          }
          case "omittedHead":
            requireHead(auxiliary.previous);
            return { ...auxiliary, previous: null };
        }
      }
      if (
        auxiliary?.kind === "ledgerOutputProofStep" &&
        auxiliary.witness?.kind === "value"
      ) {
        const witness = auxiliary.witness;
        switch (permutationWitnessMutation) {
          case "foreignIndex":
            return {
              ...auxiliary,
              witness: {
                ...witness,
                assetIndex:
                  witness.assetIndex === 0
                    ? witness.assetIndex + 1
                    : witness.assetIndex - 1,
              },
            };
          case "forgedHead": {
            const previous = requireHead(witness.previous);
            return {
              ...auxiliary,
              witness: {
                ...witness,
                previous: { ...previous, quantity: previous.quantity + 1n },
              },
            };
          }
          case "omittedHead":
            requireHead(witness.previous);
            return { ...auxiliary, witness: { ...witness, previous: null } };
        }
      }
      throw new Error("disputed step carries no permutation witness to mutate");
    })();
    const witnesses = [...challengerTrace.witnesses];
    witnesses[disputedLowIndex] = { ...adjacent, auxiliary: mutatedAuxiliary };
    challengerTrace = { ...challengerTrace, witnesses };
  }
  const redeemerSelectForgery = forgeRedeemerFoldTrace({
    trace: challengerTrace,
    disputedLowIndex,
    skipForgery: cekRedeemerSkipForgery,
    lengthDelta: cekRedeemerSelectLengthDelta,
    donorOrdinal: cekRedeemerSelectDonorOrdinal,
    selectIndices: redeemerSelectIndices,
  });
  const honestTerminal = challengerTrace.states.at(-1)!;
  if (honestTerminal.phase !== "terminal") {
    throw new Error(
      "honest accepted validation trace does not end in a terminal state",
    );
  }
  // The honest terminal with only its work root fabricated: every endpoint
  // check the source validator applies to the operator's claim (terminal
  // phase, program counter == step count, verdict, rejection code, immutable
  // context, ledger delta root) still holds, so the dispute opens and the
  // bisection -- not the source stage -- is what exposes the forgery.
  const forgedTerminal = {
    ...honestTerminal,
    verdict: "accepted" as const,
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    workRoot: Buffer.alloc(32, 0x7e),
  };
  const operatorStates = challengerTrace.states.map((state, index) =>
    index <= disputedLowIndex
      ? state
      : dishonestChallenger && scriptSourcesRejection === undefined
        ? { ...state, workRoot: Buffer.alloc(32, 0x7e) }
        : forgedTerminal,
  );
  const operatorWitnesses = [...challengerTrace.witnesses];
  forgeCekCoreSuccessor({
    challengerTrace,
    disputedLowIndex,
    operatorStates,
    operatorWitnesses,
    dishonestChallenger,
    cekBuiltinFailureBudgetForgery,
    cekDirectMapConversion,
    cekContextStage,
  });
  if (
    dishonestChallenger &&
    (ledgerOutputProofForgery !== undefined ||
      resolveInputsKind === "membershipStep" ||
      (disputedPhase === "scriptSources" && scriptSourcesSemanticIndex === 2))
  ) {
    // Supply the adjacent work witness so refusal reaches the on-chain step.
    forgeLedgerOutputProofSuccessor({
      trace: challengerTrace,
      disputedLowIndex,
      pendingInputCarrier: disputedPhase === "resolveInputs",
      forgery: ledgerOutputProofForgery ?? "versionFlip",
      operatorStates,
      operatorWitnesses,
    });
  }
  const operatorTrace: DeterministicValidationMachineTrace = {
    ...challengerTrace,
    verdict: "accepted",
    rejectionCode: null,
    states: operatorStates,
    witnesses: operatorWitnesses,
    tree: buildMidgardValidationTraceTree(
      operatorStates.map(hashMidgardValidationMachineState),
      "accepted",
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    ),
  };
  // Invalid accepted forced sources cannot have an honest accepted operator
  // trace. For these semantic refusal cases, keep that operator claim and
  // forge only the challenger's successor, preserving its rejected endpoint.
  const rejectionForgeryStates = challengerTrace.states.map((state, index) =>
    index <= disputedLowIndex
      ? state
      : { ...state, workRoot: Buffer.alloc(32, 0x7d) },
  );
  const rejectionForgery = {
    ...challengerTrace,
    states: rejectionForgeryStates,
    tree: buildMidgardValidationTraceTree(
      rejectionForgeryStates.map(hashMidgardValidationMachineState),
      challengerTrace.verdict,
      honestTerminal.rejectionCodeHash,
    ),
  };
  const claimedOperatorTrace =
    (dishonestChallenger && scriptSourcesRejection === undefined) ||
    redeemerSelectForgery !== undefined
      ? challengerTrace
      : operatorTrace;
  const claimedChallengerTrace =
    redeemerSelectForgery ??
    (dishonestChallenger
      ? scriptSourcesRejection === undefined
        ? operatorTrace
        : rejectionForgery
      : challengerTrace);
  const resolveFieldCarriage = await prepareFieldCarriage?.({
    trace: claimedChallengerTrace,
    stateIndex: disputedLowIndex,
    source: forcedTransaction.submitted_source,
  });
  const evidence = buildValidationDisputeEvidenceBundle({
    ...(resolveFieldCarriage === undefined ? {} : { resolveFieldCarriage }),
    operatorTrace: claimedOperatorTrace,
    challengerTrace: claimedChallengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction:
      cekBuiltinFailureTag === undefined
        ? forcedTransaction
        : {
            ...forcedTransaction,
            verdict:
              claimedOperatorTrace.verdict === "accepted"
                ? "ForcedTxValid"
                : {
                    ForcedTxInvalid: {
                      reason: {
                        PlutusExecutionFailed: { execution_index: 0n },
                      },
                    },
                  },
          },
    operatorTrace: claimedOperatorTrace,
    preUtxosRoot,
    postUtxosRoot,
  });
  return {
    header,
    claim,
    operatorTrace: claimedOperatorTrace,
    challengerTrace: claimedChallengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      claimedChallengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot: operatorTrace.states[0]!.ledgerDeltaRoot,
    disputedPhase,
    disputedLowIndex,
  };
};
