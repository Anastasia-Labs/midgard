import { type Emulator, type LucidEvolution } from "@lucid-evolution/lucid";

import {
  submitValidationDisputeAward,
  submitValidationDisputeSemanticResolution,
  validationDisputeValidityRange,
  type ValidationProofItemDelivery,
} from "../../src/index.js";
import {
  type Blueprint,
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  readBlueprint,
  realBlueprintPath,
} from "./submit-init-emulator-shared.js";

const ITEM_SEMANTIC_SPEND_TITLE =
  "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend";

/**
 * Whether the blueprint under test compiles the Option B complete-item wire.
 *
 * The discriminator is structural, not a hash pin: #620 removed the carriage
 * parameter from `canonical_decode_item_semantic_v1`, taking its declared
 * parameter list from three entries to two. A blueprint still declaring three
 * is the deployed pre-Option-B build, against which every journey in this
 * harness reds out at prepare-selected exactly like the two recorded
 * expected-red rows — so suites refuse it up front rather than manufacture an
 * unfalsifiable failure deep inside a journey.
 */
export const blueprintSpeaksOptionBCompleteItemWire = (
  blueprint: Blueprint,
): boolean => {
  const itemSemantic = blueprint.validators.find(
    (validator) => validator.title === ITEM_SEMANTIC_SPEND_TITLE,
  );
  if (itemSemantic === undefined) {
    throw new Error(
      `blueprint has no "${ITEM_SEMANTIC_SPEND_TITLE}" validator to probe`,
    );
  }
  return (itemSemantic.parameters ?? []).length === 2;
};

/**
 * The precondition every suite on this harness asserts at collection time.
 * Fails closed (test-quality rule 14): Option B is the shipped complete-item
 * wire, so a blueprint that still declares the retired carriage parameter is a
 * broken precondition, not a reason to report a silent pass or a skip.
 */
export const assertRealBlueprintSpeaksOptionBV1 = (): void => {
  if (
    !blueprintSpeaksOptionBCompleteItemWire(readBlueprint(realBlueprintPath))
  ) {
    throw new Error(
      "the blueprint at MIDGARD_REAL_BLUEPRINT_PATH (or " +
        "onchain/aiken/plutus.json) predates Option B (#621): " +
        "canonical_decode_item_semantic_v1 still declares the retired " +
        "carriage parameter, so every route-freedom journey would red out at " +
        "prepare-selected with `Spend[0] unexpected empty list` instead of " +
        "testing anything. Rebuild the blueprint with the pinned Aiken fork.",
    );
  }
};

export type CapturedSemanticSubmission = Awaited<
  ReturnType<typeof captureEmulatorSubmission<SemanticResolutionResult>>
>;

type SemanticResolutionResult = Awaited<
  ReturnType<typeof submitValidationDisputeSemanticResolution>
>;

/**
 * One staged lifecycle stage's captured submissions (#622): the stage label
 * the journey already logs, plus every transaction the stage submitted, in
 * submission order, measured by `captureEmulatorSubmission`.
 */
export type CapturedLifecycleStage = {
  readonly label: string;
  readonly measurements: readonly CompleteSignedTransactionMeasurement[];
};

export type RouteFreedomJourney = {
  readonly emulator: Emulator;
  readonly realBlueprint: Blueprint;
  readonly challengerLucid: LucidEvolution;
  readonly stagedThreadOutRef: string;
  readonly validityRange: () => ReturnType<
    typeof validationDisputeValidityRange
  >;
  /** The measured §5.1 complete-item byte length the fixture staged. */
  readonly completeItemBytes: number;
  /**
   * Per-stage measurements for everything staged before the semantic leg
   * (#622's measurement campaign): setup, the reference-script publications,
   * init, open, source, every bisection reveal, enter-resolution,
   * prepare-resolution, and prepare-selected — labels as logged, one entry
   * per staged call, in staging order. The semantic-resolution and award
   * legs are captured by their own submit functions.
   */
  readonly lifecycleMeasurements: readonly CapturedLifecycleStage[];
  /**
   * One semantic-resolution attempt against the staged thread, with this
   * call's routing inputs. A refusal leaves the thread untouched, so failed
   * attempts and the eventual green run all target the same
   * {@link stagedThreadOutRef}.
   */
  readonly submitSemanticResolution: (routing?: {
    readonly proofItemDelivery?: ValidationProofItemDelivery;
    readonly proofItemReferenceOutRef?: string;
  }) => Promise<CapturedSemanticSubmission>;
  readonly submitAward: (
    threadOutRef: string,
  ) => Promise<
    Awaited<
      ReturnType<
        typeof captureEmulatorSubmission<
          Awaited<ReturnType<typeof submitValidationDisputeAward>>
        >
      >
    >
  >;
  /**
   * Asserts the staged thread UTxO is still live — the pin that a refused
   * attempt submitted nothing and spent nothing.
   */
  readonly expectStagedThreadUnspent: () => Promise<void>;
  /** An out-ref this journey genuinely created and then spent on chain. */
  readonly spentOutRef: string;
};

/**
 * #622: one-line JSON dump of a campaign journey's complete measured table —
 * every staged lifecycle transaction plus the semantic leg, bytes, pre-sign
 * projections, and execution units — gated on MIDGARD_PRINT_PROOF_FIT=1 like
 * every other proof-fit print. The measurement-campaign suites call this
 * BEFORE their pins, so one red run still surrenders every number; the
 * committed pins were read off exactly this print, and re-measuring after a
 * shape change is one env var away.
 */
export const printRouteFreedomCampaignTable = (
  headline: string,
  journey: RouteFreedomJourney,
  semantic: CapturedSemanticSubmission,
): void => {
  if (process.env["MIDGARD_PRINT_PROOF_FIT"] !== "1") {
    return;
  }
  const measurementRow = (
    measurement: CompleteSignedTransactionMeasurement,
  ) => ({
    bytes: measurement.completeSignedBytes,
    mem: measurement.executionMemory.toString(),
    cpu: measurement.executionSteps.toString(),
    refInputs: measurement.referenceInputCount,
  });
  const table = {
    completeItemBytes: journey.completeItemBytes,
    lifecycle: journey.lifecycleMeasurements.map((stage) => ({
      label: stage.label,
      transactions: stage.measurements.map(measurementRow),
    })),
    stages: (semantic.result.stageTransactions ?? []).map((stage) => ({
      kind: stage.kind,
      bytes: stage.completeSignedBytes,
      projected: stage.projectedSignedBytes,
    })),
    transactions: semantic.measurements.map(measurementRow),
    refusal:
      semantic.result.proofItemInlineEnvelopeRefusal === undefined
        ? undefined
        : {
            projectedSignedBytes:
              semantic.result.proofItemInlineEnvelopeRefusal
                .projectedSignedBytes,
            maxTransactionBytes:
              semantic.result.proofItemInlineEnvelopeRefusal
                .maxTransactionBytes,
          },
  };
  console.log(`${headline} ${JSON.stringify(table)}`);
};
