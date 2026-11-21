import { decodeMidgardVersionedScript } from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  type NativeTxWitnessSetCompact,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import {
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  NativeScriptInvalidStep02DatumSchema,
  NativeScriptInvalidStep02SpendRedeemerSchema,
  NativeScriptInvalidStep03DatumSchema,
  nativeScriptItemCommitment,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
} from "@al-ft/midgard-sdk/fraud-proof";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type RedeemerContext,
  type UTxO,
} from "@lucid-evolution/lucid";

import { isPrerequisiteTurn } from "../fault-proof-turn.js";
import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import {
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.publish-fault-proof-field-carriage.js";
import {
  linearFaultStepLabel,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  advanceFieldGrammarCheckpoint,
  encodeFieldGrammarCheckpoint,
  encodeFieldSemanticCheckpoint,
  fieldGrammarCheckpointIsComplete,
  hashFieldGrammarCheckpoint,
  hashFieldSemanticCheckpoint,
  initialFieldGrammarCheckpoint,
} from "../staged-field-walk/grammar-checkpoint.js";
import {
  advanceFieldSemanticCheckpoint,
  initialFieldSemanticCheckpoint,
  resolveFieldGrammarCheckpoint,
  resolveFieldSemanticCheckpoint,
} from "../staged-field-walk/semantic-checkpoint.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import {
  NATIVE_SCRIPT_INVALID_CATEGORY_LABEL as FAMILY,
  type NativeScriptInvalidContracts,
} from "./contracts.js";

/**
 * The step 03 `script_item_hash` of a script position field 6 does not have
 * (`rule.absent_script_item_hash`): a selected script's hash is 32 bytes.
 */
export const NATIVE_SCRIPT_INVALID_ABSENT_SCRIPT_ITEM_HASH = "";

type Step02State = NonNullable<
  Data.Static<typeof NativeScriptInvalidStep02DatumSchema>["data"]
>;
type Step02Datum = Data.Static<typeof NativeScriptInvalidStep02DatumSchema>;
const Step02Datum = asDataType<Step02Datum>(
  NativeScriptInvalidStep02DatumSchema,
);
type Step03Datum = Data.Static<typeof NativeScriptInvalidStep03DatumSchema>;
const Step03Datum = asDataType<Step03Datum>(
  NativeScriptInvalidStep03DatumSchema,
);
type Step02Redeemer = Data.Static<
  typeof NativeScriptInvalidStep02SpendRedeemerSchema
>;
const Step02Redeemer = asDataType<Step02Redeemer>(
  NativeScriptInvalidStep02SpendRedeemerSchema,
);

export const buildNativeScriptInvalidStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptWitnessItems,
  scriptIndex,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  ownOutputs = createOwnOutputJournal(),
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly scriptWitnessItems: readonly Uint8Array[];
  readonly scriptIndex: bigint;
  readonly publishCarriage?: boolean;
  /** Pre-observed publications supplied by a production workflow action. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
  /** Pre-observed tier-3 certificate supplied by its mint action. */
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}) => {
  const stepIndex = 1;
  const label = linearFaultStepLabel(FAMILY, stepIndex);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<Step02State>({
    threadUtxo,
    signer,
    schema: Step02Datum,
    family: FAMILY,
    stepIndex,
  });
  // A rejection naming a script position field 6 does not have has no script
  // to evaluate: a negative position closes at once, and one past the field's
  // last witness is closed by the field's count.
  const negative = scriptIndex < 0n;
  const item = negative ? undefined : scriptWitnessItems[Number(scriptIndex)];
  const absent =
    item === undefined &&
    state.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION;
  if (item === undefined && !absent) {
    throw new Error(`${label}: script index is outside field 6`);
  }
  if (
    item !== undefined &&
    decodeMidgardVersionedScript(item).language !== "NativeCardano"
  ) {
    throw new Error(`${label}: selected witness is not a native script`);
  }
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: state.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: scriptWitnessItems,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    witnessSet,
    anchorWitnessSetHash: state.bad_tx_witness_set_hash,
    label: `${label} field 6`,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      ownOutputs,
      lucid,
      signer,
      planned,
      publisherAddress: signer.proofMaterialsAddress,
      label: `${label} field 6`,
    }));
  if (isPrerequisiteTurn(carriageUtxos)) return carriageUtxos;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  // The absent close past the field reads the count of a directly opened
  // field, or of the certified walk once a certified field's grammar is
  // complete; a negative position reads no count.
  const requiresScan = absent
    ? !negative && planned.plan.tier === "Certified" && !state.grammar_complete
    : planned.plan.tier === "Certified" || scriptWitnessItems.length > 32;
  let nextStep = 2;
  let nextState: Step02State | NonNullable<Step03Datum["data"]> = {
    subject: state.subject,
    bad_tx_id: state.bad_tx_id,
    bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
    script_item_hash:
      item === undefined
        ? NATIVE_SCRIPT_INVALID_ABSENT_SCRIPT_ITEM_HASH
        : nativeScriptItemCommitment(item),
    validity_interval_start: state.validity_interval_start,
    validity_interval_end: state.validity_interval_end,
  };
  const actionFor = (
    ctx: RedeemerContext,
    input_index: bigint,
    output_index: bigint,
  ): Extract<Step02Redeemer, { Continue: unknown }>["Continue"][0] => {
    const opening: FieldOpening = faultProofFieldOpening({
      planned,
      referenceInputs: ctx.referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${label} field 6`,
    });
    const indices = { input_index, output_index };
    if (absent && !requiresScan)
      return {
        SelectAbsentScript: {
          ...indices,
          script_index: scriptIndex,
          script_tx_wits_opening: opening,
          grammar_checkpoint_bytes: state.grammar_complete
            ? encodeFieldGrammarCheckpoint(
                resolveFieldGrammarCheckpoint({
                  txId: state.bad_tx_id,
                  items: scriptWitnessItems,
                  committedHash: state.grammar_checkpoint_hash,
                }),
              ).toString("hex")
            : "",
        },
      };
    if (!requiresScan)
      return {
        Args: {
          ...indices,
          script_index: scriptIndex,
          script_tx_wits_opening: opening,
        },
      };
    if (!state.grammar_complete) {
      const prior =
        state.grammar_checkpoint_hash === ""
          ? initialFieldGrammarCheckpoint({
              txId: state.bad_tx_id,
              items: scriptWitnessItems,
            })
          : resolveFieldGrammarCheckpoint({
              txId: state.bad_tx_id,
              items: scriptWitnessItems,
              committedHash: state.grammar_checkpoint_hash,
            });
      return {
        CertifyScriptField: {
          ...indices,
          script_tx_wits_opening: opening,
          checkpoint_bytes:
            state.grammar_checkpoint_hash === ""
              ? ""
              : encodeFieldGrammarCheckpoint(prior).toString("hex"),
          item_budget: 32n,
        },
      };
    }
    const grammar = resolveFieldGrammarCheckpoint({
      txId: state.bad_tx_id,
      items: scriptWitnessItems,
      committedHash: state.grammar_checkpoint_hash,
    });
    const prior =
      state.script_checkpoint_hash === ""
        ? initialFieldSemanticCheckpoint({
            grammar,
            items: scriptWitnessItems,
          })
        : resolveFieldSemanticCheckpoint({
            txId: state.bad_tx_id,
            items: scriptWitnessItems,
            committedHash: state.script_checkpoint_hash,
          });
    return {
      SelectCertifiedScript: {
        ...indices,
        script_index: scriptIndex,
        script_tx_wits_opening: opening,
        grammar_checkpoint_bytes:
          encodeFieldGrammarCheckpoint(grammar).toString("hex"),
        walk_checkpoint_bytes:
          state.script_checkpoint_hash === ""
            ? ""
            : encodeFieldSemanticCheckpoint(prior).toString("hex"),
        item_budget: 32n,
      },
    };
  };
  if (requiresScan) {
    if (!state.grammar_complete) {
      const prior =
        state.grammar_checkpoint_hash === ""
          ? initialFieldGrammarCheckpoint({
              txId: state.bad_tx_id,
              items: scriptWitnessItems,
            })
          : resolveFieldGrammarCheckpoint({
              txId: state.bad_tx_id,
              items: scriptWitnessItems,
              committedHash: state.grammar_checkpoint_hash,
            });
      const next = advanceFieldGrammarCheckpoint({
        checkpoint: prior,
        items: scriptWitnessItems,
        budget: 32,
      });
      nextStep = 1;
      nextState = {
        ...state,
        grammar_checkpoint_hash: hashFieldGrammarCheckpoint(next),
        grammar_complete: fieldGrammarCheckpointIsComplete(next),
      };
    } else {
      const grammar = resolveFieldGrammarCheckpoint({
        txId: state.bad_tx_id,
        items: scriptWitnessItems,
        committedHash: state.grammar_checkpoint_hash,
      });
      const prior =
        state.script_checkpoint_hash === ""
          ? initialFieldSemanticCheckpoint({
              grammar,
              items: scriptWitnessItems,
            })
          : resolveFieldSemanticCheckpoint({
              txId: state.bad_tx_id,
              items: scriptWitnessItems,
              committedHash: state.script_checkpoint_hash,
            });
      if (Number(scriptIndex) - prior.nextItemIndex >= 32) {
        const next = advanceFieldSemanticCheckpoint({
          checkpoint: prior,
          txId: state.bad_tx_id,
          items: scriptWitnessItems,
          budget: 32,
        });
        nextStep = 1;
        nextState = {
          ...state,
          script_checkpoint_hash: hashFieldSemanticCheckpoint(next),
        };
      }
    }
  }
  const nextDatum =
    nextStep === 1
      ? Data.to(
          {
            fraud_prover: signer.paymentKeyHash,
            data: nextState as Step02State,
          },
          Step02Datum,
        )
      : Data.to(
          {
            fraud_prover: signer.paymentKeyHash,
            data: nextState as NonNullable<Step03Datum["data"]>,
          },
          Step03Datum,
        );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStep]!.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const inputIndex = requireInputIndex(ctx, threadUtxo, label);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, label);
    return Data.to(
      {
        Continue: [actionFor(ctx, inputIndex, outputIndex)],
      },
      Step02Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: label,
    nextAddress: contracts.steps[nextStep]!.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
  });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex.toString()}`,
    scriptItemCbor: item === undefined ? "" : Buffer.from(item).toString("hex"),
    carriageTier: planned.plan.tier,
    nextStepIndex: nextStep,
  };
};
