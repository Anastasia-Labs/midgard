import {
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  scriptRewardAddress,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  linearFaultStepLabel,
  requireLinearFaultReferenceScript,
} from "../linear-fault-family.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { selectFeeInput } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  MIN_ADA_CATEGORY_LABEL as FAMILY,
  type MinAdaContracts,
} from "./contracts.js";
import {
  advanceMinAdaGrammarCheckpoint,
  advanceMinAdaSemanticCheckpoint,
  encodeMinAdaGrammarCheckpoint,
  encodeMinAdaSemanticCheckpoint,
  hashMinAdaGrammarCheckpoint,
  hashMinAdaSemanticCheckpoint,
  initialMinAdaGrammarCheckpoint,
  initialMinAdaSemanticCheckpoint,
  minAdaGrammarCheckpointIsComplete,
  resolveMinAdaGrammarCheckpoint,
  resolveMinAdaSemanticCheckpoint,
} from "./field-walk.js";
import type { detectMinAdaForcedReplay } from "./forced.js";
import type { PreparedMinAdaTx } from "./prepare.js";
import { minAdaInitialScanState, minAdaOutputScanEvidence } from "./scan.js";
import {
  Redeemer,
  requireStep02,
  type State,
  Step02Datum,
  Step03Datum,
  walletInputsExcludingReferences,
} from "./submit-step-02.require-step02.js";

export const submitMinAdaTxStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  prepared,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  yieldReferenceScriptUtxo,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
  unsafeSkipLocalViolationCheckForTest = false,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MinAdaContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly prepared:
    | PreparedMinAdaTx
    | ReturnType<typeof detectMinAdaForcedReplay>[number]["evidence"];
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly yieldReferenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  readonly unsafeSkipLocalViolationCheckForTest?: boolean;
}): Promise<{
  txHash: string;
  nextThreadOutRef: string;
  carriageTier: string;
  nextStepIndex: number;
}> => {
  const { stepIndex, threadUtxo, threadToken, state } = await requireStep02({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
  });
  const label = `${linearFaultStepLabel(FAMILY, stepIndex)} transaction`;
  if (
    state.bad_tx_id !== prepared.badTxId ||
    state.post_utxo !== null ||
    state.fault === "MinAdaUtxo" ||
    state.fault.MinAdaTx.output_index !== prepared.badOutputIndex
  ) {
    throw new Error(
      `${label}: prepared transaction does not match thread state`,
    );
  }
  const item = prepared.outputItemCbors[Number(prepared.badOutputIndex)];
  if (item === undefined) {
    throw new Error(`${label}: bad output index is outside field 2`);
  }
  const material = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: Number(prepared.badOutputIndex),
    outputCbor: Buffer.from(item, "hex"),
  });
  if (
    material.descriptorCbor.toString("hex") !== prepared.descriptorCbor ||
    (!unsafeSkipLocalViolationCheckForTest &&
      outputMeetsMinAda(
        MIDGARD_COINS_PER_UTXO_BYTE,
        BigInt(material.descriptor.totalLength),
        material.descriptor.lovelace,
      ) !==
        (state.direction === 1n))
  ) {
    throw new Error(`${label}: selected output does not violate min-Ada`);
  }
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: state.direction === 1n ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: state.bad_tx_id,
    nativeTxCompactCbor: prepared.nativeTxCompactCbor,
    itemCbors: prepared.outputItemCbors.map((cbor) => Buffer.from(cbor, "hex")),
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: `${label} field 2`,
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: `${label} field 2`,
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const yieldReference = requireLinearFaultReferenceScript({
    utxo: yieldReferenceScriptUtxo,
    expectedScriptHash: contracts.yields.tx.withdrawalScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const opening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs: [
      ...carriageUtxos,
      stepReference,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
      yieldReference,
    ],
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${label} field 2`,
  });
  const items = prepared.outputItemCbors.map((item) =>
    Buffer.from(item, "hex"),
  );
  let grammarBytes = "";
  let walkBytes = "";
  let nextStepIndex = 2;
  let continuationState: State | undefined;
  if (!state.grammar_complete) {
    const prior =
      state.grammar_checkpoint_hash === ""
        ? initialMinAdaGrammarCheckpoint({ txId: state.bad_tx_id, items })
        : resolveMinAdaGrammarCheckpoint({
            txId: state.bad_tx_id,
            items,
            committedHash: state.grammar_checkpoint_hash,
          });
    grammarBytes =
      state.grammar_checkpoint_hash === ""
        ? ""
        : encodeMinAdaGrammarCheckpoint(prior).toString("hex");
    const next = advanceMinAdaGrammarCheckpoint({
      checkpoint: prior,
      items,
      budget: 32,
    });
    continuationState = {
      ...state,
      grammar_checkpoint_hash: hashMinAdaGrammarCheckpoint(next),
      grammar_complete: minAdaGrammarCheckpointIsComplete(next),
    };
    nextStepIndex = 1;
  } else {
    const grammar = resolveMinAdaGrammarCheckpoint({
      txId: state.bad_tx_id,
      items,
      committedHash: state.grammar_checkpoint_hash,
    });
    grammarBytes = encodeMinAdaGrammarCheckpoint(grammar).toString("hex");
    const prior =
      state.walk_checkpoint_hash === ""
        ? initialMinAdaSemanticCheckpoint({ grammar, items })
        : resolveMinAdaSemanticCheckpoint({
            txId: state.bad_tx_id,
            items,
            committedHash: state.walk_checkpoint_hash,
          });
    walkBytes =
      state.walk_checkpoint_hash === ""
        ? ""
        : encodeMinAdaSemanticCheckpoint(prior).toString("hex");
    if (Number(prepared.badOutputIndex) - prior.nextItemIndex >= 32) {
      const next = advanceMinAdaSemanticCheckpoint({
        checkpoint: prior,
        txId: state.bad_tx_id,
        items,
        budget: 32,
      });
      continuationState = {
        ...state,
        walk_checkpoint_hash: hashMinAdaSemanticCheckpoint(next),
      };
      nextStepIndex = 1;
    }
  }
  const nextDatum = continuationState
    ? Data.to(
        { fraud_prover: signer.paymentKeyHash, data: continuationState },
        Step02Datum,
      )
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            MinAdaTxScan: {
              direction: state.direction,
              scan: minAdaInitialScanState(
                minAdaOutputScanEvidence(
                  state.bad_tx_id,
                  prepared.badOutputIndex,
                  prepared.outputItemCbors,
                ),
              ),
            },
          },
        },
        Step03Datum,
      );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
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
        Continue: [
          {
            grammar_checkpoint_bytes: grammarBytes,
            walk_checkpoint_bytes: walkBytes,
            input_index: inputIndex,
            output_index: outputIndex,
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              yieldReference,
              label,
            ),
            outputs_opening: opening,
            post_membership: null,
          },
        ],
      },
      Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const network = lucid.config().network;
  if (network === undefined) throw new Error(`${label}: Lucid network missing`);
  const transactionReferences = [
    stepReference,
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    yieldReference,
  ];
  const feeInput = selectFeeInput(
    walletInputsExcludingReferences({
      walletUtxos: await lucid.wallet().getUtxos(),
      references: transactionReferences,
    }),
  );
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(transactionReferences)
    .withdraw(
      scriptRewardAddress(network, contracts.yields.tx.withdrawalScript),
      0n,
      Data.void(),
    )
    .pay.ToContract(
      contracts.steps[nextStepIndex].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const {
    reachFraudProofPreSubmitBoundary,
    workflowReferenceScriptsUsedByTransaction,
  } = await import("../workflow/transaction-boundary.js");
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: label,
          utxo: stepReference,
          expectedScript: contracts.steps[stepIndex].spendingScript,
        },
        {
          role: `${label}-yield`,
          utxo: yieldReference,
          expectedScript: contracts.yields.tx.withdrawalScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) throw new Error(`${label}: hash mismatch`);
  if (awaitConfirmation) {
    const { DEFAULT_CONFIRMATION_POLL_MS } = await import("../runtime.js");
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  if (nextStepIndex === 1 && awaitConfirmation)
    return submitMinAdaTxStep02({
      lucid,
      contracts,
      categoryId,
      signer,
      threadOutRef: `${txHash}#${outputIndex}`,
      prepared,
      publishCarriage,
      publishedCarriageUtxos: carriageUtxos,
      certificateUtxo,
      referenceScriptUtxo,
      yieldReferenceScriptUtxo,
      publicationPreSubmitBoundary,
      preSubmitBoundary,
      awaitConfirmation,
      unsafeSkipLocalViolationCheckForTest,
    });
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    carriageTier: planned.plan.tier,
    nextStepIndex,
  };
};
