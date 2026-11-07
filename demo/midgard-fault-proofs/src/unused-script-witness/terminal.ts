/** `unusedScriptWitness` terminal: the completed witness walk convicts, the
 * computation thread burns and the permanent fraud-proof token mints. The
 * terminal reads the source selected for the witness's script hash, with its
 * source-map proof. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../step-support.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
} from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import {
  UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL,
  type UnusedScriptWitnessContracts,
} from "./contracts.js";

const TERMINAL_INDEX = SDK.UNUSED_SCRIPT_WITNESS_ROLE_ORDER.indexOf("terminal");
const LABEL = `${UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL} terminal`;

/** Twin of `terminal.convicts_v1` over the completed witness walk and the
 * selection the terminal is given. */
export const unusedScriptWitnessTerminalConvicts = (
  state: SDK.UnusedScriptWitnessWalkState,
  selection: SDK.UnusedScriptWitnessSelection | null,
): boolean => {
  const hash = state.witness_hash;
  const unused = SDK.unusedScriptWitnessIsUnused(
    hash === null || selection === null
      ? null
      : {
          selected:
            selection.source.field_index === 6n &&
            selection.source.item_index === state.script_index,
          reached:
            state.result.facts.probe === hash &&
            state.result.facts.probe_reached,
        },
  );
  return SDK.unusedScriptWitnessConvicts(
    state.result.bound.subject,
    state.result.bound.network_order_context,
    state.script_index,
    unused,
  );
};

export const submitUnusedScriptWitnessTerminal = async ({
  lucid,
  contracts,
  selection,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
  onSigned,
  requireConviction = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: UnusedScriptWitnessContracts;
  /** The source selected for the witness's script hash and its source-map
   * proof; `null` when field 6 ends before the witness. */
  readonly selection: SDK.UnusedScriptWitnessSelection | null;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  readonly onSigned?: (label: string, signed: TxSigned) => void;
  /** False only to reach the validator with a non-convicting state. */
  readonly requireConviction?: boolean;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL,
    stepIndex: TERMINAL_INDEX,
    threadOutRef,
  });
  const state = requireLinearFaultStepState({
    threadUtxo,
    signer,
    schema: SDK.UnusedScriptWitnessWalkDatum,
    family: UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL,
    stepIndex: TERMINAL_INDEX,
  });
  if (
    requireConviction &&
    !unusedScriptWitnessTerminalConvicts(state, selection)
  )
    throw new Error(`${LABEL}: the completed walk does not convict`);
  const step = contracts.steps[TERMINAL_INDEX]!;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: step.spendingScriptHash,
    family: UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL,
    stepIndex: TERMINAL_INDEX,
  });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    SDK.FraudProofTokenDatum,
  );
  let outputIndex: bigint | undefined;
  const spend = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, LABEL);
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      outputWithDatumAndUnitPredicate({
        address: contracts.fraudProof.spendingScriptAddress,
        datum: fraudProofDatum,
        unit: fraudProofUnit,
      }),
      `${LABEL} fraud-proof`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: SDK.requireInputIndex(ctx, threadUtxo, LABEL),
            output_index: outputIndex,
            fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              `${LABEL} fraud-proof`,
            ),
            selection,
          },
        ],
      },
      SDK.UnusedScriptWitnessFinalizeRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${LABEL} computation-thread burn`,
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      SDK.FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const mint = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      `${LABEL} fraud-proof mint`,
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          `${LABEL} computation-thread burn`,
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${LABEL} computation-thread burn`,
  });
  const mintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: `${LABEL} fraud-proof mint`,
  });
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spend)
    .mintAssets({ [threadToken.unit]: -1n }, burn)
    .mintAssets({ [fraudProofUnit]: 1n }, mint)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [fraudProofUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash)
    .readFrom([
      stepReference,
      ...burnCarriage.referenceInputs,
      ...mintCarriage.referenceInputs,
    ])
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined)
    throw new Error(`${LABEL}: completed layout unresolved`);
  const signed = await unsigned.sign.withWallet().complete();
  onSigned?.("terminal", signed);
  const expected = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: LABEL,
          utxo: stepReference,
          expectedScript: step.spendingScript,
        },
        {
          role: "V1 fraud-proof computation-thread minting",
          utxo: witnessReferenceScripts?.computationThreadMint,
          expectedScript: contracts.computationThread.mintingScript,
        },
        {
          role: "V1 fraud-proof token minting",
          utxo: witnessReferenceScripts?.fraudProofMint,
          expectedScript: contracts.fraudProof.mintingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expected)
    throw new Error(
      `${LABEL}: provider returned ${txHash}, expected ${expected}`,
    );
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    finalState: state,
    fraudProofOutRef: `${txHash}#${outputIndex.toString()}`,
    fraudProofUnit,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
  };
};
