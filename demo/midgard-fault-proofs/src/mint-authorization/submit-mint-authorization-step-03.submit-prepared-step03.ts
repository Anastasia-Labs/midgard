import type {
  MintAuthorizationStep03Args,
  MintAuthorizationStep03State,
  NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import {
  MintAuthorizationStep03Datum,
  MintAuthorizationStep03SpendRedeemer,
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
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../field-opening.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { witnessSpendingValidatorCarriage } from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import type { MintAuthorizationContracts } from "./contracts.js";
import {
  mintAuthorizationStepLabel,
  mintAuthorizationSubmitError,
  requireMintAuthorizationReferenceScript,
  requireMintAuthorizationStepState,
  requireMintAuthorizationThreadUtxo,
} from "./submit-common.js";

export const STEP_LABEL = mintAuthorizationStepLabel(2);

export type SubmitMintAuthorizationStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly nextStepAddress: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export type Step03Shared = {
  readonly lucid: LucidEvolution;
  readonly contracts: MintAuthorizationContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The bound transaction's compact CBOR, hex. */
  readonly nativeTxCompactCbor: string;
  /** The bound transaction's witness-set compact (three §5.1 hashes). */
  readonly witnessSet: NativeTxWitnessSetCompact;
  /** Pre-minted §8.6 certificate when the planner selects tier 3. */
  readonly certificateUtxo?: UTxO;
  /** Authenticated workflow publications; avoids publishing inside step capture. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
  /** The mandatory published step-03 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
};

export const prepareThread = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
}: Step03Shared) => {
  const { threadUtxo, threadToken } = await requireMintAuthorizationThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 2,
    threadOutRef,
  });
  const state: MintAuthorizationStep03State = requireMintAuthorizationStepState(
    {
      threadUtxo,
      signer,
      schema: MintAuthorizationStep03Datum,
      stepIndex: 2,
    },
  );
  return { threadUtxo, threadToken, state };
};

export const submitPreparedStep03 = async ({
  shared,
  threadUtxo,
  threadToken,
  planned,
  carriageUtxos,
  fieldReferenceInputs,
  fieldIndex,
  nextStepIndex,
  nextStepDatum,
  argsOf,
}: {
  readonly shared: Step03Shared;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly planned: ReturnType<typeof planFaultProofFieldOpening>;
  readonly carriageUtxos: readonly UTxO[];
  readonly fieldReferenceInputs: readonly UTxO[];
  readonly fieldIndex: number;
  readonly nextStepIndex: 3 | 4 | 5 | 6;
  readonly nextStepDatum: string;
  readonly argsOf: (
    layout: {
      readonly inputIndex: bigint;
      readonly outputIndex: bigint;
    },
    fieldOpening: ReturnType<typeof faultProofFieldOpening>,
    ctx: Parameters<BuildTxWithRedeemer>[0],
  ) => MintAuthorizationStep03Args;
}): Promise<SubmitMintAuthorizationStep03Result> => {
  const { lucid, contracts, signer, referenceScriptUtxo } = shared;
  const awaitConfirmation = shared.awaitConfirmation ?? true;
  signer.selectWallet(lucid);
  const stepReference =
    referenceScriptUtxo === undefined
      ? undefined
      : requireMintAuthorizationReferenceScript({
          utxo: referenceScriptUtxo,
          expectedScriptHash: contracts.steps[2].spendingScriptHash,
          stepIndex: 2,
        });
  const stepCarriage = witnessSpendingValidatorCarriage({
    script: contracts.steps[2].spendingScript,
    referenceUtxo: stepReference,
    label: `${STEP_LABEL} spending validator`,
  });
  // §8.7 indices count into the transaction's COMPLETE reference-input set:
  // the field carriage plus the step's own reference script.
  const referenceInputs = [
    ...fieldReferenceInputs,
    ...stepCarriage.referenceInputs,
  ];
  const fieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${STEP_LABEL} field ${fieldIndex.toString()}`,
  });
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansCarriage = carriageUtxos.reduce<readonly UTxO[]>(
    (candidates, utxo) => excludeUtxo(candidates, utxo),
    walletUtxos,
  );
  const feeInput = selectFeeInput(walletUtxosSansCarriage);
  const nextOutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextStepDatum,
    unit: threadToken.unit,
  });
  let resolvedLayout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        nextOutputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      { Continue: [argsOf(layout, fieldOpening, ctx)] },
      MintAuthorizationStep03SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const withInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  const withReferences =
    referenceInputs.length === 0
      ? withInputs
      : withInputs.readFrom(referenceInputs);
  const paid = withReferences.pay
    .ToContract(
      contracts.steps[nextStepIndex].spendingScriptAddress,
      { kind: "inline", value: nextStepDatum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = stepCarriage.attach(paid);

  const unsigned = await tx.complete({
    localUPLCEval: true,
    ...(carriageUtxos.length === 0
      ? {}
      : { presetWalletInputs: walletUtxosSansCarriage as UTxO[] }),
  });
  if (resolvedLayout === undefined) {
    throw mintAuthorizationSubmitError(
      "BuildTxWithRedeemer did not resolve the step-03 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof mint-authorization step-03",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[2].spendingScript,
        },
      ],
    }),
    boundary: shared.preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw mintAuthorizationSubmitError(
      `step-03 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef: shared.threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    nextStepAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
