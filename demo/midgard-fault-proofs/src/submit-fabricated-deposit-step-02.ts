/** Capture authenticated current history facts. Pointer references are discovered
 * afresh; the retained payload and original funds survive later list mutations. */
import {
  type FabricatedDepositEvidence,
  type FabricatedDepositEvidenceVerdict,
  FabricatedDepositStep02Datum,
  FabricatedDepositStep02SpendRedeemer,
  type FabricatedDepositStep02State,
  FabricatedDepositStep03Datum,
  fabricatedDepositStep03State,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  fabricatedHistoryOpeningCbor,
  fetchCurrentHistory,
} from "./fabricated-history-witness.js";
import { fabricatedProofValidity } from "./fabricated-proof-validity.js";
import { requireFabricatedReferenceScript } from "./fabricated-reference-script.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./step-support.js";
import {
  FABRICATED_DEPOSIT_CATEGORY_LABEL,
  type FabricatedDepositContracts,
} from "./submit-fabricated-deposit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "./workflow/transaction-boundary.js";

/** Expected semantic arm. An old pointer hint is never used as L1 authority. */
export type FabricatedDepositEvidenceArm =
  | { readonly kind: "absent_identity" }
  | { readonly kind: "present_event"; readonly eventOutRef?: string };

export const requireFabricatedDepositStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): FabricatedDepositStep02State => {
  if (threadUtxo.datum === null || threadUtxo.datum === undefined) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} has no inline fabricated-deposit step-02 datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, FabricatedDepositStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} belongs to fraud prover ${datum.fraud_prover}, not ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} carries no fabricated-deposit step-02 state.`,
    );
  }
  return datum.data;
};

export type SubmitFabricatedDepositStep02CliConfig = SubmitProviderConfig & {
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly eventOutRef?: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitFabricatedDepositStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  readonly thirdStepAddress: string;
  readonly evidenceKind: FabricatedDepositEvidenceArm["kind"];
  readonly openingCbor: string | null;
  readonly historyOutRef: string;
  readonly verdict: FabricatedDepositEvidenceVerdict;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type FabricatedDepositStep02Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly evidence: FabricatedDepositEvidence;
};

export const submitFabricatedDepositStep02 = async ({
  lucid,
  contracts,
  network,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
  now = Date.now,
  expectedOpeningCbor,
}: {
  readonly now?: () => number;
  /** A journal opening must still match the fresh semantic facts before capture. */
  readonly expectedOpeningCbor?: string | null;
  readonly lucid: LucidEvolution;
  readonly contracts: FabricatedDepositContracts;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: FabricatedDepositEvidenceArm;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFabricatedDepositStep02Result> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "fabricated-deposit step-02 computation-thread UTxO",
  });
  if (threadUtxo.address !== contracts.steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at fabricated-deposit step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: contracts.categoryId,
    categoryLabel: FABRICATED_DEPOSIT_CATEGORY_LABEL,
  });
  const state = requireFabricatedDepositStep02Datum({ threadUtxo, signer });

  const current = await fetchCurrentHistory({
    lucid,
    network,
    hubOraclePolicyId: contracts.hubOraclePolicyId,
    history: contracts.history,
    kind: "Deposit",
    id: state.committed_deposit_id,
  });
  if (
    current.stateQueuePolicyId !== state.state_queue_policy ||
    current.stateQueuePolicyId !== contracts.stateQueuePolicyId
  )
    throw new Error("History hub changed the authenticated state queue policy");
  const { witness, captured, hubOracleUtxo } = current;
  const evidenceKind = captured ? "present_event" : "absent_identity";
  const openingCbor = captured ? fabricatedHistoryOpeningCbor(captured) : null;
  if (
    evidence.kind !== evidenceKind ||
    (expectedOpeningCbor !== undefined && expectedOpeningCbor !== openingCbor)
  )
    throw new Error(
      "History facts changed before capture; reprepare the proof artifact",
    );
  const verdict: FabricatedDepositEvidenceVerdict = captured
    ? { DepositEventObserved: { commitment: captured.commitment } }
    : "DepositIdentityAbsent";
  const referenceInputs = [
    hubOracleUtxo,
    witness.anchor.utxo,
    ...(witness.kind === "Present" && witness.retainedDataUtxo
      ? [witness.retainedDataUtxo]
      : []),
  ];
  const validity = fabricatedProofValidity(state.header_end_time, now());

  const step03State = fabricatedDepositStep03State(state, verdict);
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step03Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step03State },
    FabricatedDepositStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: FabricatedDepositStep02Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "fabricated-deposit step 02");
    const hubIndex = requireReferenceInputIndex(
      ctx,
      hubOracleUtxo,
      "history hub",
    );
    const historyIndex = requireReferenceInputIndex(
      ctx,
      witness.anchor.utxo,
      "history anchor",
    );
    const armEvidence: FabricatedDepositEvidence =
      witness.kind === "Absent"
        ? {
            AbsentDepositIdentity: {
              hub_ref_input_index: hubIndex,
              history_ref_input_index: historyIndex,
            },
          }
        : {
            PresentDepositEvent: {
              hub_ref_input_index: hubIndex,
              event_ref_input_index: historyIndex,
              external_ref_input_index: witness.retainedDataUtxo
                ? requireReferenceInputIndex(
                    ctx,
                    witness.retainedDataUtxo,
                    "retained history data",
                  )
                : null,
            },
          };
    const layout: FabricatedDepositStep02Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "fabricated-deposit step 02",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        "fabricated-deposit step 02 output",
      ),
      evidence: armEvidence,
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            evidence: layout.evidence,
          },
        ],
      },
      FabricatedDepositStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      ...referenceInputs,
      requireFabricatedReferenceScript({
        utxo: referenceScriptUtxo,
        expectedScriptHash: contracts.steps[1].spendingScriptHash,
        categoryLabel: FABRICATED_DEPOSIT_CATEGORY_LABEL,
        stepIndex: 1,
      }),
    ])
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .validFrom(validity.validFrom)
    .validTo(validity.validTo);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve fabricated-deposit step 02 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof fabricated-deposit step-02",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.steps[1].spendingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `fabricated-deposit step-02 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    thirdStepAddress: contracts.steps[2].spendingScriptAddress,
    evidenceKind,
    openingCbor,
    historyOutRef: outRefLabel(witness.anchor.utxo),
    verdict,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitFabricatedDepositStep02FromFiles = async (
  config: SubmitFabricatedDepositStep02CliConfig & {
    readonly contracts: FabricatedDepositContracts;
    readonly referenceScriptUtxo: UTxO;
  },
): Promise<SubmitFabricatedDepositStep02Result> => {
  const lucid = await makeLucidForSubmit(config);
  const signer = resolveProverSigner(config);
  return await submitFabricatedDepositStep02({
    lucid,
    contracts: config.contracts,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    evidence:
      config.eventOutRef === undefined
        ? { kind: "absent_identity" }
        : { kind: "present_event", eventOutRef: config.eventOutRef },
    referenceScriptUtxo: config.referenceScriptUtxo,
    awaitConfirmation: config.awaitConfirmation,
  });
};
