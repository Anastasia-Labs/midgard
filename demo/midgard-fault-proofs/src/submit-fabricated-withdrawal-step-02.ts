/**
 * `fabricated-withdrawal` step-02 submitter (Goal task `Q40`, §9.1 output 8).
 *
 * Step 02 is the only step that reads L1, and both of its arms are authenticated
 * locally before submission rather than asserted:
 *
 * - `AbsentWithdrawalIdentity` requires the committed `WithdrawalId` to still be an
 *   **unspent** output reference. `authenticate_new_event` rule 4 requires a
 *   withdrawal event's id to be an outref the authenticating transaction spent, so a
 *   still-live outref positively proves no event with that identity was ever
 *   authenticated. A consumed outref is refused here, never silently downgraded
 *   into an absence claim.
 * - `PresentWithdrawalEvent` derives the withdrawal policy from the **authentic hub
 *   oracle datum** — its `withdrawal` field, not its `deposit` field, which
 *   registers a different event family — and the event NFT asset name from the
 *   committed identity via `out_ref_to_nonce`, then requires the referenced UTxO to
 *   carry exactly that unit and to hold a `WithdrawalOrderDatum` whose `event.id` is
 *   the committed identity.
 *
 * The verdict handed to step 03 carries a 32-byte commitment to the event datum
 * rather than the datum itself, because a withdrawal event datum embeds the
 * withdrawer-chosen `l2_value` map and `l1_datum`; step 03 re-opens the preimage
 * from its own redeemer, which also means the dispute survives the event NFT being
 * burned when the withdrawal settles or refunds.
 */
import {
  type FabricatedWithdrawalEvidenceV1,
  type FabricatedWithdrawalEvidenceVerdictV1,
  FabricatedWithdrawalStep02Datum,
  FabricatedWithdrawalStep02SpendRedeemer,
  type FabricatedWithdrawalStep02State,
  FabricatedWithdrawalStep03Datum,
  fabricatedWithdrawalStep03StateV1,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  withdrawalEventDatumCommitmentV1,
  withdrawalEventNonceV1,
  WithdrawalOrderDatum,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { requireFabricatedReferenceScriptV1 } from "./fabricated-reference-script-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  FABRICATED_WITHDRAWAL_CATEGORY_LABEL,
  type FabricatedWithdrawalContractsV1,
} from "./submit-fabricated-withdrawal-step-01.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";

/** Which L1 witness the prover intends to submit. */
export type FabricatedWithdrawalEvidenceArmV1 =
  | { readonly kind: "absent_identity" }
  | { readonly kind: "present_event"; readonly eventOutRef: string };

export const requireFabricatedWithdrawalStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): FabricatedWithdrawalStep02State => {
  if (threadUtxo.datum === null || threadUtxo.datum === undefined) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} has no inline fabricated-withdrawal step-02 datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, FabricatedWithdrawalStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} belongs to fraud prover ${datum.fraud_prover}, not ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} carries no fabricated-withdrawal step-02 state.`,
    );
  }
  return datum.data;
};

/**
 * Authenticates the withdrawal event UTxO a `PresentWithdrawalEvent` witness points
 * at, against the hub oracle's withdrawal policy and the committed identity's nonce.
 */
export const authenticateFabricatedWithdrawalEventUtxoV1 = async ({
  state,
  hubOracleUtxo,
  eventUtxo,
}: {
  readonly state: FabricatedWithdrawalStep02State;
  readonly hubOracleUtxo: UTxO;
  readonly eventUtxo: UTxO;
}): Promise<{
  readonly eventDatum: WithdrawalOrderDatum;
  readonly eventDatumHash: string;
  readonly withdrawalPolicyId: string;
  readonly expectedEventAssetName: string;
}> => {
  if (hubOracleUtxo.datum === null || hubOracleUtxo.datum === undefined) {
    throw new Error("Hub oracle UTxO has no inline datum.");
  }
  const hubDatum = Data.from(hubOracleUtxo.datum, HubOracleDatum);
  const withdrawalPolicyId = hubDatum.withdrawal;
  const expectedEventAssetName = await Effect.runPromise(
    withdrawalEventNonceV1(state.committed_withdrawal_id),
  );
  const expectedUnit = toUnit(withdrawalPolicyId, expectedEventAssetName);
  if ((eventUtxo.assets[expectedUnit] ?? 0n) !== 1n) {
    throw new Error(
      `Withdrawal event UTxO ${outRefLabel(eventUtxo)} does not carry the authentic withdrawal event NFT ${expectedUnit} for the committed identity.`,
    );
  }
  if (eventUtxo.datum === null || eventUtxo.datum === undefined) {
    throw new Error(
      `Withdrawal event UTxO ${outRefLabel(eventUtxo)} has no inline withdrawal datum.`,
    );
  }
  const eventDatum = Data.from(eventUtxo.datum, WithdrawalOrderDatum);
  if (
    Data.to(eventDatum.event.id, OutputReference) !==
    Data.to(state.committed_withdrawal_id, OutputReference)
  ) {
    throw new Error(
      `Withdrawal event UTxO ${outRefLabel(eventUtxo)} holds event id ${Data.to(eventDatum.event.id, OutputReference)}, not the committed identity ${Data.to(state.committed_withdrawal_id, OutputReference)}.`,
    );
  }
  const eventDatumHash = await Effect.runPromise(
    withdrawalEventDatumCommitmentV1(eventDatum),
  );
  return {
    eventDatum,
    eventDatumHash,
    withdrawalPolicyId,
    expectedEventAssetName,
  };
};

export type SubmitFabricatedWithdrawalStep02CliConfig = SubmitProviderConfig & {
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly eventOutRef?: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitFabricatedWithdrawalStep02Result = {
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
  readonly evidenceKind: FabricatedWithdrawalEvidenceArmV1["kind"];
  readonly verdict: FabricatedWithdrawalEvidenceVerdictV1;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type FabricatedWithdrawalStep02Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly evidence: FabricatedWithdrawalEvidenceV1;
};

export const submitFabricatedWithdrawalStep02 = async ({
  lucid,
  contracts,
  network,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: FabricatedWithdrawalContractsV1;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: FabricatedWithdrawalEvidenceArmV1;
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFabricatedWithdrawalStep02Result> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "fabricated-withdrawal step-02 computation-thread UTxO",
  });
  if (threadUtxo.address !== contracts.steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at fabricated-withdrawal step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: contracts.categoryId,
    categoryLabel: FABRICATED_WITHDRAWAL_CATEGORY_LABEL,
  });
  const state = requireFabricatedWithdrawalStep02Datum({ threadUtxo, signer });

  let referenceInputs: readonly UTxO[];
  let verdict: FabricatedWithdrawalEvidenceVerdictV1;
  let hubOracleUtxo: UTxO | undefined;
  let eventUtxo: UTxO | undefined;
  let unspentUtxo: UTxO | undefined;

  if (evidence.kind === "absent_identity") {
    const committedOutRef = `${state.committed_withdrawal_id.transactionId}#${state.committed_withdrawal_id.outputIndex.toString()}`;
    unspentUtxo = await fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(committedOutRef, "committed withdrawal identity"),
      label: `fabricated-withdrawal step-02 unspent committed identity ${committedOutRef}`,
    });
    referenceInputs = [unspentUtxo];
    verdict = "WithdrawalIdentityAbsent";
  } else {
    [hubOracleUtxo, eventUtxo] = await Promise.all([
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(contracts.hubOraclePolicyId),
        ),
        unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: "hub oracle",
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(evidence.eventOutRef, "--event-out-ref"),
        label: "fabricated-withdrawal step-02 withdrawal event UTxO",
      }),
    ]);
    const authenticated = await authenticateFabricatedWithdrawalEventUtxoV1({
      state,
      hubOracleUtxo,
      eventUtxo,
    });
    referenceInputs = [hubOracleUtxo, eventUtxo];
    verdict = {
      WithdrawalEventObserved: {
        event_datum_hash: authenticated.eventDatumHash,
        event_inclusion_time: authenticated.eventDatum.inclusion_time,
      },
    };
  }

  const step03State = fabricatedWithdrawalStep03StateV1(state, verdict);
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step03Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step03State },
    FabricatedWithdrawalStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: FabricatedWithdrawalStep02Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "fabricated-withdrawal step 02");
    const armEvidence: FabricatedWithdrawalEvidenceV1 =
      evidence.kind === "absent_identity"
        ? {
            AbsentWithdrawalIdentity: {
              unspent_ref_input_index: requireReferenceInputIndex(
                ctx,
                unspentUtxo!,
                "fabricated-withdrawal step 02 unspent committed identity",
              ),
            },
          }
        : {
            PresentWithdrawalEvent: {
              hub_ref_input_index: requireReferenceInputIndex(
                ctx,
                hubOracleUtxo!,
                "fabricated-withdrawal step 02 hub oracle",
              ),
              event_ref_input_index: requireReferenceInputIndex(
                ctx,
                eventUtxo!,
                "fabricated-withdrawal step 02 withdrawal event",
              ),
            },
          };
    const layout: FabricatedWithdrawalStep02Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "fabricated-withdrawal step 02",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        "fabricated-withdrawal step 02 output",
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
      FabricatedWithdrawalStep02SpendRedeemer,
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
      requireFabricatedReferenceScriptV1({
        utxo: referenceScriptUtxo,
        expectedScriptHash: contracts.steps[1].spendingScriptHash,
        categoryLabel: FABRICATED_WITHDRAWAL_CATEGORY_LABEL,
        stepIndex: 1,
      }),
    ])
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve fabricated-withdrawal step 02 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
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
    evidenceKind: evidence.kind,
    verdict,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitFabricatedWithdrawalStep02FromFiles = async (
  config: SubmitFabricatedWithdrawalStep02CliConfig & {
    readonly contracts: FabricatedWithdrawalContractsV1;
    readonly referenceScriptUtxo: UTxO;
  },
): Promise<SubmitFabricatedWithdrawalStep02Result> => {
  const lucid = await makeLucidForSubmit(config);
  const signer = resolveProverSigner(config);
  return await submitFabricatedWithdrawalStep02({
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
