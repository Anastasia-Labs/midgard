/**
 * `fabricated-deposit` step-02 submitter (Goal task `Q39`, §9.1 output 8).
 *
 * Step 02 is the only step that reads L1, and both of its arms are authenticated
 * locally before submission rather than asserted:
 *
 * - `AbsentDepositIdentity` requires the committed `DepositId` to still be an
 *   **unspent** output reference. `authenticate_new_event` rule 4 requires a
 *   deposit event's id to be an outref the authenticating transaction spent, so a
 *   still-live outref positively proves no event with that identity was ever
 *   authenticated. A consumed outref is refused here, never silently downgraded
 *   into an absence claim.
 * - `PresentDepositEvent` derives the deposit policy from the **authentic hub
 *   oracle datum** and the event NFT asset name from the committed identity via
 *   `out_ref_to_nonce`, then requires the referenced UTxO to carry exactly that
 *   unit and to hold a `DepositDatum` whose `event.id` is the committed identity.
 *
 * The verdict handed to step 03 carries a 32-byte commitment to the event datum
 * rather than the datum itself, because `DepositInfo.l2_datum` is attacker-chosen
 * unbounded data; step 03 re-opens the preimage from its own redeemer.
 */
import {
  DepositDatum,
  depositEventDatumCommitmentV1,
  depositEventNonceV1,
  type FabricatedDepositEvidenceV1,
  type FabricatedDepositEvidenceVerdictV1,
  FabricatedDepositStep02Datum,
  FabricatedDepositStep02SpendRedeemer,
  type FabricatedDepositStep02State,
  FabricatedDepositStep03Datum,
  fabricatedDepositStep03StateV1,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
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
  FABRICATED_DEPOSIT_CATEGORY_LABEL,
  type FabricatedDepositContractsV1,
} from "./submit-fabricated-deposit-step-01.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";

/** Which L1 witness the prover intends to submit. */
export type FabricatedDepositEvidenceArmV1 =
  | { readonly kind: "absent_identity" }
  | { readonly kind: "present_event"; readonly eventOutRef: string };

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

/**
 * Authenticates the deposit event UTxO a `PresentDepositEvent` witness points at,
 * against the hub oracle's deposit policy and the committed identity's nonce.
 */
export const authenticateFabricatedDepositEventUtxoV1 = async ({
  state,
  hubOracleUtxo,
  eventUtxo,
}: {
  readonly state: FabricatedDepositStep02State;
  readonly hubOracleUtxo: UTxO;
  readonly eventUtxo: UTxO;
}): Promise<{
  readonly eventDatum: DepositDatum;
  readonly eventDatumHash: string;
  readonly depositPolicyId: string;
  readonly expectedEventAssetName: string;
}> => {
  if (hubOracleUtxo.datum === null || hubOracleUtxo.datum === undefined) {
    throw new Error("Hub oracle UTxO has no inline datum.");
  }
  const hubDatum = Data.from(hubOracleUtxo.datum, HubOracleDatum);
  const depositPolicyId = hubDatum.deposit;
  const expectedEventAssetName = await Effect.runPromise(
    depositEventNonceV1(state.committed_deposit_id),
  );
  const expectedUnit = toUnit(depositPolicyId, expectedEventAssetName);
  if ((eventUtxo.assets[expectedUnit] ?? 0n) !== 1n) {
    throw new Error(
      `Deposit event UTxO ${outRefLabel(eventUtxo)} does not carry the authentic deposit event NFT ${expectedUnit} for the committed identity.`,
    );
  }
  if (eventUtxo.datum === null || eventUtxo.datum === undefined) {
    throw new Error(
      `Deposit event UTxO ${outRefLabel(eventUtxo)} has no inline deposit datum.`,
    );
  }
  const eventDatum = Data.from(eventUtxo.datum, DepositDatum);
  if (
    Data.to(eventDatum.event.id, OutputReference) !==
    Data.to(state.committed_deposit_id, OutputReference)
  ) {
    throw new Error(
      `Deposit event UTxO ${outRefLabel(eventUtxo)} holds event id ${Data.to(eventDatum.event.id, OutputReference)}, not the committed identity ${Data.to(state.committed_deposit_id, OutputReference)}.`,
    );
  }
  const eventDatumHash = await Effect.runPromise(
    depositEventDatumCommitmentV1(eventDatum),
  );
  return {
    eventDatum,
    eventDatumHash,
    depositPolicyId,
    expectedEventAssetName,
  };
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
  readonly evidenceKind: FabricatedDepositEvidenceArmV1["kind"];
  readonly verdict: FabricatedDepositEvidenceVerdictV1;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type FabricatedDepositStep02Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly evidence: FabricatedDepositEvidenceV1;
};

export const submitFabricatedDepositStep02 = async ({
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
  readonly contracts: FabricatedDepositContractsV1;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: FabricatedDepositEvidenceArmV1;
  readonly referenceScriptUtxo: UTxO;
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

  let referenceInputs: readonly UTxO[];
  let verdict: FabricatedDepositEvidenceVerdictV1;
  let hubOracleUtxo: UTxO | undefined;
  let eventUtxo: UTxO | undefined;
  let unspentUtxo: UTxO | undefined;

  if (evidence.kind === "absent_identity") {
    const committedOutRef = `${state.committed_deposit_id.transactionId}#${state.committed_deposit_id.outputIndex.toString()}`;
    unspentUtxo = await fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(committedOutRef, "committed deposit identity"),
      label: `fabricated-deposit step-02 unspent committed identity ${committedOutRef}`,
    });
    referenceInputs = [unspentUtxo];
    verdict = "DepositIdentityAbsent";
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
        label: "fabricated-deposit step-02 deposit event UTxO",
      }),
    ]);
    const authenticated = await authenticateFabricatedDepositEventUtxoV1({
      state,
      hubOracleUtxo,
      eventUtxo,
    });
    referenceInputs = [hubOracleUtxo, eventUtxo];
    verdict = {
      DepositEventObserved: {
        event_datum_hash: authenticated.eventDatumHash,
        event_inclusion_time: authenticated.eventDatum.inclusion_time,
      },
    };
  }

  const step03State = fabricatedDepositStep03StateV1(state, verdict);
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
    const armEvidence: FabricatedDepositEvidenceV1 =
      evidence.kind === "absent_identity"
        ? {
            AbsentDepositIdentity: {
              unspent_ref_input_index: requireReferenceInputIndex(
                ctx,
                unspentUtxo!,
                "fabricated-deposit step 02 unspent committed identity",
              ),
            },
          }
        : {
            PresentDepositEvent: {
              hub_ref_input_index: requireReferenceInputIndex(
                ctx,
                hubOracleUtxo!,
                "fabricated-deposit step 02 hub oracle",
              ),
              event_ref_input_index: requireReferenceInputIndex(
                ctx,
                eventUtxo!,
                "fabricated-deposit step 02 deposit event",
              ),
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
      requireFabricatedReferenceScriptV1({
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
    .addSignerKey(signer.paymentKeyHash);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve fabricated-deposit step 02 layout.",
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

export const submitFabricatedDepositStep02FromFiles = async (
  config: SubmitFabricatedDepositStep02CliConfig & {
    readonly contracts: FabricatedDepositContractsV1;
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
