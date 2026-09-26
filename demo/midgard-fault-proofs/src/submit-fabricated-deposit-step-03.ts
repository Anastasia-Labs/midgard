import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
/** Reopen payload and original Value authenticated by the history-capture stage.
 * Pointer churn cannot change the retained facts. No operator archive supplies
 * authority: every opening must match the commitment in the authentic thread. */
import {
  depositInfoCommitmentCbor,
  FabricatedDepositAuthenticContentOpening,
  type FabricatedDepositFault,
  FabricatedDepositStep03Datum,
  FabricatedDepositStep03SpendRedeemer,
  type FabricatedDepositStep03State,
  FabricatedDepositStep04Datum,
  type FabricatedDepositStep04State,
  fabricatedDepositStep04State,
  isFabricatedDepositFault,
  opensEventHistoryCommitmentCbor,
  OutputReference,
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
import { Effect } from "effect";

import { fabricatedProofValidity } from "./fabricated-proof-validity.js";
import { requireFabricatedReferenceScript } from "./fabricated-reference-script.js";
import { parseHex, readJsonFile, requireRecord } from "./json-file.js";
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

/** The step-03 handoff: the opening the redeemer carries and the fault it yields. */
export type FabricatedDepositStep03Handoff = {
  readonly opening: FabricatedDepositAuthenticContentOpening;
  readonly openingCbor: string;
  readonly fault: FabricatedDepositFault;
  readonly step04State: FabricatedDepositStep04State;
};

export const requireFabricatedDepositStep03Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): FabricatedDepositStep03State => {
  if (threadUtxo.datum === null || threadUtxo.datum === undefined) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} has no inline fabricated-deposit step-03 datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, FabricatedDepositStep03Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} belongs to fraud prover ${datum.fraud_prover}, not ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} carries no fabricated-deposit step-03 state.`,
    );
  }
  return datum.data;
};

/** Exact twin of the on-chain opening and fault-classification predicates. */
export const deriveFabricatedDepositStep03Handoff = async ({
  state,
  openingCbor,
}: {
  readonly state: FabricatedDepositStep03State;
  readonly openingCbor?: string;
}): Promise<FabricatedDepositStep03Handoff> => {
  const opening =
    openingCbor === undefined
      ? "NoAuthenticContent"
      : Data.from(openingCbor, FabricatedDepositAuthenticContentOpening);
  const retainedCbor =
    openingCbor ??
    Data.to("NoAuthenticContent", FabricatedDepositAuthenticContentOpening);
  let fault: FabricatedDepositFault;
  if (state.verdict === "DepositIdentityAbsent") {
    if (opening !== "NoAuthenticContent")
      throw new Error("Authenticated absence admits no retained event opening");
    fault = "NonexistentDepositIdentity";
  } else {
    if (opening === "NoAuthenticContent")
      throw new Error(
        "Observed Order requires its retained payload and original Value",
      );
    const { commitment } = state.verdict.DepositEventObserved;
    const { payload } = opening.RetainedEventData;
    if (
      commitment.kind !== "Deposit" ||
      Data.to(commitment.event_id, OutputReference) !==
        Data.to(state.committed_deposit_id, OutputReference) ||
      !opensEventHistoryCommitmentCbor(
        commitment,
        plutusConstrFieldCbor(retainedCbor, [0]),
        plutusConstrFieldCbor(retainedCbor, [1]),
      )
    )
      throw new Error(
        "Retained event opening does not match the authenticated history commitment",
      );
    if (!("DepositPayload" in payload))
      throw new Error("Retained payload has the wrong event kind");
    const event_inclusion_time = commitment.inclusion_time;
    if (
      !(
        state.header_start_time < event_inclusion_time &&
        event_inclusion_time <= state.header_end_time
      )
    ) {
      fault = { IneligibleDepositEvent: { event_inclusion_time } };
    } else {
      const authenticHash = await Effect.runPromise(
        depositInfoCommitmentCbor(
          plutusConstrFieldCbor(retainedCbor, [0, 0, 1]),
        ),
      );
      if (authenticHash === state.committed_deposit_info_hash)
        throw new Error(
          "Authentic eligible event content matches the header commitment",
        );
      fault = {
        MismatchedDepositContent: {
          committed_deposit_info_hash: state.committed_deposit_info_hash,
          authentic_deposit_info_hash: authenticHash,
          event_inclusion_time,
        },
      };
    }
  }
  const step04State = fabricatedDepositStep04State(state, fault);
  if (!isFabricatedDepositFault(step04State))
    throw new Error("Retained event does not establish the classified fault");
  return { opening, openingCbor: retainedCbor, fault, step04State };
};

export type SubmitFabricatedDepositStep03CliConfig = SubmitProviderConfig & {
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly authenticContentPath?: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitFabricatedDepositAuthenticContent = {
  readonly openingCbor: string | null;
};

export const parseSubmitFabricatedDepositAuthenticContent = (
  value: unknown,
): SubmitFabricatedDepositAuthenticContent => {
  const record = requireRecord(value, "fabricated-deposit authentic content");
  if (Object.keys(record).length !== 1 || !("openingCbor" in record))
    throw new Error("Authentic content requires exactly openingCbor");
  return {
    openingCbor:
      record["openingCbor"] === null
        ? null
        : parseHex(
            record["openingCbor"],
            "fabricated-deposit authentic content openingCbor",
          ),
  };
};

export type SubmitFabricatedDepositStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  readonly fourthStepAddress: string;
  readonly fault: FabricatedDepositFault;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type FabricatedDepositStep03Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

export const submitFabricatedDepositStep03 = async ({
  lucid,
  contracts,
  signer,
  threadOutRef,
  openingCbor,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
  now = Date.now,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: FabricatedDepositContracts;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly openingCbor?: string;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  readonly now?: () => number;
}): Promise<SubmitFabricatedDepositStep03Result> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "fabricated-deposit step-03 computation-thread UTxO",
  });
  if (threadUtxo.address !== contracts.steps[2].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at fabricated-deposit step 03.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: contracts.categoryId,
    categoryLabel: FABRICATED_DEPOSIT_CATEGORY_LABEL,
  });
  const state = requireFabricatedDepositStep03Datum({ threadUtxo, signer });
  const handoff = await deriveFabricatedDepositStep03Handoff({
    state,
    openingCbor,
  });

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step04Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: handoff.step04State },
    FabricatedDepositStep04Datum,
  );
  const step04OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
    datum: step04Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: FabricatedDepositStep03Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "fabricated-deposit step 03");
    const layout: FabricatedDepositStep03Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "fabricated-deposit step 03",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step04OutputMatches,
        "fabricated-deposit step 03 output",
      ),
    };
    resolvedLayout = layout;
    return replacePlutusConstrFieldCbor(
      Data.to(
        {
          Continue: [
            {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              authentic_content: handoff.opening,
            },
          ],
        },
        FabricatedDepositStep03SpendRedeemer,
      ),
      [0, 2],
      handoff.openingCbor,
    );
  }) satisfies BuildTxWithRedeemer;

  const validity = fabricatedProofValidity(state.header_end_time, now());
  const tx = lucid
    .newTx()
    .validFrom(validity.validFrom)
    .validTo(validity.validTo)
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      requireFabricatedReferenceScript({
        utxo: referenceScriptUtxo,
        expectedScriptHash: contracts.steps[2].spendingScriptHash,
        categoryLabel: FABRICATED_DEPOSIT_CATEGORY_LABEL,
        stepIndex: 2,
      }),
    ])
    .pay.ToContract(
      contracts.steps[3].spendingScriptAddress,
      { kind: "inline", value: step04Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve fabricated-deposit step 03 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof fabricated-deposit step-03",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.steps[2].spendingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `fabricated-deposit step-03 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    thirdStepAddress: contracts.steps[2].spendingScriptAddress,
    fourthStepAddress: contracts.steps[3].spendingScriptAddress,
    fault: handoff.fault,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitFabricatedDepositStep03FromFiles = async (
  config: SubmitFabricatedDepositStep03CliConfig & {
    readonly contracts: FabricatedDepositContracts;
    readonly referenceScriptUtxo: UTxO;
  },
): Promise<SubmitFabricatedDepositStep03Result> => {
  const [authenticContentJson, lucid] = await Promise.all([
    config.authenticContentPath === undefined
      ? Promise.resolve(undefined)
      : readJsonFile(config.authenticContentPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitFabricatedDepositStep03({
    lucid,
    contracts: config.contracts,
    signer,
    threadOutRef: config.threadOutRef,
    openingCbor:
      authenticContentJson === undefined
        ? undefined
        : (parseSubmitFabricatedDepositAuthenticContent(authenticContentJson)
            .openingCbor ?? undefined),
    referenceScriptUtxo: config.referenceScriptUtxo,
    awaitConfirmation: config.awaitConfirmation,
  });
};
