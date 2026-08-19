/**
 * `fabricated-withdrawal` step-01 submitter (Goal task `Q40`, §9.1 output 8).
 *
 * Nothing in the prepared JSON is trusted. Before a transaction is built this
 * module re-derives, from the **on-chain** state-queue block header:
 *
 * - the counted `withdrawals_root` over the supplied raw PHAS root and the
 *   header's own `withdrawal_count`, which must equal the committed
 *   `withdrawalsRoot`; and
 * - the step-02 handoff state, from the committed leaf bytes rather than from the
 *   prepared file.
 *
 * The membership witness this module puts in the redeemer therefore carries the
 * header's own `root` and `count`; only the raw PHAS root, the leaf bytes and the
 * MPF proof come from the prepared file, and all three are checked against the
 * header before submission. A prepared file that claims a leaf the chain does not
 * commit is rejected locally, before any submission.
 *
 * A withdrawal leaf carries a `Value` map, so this family additionally requires the
 * supplied leaf bytes to be in `serialiseData` form. That is not cosmetic: on chain
 * the membership check re-serialises the typed key and value through
 * `cbor.serialise`, which writes non-empty maps **definite**, whereas Lucid's
 * `Data.to` writes them indefinite. Leaf bytes in the indefinite form would hash to
 * a different MPF leaf than the one the header committed and than the one the script
 * will recompute, so they are refused here rather than left to fail on chain.
 *
 * Deployment resolution is passed in as an already-resolved contracts record:
 * the `fabricatedWithdrawal` catalogue category is not registered yet (that is
 * parent-owned integration work), so this family cannot go through
 * `resolveFaultProofDeploymentContracts`.
 */
import {
  commitCountedRootProgram,
  committedWithdrawalKeyBytesV1,
  committedWithdrawalValueBytesV1,
  FabricatedWithdrawalStep01SpendRedeemer,
  FabricatedWithdrawalStep02Datum,
  type FabricatedWithdrawalStep02State,
  fabricatedWithdrawalStep02StateV1,
  getHeaderV1FromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  type HeaderV1,
  HUB_ORACLE_ASSET_NAME,
  OutputReference,
  Proof,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  ROOT_DOMAINS,
  type RootMembershipProof,
  WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { parseHex, readJsonFile, requireRecord } from "./json-file.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  requireInitialStepDatum,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";

/** Human-readable family label used in every local failure message. */
export const FABRICATED_WITHDRAWAL_CATEGORY_LABEL = "fabricated-withdrawal";

/** One deployed step of the `fabricated-withdrawal` chain. */
export type FabricatedWithdrawalStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * The already-resolved contracts a `fabricated-withdrawal` submission needs.
 *
 * Passed in explicitly rather than resolved from deployment info, because the
 * `fabricatedWithdrawal` catalogue category, its `SupportedFaultProofCategoryName`
 * entry and its chain builder are parent-owned surfaces that land with catalogue
 * registration. Keeping the record explicit also keeps the #609 arity guard in
 * `applyBlueprintParams` the single place where scripts are parameterized.
 */
export type FabricatedWithdrawalContractsV1 = {
  /** Steps 01..04, in order. */
  readonly steps: readonly [
    FabricatedWithdrawalStepContractV1,
    FabricatedWithdrawalStepContractV1,
    FabricatedWithdrawalStepContractV1,
    FabricatedWithdrawalStepContractV1,
  ];
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptAddress: string;
  };
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
  /** Catalogue category id of `fabricatedWithdrawal`, as deployed. */
  readonly categoryId: string;
};

/**
 * Prepared committed-withdrawal inclusion produced by
 * `prepare-fabricated-withdrawal`.
 */
export type SubmitFabricatedWithdrawalInclusion = {
  readonly committedWithdrawalIdCbor: string;
  readonly committedWithdrawalInfoCbor: string;
  readonly withdrawalsPhasRoot: string;
  readonly withdrawalMembershipProof: Proof;
  readonly withdrawalMembershipProofCbor: string;
};

export const parseSubmitFabricatedWithdrawalInclusion = (
  value: unknown,
): SubmitFabricatedWithdrawalInclusion => {
  const record = requireRecord(value, "--withdrawal-inclusion");
  const committedWithdrawalIdCbor = parseHex(
    record.committedWithdrawalIdCbor,
    "--withdrawal-inclusion.committedWithdrawalIdCbor",
  );
  const committedWithdrawalInfoCbor = parseHex(
    record.committedWithdrawalInfoCbor,
    "--withdrawal-inclusion.committedWithdrawalInfoCbor",
  );
  const withdrawalsPhasRoot = parseHex(
    record.withdrawalsPhasRoot,
    "--withdrawal-inclusion.withdrawalsPhasRoot",
    32,
  );
  const withdrawalMembershipProofCbor = parseHex(
    record.withdrawalMembershipProofCbor,
    "--withdrawal-inclusion.withdrawalMembershipProofCbor",
  );
  return {
    committedWithdrawalIdCbor,
    committedWithdrawalInfoCbor,
    withdrawalsPhasRoot,
    withdrawalMembershipProof: Data.from(withdrawalMembershipProofCbor, Proof),
    withdrawalMembershipProofCbor,
  };
};

/** The membership witness the step-01 redeemer carries, plus its handoff state. */
export type FabricatedWithdrawalStep01HandoffV1 = {
  readonly committedWithdrawal: RootMembershipProof<
    OutputReference,
    WithdrawalInfo
  >;
  readonly step02State: FabricatedWithdrawalStep02State;
};

/**
 * Re-derives the step-01 handoff from the **on-chain** header.
 *
 * Fails closed when the supplied raw PHAS root and the header's own
 * `withdrawal_count` do not commit the header's `withdrawals_root`: that is exactly
 * the counted-root equality the L1 step re-establishes, so a witness that cannot
 * satisfy it locally can never satisfy it on chain. Fails closed as well when the
 * supplied leaf bytes are not the `serialiseData` bytes the on-chain re-serialisation
 * will produce.
 */
export const deriveFabricatedWithdrawalStep01HandoffV1 = async ({
  header,
  headerHash,
  inclusion,
}: {
  readonly header: HeaderV1;
  readonly headerHash: string;
  readonly inclusion: SubmitFabricatedWithdrawalInclusion;
}): Promise<FabricatedWithdrawalStep01HandoffV1> => {
  const countedWithdrawalsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.withdrawals,
      phasRoot: inclusion.withdrawalsPhasRoot,
      count: header.withdrawalCount,
    }),
  );
  if (countedWithdrawalsRoot !== header.withdrawalsRoot) {
    throw new Error(
      `--withdrawal-inclusion.withdrawalsPhasRoot does not open the committed withdrawals_root: derived=${countedWithdrawalsRoot}, header=${header.withdrawalsRoot}.`,
    );
  }
  const key = Data.from(inclusion.committedWithdrawalIdCbor, OutputReference);
  const value = Data.from(
    inclusion.committedWithdrawalInfoCbor,
    WithdrawalInfo,
  );
  if (
    committedWithdrawalKeyBytesV1(key) !== inclusion.committedWithdrawalIdCbor
  ) {
    throw new Error(
      `--withdrawal-inclusion.committedWithdrawalIdCbor is not in serialiseData form: the on-chain membership check will hash ${committedWithdrawalKeyBytesV1(key)}, not ${inclusion.committedWithdrawalIdCbor}.`,
    );
  }
  if (
    committedWithdrawalValueBytesV1(value) !==
    inclusion.committedWithdrawalInfoCbor
  ) {
    throw new Error(
      `--withdrawal-inclusion.committedWithdrawalInfoCbor is not in serialiseData form: the on-chain membership check will hash ${committedWithdrawalValueBytesV1(value)}, not ${inclusion.committedWithdrawalInfoCbor}.`,
    );
  }
  const committedWithdrawal: RootMembershipProof<
    OutputReference,
    WithdrawalInfo
  > = {
    domain: ROOT_DOMAINS.withdrawals,
    root: header.withdrawalsRoot,
    phas_root: inclusion.withdrawalsPhasRoot,
    count: header.withdrawalCount,
    key,
    value,
    proof: inclusion.withdrawalMembershipProof,
  };
  const step02State = await Effect.runPromise(
    fabricatedWithdrawalStep02StateV1({
      challengedHeaderHash: headerHash,
      headerStartTime: header.startTime,
      headerEndTime: header.endTime,
      committedWithdrawal,
    }),
  );
  return { committedWithdrawal, step02State };
};

export type SubmitFabricatedWithdrawalStep01CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly withdrawalInclusionPath: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitFabricatedWithdrawalStep01Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly firstStepAddress: string;
  readonly secondStepAddress: string;
  readonly committedWithdrawalIdCbor: string;
  readonly committedWithdrawalInfoHash: string;
  readonly withdrawalsPhasRoot: string;
  readonly committedWithdrawalsRoot: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type FabricatedWithdrawalStep01Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

export const submitFabricatedWithdrawalStep01 = async ({
  lucid,
  contracts,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  withdrawalInclusion,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: FabricatedWithdrawalContractsV1;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly withdrawalInclusion: SubmitFabricatedWithdrawalInclusion;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFabricatedWithdrawalStep01Result> => {
  const parsedThreadOutRef = parseOutRef(threadOutRef, "--thread-out-ref");
  const parsedStateQueueBlockOutRef = parseOutRef(
    stateQueueBlockOutRef,
    "--state-queue-block-out-ref",
  );
  const [threadUtxo, hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parsedThreadOutRef,
      label: "fabricated-withdrawal step-01 computation-thread UTxO",
    }),
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
      outRef: parsedStateQueueBlockOutRef,
      label: "state-queue block UTxO",
    }),
  ]);
  if (threadUtxo.address !== contracts.steps[0].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at fabricated-withdrawal step 01.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: contracts.categoryId,
    categoryLabel: FABRICATED_WITHDRAWAL_CATEGORY_LABEL,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  const stateQueueHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (stateQueueHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw new Error(
      `State-queue block header hash ${stateQueueHeaderHash} does not match computation-thread header hash ${threadToken.fraudulentHeaderHash}.`,
    );
  }

  const stateQueueNodeView = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
  );
  const header = await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(stateQueueNodeView),
  );
  const { committedWithdrawal, step02State } =
    await deriveFabricatedWithdrawalStep01HandoffV1({
      header,
      headerHash: stateQueueHeaderHash,
      inclusion: withdrawalInclusion,
    });

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const referenceInputs = [hubOracleUtxo, stateQueueBlockUtxo];
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step02State },
    FabricatedWithdrawalStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: FabricatedWithdrawalStep01Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "fabricated-withdrawal step 01");
    const layout: FabricatedWithdrawalStep01Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "fabricated-withdrawal step 01",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        "fabricated-withdrawal step 01 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "fabricated-withdrawal step 01 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "fabricated-withdrawal step 01 state-queue node",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            hub_ref_input_index: layout.hubOracleRefInputIndex,
            state_queue_node_ref_input_index:
              layout.stateQueueNodeRefInputIndex,
            committed_withdrawal: committedWithdrawal,
          },
        ],
      },
      FabricatedWithdrawalStep01SpendRedeemer,
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
    .readFrom(referenceInputs)
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(contracts.steps[0].spendingScript);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve fabricated-withdrawal step 01 layout.",
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
    stateQueueBlockOutRef,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    firstStepAddress: contracts.steps[0].spendingScriptAddress,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    committedWithdrawalIdCbor: withdrawalInclusion.committedWithdrawalIdCbor,
    committedWithdrawalInfoHash: step02State.committed_withdrawal_info_hash,
    withdrawalsPhasRoot: withdrawalInclusion.withdrawalsPhasRoot,
    committedWithdrawalsRoot: header.withdrawalsRoot,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    hubOracleRefInputIndex: Number(resolvedLayout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(
      resolvedLayout.stateQueueNodeRefInputIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitFabricatedWithdrawalStep01FromFiles = async (
  config: SubmitFabricatedWithdrawalStep01CliConfig & {
    readonly contracts: FabricatedWithdrawalContractsV1;
  },
): Promise<SubmitFabricatedWithdrawalStep01Result> => {
  const [withdrawalInclusionJson, lucid] = await Promise.all([
    readJsonFile(config.withdrawalInclusionPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitFabricatedWithdrawalStep01({
    lucid,
    contracts: config.contracts,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    withdrawalInclusion: parseSubmitFabricatedWithdrawalInclusion(
      withdrawalInclusionJson,
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};
