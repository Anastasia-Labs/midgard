/**
 * `fabricated-deposit` step-01 submitter (Goal task `Q39`, §9.1 output 8).
 *
 * Nothing in the prepared JSON is trusted. Before a transaction is built this
 * module re-derives, from the **on-chain** state-queue block header:
 *
 * - the counted `deposits_root` over the supplied raw PHAS root and the header's
 *   own `deposit_count`, which must equal the committed `depositsRoot`; and
 * - the step-02 handoff state, from the committed leaf bytes rather than from the
 *   prepared file.
 *
 * The membership witness this module puts in the redeemer therefore carries the
 * header's own `root` and `count`; only the raw PHAS root, the leaf bytes and the
 * MPF proof come from the prepared file, and all three are checked against the
 * header before submission. A prepared file that claims a leaf the chain does not
 * commit is rejected locally, before any submission.
 *
 * Deployment resolution is passed in as an already-resolved contracts record
 * produced from the canonical `fabricatedDeposit` catalogue category.
 */
import {
  commitCountedRootProgram,
  DepositInfo,
  FabricatedDepositStep01SpendRedeemer,
  FabricatedDepositStep02Datum,
  type FabricatedDepositStep02State,
  fabricatedDepositStep02StateV1,
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
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { requireFabricatedReferenceScriptV1 } from "./fabricated-reference-script-v1.js";
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
export const FABRICATED_DEPOSIT_CATEGORY_LABEL = "fabricated-deposit";

/** One deployed step of the `fabricated-deposit` chain. */
export type FabricatedDepositStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * The already-resolved contracts a `fabricated-deposit` submission needs.
 *
 * Passed in explicitly after canonical deployment resolution. This keeps the
 * #609 arity guard in `applyBlueprintParams` the single place where scripts are
 * parameterized.
 */
export type FabricatedDepositContractsV1 = {
  /** Steps 01..04, in order. */
  readonly steps: readonly [
    FabricatedDepositStepContractV1,
    FabricatedDepositStepContractV1,
    FabricatedDepositStepContractV1,
    FabricatedDepositStepContractV1,
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
  /** Catalogue category id of `fabricatedDeposit`, as deployed. */
  readonly categoryId: string;
};

/** Prepared committed-deposit inclusion produced by `prepare-fabricated-deposit`. */
export type SubmitFabricatedDepositInclusion = {
  readonly committedDepositIdCbor: string;
  readonly committedDepositInfoCbor: string;
  readonly depositsPhasRoot: string;
  readonly depositMembershipProof: Proof;
  readonly depositMembershipProofCbor: string;
};

export const parseSubmitFabricatedDepositInclusion = (
  value: unknown,
): SubmitFabricatedDepositInclusion => {
  const record = requireRecord(value, "--deposit-inclusion");
  const committedDepositIdCbor = parseHex(
    record.committedDepositIdCbor,
    "--deposit-inclusion.committedDepositIdCbor",
  );
  const committedDepositInfoCbor = parseHex(
    record.committedDepositInfoCbor,
    "--deposit-inclusion.committedDepositInfoCbor",
  );
  const depositsPhasRoot = parseHex(
    record.depositsPhasRoot,
    "--deposit-inclusion.depositsPhasRoot",
    32,
  );
  const depositMembershipProofCbor = parseHex(
    record.depositMembershipProofCbor,
    "--deposit-inclusion.depositMembershipProofCbor",
  );
  return {
    committedDepositIdCbor,
    committedDepositInfoCbor,
    depositsPhasRoot,
    depositMembershipProof: Data.from(depositMembershipProofCbor, Proof),
    depositMembershipProofCbor,
  };
};

/** The membership witness the step-01 redeemer carries, plus its handoff state. */
export type FabricatedDepositStep01HandoffV1 = {
  readonly committedDeposit: RootMembershipProof<OutputReference, DepositInfo>;
  readonly step02State: FabricatedDepositStep02State;
};

/**
 * Re-derives the step-01 handoff from the **on-chain** header.
 *
 * Fails closed when the supplied raw PHAS root and the header's own
 * `deposit_count` do not commit the header's `deposits_root`: that is exactly the
 * counted-root equality the L1 step re-establishes, so a witness that cannot
 * satisfy it locally can never satisfy it on chain.
 */
export const deriveFabricatedDepositStep01HandoffV1 = async ({
  header,
  headerHash,
  inclusion,
}: {
  readonly header: HeaderV1;
  readonly headerHash: string;
  readonly inclusion: SubmitFabricatedDepositInclusion;
}): Promise<FabricatedDepositStep01HandoffV1> => {
  const countedDepositsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.deposits,
      phasRoot: inclusion.depositsPhasRoot,
      count: header.depositCount,
    }),
  );
  if (countedDepositsRoot !== header.depositsRoot) {
    throw new Error(
      `--deposit-inclusion.depositsPhasRoot does not open the committed deposits_root: derived=${countedDepositsRoot}, header=${header.depositsRoot}.`,
    );
  }
  const key = Data.from(inclusion.committedDepositIdCbor, OutputReference);
  const value = Data.from(inclusion.committedDepositInfoCbor, DepositInfo);
  const committedDeposit: RootMembershipProof<OutputReference, DepositInfo> = {
    domain: ROOT_DOMAINS.deposits,
    root: header.depositsRoot,
    phas_root: inclusion.depositsPhasRoot,
    count: header.depositCount,
    key,
    value,
    proof: inclusion.depositMembershipProof,
  };
  const step02State = await Effect.runPromise(
    fabricatedDepositStep02StateV1({
      challengedHeaderHash: headerHash,
      headerStartTime: header.startTime,
      headerEndTime: header.endTime,
      committedDeposit,
    }),
  );
  return { committedDeposit, step02State };
};

export type SubmitFabricatedDepositStep01CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly depositInclusionPath: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitFabricatedDepositStep01Result = {
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
  readonly committedDepositIdCbor: string;
  readonly committedDepositInfoHash: string;
  readonly depositsPhasRoot: string;
  readonly committedDepositsRoot: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type FabricatedDepositStep01Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

export const submitFabricatedDepositStep01 = async ({
  lucid,
  contracts,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  depositInclusion,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: FabricatedDepositContractsV1;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly depositInclusion: SubmitFabricatedDepositInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFabricatedDepositStep01Result> => {
  const parsedThreadOutRef = parseOutRef(threadOutRef, "--thread-out-ref");
  const parsedStateQueueBlockOutRef = parseOutRef(
    stateQueueBlockOutRef,
    "--state-queue-block-out-ref",
  );
  const [threadUtxo, hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parsedThreadOutRef,
      label: "fabricated-deposit step-01 computation-thread UTxO",
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
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at fabricated-deposit step 01.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: contracts.categoryId,
    categoryLabel: FABRICATED_DEPOSIT_CATEGORY_LABEL,
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
  const { committedDeposit, step02State } =
    await deriveFabricatedDepositStep01HandoffV1({
      header,
      headerHash: stateQueueHeaderHash,
      inclusion: depositInclusion,
    });

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const referenceInputs = [hubOracleUtxo, stateQueueBlockUtxo];
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step02State },
    FabricatedDepositStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: FabricatedDepositStep01Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "fabricated-deposit step 01");
    const layout: FabricatedDepositStep01Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "fabricated-deposit step 01",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        "fabricated-deposit step 01 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "fabricated-deposit step 01 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "fabricated-deposit step 01 state-queue node",
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
            committed_deposit: committedDeposit,
          },
        ],
      },
      FabricatedDepositStep01SpendRedeemer,
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
        expectedScriptHash: contracts.steps[0].spendingScriptHash,
        categoryLabel: FABRICATED_DEPOSIT_CATEGORY_LABEL,
        stepIndex: 0,
      }),
    ])
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve fabricated-deposit step 01 layout.",
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
    committedDepositIdCbor: depositInclusion.committedDepositIdCbor,
    committedDepositInfoHash: step02State.committed_deposit_info_hash,
    depositsPhasRoot: depositInclusion.depositsPhasRoot,
    committedDepositsRoot: header.depositsRoot,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    hubOracleRefInputIndex: Number(resolvedLayout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(
      resolvedLayout.stateQueueNodeRefInputIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitFabricatedDepositStep01FromFiles = async (
  config: SubmitFabricatedDepositStep01CliConfig & {
    readonly contracts: FabricatedDepositContractsV1;
    readonly referenceScriptUtxo: UTxO;
  },
): Promise<SubmitFabricatedDepositStep01Result> => {
  const [depositInclusionJson, lucid] = await Promise.all([
    readJsonFile(config.depositInclusionPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitFabricatedDepositStep01({
    lucid,
    contracts: config.contracts,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    depositInclusion:
      parseSubmitFabricatedDepositInclusion(depositInclusionJson),
    referenceScriptUtxo: config.referenceScriptUtxo,
    awaitConfirmation: config.awaitConfirmation,
  });
};
