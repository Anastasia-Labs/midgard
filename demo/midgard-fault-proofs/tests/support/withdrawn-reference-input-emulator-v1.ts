import {
  computeMidgardNativeTxIdV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardNativeTxCompactV1,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { ResolvedProverSigner } from "../../src/runtime.js";
import {
  nativeTxFromCoreCompact,
  selectFeeInput,
} from "../../src/submit-step-01.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../../src/transition-trace/phas.js";
import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../../src/tx-layout.js";
import type { WithdrawnReferenceInputContractsV1 } from "../../src/withdrawn-reference-input/contracts-v1.js";
import {
  type PreparedWithdrawnReferenceInputV1,
  prepareWithdrawnReferenceInputV1,
} from "../../src/withdrawn-reference-input/prepare-withdrawn-reference-input-v1.js";
import {
  requireWithdrawnReferenceInputReferenceScriptV1,
  requireWithdrawnReferenceInputThreadUtxoV1,
} from "../../src/withdrawn-reference-input/submit-common-v1.js";
import { decodingSubjectTransactionV1 } from "./native-script-decoding-emulator-v1.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarnessV1,
  makeHeader,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export const WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF_V1: SDK.MidgardTxInput = {
  tx_id: "ab".repeat(32),
  output_index: 0n,
};

export const withdrawnReferenceInputInfoV1 = ({
  outRef = WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF_V1,
  validity = "WithdrawalIsValid",
}: {
  readonly outRef?: SDK.MidgardTxInput;
  readonly validity?: SDK.WithdrawalValidity;
} = {}): SDK.WithdrawalInfo => ({
  body: {
    l2_outref: {
      transactionId: outRef.tx_id,
      outputIndex: outRef.output_index,
    },
    l2_owner: "55".repeat(28),
    l2_value: new Map(),
    l1_address: {
      paymentCredential: { PublicKeyCredential: ["66".repeat(28)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: ["77".repeat(32), "88".repeat(64)],
  validity,
});

export type WithdrawnReferenceInputEmulatorHarnessV1 = Awaited<
  ReturnType<typeof makeWithdrawnReferenceInputEmulatorHarnessV1>
>;

export const makeWithdrawnReferenceInputEmulatorHarnessV1 = async () => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realWithdrawnReferenceInput: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const family = harness.contracts.withdrawnReferenceInput;
  const category = harness.catalogue.categories.withdrawnReferenceInput;
  if (family === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the withdrawn-reference-input contracts/category",
    );
  }
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.withdrawnReferenceInput
  ) {
    throw new Error("Unexpected withdrawn-reference-input category id");
  }
  return { ...harness, family, category };
};

export const publishWithdrawnReferenceInputReferenceScriptsV1 = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: WithdrawnReferenceInputContractsV1;
}): Promise<readonly [UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `withdrawn-reference-input step-0${(index + 1).toString()}`,
      oversized: true,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO, UTxO];
};

export type WithdrawnReferenceInputScenarioV1 = {
  readonly header: SDK.HeaderV1;
  readonly headerHash: string;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly blockTxs: readonly [
    { readonly nodeTxId: string; readonly txCbor: string },
  ];
  readonly withdrawals: readonly [SDK.WithdrawalEvent];
  readonly prepared: PreparedWithdrawnReferenceInputV1;
};

export const setupWithdrawnReferenceInputScenarioV1 = async ({
  harness,
  withdrawalInfo = withdrawnReferenceInputInfoV1(),
}: {
  readonly harness: WithdrawnReferenceInputEmulatorHarnessV1;
  readonly withdrawalInfo?: SDK.WithdrawalInfo;
}): Promise<WithdrawnReferenceInputScenarioV1> => {
  const referenceInputCbor = SDK.encodeMidgardTxInputCanonicalV1(
    WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF_V1,
  );
  const nativeTx = decodingSubjectTransactionV1({
    referenceInputCbors: [referenceInputCbor],
    fee: 1_000n,
  });
  const txId = computeMidgardNativeTxIdV1(nativeTx).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompactV1(nativeTx.compact);
  const fullCbor = encodeMidgardNativeTxCanonicalV1(nativeTx);
  const countedTransactions = await buildCountedRoot(
    SDK.ROOT_DOMAINS.transactionsV1,
    [{ key: Buffer.from(txId, "hex"), value: compactCbor }],
  );
  const withdrawal: SDK.WithdrawalEvent = {
    id: { transactionId: "42".repeat(32), outputIndex: 0n },
    info: withdrawalInfo,
  };
  const countedWithdrawals = await buildCountedRoot(
    SDK.ROOT_DOMAINS.withdrawals,
    [
      {
        key: Buffer.from(
          SDK.committedWithdrawalKeyBytesV1(withdrawal.id),
          "hex",
        ),
        value: Buffer.from(
          SDK.committedWithdrawalValueBytesV1(withdrawal.info),
          "hex",
        ),
      },
    ],
  );
  const funderKeyHash = await funderPaymentKeyHash(harness.funderLucid);
  const startTime =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header: SDK.HeaderV1 = {
    ...makeHeader(funderKeyHash, startTime, countedTransactions.root, 1n),
    withdrawalsRoot: countedWithdrawals.root,
    withdrawalCount: 1n,
    totalEventCount: 2n,
    transitionStepCount: 2n,
    validationTraceCount: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  const blockTxs = [
    { nodeTxId: txId, txCbor: fullCbor.toString("hex") },
  ] as const;
  const withdrawals = [withdrawal] as const;
  const prepared = await prepareWithdrawnReferenceInputV1({
    header,
    blockTxs,
    withdrawals,
    accusedTxId: txId,
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  return { header, headerHash, setup, blockTxs, withdrawals, prepared };
};

/**
 * Commits an arbitrary withdrawal leaf while retaining valid tx-inclusion and
 * membership material. Adversarial tests use this to bypass the honest
 * classifier and reach the validator's exact refusal checks.
 */
export const setupWithdrawnReferenceInputUncheckedScenarioV1 = async ({
  harness,
  withdrawalInfo,
}: {
  readonly harness: WithdrawnReferenceInputEmulatorHarnessV1;
  readonly withdrawalInfo: SDK.WithdrawalInfo;
}) => {
  const referenceInputs = [
    WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF_V1,
  ] as const;
  const nativeTx = decodingSubjectTransactionV1({
    referenceInputCbors: [
      SDK.encodeMidgardTxInputCanonicalV1(referenceInputs[0]),
    ],
    fee: 1_000n,
  });
  const txId = computeMidgardNativeTxIdV1(nativeTx).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompactV1(nativeTx.compact);
  const countedTransactions = await buildCountedRoot(
    SDK.ROOT_DOMAINS.transactionsV1,
    [{ key: Buffer.from(txId, "hex"), value: compactCbor }],
  );
  const txProof = await keyValuePhasProof(
    { ...countedTransactions, root: countedTransactions.phasRoot },
    Buffer.from(txId, "hex"),
    compactCbor,
  );
  const withdrawal: SDK.WithdrawalEvent = {
    id: { transactionId: "42".repeat(32), outputIndex: 0n },
    info: withdrawalInfo,
  };
  const withdrawalKey = Buffer.from(
    SDK.committedWithdrawalKeyBytesV1(withdrawal.id),
    "hex",
  );
  const withdrawalValue = Buffer.from(
    SDK.committedWithdrawalValueBytesV1(withdrawal.info),
    "hex",
  );
  const countedWithdrawals = await buildCountedRoot(
    SDK.ROOT_DOMAINS.withdrawals,
    [{ key: withdrawalKey, value: withdrawalValue }],
  );
  const withdrawalProof = await keyValuePhasProof(
    { ...countedWithdrawals, root: countedWithdrawals.phasRoot },
    withdrawalKey,
    withdrawalValue,
  );
  const funderKeyHash = await funderPaymentKeyHash(harness.funderLucid);
  const startTime =
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1;
  const header: SDK.HeaderV1 = {
    ...makeHeader(funderKeyHash, startTime, countedTransactions.root, 1n),
    withdrawalsRoot: countedWithdrawals.root,
    withdrawalCount: 1n,
    totalEventCount: 2n,
    transitionStepCount: 2n,
    validationTraceCount: 1n,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header,
  });
  const blockTxs = [
    {
      nodeTxId: txId,
      txCbor: encodeMidgardNativeTxCanonicalV1(nativeTx).toString("hex"),
    },
  ] as const;
  const withdrawals = [withdrawal] as const;
  return {
    header,
    setup,
    referenceInputs,
    txInclusion: {
      nativeTxId: txId,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compactCbor.toString("hex"),
      transactionsPhasRoot: countedTransactions.phasRoot,
      txMembershipProof: txProof,
      txMembershipProofCbor: Data.to(txProof, SDK.Proof),
    },
    withdrawal,
    blockTxs,
    withdrawals,
    withdrawalMembership: {
      domain: SDK.ROOT_DOMAINS.withdrawals,
      root: countedWithdrawals.root,
      phas_root: countedWithdrawals.phasRoot,
      count: 1n,
      key: withdrawal.id,
      value: withdrawal.info,
      proof: withdrawalProof,
    } satisfies SDK.WithdrawalSourceMembershipProof,
  };
};

export type RawWithdrawnReferenceInputStepLayoutV1 = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

const RawWithdrawnCancelRedeemerSchema = SDK.faultProofStepRedeemerSchema(
  Data.Any(),
);
type RawWithdrawnCancelRedeemer = Data.Static<
  typeof RawWithdrawnCancelRedeemerSchema
>;
const RawWithdrawnCancelRedeemer =
  RawWithdrawnCancelRedeemerSchema as unknown as RawWithdrawnCancelRedeemer;

/** Test-only advancement that bypasses the honest step-02 guards. */
export const submitRawWithdrawnReferenceInputStep02V1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nextDatumCbor,
  buildRedeemer,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawnReferenceInputContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nextDatumCbor: string;
  readonly buildRedeemer: (
    layout: RawWithdrawnReferenceInputStepLayoutV1,
  ) => string;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireWithdrawnReferenceInputThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatumCbor,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, "raw withdrawn step 02");
    return buildRedeemer({
      inputIndex: SDK.requireInputIndex(
        ctx,
        threadUtxo,
        "raw withdrawn step 02",
      ),
      outputIndex: SDK.requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "raw withdrawn step 02 output",
      ),
    });
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const reference = requireWithdrawnReferenceInputReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    stepIndex: 1,
  });
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([reference])
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: nextDatumCbor },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/** Test-only finalizer that sends an arbitrary membership proof on-chain. */
export const submitRawWithdrawnReferenceInputStep03V1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  withdrawalMembership,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawnReferenceInputContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly withdrawalMembership: SDK.WithdrawalSourceMembershipProof;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireWithdrawnReferenceInputThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 2,
      threadOutRef,
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
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  const spendRedeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, "raw withdrawn step 03");
    return Data.to(
      {
        Continue: [
          {
            input_index: SDK.requireInputIndex(
              ctx,
              threadUtxo,
              "raw withdrawn step 03",
            ),
            output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "raw withdrawn step 03 output",
            ),
            fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw withdrawn step 03 fraud proof",
            ),
            withdrawal_membership: withdrawalMembership,
          },
        ],
      },
      SDK.WithdrawnReferenceInputStep03SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw withdrawn step 03 thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      SDK.FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const fraudMint = ((ctx) =>
    Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "raw withdrawn step 03 thread burn",
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const reference = requireWithdrawnReferenceInputReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    stepIndex: 2,
  });
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([reference])
    .mintAssets({ [threadToken.unit]: -1n }, threadBurn)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudMint)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/** Test-only cancellation signed by an arbitrary wallet. */
export const submitRawWithdrawnReferenceInputCancelV1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  stepIndex,
  threadOutRef,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawnReferenceInputContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: 0 | 1 | 2;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireWithdrawnReferenceInputThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex,
      threadOutRef,
    });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const spendRedeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, "raw withdrawn cancel");
    return Data.to(
      {
        Cancel: {
          input_index: SDK.requireInputIndex(
            ctx,
            threadUtxo,
            "raw withdrawn cancel",
          ),
          computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
            ctx,
            contracts.computationThread.policyId,
            "raw withdrawn cancel thread burn",
          ),
        },
      },
      RawWithdrawnCancelRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurn = ((ctx) => {
    SDK.requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw withdrawn cancel thread burn",
    );
    return Data.to(
      {
        BurnForCancellation: {
          burning_token_asset_name: threadToken.assetName,
        },
      },
      SDK.FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const reference = requireWithdrawnReferenceInputReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    stepIndex,
  });
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([reference])
    .mintAssets({ [threadToken.unit]: -1n }, threadBurn)
    .addSignerKey(signer.paymentKeyHash)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
