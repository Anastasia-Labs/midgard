import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxCompactV1,
  encodeMidgardNativeTxCompactV1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonicalV1,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type CommittedFieldClaimV1,
  CommittedFieldShapeStep01SpendRedeemer,
  CommittedFieldShapeStep02Datum,
  CommittedFieldShapeStep02SpendRedeemer,
  type CommittedFieldShapeStep02State,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  HUB_ORACLE_ASSET_NAME,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { CommittedFieldShapeContractsV1 } from "../../src/committed-field-shape/contracts-v1.js";
import type { PreparedCommittedFieldShapeV1 } from "../../src/committed-field-shape/prepare-committed-field-shape-v1.js";
import {
  type CommittedFieldShapeCatalogueCategoryV1,
  requireCommittedFieldShapeThreadUtxoV1,
} from "../../src/committed-field-shape/submit-common-v1.js";
import type { RemoveFraudulentBlockExplicitCategory } from "../../src/remove-fraudulent-block.js";
import {
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveProverSigner,
} from "../../src/runtime.js";
import {
  nativeTxFromCoreCompact,
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireInitialStepDatum,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../../src/submit-step-01.js";
import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../../src/tx-layout.js";
import { setupFraudulentBlockV1 } from "./submit-init-emulator-fixtures.js";
import {
  COMMITTED_FIELD_SHAPE_REMOVAL_DEPLOYMENT_ENTRY_V1,
  COMMITTED_FIELD_SHAPE_TEST_CATEGORY_ID_V1,
  makeFaultProofEmulatorHarnessV1,
  makeNativeTx,
  network,
  publishPlainReferenceScriptUtxo,
} from "./submit-init-emulator-shared.js";

const EMPTY = Buffer.from("80", "hex");

export type CommittedFieldShapeEmulatorHarnessV1 = Awaited<
  ReturnType<typeof makeCommittedFieldShapeEmulatorHarnessV1>
>;

/** Builds the real two-step chain plus a third, initially empty, wallet. */
export const makeCommittedFieldShapeEmulatorHarnessV1 = async () => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realCommittedFieldShape: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const committedFieldShape = harness.contracts.committedFieldShape;
  const category = harness.catalogue.extraCategories.committedFieldShape as
    | CommittedFieldShapeCatalogueCategoryV1
    | undefined;
  if (committedFieldShape === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the committed-field-shape contracts/category",
    );
  }
  if (category.categoryId !== COMMITTED_FIELD_SHAPE_TEST_CATEGORY_ID_V1) {
    throw new Error("Unexpected committed-field-shape test category id");
  }
  const outsider = generateEmulatorAccount({ lovelace: 0n });
  const outsiderLucid = await Lucid(harness.emulator, "Custom");
  outsiderLucid.selectWallet.fromSeed(outsider.seedPhrase);
  const outsiderSigner = resolveProverSigner({
    network,
    walletSeedPhrase: outsider.seedPhrase,
  });
  return {
    ...harness,
    committedFieldShape,
    category,
    outsiderLucid,
    outsiderSigner,
  };
};

export const committedFieldShapeRemovalCategoryV1 = (
  harness: CommittedFieldShapeEmulatorHarnessV1,
): RemoveFraudulentBlockExplicitCategory => ({
  name: "committedFieldShape",
  categoryId: harness.category.categoryId,
  firstStepDeploymentEntry: COMMITTED_FIELD_SHAPE_REMOVAL_DEPLOYMENT_ENTRY_V1,
  firstStepScriptHash: harness.committedFieldShape.steps[0].spendingScriptHash,
  fraudProof: {
    policyId: harness.committedFieldShape.fraudProof.policyId,
    spendingScriptHash: harness.contracts.fraudProof.spendingScriptHash,
    spendingScriptAddress:
      harness.committedFieldShape.fraudProof.spendingScriptAddress,
  },
});

/** Both step validators are always published and consumed by reference. */
export const publishCommittedFieldShapeReferenceScriptsV1 = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: CommittedFieldShapeContractsV1;
}): Promise<readonly [UTxO, UTxO]> => {
  const publications: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script: step.spendingScript,
      label: `committed-field-shape step-0${(index + 1).toString()}`,
      oversized: true,
    });
    publications.push(utxo);
  }
  return publications as unknown as readonly [UTxO, UTxO];
};

export type CommittedFieldShapeScenarioKindV1 =
  | "wrong-stride"
  | "honest"
  | "non-envelope";

export type CommittedFieldShapeScenarioV1 = {
  readonly kind: CommittedFieldShapeScenarioKindV1;
  readonly canonicalTx: MidgardNativeTxCanonicalV1 | null;
  readonly fullTx: MidgardNativeTxFullV1 | null;
  readonly nativeTxId: string;
  readonly compactCbor: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Buffer;
  readonly inclusion: SubmitStep01TxInclusion;
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly setup: Awaited<ReturnType<typeof setupFraudulentBlockV1>>;
};

const invalidNonEnvelopeCanonicalV1 = (): MidgardNativeTxCanonicalV1 => ({
  version: MIDGARD_NATIVE_TX_V1_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY,
    referenceInputsPreimageCbor: EMPTY,
    outputsPreimageCbor: Buffer.from("8041", "hex"),
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY,
    requiredSignersPreimageCbor: EMPTY,
    mintPreimageCbor: EMPTY,
    scriptIntegrityHash: Buffer.alloc(32),
    auxiliaryDataHash: Buffer.alloc(32),
    networkId: 0n,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY,
    scriptTxWitsPreimageCbor: EMPTY,
    redeemerTxWitsPreimageCbor: EMPTY,
  },
});

const scenarioMaterialV1 = (
  kind: CommittedFieldShapeScenarioKindV1,
): {
  readonly canonicalTx: MidgardNativeTxCanonicalV1 | null;
  readonly fullTx: MidgardNativeTxFullV1 | null;
  readonly compact: ReturnType<typeof deriveMidgardNativeTxCompactV1>;
  readonly fieldIndex: number;
  readonly committedPreimage: Buffer;
} => {
  if (kind === "non-envelope") {
    const invalid = invalidNonEnvelopeCanonicalV1();
    return {
      canonicalTx: null,
      fullTx: null,
      compact: deriveMidgardNativeTxCompactV1(
        invalid.body,
        invalid.witnessSet,
        invalid.validity,
      ),
      fieldIndex: 2,
      committedPreimage: Buffer.from(invalid.body.outputsPreimageCbor),
    };
  }
  const spendItem =
    kind === "wrong-stride"
      ? Buffer.from("deadbeef", "hex")
      : Buffer.alloc(38, 0xa5);
  const fullTx = makeNativeTx({ spendInputCbors: [spendItem], fee: 7n });
  return {
    canonicalTx: fullTx,
    fullTx,
    compact: fullTx.compact,
    fieldIndex: 0,
    committedPreimage: Buffer.from(fullTx.body.spendInputsPreimageCbor),
  };
};

/** Commits the chosen real shape as a one-leaf transactions MPF and block. */
export const setupCommittedFieldShapeScenarioV1 = async ({
  harness,
  kind,
}: {
  readonly harness: CommittedFieldShapeEmulatorHarnessV1;
  readonly kind: CommittedFieldShapeScenarioKindV1;
}): Promise<CommittedFieldShapeScenarioV1> => {
  const material = scenarioMaterialV1(kind);
  const nativeTxId = computeMidgardNativeTxIdV1(material.compact).toString(
    "hex",
  );
  const compact = encodeMidgardNativeTxCompactV1(material.compact);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(Buffer.from(nativeTxId, "hex"), compact);
  const proof = await trie.prove(Buffer.from(nativeTxId, "hex"));
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const inclusion: SubmitStep01TxInclusion = {
    nativeTxId,
    nativeTx: nativeTxFromCoreCompact(material.compact),
    nativeTxCompactCbor: compact.toString("hex"),
    transactionsPhasRoot: transactionsRoot,
    txMembershipProof: Data.from(proof.toCBOR().toString("hex"), SDK.Proof),
    txMembershipProofCbor: proof.toCBOR().toString("hex"),
  };
  const setup = await setupFraudulentBlockV1({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: harness.catalogue,
    fixture: { transactionsRoot, l2TransactionCount: 1n },
  });
  return {
    kind,
    canonicalTx: material.canonicalTx,
    fullTx: material.fullTx,
    nativeTxId,
    compactCbor: compact.toString("hex"),
    fieldIndex: material.fieldIndex,
    committedPreimage: material.committedPreimage,
    inclusion,
    transactionsRoot,
    l2TransactionCount: 1n,
    setup,
  };
};

/** Funds the outsider after setup has consumed the parameterizing nonce. */
export const fundCommittedFieldShapeOutsiderV1 = async (
  harness: CommittedFieldShapeEmulatorHarnessV1,
): Promise<void> => {
  const outsiderAddress = await harness.outsiderLucid.wallet().address();
  const unsigned = await harness.funderLucid
    .newTx()
    .pay.ToAddress(outsiderAddress, { lovelace: 1_000_000_000n })
    .pay.ToAddress(outsiderAddress, { lovelace: 1_000_000_000n })
    .complete();
  const signed = await unsigned.sign.withWallet().complete();
  await harness.funderLucid.awaitTx(await signed.submit());
};

/**
 * Raw step-01 with no evidence/verdict guard. Used only to prove the validator
 * itself refuses fabricated verdicts and uncommitted preimages.
 */
export const submitRawCommittedFieldShapeStep01V1 = async ({
  harness,
  threadOutRef,
  scenario,
  claim,
  forwardedState,
  referenceScriptUtxo,
}: {
  readonly harness: CommittedFieldShapeEmulatorHarnessV1;
  readonly threadOutRef: string;
  readonly scenario: CommittedFieldShapeScenarioV1;
  readonly claim: CommittedFieldClaimV1;
  readonly forwardedState: CommittedFieldShapeStep02State;
  readonly referenceScriptUtxo: UTxO;
}): Promise<{ readonly txHash: string; readonly nextThreadOutRef: string }> => {
  const { threadUtxo, threadToken } =
    await requireCommittedFieldShapeThreadUtxoV1({
      lucid: harness.proverLucid,
      contracts: harness.committedFieldShape,
      categoryId: harness.category.categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  requireInitialStepDatum({ threadUtxo, signer: harness.proverSigner });
  const [stateQueueBlockUtxo, hubOracleUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid: harness.proverLucid,
      outRef: parseOutRef(
        scenario.setup.fraudulentBlockOutRef,
        "raw committed-field-shape block",
      ),
      label: "raw committed-field-shape block",
    }),
    requireSingletonUtxo({
      lucid: harness.proverLucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(harness.committedFieldShape.hubOraclePolicyId),
      ),
      unit: toUnit(
        harness.committedFieldShape.hubOraclePolicyId,
        HUB_ORACLE_ASSET_NAME,
      ),
      label: "raw committed-field-shape hub oracle",
    }),
  ]);
  const observedHeader = resolveFraudulentHeaderHash({
    stateQueuePolicyId: harness.committedFieldShape.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (observedHeader !== threadToken.fraudulentHeaderHash) {
    throw new Error("raw step-01 scenario/thread header mismatch");
  }
  harness.proverSigner.selectWallet(harness.proverLucid);
  const feeInput = selectFeeInput(
    await harness.proverLucid.wallet().getUtxos(),
  );
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(
      harness.realBlueprint,
      PHAS_MEMBERSHIP_WITHDRAW_TITLE,
    ),
  };
  const phasAddress = phasMembershipRewardAddress(network, phasScript);
  const step02Datum = Data.to(
    {
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: forwardedState,
    },
    CommittedFieldShapeStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: harness.committedFieldShape.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw committed-field-shape step-01",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "raw committed-field-shape step-01",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "raw committed-field-shape step-01 output",
    );
    return Data.to(
      {
        Continue: [
          {
            inclusion: {
              RedeemerCarriedInclusion: [
                {
                  input_index: inputIndex,
                  output_index: outputIndex,
                  hub_ref_input_index: requireReferenceInputIndex(
                    ctx,
                    hubOracleUtxo,
                    "raw committed-field-shape hub oracle",
                  ),
                  state_queue_node_ref_input_index: requireReferenceInputIndex(
                    ctx,
                    stateQueueBlockUtxo,
                    "raw committed-field-shape block",
                  ),
                  native_tx_id: scenario.inclusion.nativeTxId,
                  native_tx_compact_cbor:
                    scenario.inclusion.nativeTxCompactCbor,
                  transactions_phas_root:
                    scenario.inclusion.transactionsPhasRoot,
                  tx_membership_proof: scenario.inclusion.txMembershipProof,
                  inclusion_proof_script_withdraw_redeemer_index:
                    requireWithdrawalRedeemerIndex(
                      ctx,
                      phasAddress,
                      "raw committed-field-shape PHAS membership",
                    ),
                },
              ],
            },
            claim,
          },
        ],
      },
      CommittedFieldShapeStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const unsigned = await harness.proverLucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([hubOracleUtxo, stateQueueBlockUtxo, referenceScriptUtxo])
    .withdraw(
      phasAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: scenario.inclusion.transactionsPhasRoot,
        keyBytes: scenario.inclusion.nativeTxId,
        valueBytes: scenario.inclusion.nativeTxCompactCbor,
        membershipProofCbor: scenario.inclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      harness.committedFieldShape.steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(harness.proverSigner.paymentKeyHash)
    .attach.WithdrawalValidator(phasScript)
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined) {
    throw new Error("raw step-01 layout did not resolve");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/** Raw finalization without the production submitter's predicate guard. */
export const submitRawCommittedFieldShapeStep02V1 = async ({
  harness,
  threadOutRef,
  referenceScriptUtxo,
}: {
  readonly harness: CommittedFieldShapeEmulatorHarnessV1;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireCommittedFieldShapeThreadUtxoV1({
      lucid: harness.proverLucid,
      contracts: harness.committedFieldShape,
      categoryId: harness.category.categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  harness.proverSigner.selectWallet(harness.proverLucid);
  const feeInput = selectFeeInput(
    await harness.proverLucid.wallet().getUtxos(),
  );
  const fraudProofUnit = toUnit(
    harness.committedFieldShape.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: harness.proverSigner.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: harness.committedFieldShape.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw committed-field-shape step-02",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "raw committed-field-shape step-02",
            ),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "raw committed-field-shape fraud-proof output",
            ),
            fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              harness.committedFieldShape.fraudProof.policyId,
              "raw committed-field-shape fraud-proof mint",
            ),
          },
        ],
      },
      CommittedFieldShapeStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      harness.committedFieldShape.computationThread.policyId,
      "raw committed-field-shape thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const fraudMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      harness.committedFieldShape.fraudProof.policyId,
      "raw committed-field-shape fraud mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          harness.committedFieldShape.computationThread.policyId,
          "raw committed-field-shape thread burn",
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const unsigned = await harness.proverLucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([referenceScriptUtxo])
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudMintRedeemer)
    .pay.ToContract(
      harness.committedFieldShape.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(harness.proverSigner.paymentKeyHash)
    .attach.MintingPolicy(
      harness.committedFieldShape.computationThread.mintingScript,
    )
    .attach.MintingPolicy(harness.committedFieldShape.fraudProof.mintingScript)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  return txHash;
};

const RawCancelSchemaValue = SDK.faultProofStepRedeemerSchema(Data.Any());
type RawCancelSchema = Data.Static<typeof RawCancelSchemaValue>;
const RawCancelSchema = RawCancelSchemaValue as unknown as RawCancelSchema;

/** Raw outsider cancel, bypassing only the off-chain signer guard. */
export const submitRawCommittedFieldShapeCancelV1 = async ({
  lucid,
  contracts,
  signer,
  stepIndex,
  threadUtxo,
  threadUnit,
  threadAssetName,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: CommittedFieldShapeContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: 0 | 1;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly threadAssetName: string;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  if (threadUtxo.address !== contracts.steps[stepIndex].spendingScriptAddress) {
    throw new Error("raw cancel thread is not at the named family step");
  }
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw committed-field-shape cancel");
    return Data.to(
      {
        Cancel: {
          input_index: requireInputIndex(
            ctx,
            threadUtxo,
            "raw committed-field-shape cancel",
          ),
          computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.computationThread.policyId,
            "raw committed-field-shape cancel burn",
          ),
        },
      },
      RawCancelSchema,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw committed-field-shape cancel burn",
    );
    return Data.to(
      { BurnForCancellation: { burning_token_asset_name: threadAssetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([referenceScriptUtxo])
    .mintAssets({ [threadUnit]: -1n }, burnRedeemer)
    .addSignerKey(signer.paymentKeyHash)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/** Exact evaluator-failure assertion: off-chain errors are not security proof. */
export const expectCommittedFieldShapeOnchainRefusalV1 = async (
  build: () => Promise<unknown>,
): Promise<string> => {
  let failure: unknown;
  try {
    await build();
  } catch (error) {
    failure = error;
  }
  if (failure === undefined) {
    throw new Error("expected an on-chain refusal, but the transaction landed");
  }
  const text = failure instanceof Error ? failure.message : String(failure);
  if (!/failed script execution/u.test(text)) {
    throw new Error(
      `expected failed script execution, got an off-chain failure: ${text}`,
    );
  }
  return text;
};

/** Convenience inline claim for raw adversarial builders. */
export const committedFieldShapeInlineClaimV1 = ({
  fieldIndex,
  preimage,
}: {
  readonly fieldIndex: number;
  readonly preimage: Uint8Array;
}): CommittedFieldClaimV1 => ({
  BodyFieldClaim: {
    field_index: BigInt(fieldIndex),
    carriage: {
      Inline: { preimage: Buffer.from(preimage).toString("hex") },
    },
  },
});

export const preparedFromScenarioV1 = (
  scenario: CommittedFieldShapeScenarioV1,
  prepare: (tx: MidgardNativeTxCanonicalV1) => PreparedCommittedFieldShapeV1,
): PreparedCommittedFieldShapeV1 => {
  if (scenario.canonicalTx === null) {
    throw new Error("scenario has no canonical transaction for prepare");
  }
  return prepare(scenario.canonicalTx);
};
