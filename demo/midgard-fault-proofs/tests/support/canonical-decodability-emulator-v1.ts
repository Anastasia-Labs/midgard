import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxWitnessSetCompactV1,
  encodeMidgardNativeTxCompactV1,
  encodeMidgardNativeTxWitnessSetCompactV1,
  type MidgardNativeTxCompactV1,
} from "@al-ft/midgard-core";
import {
  CanonicalDecodabilityStep01SpendRedeemer,
  CanonicalDecodabilityStep02Datum,
  CanonicalDecodabilityStep02SpendRedeemer,
  type CanonicalDecodabilityStep02State,
  type CommittedFieldClaimV1,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  HUB_ORACLE_ASSET_NAME,
  miscountedMidgardFieldPreimageV1,
  type NativeTxWitnessSetCompact,
  Proof,
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
  type LucidEvolution,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type CanonicalDecodabilityContractsV1,
  prepareCanonicalDecodabilityV1,
  requireCanonicalDecodabilityReferenceScriptV1,
  requireCanonicalDecodabilityThreadUtxoV1,
} from "../../src/canonical-decodability/index.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
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
  makeFaultProofEmulatorHarnessV1,
  makeNativeTx,
  network,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

export const CANONICAL_DECODABILITY_BODY_FIELD_INDEX_V1 = 2;
export const CANONICAL_DECODABILITY_WITNESS_FIELD_INDEX_V1 = 6;

export type CanonicalDecodabilityCommittedFieldFixtureV1 = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: 1n;
  readonly badTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Buffer;
  readonly witnessSet?: NativeTxWitnessSetCompact;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly prepared: ReturnType<typeof prepareCanonicalDecodabilityV1> | null;
};

const witnessSetDataV1 = (
  compact: ReturnType<typeof deriveMidgardNativeTxWitnessSetCompactV1>,
): NativeTxWitnessSetCompact => ({
  addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
  script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString("hex"),
  redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
    "hex",
  ),
});

const buildCommittedFixtureV1 = async ({
  compact,
  fieldIndex,
  committedPreimage,
  witnessSet,
  allowGrammatical = false,
}: {
  readonly compact: MidgardNativeTxCompactV1;
  readonly fieldIndex: number;
  readonly committedPreimage: Buffer;
  readonly witnessSet?: NativeTxWitnessSetCompact;
  readonly allowGrammatical?: boolean;
}): Promise<CanonicalDecodabilityCommittedFieldFixtureV1> => {
  const badTxId = computeMidgardNativeTxIdV1(compact).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompactV1(compact);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(Buffer.from(badTxId, "hex"), compactCbor);
  const proof = await trie.prove(Buffer.from(badTxId, "hex"));
  const proofCbor = proof.toCBOR().toString("hex");
  const nativeTxCompactCbor = compactCbor.toString("hex");
  const txInclusion: SubmitStep01TxInclusion = {
    nativeTxId: badTxId,
    nativeTx: nativeTxFromCoreCompact(compact),
    nativeTxCompactCbor,
    transactionsPhasRoot: trieRootHex(trie),
    txMembershipProof: Data.from(proofCbor, Proof),
    txMembershipProofCbor: proofCbor,
  };
  let prepared: ReturnType<typeof prepareCanonicalDecodabilityV1> | null = null;
  try {
    prepared = prepareCanonicalDecodabilityV1({
      badTxId,
      nativeTxCompactCbor,
      fieldIndex,
      committedPreimage,
      ...(witnessSet === undefined ? {} : { witnessSet }),
    });
  } catch (cause) {
    if (!allowGrammatical) throw cause;
  }
  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 1n,
    badTxId,
    nativeTxCompactCbor,
    fieldIndex,
    committedPreimage,
    ...(witnessSet === undefined ? {} : { witnessSet }),
    txInclusion,
    prepared,
  };
};

export const buildCanonicalDecodabilityBodyFixtureV1 = async ({
  grammatical = false,
}: {
  readonly grammatical?: boolean;
} = {}): Promise<CanonicalDecodabilityCommittedFieldFixtureV1> => {
  const honest = makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    referenceByte: "61",
    outputByte: "62",
    witnessByte: "63",
  });
  const committedPreimage = grammatical
    ? Buffer.from(honest.body.outputsPreimageCbor)
    : miscountedMidgardFieldPreimageV1(1, [
        Buffer.from("aa", "hex"),
        Buffer.from("bb", "hex"),
      ]);
  const compact: MidgardNativeTxCompactV1 = {
    ...honest.compact,
    transactionBody: {
      ...honest.compact.transactionBody,
      outputsHash: computeHash32(committedPreimage),
    },
  };
  return await buildCommittedFixtureV1({
    compact,
    fieldIndex: CANONICAL_DECODABILITY_BODY_FIELD_INDEX_V1,
    committedPreimage,
    allowGrammatical: grammatical,
  });
};

export const buildCanonicalDecodabilityWitnessFixtureV1 = async () => {
  const honest = makeNativeTx({ spendInputCbors: [], fee: 9n });
  const committedPreimage = Buffer.from([0x81]);
  const original = deriveMidgardNativeTxWitnessSetCompactV1(honest.witnessSet);
  const mutated = {
    ...original,
    scriptTxWitsHash: computeHash32(committedPreimage),
  };
  const witnessSet = witnessSetDataV1(mutated);
  const compact: MidgardNativeTxCompactV1 = {
    ...honest.compact,
    transactionWitnessSetHash: computeHash32(
      encodeMidgardNativeTxWitnessSetCompactV1(mutated),
    ),
  };
  return await buildCommittedFixtureV1({
    compact,
    fieldIndex: CANONICAL_DECODABILITY_WITNESS_FIELD_INDEX_V1,
    committedPreimage,
    witnessSet,
  });
};

export const makeCanonicalDecodabilityEmulatorHarnessV1 = async () => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realCanonicalDecodability: true,
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const canonicalDecodability = harness.contracts.canonicalDecodability;
  const category = harness.catalogue.categories.canonicalDecodability;
  if (canonicalDecodability === undefined || category === undefined) {
    throw new Error(
      "Harness did not build canonical-decodability contracts/category",
    );
  }
  if (
    category.categoryId !==
    FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.canonicalDecodability
  ) {
    throw new Error("Unexpected canonical-decodability category id");
  }
  return { ...harness, canonicalDecodability, category };
};

export const setupCanonicalDecodabilityScenarioV1 = async ({
  harness,
  fixture,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeCanonicalDecodabilityEmulatorHarnessV1>
  >;
  readonly fixture: CanonicalDecodabilityCommittedFieldFixtureV1;
}) =>
  await setupFraudulentBlockV1({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: harness.catalogue,
    fixture,
  });

export const publishCanonicalDecodabilityReferenceScriptsV1 = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: CanonicalDecodabilityContractsV1;
}): Promise<readonly [UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script: step.spendingScript as Script,
      label: `canonical-decodability step-0${(index + 1).toString()}`,
      oversized: true,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO];
};

/** Guard-bypassing step-01 builder used only for validator-negative tests. */
export const submitCanonicalDecodabilityStep01RawV1 = async ({
  lucid,
  blueprint,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  claim,
  step02State,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: CanonicalDecodabilityContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly claim: CommittedFieldClaimV1;
  readonly step02State: CanonicalDecodabilityStep02State;
  readonly referenceScriptUtxo: UTxO;
}): Promise<{ readonly txHash: string; readonly nextThreadOutRef: string }> => {
  const { threadUtxo, threadToken } =
    await requireCanonicalDecodabilityThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  requireInitialStepDatum({ threadUtxo, signer });
  const [stateQueueUtxo, hubOracleUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "raw state queue out-ref"),
      label: "raw canonical-decodability state queue node",
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: "raw canonical-decodability hub oracle",
    }),
  ]);
  const stepReference = requireCanonicalDecodabilityReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(network, phasScript);
  const datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step02State },
    CanonicalDecodabilityStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw canonical step-01");
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "raw canonical step-01",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "raw canonical step-01 output",
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
                    "raw canonical step-01 hub",
                  ),
                  state_queue_node_ref_input_index: requireReferenceInputIndex(
                    ctx,
                    stateQueueUtxo,
                    "raw canonical step-01 state queue",
                  ),
                  native_tx_id: txInclusion.nativeTxId,
                  native_tx_compact_cbor: txInclusion.nativeTxCompactCbor,
                  transactions_phas_root: txInclusion.transactionsPhasRoot,
                  tx_membership_proof: txInclusion.txMembershipProof,
                  inclusion_proof_script_withdraw_redeemer_index:
                    requireWithdrawalRedeemerIndex(
                      ctx,
                      phasRewardAddress,
                      "raw canonical step-01 membership",
                    ),
                },
              ],
            },
            claim,
          },
        ],
      },
      CanonicalDecodabilityStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([hubOracleUtxo, stateQueueUtxo, stepReference])
    .withdraw(
      phasRewardAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.nativeTxCompactCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.WithdrawalValidator(phasScript)
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined)
    throw new Error("Raw step-01 layout unresolved");
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/** Guard-bypassing finalizer used to prove verdict-0 is refused on-chain. */
export const submitCanonicalDecodabilityStep02RawV1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: CanonicalDecodabilityContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireCanonicalDecodabilityThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const stepReference = requireCanonicalDecodabilityReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    stepIndex: 1,
  });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw canonical step-02");
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "raw canonical step-02",
            ),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "raw canonical step-02 output",
            ),
            fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw canonical fraud-proof mint",
            ),
          },
        ],
      },
      CanonicalDecodabilityStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw canonical thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const mintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      "raw canonical fraud-proof mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "raw canonical thread burn",
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([stepReference])
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, mintRedeemer)
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
  await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return txHash;
};

export { network };
