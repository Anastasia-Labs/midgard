import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeHash32,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxWitnessSetCompact,
  type MidgardNativeTxCompact,
  type MidgardNativeTxFull,
  midgardNativeTxProofFieldPreimageLengths,
  type MidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import {
  CanonicalDecodabilityStep01SpendRedeemer,
  CanonicalDecodabilityStep02Datum,
  CanonicalDecodabilityStep02SpendRedeemer,
  type CanonicalDecodabilityStep02State,
  type CommittedFieldClaim,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  HUB_ORACLE_ASSET_NAME,
  miscountedMidgardFieldPreimage,
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
  type CanonicalDecodabilityContracts,
  prepareCanonicalDecodability,
  requireCanonicalDecodabilityReferenceScript,
  requireCanonicalDecodabilityThreadUtxo,
} from "../../src/canonical-decodability/index.js";
import { encodeL2TransactionSourceValue } from "../../src/prepare-double-spend.js";
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
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessWithdrawalValidatorCarriage,
} from "../../src/witness-reference-scripts-v1.js";
import { setupFraudulentBlock } from "./submit-init-emulator-fixtures.js";
import {
  makeFaultProofEmulatorHarness,
  makeNativeTx,
  network,
  publishPlainReferenceScriptUtxo,
  registerChunkedVerifyRewardAccount,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

export const CANONICAL_DECODABILITY_BODY_FIELD_INDEX = 2;
export const CANONICAL_DECODABILITY_WITNESS_FIELD_INDEX = 6;

export type CanonicalDecodabilityCommittedFieldFixture = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: 1n;
  readonly badTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Buffer;
  readonly witnessSet?: NativeTxWitnessSetCompact;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly prepared: ReturnType<typeof prepareCanonicalDecodability> | null;
};

const witnessSetData = (
  compact: ReturnType<typeof deriveMidgardNativeTxWitnessSetCompact>,
): NativeTxWitnessSetCompact => ({
  addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
  script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString("hex"),
  redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
    "hex",
  ),
});

const buildCommittedFixture = async ({
  compact,
  fieldIndex,
  committedPreimage,
  witnessSet,
  proofSource,
  allowGrammatical = false,
}: {
  readonly compact: MidgardNativeTxCompact;
  readonly fieldIndex: number;
  readonly committedPreimage: Buffer;
  readonly witnessSet?: NativeTxWitnessSetCompact;
  readonly proofSource: MidgardNativeTxProofSource;
  readonly allowGrammatical?: boolean;
}): Promise<CanonicalDecodabilityCommittedFieldFixture> => {
  const badTxId = computeMidgardNativeTxId(compact).toString("hex");
  const compactCbor = encodeMidgardNativeTxCompact(compact);
  const l2TransactionSourceCbor = encodeL2TransactionSourceValue({
    txId: badTxId,
    proofSource,
  });
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTxId, "hex"),
    Buffer.from(l2TransactionSourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(badTxId, "hex"));
  const proofCbor = proof.toCBOR().toString("hex");
  const nativeTxCompactCbor = compactCbor.toString("hex");
  const txInclusion: SubmitStep01TxInclusion = {
    nativeTxId: badTxId,
    nativeTx: nativeTxFromCoreCompact(compact),
    nativeTxCompactCbor,
    l2TransactionSourceCbor,
    transactionsPhasRoot: trieRootHex(trie),
    txMembershipProof: Data.from(proofCbor, Proof),
    txMembershipProofCbor: proofCbor,
  };
  let prepared: ReturnType<typeof prepareCanonicalDecodability> | null = null;
  try {
    prepared = prepareCanonicalDecodability({
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

const proofSourceForCommittedField = ({
  honest,
  compact,
  fieldIndex,
  committedPreimage,
  witnessSetCompactCbor,
}: {
  readonly honest: MidgardNativeTxFull;
  readonly compact: MidgardNativeTxCompact;
  readonly fieldIndex: number;
  readonly committedPreimage: Buffer;
  readonly witnessSetCompactCbor?: Buffer;
}): MidgardNativeTxProofSource => {
  const base = deriveMidgardNativeTxProofSource(honest);
  const lengths = [...midgardNativeTxProofFieldPreimageLengths(honest)];
  lengths[fieldIndex] = committedPreimage.length;
  return {
    compactCbor: encodeMidgardNativeTxCompact(compact),
    witnessSetCompactCbor: witnessSetCompactCbor ?? base.witnessSetCompactCbor,
    fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths(lengths),
  };
};

export const buildCanonicalDecodabilityBodyFixture = async ({
  grammatical = false,
}: {
  readonly grammatical?: boolean;
} = {}): Promise<CanonicalDecodabilityCommittedFieldFixture> => {
  const honest = makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    referenceByte: "61",
    outputByte: "62",
    witnessByte: "63",
  });
  const committedPreimage = grammatical
    ? Buffer.from(honest.body.outputsPreimageCbor)
    : miscountedMidgardFieldPreimage(1, [
        Buffer.from("aa", "hex"),
        Buffer.from("bb", "hex"),
      ]);
  const compact: MidgardNativeTxCompact = {
    ...honest.compact,
    transactionBody: {
      ...honest.compact.transactionBody,
      outputsHash: computeHash32(committedPreimage),
    },
  };
  return await buildCommittedFixture({
    compact,
    fieldIndex: CANONICAL_DECODABILITY_BODY_FIELD_INDEX,
    committedPreimage,
    proofSource: proofSourceForCommittedField({
      honest,
      compact,
      fieldIndex: CANONICAL_DECODABILITY_BODY_FIELD_INDEX,
      committedPreimage,
    }),
    allowGrammatical: grammatical,
  });
};

export const buildCanonicalDecodabilityWitnessFixture = async () => {
  const honest = makeNativeTx({ spendInputCbors: [], fee: 9n });
  const committedPreimage = Buffer.from([0x81]);
  const original = deriveMidgardNativeTxWitnessSetCompact(honest.witnessSet);
  const mutated = {
    ...original,
    scriptTxWitsHash: computeHash32(committedPreimage),
  };
  const witnessSet = witnessSetData(mutated);
  const compact: MidgardNativeTxCompact = {
    ...honest.compact,
    transactionWitnessSetHash: computeHash32(
      encodeMidgardNativeTxWitnessSetCompact(mutated),
    ),
  };
  return await buildCommittedFixture({
    compact,
    fieldIndex: CANONICAL_DECODABILITY_WITNESS_FIELD_INDEX,
    committedPreimage,
    witnessSet,
    proofSource: proofSourceForCommittedField({
      honest,
      compact,
      fieldIndex: CANONICAL_DECODABILITY_WITNESS_FIELD_INDEX,
      committedPreimage,
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(mutated),
    }),
  });
};

export const makeCanonicalDecodabilityEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
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

export const setupCanonicalDecodabilityScenario = async ({
  harness,
  fixture,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeCanonicalDecodabilityEmulatorHarness>
  >;
  readonly fixture: CanonicalDecodabilityCommittedFieldFixture;
}) =>
  await setupFraudulentBlock({
    funderLucid: harness.funderLucid,
    emulator: harness.emulator,
    contracts: harness.contracts,
    catalogue: harness.catalogue,
    fixture,
  });

export const publishCanonicalDecodabilityReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: CanonicalDecodabilityContracts;
}): Promise<readonly [UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script: step.spendingScript as Script,
      label: `canonical-decodability step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO];
};

/** Guard-bypassing step-01 builder used only for validator-negative tests. */
export const submitCanonicalDecodabilityStep01Raw = async ({
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
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: CanonicalDecodabilityContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly claim: CommittedFieldClaim;
  readonly step02State: CanonicalDecodabilityStep02State;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<{ readonly txHash: string; readonly nextThreadOutRef: string }> => {
  const { threadUtxo, threadToken } =
    await requireCanonicalDecodabilityThreadUtxo({
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
  const stepReference = requireCanonicalDecodabilityReferenceScript({
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
  const phasCarriage = witnessWithdrawalValidatorCarriage({
    script: phasScript,
    referenceUtxo: witnessReferenceScripts.phasMembershipWithdraw,
    label: "raw canonical step-01 PHAS membership",
  });
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
                  l2_transaction_source_cbor:
                    txInclusion.l2TransactionSourceCbor,
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
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      hubOracleUtxo,
      stateQueueUtxo,
      stepReference,
      ...phasCarriage.referenceInputs,
    ])
    .withdraw(
      phasRewardAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.l2TransactionSourceCbor,
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
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await phasCarriage
    .attach(base)
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined)
    throw new Error("Raw step-01 layout unresolved");
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/** Guard-bypassing finalizer used to prove verdict-0 is refused on-chain. */
export const submitCanonicalDecodabilityStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: CanonicalDecodabilityContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireCanonicalDecodabilityThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const stepReference = requireCanonicalDecodabilityReferenceScript({
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
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: "raw canonical step-02 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts.fraudProofMint,
    label: "raw canonical step-02 fraud-proof mint",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      stepReference,
      ...computationThreadCarriage.referenceInputs,
      ...fraudProofCarriage.referenceInputs,
    ])
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
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await fraudProofCarriage
    .attach(computationThreadCarriage.attach(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return txHash;
};

export { network };
