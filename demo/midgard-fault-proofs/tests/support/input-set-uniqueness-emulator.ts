/**
 * Shared emulator fixtures for the `input-set-uniqueness` family.
 *
 * The family convicts an operator-ACCEPTED committed transaction whose
 * intra-transaction input sets violate uniqueness/disjointness. What every
 * scenario needs and no existing helper produces is a committed transaction
 * with a **caller-chosen reference-input list**: `makeNativeTx` fixes field 1
 * to at most one opaque 32-byte item, but this family's claims name canonical
 * §5.3 out-ref items in both fields, so the fixture materializes the native
 * transaction directly from its canonical preimages.
 *
 * Fixtures are deliberately tiny: two or three items per field decide every
 * sub-variant, and the openings always ride tier-1 inline carriage.
 */
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  encodeMidgardTxInputCanonical,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  HUB_ORACLE_ASSET_NAME,
  InputSetUniquenessStep01SpendRedeemer,
  type InputSetUniquenessStep02Args,
  InputSetUniquenessStep02Datum,
  InputSetUniquenessStep02SpendRedeemer,
  type MidgardTxInput,
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
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { InputSetUniquenessContracts } from "../../src/input-set-uniqueness/contracts.js";
import { requireInputSetUniquenessThreadUtxo } from "../../src/input-set-uniqueness/submit-common.js";
import {
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
} from "../../src/runtime.js";
import {
  nativeTxFromCoreCompact,
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../../src/submit-step-01.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
  witnessWithdrawalValidatorCarriage,
} from "../../src/witness-reference-scripts.js";
import { publishFaultProofWitnessReferenceScripts } from "./emulator/reference-scripts.js";
import { countedTransactionsRoot } from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  l2TransactionSourceCbor as l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarness,
  makeHeader,
  makeNativeTx,
  network as emulatorNetwork,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

export { expectOnchainRefusal } from "./native-script-decoding-emulator.js";

// ---------------------------------------------------------------------------
// The committed transaction and its MPF inclusion
// ---------------------------------------------------------------------------

/** A readable fixture out-ref: `tx_id` is one byte repeated 32 times. */
export const isuOutRef = (
  txIdByte: string,
  outputIndex: number,
): MidgardTxInput => ({
  tx_id: txIdByte.repeat(32),
  output_index: BigInt(outputIndex),
});

/** The canonical §5.3 item bytes for one out-ref, hex. */
export const isuItemCbor = (outRef: MidgardTxInput): string =>
  Buffer.from(encodeMidgardTxInputCanonical(outRef)).toString("hex");

export type InputSetUniquenessFixture = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
  readonly forcedSource: {
    readonly compact_cbor: string;
    readonly witness_set_compact_cbor: string;
    readonly field_preimage_lengths_cbor: string;
  };
  readonly fullTransactionCbor: Buffer;
};

/**
 * Materializes a committed native transaction with caller-chosen §2.5 fields
 * 0 and 1 (canonical out-ref items in committed order), commits it into a
 * transactions MPF trie beside one honest decoy leaf, and returns the full
 * step-01 inclusion material.
 *
 * `validity: "TxIsInvalid"` builds the §2.4.3(d) negative — a transaction
 * the operator honestly recorded as a no-op, which the family must never
 * convict however degenerate its input sets are.
 */
export const buildInputSetUniquenessFixture = async ({
  spendInputs,
  referenceInputs,
  validity = "TxIsValid",
}: {
  readonly spendInputs: readonly MidgardTxInput[];
  readonly referenceInputs: readonly MidgardTxInput[];
  readonly validity?: "TxIsValid" | "TxIsInvalid";
}): Promise<InputSetUniquenessFixture> => {
  const spendItems = spendInputs.map((outRef) =>
    Buffer.from(encodeMidgardTxInputCanonical(outRef)),
  );
  const referenceItems = referenceInputs.map((outRef) =>
    Buffer.from(encodeMidgardTxInputCanonical(outRef)),
  );
  const badTx: MidgardNativeTxFull = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity,
    body: {
      spendInputsPreimageCbor: encodeCbor(spendItems),
      referenceInputsPreimageCbor:
        referenceItems.length === 0
          ? EMPTY_CBOR_LIST
          : encodeCbor(referenceItems),
      outputsPreimageCbor: encodeCbor([Buffer.from("f0".repeat(32), "hex")]),
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee: 1_000n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: encodeCbor([Buffer.from("f1".repeat(32), "hex")]),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  // One honest decoy leaf, so the membership proof has at least one step.
  const decoyTx = makeNativeTx({
    spendInputCbors: [
      Buffer.from(encodeMidgardTxInputCanonical(isuOutRef("dd", 0))),
    ],
    fee: 5n,
  });
  const badTxId = computeMidgardNativeTxId(badTx).toString("hex");
  const badTxCompactCbor = Buffer.from(
    encodeMidgardNativeTxCompact(badTx.compact),
  ).toString("hex");
  const badTxSourceCbor = l2TransactionSourceCborV1(badTx);
  const forcedSource = deriveMidgardNativeTxProofSource(badTx);
  const decoyTxSourceCbor = l2TransactionSourceCborV1(decoyTx);
  const decoyTxId = computeMidgardNativeTxId(decoyTx).toString("hex");
  if (decoyTxId === badTxId) {
    throw new Error("fixture decoy collides with the disputed transaction");
  }
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTxId, "hex"),
    Buffer.from(badTxSourceCbor, "hex"),
  );
  await trie.insert(
    Buffer.from(decoyTxId, "hex"),
    Buffer.from(decoyTxSourceCbor, "hex"),
  );
  const proof = await trie.prove(Buffer.from(badTxId, "hex"));
  const txMembershipProofCbor = proof.toCBOR().toString("hex");
  return {
    transactionsRoot: trieRootHex(trie),
    l2TransactionCount: 2n,
    nativeTxId: badTxId,
    nativeTxCompactCbor: badTxCompactCbor,
    txInclusion: {
      nativeTxId: badTxId,
      nativeTx: nativeTxFromCoreCompact(badTx.compact),
      nativeTxCompactCbor: badTxCompactCbor,
      l2TransactionSourceCbor: badTxSourceCbor,
      transactionsPhasRoot: trieRootHex(trie),
      txMembershipProof: Data.from(txMembershipProofCbor, Proof),
      txMembershipProofCbor,
    },
    spendInputItemCbors: spendItems.map((item) => item.toString("hex")),
    referenceInputItemCbors: referenceItems.map((item) => item.toString("hex")),
    forcedSource: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    fullTransactionCbor: encodeMidgardNativeTxCanonical(badTx),
  };
};

// ---------------------------------------------------------------------------
// Harness, committed header, reference scripts, removal category
// ---------------------------------------------------------------------------

export const makeInputSetUniquenessEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realInputSetUniqueness: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const family = harness.contracts.inputSetUniqueness;
  const category = harness.catalogue.categories.inputSetUniqueness;
  if (family === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the input-set-uniqueness contracts/category",
    );
  }
  if (
    category.categoryId !==
    FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.inputSetUniqueness
  ) {
    throw new Error("Unexpected input-set-uniqueness catalogue category id");
  }
  return { ...harness, family, category };
};

export type InputSetUniquenessHarness = Awaited<
  ReturnType<typeof makeInputSetUniquenessEmulatorHarness>
>;

/**
 * Commits a header carrying the fixture's counted `transactions_root` on the
 * emulator, ready for Init.
 */
export const setupInputSetUniquenessScenario = async ({
  harness,
  fixture,
}: {
  readonly harness: InputSetUniquenessHarness;
  readonly fixture: InputSetUniquenessFixture;
}) => {
  const {
    emulator,
    funderLucid,
    proverLucid,
    realBlueprint,
    contracts,
    family,
    catalogue,
    nonceUtxo,
  } = harness;
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScripts({
      lucid: proverLucid,
      realBlueprint,
      computationThreadMintingScript: family.computationThread.mintingScript,
      fraudProofMintingScript: family.fraudProof.mintingScript,
    });
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(funderLucid, emulator.now() + 120_000) -
    1;
  const funderKeyHash = await funderPaymentKeyHash(funderLucid);
  const header = makeHeader(
    funderKeyHash,
    headerStartTime,
    await countedTransactionsRoot(
      fixture.transactionsRoot,
      fixture.l2TransactionCount,
    ),
    fixture.l2TransactionCount,
  );
  const setup = await submitSetupTx({
    lucid: funderLucid,
    contracts,
    nonceUtxo,
    catalogue,
    header,
  });
  return { header, setup: { ...setup, witnessReferenceScripts } };
};

/**
 * Publishes all four step validators as reference scripts (production deployment
 * shape per the standing reference-script ruling).
 */
export const publishInputSetUniquenessReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: InputSetUniquenessContracts;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `input-set-uniqueness step-0${(index + 1).toString()}`,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
};

// ---------------------------------------------------------------------------
// Raw builders — the honest submitters' transactions WITHOUT their local
// fail-closed guards, so the adversarial suite can watch the VALIDATOR refuse
// (see `expectOnchainRefusal`). Production code never takes these paths.
// ---------------------------------------------------------------------------

/**
 * A raw step-01 bind: the honest submitter's redeemer-carried inclusion
 * transaction minus its §2.4.3(d) validity re-check and header cross-check,
 * so an honestly-rejected committed leaf reaches the validator's own
 * `validity_code == 0` refusal.
 */
export const submitRawInputSetUniquenessBind = async ({
  harness,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly harness: InputSetUniquenessHarness;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const contracts = harness.family;
  const { threadUtxo, threadToken } = await requireInputSetUniquenessThreadUtxo(
    {
      lucid,
      contracts,
      categoryId: harness.category.categoryId,
      stepIndex: 0,
      threadOutRef,
    },
  );
  const stateQueueBlockUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
    label: "raw input-set-uniqueness state-queue block UTxO",
  });
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      emulatorNetwork,
      scriptHashToCredential(contracts.hubOraclePolicyId),
    ),
    unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
    label: "raw input-set-uniqueness hub oracle",
  });
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(
      harness.realBlueprint,
      PHAS_MEMBERSHIP_WITHDRAW_TITLE,
    ),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    emulatorNetwork,
    phasMembershipScript,
  );
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriage({
    script: phasMembershipScript,
    referenceUtxo: witnessReferenceScripts.phasMembershipWithdraw,
    label: "raw input-set-uniqueness PHAS membership",
  });
  const stepCarriage = witnessSpendingValidatorCarriage({
    script: contracts.steps[0].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "raw input-set-uniqueness step-01",
  });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step02Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: { bad_tx_id: txInclusion.nativeTxId },
    },
    InputSetUniquenessStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw input-set-uniqueness bind");
    return Data.to(
      {
        Continue: [
          {
            source: {
              AcceptedSource: {
                inclusion: {
                  RedeemerCarriedInclusion: [
                    {
                      input_index: requireInputIndex(
                        ctx,
                        threadUtxo,
                        "raw input-set-uniqueness bind",
                      ),
                      output_index: requireUniqueOutputIndex(
                        ctx.outputs,
                        outputMatches,
                        "raw input-set-uniqueness bind output",
                      ),
                      hub_ref_input_index: requireReferenceInputIndex(
                        ctx,
                        hubOracleUtxo,
                        "raw input-set-uniqueness hub oracle",
                      ),
                      state_queue_node_ref_input_index:
                        requireReferenceInputIndex(
                          ctx,
                          stateQueueBlockUtxo,
                          "raw input-set-uniqueness state-queue node",
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
                          "raw input-set-uniqueness PHAS membership",
                        ),
                    },
                  ],
                },
              },
            },
          },
        ],
      },
      InputSetUniquenessStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      hubOracleUtxo,
      stateQueueBlockUtxo,
      ...stepCarriage.referenceInputs,
      ...phasMembershipCarriage.referenceInputs,
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
      { kind: "inline", value: step02Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = phasMembershipCarriage.attach(stepCarriage.attach(base));
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/** The finalize layout a raw args builder is handed. */
export type RawInputSetUniquenessFinalizeLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

/**
 * A raw step-02 finalize: the honest submitter's thread-burn/token-mint
 * transaction with a caller-built `Continue` argument and NONE of the local
 * conviction twins, so a claim the validator must refuse — unequal items,
 * `i >= j`, an out-of-range index — reaches the exact on-chain check.
 */
export const submitRawInputSetUniquenessFinalize = async ({
  harness,
  threadOutRef,
  buildArgs,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly harness: InputSetUniquenessHarness;
  readonly threadOutRef: string;
  readonly buildArgs: (
    layout: RawInputSetUniquenessFinalizeLayout,
  ) => InputSetUniquenessStep02Args;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const contracts = harness.family;
  const { threadUtxo, threadToken } = await requireInputSetUniquenessThreadUtxo(
    {
      lucid,
      contracts,
      categoryId: harness.category.categoryId,
      stepIndex: 1,
      threadOutRef,
    },
  );
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
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw input-set-uniqueness finalize",
    );
    const outputIndex = ctx.outputs.findIndex(
      (output) => output.address === contracts.fraudProof.spendingScriptAddress,
    );
    if (outputIndex < 0) {
      throw new Error("raw finalize built no fraud-proof output");
    }
    return Data.to(
      {
        Continue: [
          buildArgs({
            inputIndex: requireInputIndex(
              ctx,
              threadUtxo,
              "raw input-set-uniqueness finalize",
            ),
            outputIndex: BigInt(outputIndex),
            fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
              ctx,
              contracts.fraudProof.policyId,
              "raw input-set-uniqueness fraud-proof",
            ),
          }),
        ],
      },
      InputSetUniquenessStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "raw input-set-uniqueness thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const fraudProofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      "raw input-set-uniqueness fraud-proof mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          "raw input-set-uniqueness thread burn",
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: "raw input-set-uniqueness computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts.fraudProofMint,
    label: "raw input-set-uniqueness fraud-proof mint",
  });
  const stepCarriage = witnessSpendingValidatorCarriage({
    script: contracts.steps[1].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "raw input-set-uniqueness step-02",
  });
  const referenceInputs = [
    ...stepCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const withReferences = base.readFrom(referenceInputs);
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(stepCarriage.attach(withReferences)),
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
