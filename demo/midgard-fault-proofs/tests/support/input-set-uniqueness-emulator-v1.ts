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
  computeMidgardNativeTxIdV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCompactV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import {
  encodeMidgardTxInputCanonicalV1,
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

import type { InputSetUniquenessContractsV1 } from "../../src/input-set-uniqueness/contracts-v1.js";
import { requireInputSetUniquenessThreadUtxoV1 } from "../../src/input-set-uniqueness/submit-common-v1.js";
import type { RemoveFraudulentBlockExplicitCategory } from "../../src/remove-fraudulent-block.js";
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
import { countedTransactionsRoot } from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  INPUT_SET_UNIQUENESS_REMOVAL_DEPLOYMENT_ENTRY_V1,
  INPUT_SET_UNIQUENESS_TEST_CATEGORY_ID_V1,
  makeFaultProofEmulatorHarnessV1,
  makeHeader,
  makeNativeTx,
  network as emulatorNetworkV1,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
  trieRootHex,
} from "./submit-init-emulator-shared.js";

export { expectOnchainRefusalV1 } from "./native-script-decoding-emulator-v1.js";

// ---------------------------------------------------------------------------
// The committed transaction and its MPF inclusion
// ---------------------------------------------------------------------------

/** A readable fixture out-ref: `tx_id` is one byte repeated 32 times. */
export const isuOutRefV1 = (
  txIdByte: string,
  outputIndex: number,
): MidgardTxInput => ({
  tx_id: txIdByte.repeat(32),
  output_index: BigInt(outputIndex),
});

/** The canonical §5.3 item bytes for one out-ref, hex. */
export const isuItemCborV1 = (outRef: MidgardTxInput): string =>
  Buffer.from(encodeMidgardTxInputCanonicalV1(outRef)).toString("hex");

export type InputSetUniquenessFixtureV1 = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
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
export const buildInputSetUniquenessFixtureV1 = async ({
  spendInputs,
  referenceInputs,
  validity = "TxIsValid",
}: {
  readonly spendInputs: readonly MidgardTxInput[];
  readonly referenceInputs: readonly MidgardTxInput[];
  readonly validity?: "TxIsValid" | "TxIsInvalid";
}): Promise<InputSetUniquenessFixtureV1> => {
  const spendItems = spendInputs.map((outRef) =>
    Buffer.from(encodeMidgardTxInputCanonicalV1(outRef)),
  );
  const referenceItems = referenceInputs.map((outRef) =>
    Buffer.from(encodeMidgardTxInputCanonicalV1(outRef)),
  );
  const badTx: MidgardNativeTxFullV1 =
    materializeMidgardNativeTxFromCanonicalV1({
      version: MIDGARD_NATIVE_TX_V1_VERSION,
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
        addrTxWitsPreimageCbor: encodeCbor([
          Buffer.from("f1".repeat(32), "hex"),
        ]),
        scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    });
  // One honest decoy leaf, so the membership proof has at least one step.
  const decoyTx = makeNativeTx({
    spendInputCbors: [
      Buffer.from(encodeMidgardTxInputCanonicalV1(isuOutRefV1("dd", 0))),
    ],
    fee: 5n,
  });
  const badTxId = computeMidgardNativeTxIdV1(badTx).toString("hex");
  const badTxCompactCbor = Buffer.from(
    encodeMidgardNativeTxCompactV1(badTx.compact),
  ).toString("hex");
  const decoyTxId = computeMidgardNativeTxIdV1(decoyTx).toString("hex");
  if (decoyTxId === badTxId) {
    throw new Error("fixture decoy collides with the disputed transaction");
  }
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(badTxId, "hex"),
    Buffer.from(badTxCompactCbor, "hex"),
  );
  await trie.insert(
    Buffer.from(decoyTxId, "hex"),
    Buffer.from(encodeMidgardNativeTxCompactV1(decoyTx.compact)),
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
      transactionsPhasRoot: trieRootHex(trie),
      txMembershipProof: Data.from(txMembershipProofCbor, Proof),
      txMembershipProofCbor,
    },
    spendInputItemCbors: spendItems.map((item) => item.toString("hex")),
    referenceInputItemCbors: referenceItems.map((item) => item.toString("hex")),
  };
};

// ---------------------------------------------------------------------------
// Harness, committed header, reference scripts, removal category
// ---------------------------------------------------------------------------

export const makeInputSetUniquenessEmulatorHarnessV1 = async () => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realInputSetUniqueness: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const family = harness.contracts.inputSetUniqueness;
  const category = harness.catalogue.extraCategories.inputSetUniqueness;
  if (family === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the input-set-uniqueness contracts/category",
    );
  }
  if (category.categoryId !== INPUT_SET_UNIQUENESS_TEST_CATEGORY_ID_V1) {
    throw new Error("Unexpected input-set-uniqueness catalogue category id");
  }
  return { ...harness, family, category };
};

export type InputSetUniquenessHarnessV1 = Awaited<
  ReturnType<typeof makeInputSetUniquenessEmulatorHarnessV1>
>;

/**
 * Commits a header carrying the fixture's counted `transactions_root` on the
 * emulator, ready for Init.
 */
export const setupInputSetUniquenessScenarioV1 = async ({
  harness,
  fixture,
}: {
  readonly harness: InputSetUniquenessHarnessV1;
  readonly fixture: InputSetUniquenessFixtureV1;
}) => {
  const { emulator, funderLucid, contracts, catalogue, nonceUtxo } = harness;
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
  return { header, setup };
};

/**
 * Publishes both step validators as reference scripts (production deployment
 * shape per the standing reference-script ruling).
 */
export const publishInputSetUniquenessReferenceScriptsV1 = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: InputSetUniquenessContractsV1;
}): Promise<readonly [UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `input-set-uniqueness step-0${(index + 1).toString()}`,
      oversized: true,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO];
};

/**
 * The explicit removal-category record for the family: the removal submitter
 * cannot resolve a pre-registration category through the SDK's canonical
 * builders, so the harness hands it the already-resolved facts. The
 * spend-script hash rides the harness's shared `fraudProof` contracts because
 * the family record deliberately carries only the pair's policy id and
 * address; the two are the same deployment (see `contracts.ts`).
 */
export const inputSetUniquenessRemovalCategoryV1 = (
  harness: InputSetUniquenessHarnessV1,
): RemoveFraudulentBlockExplicitCategory => ({
  name: "inputSetUniqueness",
  categoryId: harness.category.categoryId,
  firstStepDeploymentEntry: INPUT_SET_UNIQUENESS_REMOVAL_DEPLOYMENT_ENTRY_V1,
  firstStepScriptHash: harness.family.steps[0].spendingScriptHash,
  fraudProof: {
    policyId: harness.family.fraudProof.policyId,
    spendingScriptHash: harness.contracts.fraudProof.spendingScriptHash,
    spendingScriptAddress: harness.family.fraudProof.spendingScriptAddress,
  },
});

// ---------------------------------------------------------------------------
// Raw builders — the honest submitters' transactions WITHOUT their local
// fail-closed guards, so the adversarial suite can watch the VALIDATOR refuse
// (see `expectOnchainRefusalV1`). Production code never takes these paths.
// ---------------------------------------------------------------------------

/**
 * A raw step-01 bind: the honest submitter's redeemer-carried inclusion
 * transaction minus its §2.4.3(d) validity re-check and header cross-check,
 * so an honestly-rejected committed leaf reaches the validator's own
 * `validity_code == 0` refusal.
 */
export const submitRawInputSetUniquenessBindV1 = async ({
  harness,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  referenceScriptUtxo,
}: {
  readonly harness: InputSetUniquenessHarnessV1;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo?: UTxO;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const contracts = harness.family;
  const { threadUtxo, threadToken } =
    await requireInputSetUniquenessThreadUtxoV1({
      lucid,
      contracts,
      categoryId: harness.category.categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  const stateQueueBlockUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
    label: "raw input-set-uniqueness state-queue block UTxO",
  });
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      emulatorNetworkV1,
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
    emulatorNetworkV1,
    phasMembershipScript,
  );
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
                state_queue_node_ref_input_index: requireReferenceInputIndex(
                  ctx,
                  stateQueueBlockUtxo,
                  "raw input-set-uniqueness state-queue node",
                ),
                native_tx_id: txInclusion.nativeTxId,
                native_tx_compact_cbor: txInclusion.nativeTxCompactCbor,
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
      ...(referenceScriptUtxo === undefined ? [] : [referenceScriptUtxo]),
    ])
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
      { kind: "inline", value: step02Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.WithdrawalValidator(phasMembershipScript);
  const tx =
    referenceScriptUtxo === undefined
      ? base.attach.SpendingValidator(contracts.steps[0].spendingScript)
      : base;
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/** The finalize layout a raw args builder is handed. */
export type RawInputSetUniquenessFinalizeLayoutV1 = {
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
export const submitRawInputSetUniquenessFinalizeV1 = async ({
  harness,
  threadOutRef,
  buildArgs,
  referenceScriptUtxo,
}: {
  readonly harness: InputSetUniquenessHarnessV1;
  readonly threadOutRef: string;
  readonly buildArgs: (
    layout: RawInputSetUniquenessFinalizeLayoutV1,
  ) => InputSetUniquenessStep02Args;
  readonly referenceScriptUtxo?: UTxO;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const contracts = harness.family;
  const { threadUtxo, threadToken } =
    await requireInputSetUniquenessThreadUtxoV1({
      lucid,
      contracts,
      categoryId: harness.category.categoryId,
      stepIndex: 1,
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
    .addSignerKey(signer.paymentKeyHash)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .attach.MintingPolicy(contracts.fraudProof.mintingScript);
  const tx =
    referenceScriptUtxo === undefined
      ? base.attach.SpendingValidator(contracts.steps[1].spendingScript)
      : base.readFrom([referenceScriptUtxo]);
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
