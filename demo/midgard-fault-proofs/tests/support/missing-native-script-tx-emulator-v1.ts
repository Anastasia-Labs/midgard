import {
  computeMidgardNativeTxIdV1,
  decodeMidgardFieldPreimageV1,
  deriveMidgardNativeTxWitnessSetCompactV1,
  encodeCbor,
  encodeMidgardFieldPreimageV1,
  encodeMidgardNativeScript,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import {
  encodeMidgardTxInputCanonicalV1,
  faultProofStepRedeemerSchema,
  fieldOpeningV1ForField,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  MIDGARD_FIELD_INDEX_V1,
  type MidgardTxInput,
  MissingNativeScriptTxStep03Datum,
  MissingNativeScriptTxStep03SpendRedeemer,
  type MissingNativeScriptTxStep03State,
  MissingNativeScriptTxStep04Datum,
  MissingNativeScriptTxStep04SpendRedeemer,
  type MissingNativeScriptTxStep04State,
  MissingNativeScriptTxStep05Datum,
  MissingNativeScriptTxStep05SpendRedeemer,
  type MissingNativeScriptTxStep05State,
  MissingNativeScriptTxStep06Datum,
  missingNativeScriptTxStep06ReadyStateV1,
  MissingNativeScriptTxStep06SpendRedeemer,
  type NativeTxWitnessSetCompact,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { prepareFamilyClaimRegistryMutationV1 } from "../../src/claim-registry-transaction-v1.js";
import type { MissingNativeScriptTxContractsV1 } from "../../src/missing-native-script-tx/contracts-v1.js";
import {
  requireMissingNativeScriptTxStepStateV1,
  requireMissingNativeScriptTxThreadUtxoV1,
} from "../../src/missing-native-script-tx/submit-common-v1.js";
import { submitMissingNativeScriptTxBindingV1 } from "../../src/missing-native-script-tx/submit-native-binding-v1.js";
import { resolveProverSigner } from "../../src/runtime.js";
import { excludeUtxo } from "../../src/spend-input-witness.js";
import type { SubmitStep01TxInclusion } from "../../src/submit-step-01.js";
import { selectFeeInput } from "../../src/submit-step-01.js";
import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../../src/tx-layout.js";
import { witnessMintingPolicyCarriageV1 } from "../../src/witness-reference-scripts-v1.js";
import {
  buildDecodingBlockFixtureV1,
  type DecodingBlockFixtureV1,
} from "./native-script-decoding-emulator-v1.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  makeFaultProofEmulatorHarnessV1,
  makeNativeTx,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export const missingNativeScriptBytesV1 = (): Buffer =>
  encodeMidgardNativeScript({
    type: "all",
    scripts: [
      {
        type: "sig",
        keyHash: Buffer.from("31".repeat(28), "hex"),
      },
    ],
  });

export const missingVersionedScriptV1 = () => {
  const scriptBytes = missingNativeScriptBytesV1();
  return {
    language: "NativeCardano" as const,
    scriptBytes,
    nativeScript: {
      type: "all" as const,
      scripts: [
        {
          type: "sig" as const,
          keyHash: Buffer.from("31".repeat(28), "hex"),
        },
      ],
    },
  };
};

const scriptLockedOutputCborV1 = ({
  credentialHash,
  keyLocked,
}: {
  readonly credentialHash: string;
  readonly keyLocked: boolean;
}): Buffer =>
  encodeMidgardTxOutput({
    // Testnet enterprise: 0x60 key, 0x70 script.
    address: Buffer.concat([
      Buffer.from([keyLocked ? 0x60 : 0x70]),
      Buffer.from(credentialHash, "hex"),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });

const sdkWitnessSet = (
  tx: MidgardNativeTxFullV1,
): NativeTxWitnessSetCompact => {
  const compact = deriveMidgardNativeTxWitnessSetCompactV1(tx.witnessSet);
  return {
    addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
      "hex",
    ),
  };
};

export const makeMissingNativeScriptTxEmulatorHarnessV1 = async () => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realMissingNativeScriptTx: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const family = harness.contracts.missingNativeScriptTx;
  const category = harness.catalogue.categories.missingNativeScriptTx;
  if (family === undefined || category === undefined) {
    throw new Error("Harness did not build missing-native-script-tx");
  }
  if (
    category.categoryId !==
    FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.missingNativeScriptTx
  ) {
    throw new Error("Unexpected missing-native-script-tx category id");
  }
  const outsider = generateEmulatorAccount({ lovelace: 0n });
  const outsiderLucid = await Lucid(harness.emulator, "Custom");
  outsiderLucid.selectWallet.fromSeed(outsider.seedPhrase);
  const outsiderSigner = resolveProverSigner({
    network,
    walletSeedPhrase: outsider.seedPhrase,
  });
  return { ...harness, family, category, outsiderLucid, outsiderSigner };
};

export type MissingNativeScriptTxFixtureV1 = {
  readonly block: DecodingBlockFixtureV1;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly producingTx: MidgardNativeTxFullV1;
  readonly badTx: MidgardNativeTxFullV1;
  readonly producingTxId: string;
  readonly badTxId: string;
  readonly producingOutputItemCbors: readonly Buffer[];
  readonly badTxSpendInputs: readonly MidgardTxInput[];
  /** Where the accused input landed after the canonical §5.3 sort. */
  readonly badInputIndex: number;
  readonly badTxWitnessSet: NativeTxWitnessSetCompact;
  readonly badTxScriptWitnessItemCbors: readonly Buffer[];
  readonly nativeScriptBytes: Buffer;
  readonly versionedScriptItem: Buffer;
  readonly expectedScriptHash: string;
};

export const setupMissingNativeScriptTxFixtureV1 = async ({
  harness,
  scriptPresent = false,
  keyLockedProducingOutput = false,
  decoySpendInputCount = 0,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarnessV1>
  >;
  readonly scriptPresent?: boolean;
  readonly keyLockedProducingOutput?: boolean;
  /**
   * Extra fabricated spend inputs committed alongside the accused pair, so a
   * test can grow the bad transaction's field-0 preimage past the §8.4
   * tier-1 bound and let size alone select tier-2 carriage.
   */
  readonly decoySpendInputCount?: number;
}): Promise<MissingNativeScriptTxFixtureV1> => {
  const versionedScript = missingVersionedScriptV1();
  const nativeScriptBytes = Buffer.from(versionedScript.scriptBytes);
  const versionedScriptItem = encodeMidgardVersionedScript(versionedScript);
  const expectedScriptHash = hashMidgardVersionedScript(versionedScript);
  const producingOutput = scriptLockedOutputCborV1({
    credentialHash: expectedScriptHash,
    keyLocked: keyLockedProducingOutput,
  });
  const keyLockedControlOutput = scriptLockedOutputCborV1({
    credentialHash: "99".repeat(28),
    keyLocked: true,
  });
  const producingTx = makeNativeTx({
    spendInputCbors: [],
    fee: 1_000n,
    outputCbors: [producingOutput, keyLockedControlOutput],
  });
  const producingTxId = computeMidgardNativeTxIdV1(producingTx).toString("hex");
  const accusedInput: MidgardTxInput = {
    tx_id: producingTxId,
    output_index: 0n,
  };
  const keyLockedControlInput: MidgardTxInput = {
    tx_id: producingTxId,
    output_index: 1n,
  };
  const decoySpendInputs: readonly MidgardTxInput[] = Array.from(
    { length: decoySpendInputCount },
    (_, index): MidgardTxInput => ({
      tx_id: (index + 1).toString(16).padStart(64, "0"),
      output_index: 0n,
    }),
  );
  const badTxSpendInputs = [
    accusedInput,
    keyLockedControlInput,
    ...decoySpendInputs,
  ].sort((left, right) =>
    Buffer.compare(
      encodeMidgardTxInputCanonicalV1(left),
      encodeMidgardTxInputCanonicalV1(right),
    ),
  );
  const badInputIndex = badTxSpendInputs.findIndex(
    (input) =>
      input.tx_id === accusedInput.tx_id &&
      input.output_index === accusedInput.output_index,
  );
  const badTx = makeNativeTx({
    spendInputCbors: badTxSpendInputs.map(encodeMidgardTxInputCanonicalV1),
    fee: 2_000n,
    scriptTxWitsPreimageCbor: scriptPresent
      ? encodeCbor([versionedScriptItem])
      : encodeCbor([]),
  });
  const badTxId = computeMidgardNativeTxIdV1(badTx).toString("hex");
  const paymentCredential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (paymentCredential?.type !== "Key") {
    throw new Error("Funder has no payment key hash");
  }
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1,
  );
  const block = await buildDecodingBlockFixtureV1({
    operatorVkey: paymentCredential.hash,
    startTime,
    priorLedgerRoot: "00".repeat(32),
    subject: { kind: "normal", nativeTx: badTx },
    additionalTransactions: [producingTx],
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: block.header,
  });
  return {
    block,
    setup,
    producingTx,
    badTx,
    producingTxId,
    badTxId,
    producingOutputItemCbors: [producingOutput, keyLockedControlOutput],
    badTxSpendInputs,
    badInputIndex,
    badTxWitnessSet: sdkWitnessSet(badTx),
    badTxScriptWitnessItemCbors: decodeMidgardFieldPreimageV1(
      badTx.witnessSet.scriptTxWitsPreimageCbor,
    ),
    nativeScriptBytes,
    versionedScriptItem,
    expectedScriptHash,
  };
};

export const publishMissingNativeScriptTxReferenceScriptsV1 = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: MissingNativeScriptTxContractsV1;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script: step.spendingScript as Script,
      label: `missing-native-script-tx step-0${(index + 1).toString()}`,
      oversized: true,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
    UTxO,
  ];
};

export const fundMissingNativeScriptTxOutsiderV1 = async (
  harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarnessV1>
  >,
): Promise<void> => {
  // Both of the outsider's addresses are funded. `selectWallet.fromSeed`
  // derives the seed's base address while `resolveProverSigner` derives its
  // enterprise address, and the raw drivers re-select through the signer, so
  // funding only the base address strands every transaction the outsider
  // builds after that call.
  const address = await harness.outsiderLucid.wallet().address();
  const unsigned = await harness.funderLucid
    .newTx()
    .pay.ToAddress(address, { lovelace: 1_000_000_000n })
    .pay.ToAddress(address, { lovelace: 1_000_000_000n })
    .pay.ToAddress(harness.outsiderSigner.address, { lovelace: 1_000_000_000n })
    .pay.ToAddress(harness.outsiderSigner.address, { lovelace: 1_000_000_000n })
    .complete();
  const signed = await unsigned.sign.withWallet().complete();
  await harness.funderLucid.awaitTx(await signed.submit());
};

type RawAdvanceStepV1 = 3 | 4;

const submitRawAdvanceV1 = async ({
  harness,
  stepIndex,
  threadOutRef,
  nextDatum,
  redeemerSchema,
  makeArgs,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarnessV1>
  >;
  readonly stepIndex: RawAdvanceStepV1;
  readonly threadOutRef: string;
  readonly nextDatum: string;
  readonly redeemerSchema: Parameters<typeof Data.to>[1];
  readonly makeArgs: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
  }) => unknown;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxoV1({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      stepIndex,
      threadOutRef,
    });
  harness.proverSigner.selectWallet(harness.proverLucid);
  const feeInput = selectFeeInput(
    await harness.proverLucid.wallet().getUtxos(),
  );
  const outputMatches = computationThreadOutputPredicate({
    address: harness.family.steps[stepIndex + 1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw missing-native-script-tx");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "raw missing-native-script-tx",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "raw missing-native-script-tx output",
      ),
    };
    return Data.to({ Continue: [makeArgs(layout)] }, redeemerSchema);
  }) satisfies BuildTxWithRedeemer;
  const unsigned = await harness.proverLucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([referenceScriptUtxo])
    .pay.ToContract(
      harness.family.steps[stepIndex + 1].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(harness.proverSigner.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  return txHash;
};

export const submitRawMissingNativeScriptTxStep03V1 = async ({
  harness,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarnessV1>
  >;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxoV1({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      stepIndex: 2,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep03State =
    requireMissingNativeScriptTxStepStateV1({
      threadUtxo,
      signer: harness.proverSigner,
      schema: MissingNativeScriptTxStep03Datum,
      stepIndex: 2,
    });
  const nextDatum = Data.to(
    {
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: {
        producing_tx_id: txInclusion.nativeTxId,
        bad_input_output_index: state.input_with_missing_script.output_index,
        bad_tx_id: state.bad_tx_id,
        bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
      },
    },
    MissingNativeScriptTxStep04Datum,
  );
  const result = await submitMissingNativeScriptTxBindingV1({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.family,
    signer: harness.proverSigner,
    stepIndex: 2,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef,
    txInclusion,
    nextDatum,
    spendRedeemerSchema: MissingNativeScriptTxStep03SpendRedeemer,
    referenceScriptUtxo,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    awaitConfirmation: true,
  });
  return result.txHash;
};

export const submitRawMissingNativeScriptTxStep04V1 = async ({
  harness,
  threadOutRef,
  nativeTxCompactCbor,
  outputItemCbors,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarnessV1>
  >;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly outputItemCbors: readonly Uint8Array[];
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo } = await requireMissingNativeScriptTxThreadUtxoV1({
    lucid: harness.proverLucid,
    contracts: harness.family,
    categoryId: harness.category.categoryId,
    stepIndex: 3,
    threadOutRef,
  });
  const state: MissingNativeScriptTxStep04State =
    requireMissingNativeScriptTxStepStateV1({
      threadUtxo,
      signer: harness.proverSigner,
      schema: MissingNativeScriptTxStep04Datum,
      stepIndex: 3,
    });
  const nextDatum = Data.to(
    {
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: {
        expected_missing_script_hash: "44".repeat(28),
        bad_tx_id: state.bad_tx_id,
        bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
      },
    },
    MissingNativeScriptTxStep05Datum,
  );
  const opening = fieldOpeningV1ForField({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.outputs,
    nativeTxCompactCbor,
    carriage: {
      Inline: {
        preimage: encodeMidgardFieldPreimageV1(
          outputItemCbors.map((item) => Buffer.from(item)),
        ).toString("hex"),
      },
    },
  });
  return await submitRawAdvanceV1({
    harness,
    stepIndex: 3,
    threadOutRef,
    nextDatum,
    redeemerSchema: MissingNativeScriptTxStep04SpendRedeemer,
    makeArgs: ({ inputIndex, outputIndex }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      outputs_opening: opening,
    }),
    referenceScriptUtxo,
  });
};

export const submitRawMissingNativeScriptTxStep05V1 = async ({
  harness,
  threadOutRef,
  missingNativeScriptBytes,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarnessV1>
  >;
  readonly threadOutRef: string;
  readonly missingNativeScriptBytes: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo } = await requireMissingNativeScriptTxThreadUtxoV1({
    lucid: harness.proverLucid,
    contracts: harness.family,
    categoryId: harness.category.categoryId,
    stepIndex: 4,
    threadOutRef,
  });
  const state: MissingNativeScriptTxStep05State =
    requireMissingNativeScriptTxStepStateV1({
      threadUtxo,
      signer: harness.proverSigner,
      schema: MissingNativeScriptTxStep05Datum,
      stepIndex: 4,
    });
  const nextDatum = Data.to(
    {
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: missingNativeScriptTxStep06ReadyStateV1(state),
    },
    MissingNativeScriptTxStep06Datum,
  );
  return await submitRawAdvanceV1({
    harness,
    stepIndex: 4,
    threadOutRef,
    nextDatum,
    redeemerSchema: MissingNativeScriptTxStep05SpendRedeemer,
    makeArgs: ({ inputIndex, outputIndex }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      missing_native_script_bytes: Buffer.from(
        missingNativeScriptBytes,
      ).toString("hex"),
    }),
    referenceScriptUtxo,
  });
};

export const submitRawMissingNativeScriptTxStep06V1 = async ({
  harness,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptTxWitsItems,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarnessV1>
  >;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly scriptTxWitsItems: readonly Uint8Array[];
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxoV1({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      stepIndex: 5,
      threadOutRef,
    });
  const opening = fieldOpeningV1ForField({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.scriptWitnesses,
    nativeTxCompactCbor,
    witnessSet,
    carriage: {
      Inline: {
        preimage: encodeMidgardFieldPreimageV1(
          scriptTxWitsItems.map((item) => Buffer.from(item)),
        ).toString("hex"),
      },
    },
  });
  harness.proverSigner.selectWallet(harness.proverLucid);
  const claimRegistryMutation = await prepareFamilyClaimRegistryMutationV1({
    lucid: harness.proverLucid,
    claimRegistry: harness.family.claimRegistry,
    claimRegistryReferenceUtxo:
      harness.witnessReferenceScripts.claimRegistrySpend,
    hubOraclePolicyId: harness.family.hubOraclePolicyId,
    computationThreadPolicyId: harness.family.computationThread.policyId,
    claimId: threadToken.assetName,
    kind: "close",
  });
  const feeInput = selectFeeInput(
    claimRegistryMutation.referenceInputs.reduce<readonly UTxO[]>(
      (utxos, reference) => excludeUtxo(utxos, reference),
      await harness.proverLucid.wallet().getUtxos(),
    ),
  );
  const fraudProofUnit = toUnit(
    harness.family.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: harness.proverSigner.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: harness.family.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw missing-native-script-tx step 06",
    );
    return Data.to(
      {
        Continue: [
          {
            DirectFinalize: {
              input_index: requireInputIndex(
                ctx,
                threadUtxo,
                "raw missing-native-script-tx step 06",
              ),
              output_index: requireUniqueOutputIndex(
                ctx.outputs,
                outputMatches,
                "raw missing-native-script-tx fraud proof",
              ),
              fraud_proof_mint_redeemer_index: requireMintRedeemerIndex(
                ctx,
                harness.family.fraudProof.policyId,
                "raw missing-native-script-tx fraud-proof mint",
              ),
              script_tx_wits_opening: opening,
            },
          },
        ],
      },
      MissingNativeScriptTxStep06SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      harness.family.computationThread.policyId,
      "raw missing-native-script-tx thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const mintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      harness.family.fraudProof.policyId,
      "raw missing-native-script-tx fraud-proof mint",
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
          ctx,
          harness.family.computationThread.policyId,
          "raw missing-native-script-tx thread burn",
        ),
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const computationThreadCarriage = witnessMintingPolicyCarriageV1({
    script: harness.family.computationThread.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.computationThreadMint,
    label: "raw missing-native-script-tx step-06 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriageV1({
    script: harness.family.fraudProof.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.fraudProofMint,
    label: "raw missing-native-script-tx step-06 fraud-proof mint",
  });
  const base = harness.proverLucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      referenceScriptUtxo,
      ...computationThreadCarriage.referenceInputs,
      ...fraudProofCarriage.referenceInputs,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, mintRedeemer)
    .pay.ToContract(
      harness.family.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(harness.proverSigner.paymentKeyHash);
  const unsigned = await fraudProofCarriage
    .attach(computationThreadCarriage.attach(claimRegistryMutation.apply(base)))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  return txHash;
};

const RawCancelSpendRedeemerSchema = faultProofStepRedeemerSchema(Data.Any());
type RawCancelSpendRedeemer = Data.Static<typeof RawCancelSpendRedeemerSchema>;
const RawCancelSpendRedeemer =
  RawCancelSpendRedeemerSchema as unknown as RawCancelSpendRedeemer;

export const submitRawMissingNativeScriptTxOutsiderCancelV1 = async ({
  harness,
  threadOutRef,
  stepIndex,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarnessV1>
  >;
  readonly threadOutRef: string;
  readonly stepIndex: 0 | 1 | 2 | 3 | 4 | 5;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxoV1({
      lucid: harness.outsiderLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      stepIndex,
      threadOutRef,
    });
  harness.outsiderSigner.selectWallet(harness.outsiderLucid);
  const claimRegistryMutation = await prepareFamilyClaimRegistryMutationV1({
    lucid: harness.outsiderLucid,
    claimRegistry: harness.family.claimRegistry,
    claimRegistryReferenceUtxo:
      harness.witnessReferenceScripts.claimRegistrySpend,
    hubOraclePolicyId: harness.family.hubOraclePolicyId,
    computationThreadPolicyId: harness.family.computationThread.policyId,
    claimId: threadToken.assetName,
    kind: "cancel",
  });
  const feeInput = selectFeeInput(
    claimRegistryMutation.referenceInputs.reduce<readonly UTxO[]>(
      (utxos, reference) => excludeUtxo(utxos, reference),
      await harness.outsiderLucid.wallet().getUtxos(),
    ),
  );
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw outsider cancel");
    return Data.to(
      {
        Cancel: {
          input_index: requireInputIndex(
            ctx,
            threadUtxo,
            "raw outsider cancel",
          ),
          computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
            ctx,
            harness.family.computationThread.policyId,
            "raw outsider cancel burn",
          ),
        },
      },
      RawCancelSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      harness.family.computationThread.policyId,
      "raw outsider cancel burn",
    );
    return Data.to(
      {
        BurnForCancellation: {
          burning_token_asset_name: threadToken.assetName,
        },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const computationThreadCarriage = witnessMintingPolicyCarriageV1({
    script: harness.family.computationThread.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.computationThreadMint,
    label: "raw missing-native-script-tx cancel computation-thread mint",
  });
  const base = harness.outsiderLucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      referenceScriptUtxo,
      ...computationThreadCarriage.referenceInputs,
    ])
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .addSignerKey(harness.outsiderSigner.paymentKeyHash);
  const unsigned = await computationThreadCarriage
    .attach(claimRegistryMutation.apply(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.outsiderLucid.awaitTx(txHash);
  return txHash;
};
