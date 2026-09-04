import {
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeScript,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  encodeMidgardTxInputCanonical,
  faultProofStepRedeemerSchema,
  fieldOpeningForField,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  MIDGARD_FIELD_INDEX,
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
  missingNativeScriptTxStep06ReadyState,
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

import type { MissingNativeScriptTxContracts } from "../../src/missing-native-script-tx/contracts.js";
import {
  requireMissingNativeScriptTxStepState,
  requireMissingNativeScriptTxThreadUtxo,
} from "../../src/missing-native-script-tx/submit-common.js";
import { submitMissingNativeScriptTxBinding } from "../../src/missing-native-script-tx/submit-native-binding.js";
import { resolveProverSigner } from "../../src/runtime.js";
import type { SubmitStep01TxInclusion } from "../../src/submit-step-01.js";
import { selectFeeInput } from "../../src/submit-step-01.js";
import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../../src/tx-layout.js";
import { witnessMintingPolicyCarriage } from "../../src/witness-reference-scripts.js";
import {
  buildDecodingBlockFixture,
  type DecodingBlockFixture,
} from "./native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  makeFaultProofEmulatorHarness,
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

export const missingVersionedScript = () => {
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

const scriptLockedOutputCbor = ({
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

const sdkWitnessSet = (tx: MidgardNativeTxFull): NativeTxWitnessSetCompact => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet);
  return {
    addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
      "hex",
    ),
  };
};

export const makeMissingNativeScriptTxEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
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

export type MissingNativeScriptTxFixture = {
  readonly block: DecodingBlockFixture;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>>;
  readonly producingTx: MidgardNativeTxFull;
  readonly badTx: MidgardNativeTxFull;
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

export const setupMissingNativeScriptTxFixture = async ({
  harness,
  scriptPresent = false,
  keyLockedProducingOutput = false,
  decoySpendInputCount = 0,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly scriptPresent?: boolean;
  readonly keyLockedProducingOutput?: boolean;
  /**
   * Extra fabricated spend inputs committed alongside the accused pair, so a
   * test can grow the bad transaction's field-0 preimage past the §8.4
   * tier-1 bound and let size alone select tier-2 carriage.
   */
  readonly decoySpendInputCount?: number;
}): Promise<MissingNativeScriptTxFixture> => {
  const versionedScript = missingVersionedScript();
  const nativeScriptBytes = Buffer.from(versionedScript.scriptBytes);
  const versionedScriptItem = encodeMidgardVersionedScript(versionedScript);
  const expectedScriptHash = hashMidgardVersionedScript(versionedScript);
  const producingOutput = scriptLockedOutputCbor({
    credentialHash: expectedScriptHash,
    keyLocked: keyLockedProducingOutput,
  });
  const keyLockedControlOutput = scriptLockedOutputCbor({
    credentialHash: "99".repeat(28),
    keyLocked: true,
  });
  const producingTx = makeNativeTx({
    spendInputCbors: [],
    fee: 1_000n,
    outputCbors: [producingOutput, keyLockedControlOutput],
  });
  const producingTxId = computeMidgardNativeTxId(producingTx).toString("hex");
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
      encodeMidgardTxInputCanonical(left),
      encodeMidgardTxInputCanonical(right),
    ),
  );
  const badInputIndex = badTxSpendInputs.findIndex(
    (input) =>
      input.tx_id === accusedInput.tx_id &&
      input.output_index === accusedInput.output_index,
  );
  const badTx = makeNativeTx({
    spendInputCbors: badTxSpendInputs.map(encodeMidgardTxInputCanonical),
    fee: 2_000n,
    scriptTxWitsPreimageCbor: scriptPresent
      ? encodeCbor([versionedScriptItem])
      : encodeCbor([]),
  });
  const badTxId = computeMidgardNativeTxId(badTx).toString("hex");
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
  const block = await buildDecodingBlockFixture({
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
    badTxScriptWitnessItemCbors: decodeMidgardFieldPreimage(
      badTx.witnessSet.scriptTxWitsPreimageCbor,
    ),
    nativeScriptBytes,
    versionedScriptItem,
    expectedScriptHash,
  };
};

export const publishMissingNativeScriptTxReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: MissingNativeScriptTxContracts;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script: step.spendingScript as Script,
      label: `missing-native-script-tx step-0${(index + 1).toString()}`,
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

export const fundMissingNativeScriptTxOutsider = async (
  harness: Awaited<ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>>,
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

type RawAdvanceStep = 3 | 4;

const submitRawAdvance = async ({
  harness,
  stepIndex,
  threadOutRef,
  nextDatum,
  redeemerSchema,
  makeArgs,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly stepIndex: RawAdvanceStep;
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
    await requireMissingNativeScriptTxThreadUtxo({
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

export const submitRawMissingNativeScriptTxStep03 = async ({
  harness,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxo({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      stepIndex: 2,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep03State =
    requireMissingNativeScriptTxStepState({
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
  const result = await submitMissingNativeScriptTxBinding({
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

export const submitRawMissingNativeScriptTxStep04 = async ({
  harness,
  threadOutRef,
  nativeTxCompactCbor,
  outputItemCbors,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly outputItemCbors: readonly Uint8Array[];
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo } = await requireMissingNativeScriptTxThreadUtxo({
    lucid: harness.proverLucid,
    contracts: harness.family,
    categoryId: harness.category.categoryId,
    stepIndex: 3,
    threadOutRef,
  });
  const state: MissingNativeScriptTxStep04State =
    requireMissingNativeScriptTxStepState({
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
  const opening = fieldOpeningForField({
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    nativeTxCompactCbor,
    carriage: {
      Inline: {
        preimage: encodeMidgardFieldPreimage(
          outputItemCbors.map((item) => Buffer.from(item)),
        ).toString("hex"),
      },
    },
  });
  return await submitRawAdvance({
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

export const submitRawMissingNativeScriptTxStep05 = async ({
  harness,
  threadOutRef,
  missingNativeScriptBytes,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly threadOutRef: string;
  readonly missingNativeScriptBytes: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo } = await requireMissingNativeScriptTxThreadUtxo({
    lucid: harness.proverLucid,
    contracts: harness.family,
    categoryId: harness.category.categoryId,
    stepIndex: 4,
    threadOutRef,
  });
  const state: MissingNativeScriptTxStep05State =
    requireMissingNativeScriptTxStepState({
      threadUtxo,
      signer: harness.proverSigner,
      schema: MissingNativeScriptTxStep05Datum,
      stepIndex: 4,
    });
  const nextDatum = Data.to(
    {
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: missingNativeScriptTxStep06ReadyState(state),
    },
    MissingNativeScriptTxStep06Datum,
  );
  return await submitRawAdvance({
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

export const submitRawMissingNativeScriptTxStep06 = async ({
  harness,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptTxWitsItems,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly threadOutRef: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly scriptTxWitsItems: readonly Uint8Array[];
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxo({
      lucid: harness.proverLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      stepIndex: 5,
      threadOutRef,
    });
  const opening = fieldOpeningForField({
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    nativeTxCompactCbor,
    witnessSet,
    carriage: {
      Inline: {
        preimage: encodeMidgardFieldPreimage(
          scriptTxWitsItems.map((item) => Buffer.from(item)),
        ).toString("hex"),
      },
    },
  });
  harness.proverSigner.selectWallet(harness.proverLucid);
  const feeInput = selectFeeInput(
    await harness.proverLucid.wallet().getUtxos(),
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
  const computationThreadCarriage = witnessMintingPolicyCarriage({
    script: harness.family.computationThread.mintingScript,
    referenceUtxo: harness.witnessReferenceScripts.computationThreadMint,
    label: "raw missing-native-script-tx step-06 computation-thread mint",
  });
  const fraudProofCarriage = witnessMintingPolicyCarriage({
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
    .attach(computationThreadCarriage.attach(base))
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.proverLucid.awaitTx(txHash);
  return txHash;
};

const RawCancelSpendRedeemerSchema = faultProofStepRedeemerSchema(Data.Any());
type RawCancelSpendRedeemer = Data.Static<typeof RawCancelSpendRedeemerSchema>;
const RawCancelSpendRedeemer = asDataType<RawCancelSpendRedeemer>(
  RawCancelSpendRedeemerSchema,
);

export const submitRawMissingNativeScriptTxOutsiderCancel = async ({
  harness,
  threadOutRef,
  stepIndex,
  referenceScriptUtxo,
}: {
  readonly harness: Awaited<
    ReturnType<typeof makeMissingNativeScriptTxEmulatorHarness>
  >;
  readonly threadOutRef: string;
  readonly stepIndex: 0 | 1 | 2 | 3 | 4 | 5;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxo({
      lucid: harness.outsiderLucid,
      contracts: harness.family,
      categoryId: harness.category.categoryId,
      stepIndex,
      threadOutRef,
    });
  harness.outsiderSigner.selectWallet(harness.outsiderLucid);
  const feeInput = selectFeeInput(
    await harness.outsiderLucid.wallet().getUtxos(),
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
  const computationThreadCarriage = witnessMintingPolicyCarriage({
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
    .attach(base)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.outsiderLucid.awaitTx(txHash);
  return txHash;
};
