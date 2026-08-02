import { readFileSync } from "node:fs";

import {
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  encodeCbor,
  midgardNativeTxFullToCardanoTxEncoding,
} from "@al-ft/midgard-core";
import {
  applyDoubleCborEncoding,
  CML,
  Data,
  Emulator,
  Lucid,
  type SpendingValidator,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildCollateralFreeMidgardSchemaParallelCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  measureCollateralizedPlutusFeasibilityCandidateV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

const alwaysSucceedsBlueprint = JSON.parse(
  readFileSync(
    new URL(
      "../../midgard-node/blueprints/always-succeeds/plutus.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as {
  readonly validators: readonly BlueprintValidator[];
};

const alwaysSucceedsCompiledCode = alwaysSucceedsBlueprint.validators.find(
  (validator) => validator.title === "midgard.deposit_spend.else",
)?.compiledCode;
if (alwaysSucceedsCompiledCode === undefined) {
  throw new Error(
    "Missing always-succeeds blueprint entry midgard.deposit_spend.else",
  );
}

const spendingScript: SpendingValidator = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding(alwaysSucceedsCompiledCode),
};

const optionalCborHex = (
  value: { readonly to_cbor_bytes: () => Uint8Array } | undefined,
): string | undefined =>
  value === undefined
    ? undefined
    : Buffer.from(value.to_cbor_bytes()).toString("hex");

const collectionItemCborHexes = (collection: {
  readonly len: () => number;
  readonly get: (index: number) => { readonly to_cbor_bytes: () => Uint8Array };
}): readonly string[] =>
  Array.from({ length: collection.len() }, (_, index) =>
    Buffer.from(collection.get(index).to_cbor_bytes()).toString("hex"),
  );

const optionalCollectionItemCborHexes = (
  collection:
    | {
        readonly len: () => number;
        readonly get: (index: number) => {
          readonly to_cbor_bytes: () => Uint8Array;
        };
      }
    | undefined,
): readonly string[] | undefined =>
  collection === undefined ? undefined : collectionItemCborHexes(collection);

const optionalKeyHashHexes = (
  collection: CML.Ed25519KeyHashList | undefined,
): readonly string[] | undefined =>
  collection === undefined
    ? undefined
    : Array.from({ length: collection.len() }, (_, index) =>
        collection.get(index).to_hex(),
      );

const withdrawalEntries = (
  withdrawals: CML.MapRewardAccountToCoin | undefined,
): readonly {
  readonly rewardAccountHex: string;
  readonly amount: bigint;
}[] => {
  if (withdrawals === undefined) {
    return [];
  }
  const keys = withdrawals.keys();
  return Array.from({ length: keys.len() }, (_, index) => {
    const rewardAccount = keys.get(index);
    const amount = withdrawals.get(rewardAccount);
    if (amount === undefined) {
      throw new Error("Withdrawal entry has no amount");
    }
    return {
      rewardAccountHex: Buffer.from(
        rewardAccount.to_address().to_raw_bytes(),
      ).toString("hex"),
      amount,
    };
  }).sort((left, right) =>
    left.rewardAccountHex.localeCompare(right.rewardAccountHex),
  );
};

const mintEntries = (
  mint: CML.Mint | undefined,
): readonly {
  readonly policyIdHex: string;
  readonly assets: readonly {
    readonly assetNameHex: string;
    readonly quantity: bigint;
  }[];
}[] => {
  if (mint === undefined) {
    return [];
  }
  const policies = mint.keys();
  return Array.from({ length: policies.len() }, (_, policyIndex) => {
    const policy = policies.get(policyIndex);
    const policyAssets = mint.get_assets(policy);
    if (policyAssets === undefined) {
      throw new Error("Mint policy has no asset map");
    }
    const assetNames = policyAssets.keys();
    const assets = Array.from({ length: assetNames.len() }, (_, assetIndex) => {
      const assetName = assetNames.get(assetIndex);
      const quantity = policyAssets.get(assetName);
      if (quantity === undefined) {
        throw new Error("Mint asset has no quantity");
      }
      return {
        assetNameHex: assetName.to_hex(),
        quantity,
      };
    }).sort((left, right) =>
      left.assetNameHex.localeCompare(right.assetNameHex),
    );
    return {
      policyIdHex: policy.to_hex(),
      assets,
    };
  }).sort((left, right) => left.policyIdHex.localeCompare(right.policyIdHex));
};

const sharedBodyFields = (
  body: CML.TransactionBody,
): {
  readonly spendInputs: readonly string[];
  readonly referenceInputs: readonly string[] | undefined;
  readonly outputs: readonly string[];
  readonly fee: bigint;
  readonly validityStart: bigint | undefined;
  readonly ttl: bigint | undefined;
  readonly withdrawals: readonly {
    readonly rewardAccountHex: string;
    readonly amount: bigint;
  }[];
  readonly requiredSigners: readonly string[] | undefined;
  readonly mint: readonly {
    readonly policyIdHex: string;
    readonly assets: readonly {
      readonly assetNameHex: string;
      readonly quantity: bigint;
    }[];
  }[];
  readonly scriptDataHash: string | undefined;
  readonly auxiliaryDataHash: string | undefined;
  readonly networkId: bigint | undefined;
} => ({
  spendInputs: collectionItemCborHexes(body.inputs()),
  referenceInputs: optionalCollectionItemCborHexes(body.reference_inputs()),
  outputs: collectionItemCborHexes(body.outputs()),
  fee: body.fee(),
  validityStart: body.validity_interval_start(),
  ttl: body.ttl(),
  withdrawals: withdrawalEntries(body.withdrawals()),
  requiredSigners: optionalKeyHashHexes(body.required_signers()),
  mint: mintEntries(body.mint()),
  scriptDataHash: body.script_data_hash()?.to_hex(),
  auxiliaryDataHash: body.auxiliary_data_hash()?.to_hex(),
  networkId: body.network_id()?.network(),
});

describe("canonical V1 redeemer/collateral schema feasibility", () => {
  it("maps one genuine collateralized Plutus redeemer to an exact collateral-free shared-schema fixture", async () => {
    const privateKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const walletAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(privateKey.to_public().hash()),
    )
      .to_address()
      .to_bech32();
    const walletAccount = {
      seedPhrase: "",
      privateKey: privateKey.to_bech32(),
      address: walletAddress,
      assets: { lovelace: 100_000_000n },
    };
    const scriptAddress = validatorToAddress("Custom", spendingScript);
    const emulator = new Emulator(
      [
        walletAccount,
        walletAccount,
        {
          seedPhrase: "",
          privateKey: "",
          address: scriptAddress,
          assets: { lovelace: 100_000_000n },
          outputData: { inline: Data.void() },
        },
      ],
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
    );
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromPrivateKey(privateKey.to_bech32());
    const walletUtxos = (await emulator.getUtxos(walletAddress)).sort(
      (left, right) => left.outputIndex - right.outputIndex,
    );
    expect(walletUtxos).toHaveLength(2);
    expect(walletUtxos.map((utxo) => utxo.outputIndex)).toEqual([0, 1]);
    const feeFundingKeyUtxo = walletUtxos[1];
    const [scriptUtxo] = await emulator.getUtxos(scriptAddress);
    expect(scriptUtxo).toBeDefined();
    expect(scriptUtxo!.txHash).toBe("00".repeat(32));
    expect(scriptUtxo!.outputIndex).toBe(2);
    expect(scriptUtxo!.datum).toBe(Data.void());

    const completed = await lucid
      .newTx()
      .collectFrom([feeFundingKeyUtxo!])
      .collectFrom([scriptUtxo!], Data.void())
      .pay.ToAddress(walletAddress, { lovelace: 10_000_000n })
      .attach.SpendingValidator(spendingScript)
      .complete({ localUPLCEval: true });
    const signed = await completed.sign.withWallet().complete();
    const collateralizedCardanoCborHex = signed.toCBOR();
    const collateralized = measureCollateralizedPlutusFeasibilityCandidateV1(
      collateralizedCardanoCborHex,
    );
    const collateralizedTransaction = CML.Transaction.from_cbor_hex(
      collateralizedCardanoCborHex,
    );
    const collateralizedRedeemers = collateralizedTransaction
      .witness_set()
      .redeemers();
    expect(collateralizedRedeemers).toBeDefined();
    const collateralizedFlatRedeemers =
      collateralizedRedeemers!.to_flat_format();
    expect(collateralizedFlatRedeemers.len()).toBe(1);
    const collateralizedRedeemer = collateralizedFlatRedeemers.get(0);
    const expectedMidgardRedeemersCbor = encodeCbor([
      [
        collateralizedRedeemer.tag(),
        collateralizedRedeemer.index(),
        Buffer.from(collateralizedRedeemer.data().to_canonical_cbor_bytes()),
        [
          collateralizedRedeemer.ex_units().mem(),
          collateralizedRedeemer.ex_units().steps(),
        ],
      ],
    ]);
    expect(collateralizedRedeemer.tag()).toBe(CML.RedeemerTag.Spend);
    expect(collateralizedRedeemer.data().to_canonical_cbor_hex()).toBe(
      "d87980",
    );
    const fullSpendOutRefs = Array.from(
      {
        length: collateralizedTransaction.body().inputs().len(),
      },
      (_, index) => {
        const input = collateralizedTransaction.body().inputs().get(index);
        return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
      },
    ).sort();
    const genesisKeyOutRefs = [`${"00".repeat(32)}#0`, `${"00".repeat(32)}#1`];
    const retainedKeySpendOutRefs = fullSpendOutRefs.filter((outRef) =>
      genesisKeyOutRefs.includes(outRef),
    );
    expect(collateralized.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(collateralized.inputCount).toBe(2);
    expect(fullSpendOutRefs).toContain(`${"00".repeat(32)}#2`);
    expect(retainedKeySpendOutRefs).toHaveLength(1);
    expect(collateralized.collateralInputOutRefs).toHaveLength(1);
    expect(genesisKeyOutRefs).toContain(
      collateralized.collateralInputOutRefs[0],
    );
    expect(collateralized.collateralInputOutRefs[0]).not.toBe(
      retainedKeySpendOutRefs[0],
    );
    expect(collateralized.redeemerCount).toBe(1);
    expect(collateralized.plutusV3ScriptCount).toBe(1);
    expect(collateralized.vkeyWitnessCount).toBe(1);
    expect(collateralized.scriptDataHashHex).toBeDefined();
    expect(collateralizedTransaction.body().withdrawals()).toBeUndefined();
    expect(collateralizedTransaction.body().mint()).toBeUndefined();
    expect(collateralized.executionMemory).toBeGreaterThan(0n);
    expect(collateralized.executionSteps).toBeGreaterThan(0n);
    expect(collateralized.redeemerDataCborHexes).toEqual([Data.void()]);
    const txHash = await signed.submit();
    await expect(lucid.awaitTx(txHash)).resolves.toBe(true);

    let collateralRejection:
      | {
          readonly message: string;
          readonly code: string | null;
          readonly detail: string | null;
        }
      | undefined;
    try {
      cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
        Buffer.from(collateralizedCardanoCborHex, "hex"),
      );
    } catch (error) {
      const structured = error as {
        readonly code?: unknown;
        readonly detail?: unknown;
      };
      collateralRejection = {
        message: error instanceof Error ? error.message : String(error),
        code: typeof structured.code === "string" ? structured.code : null,
        detail:
          typeof structured.detail === "string" ? structured.detail : null,
      };
    }
    expect(collateralRejection).toEqual({
      message:
        "Cardano tx cannot be converted to Midgard native format without dropping fields",
      code: "E_CONVERSION_UNSUPPORTED_FEATURE",
      detail: "collateral_inputs",
    });

    const parallel = buildCollateralFreeMidgardSchemaParallelCandidateV1({
      collateralizedCardanoCborHex,
      privateKeyBech32: privateKey.to_bech32(),
    });
    expect(parallel.parallelRedeemersCborHex).toBe(
      parallel.collateralizedRedeemersCborHex,
    );
    expect(parallel.parallelRedeemersCborHex).toBe(
      collateralized.redeemersCborHex,
    );
    const parallelTransaction = CML.Transaction.from_cbor_hex(parallel.cborHex);
    const parallelSpendOutRefs = Array.from(
      { length: parallelTransaction.body().inputs().len() },
      (_, index) => {
        const input = parallelTransaction.body().inputs().get(index);
        return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
      },
    ).sort();
    expect(parallelSpendOutRefs).toEqual(fullSpendOutRefs);
    expect(sharedBodyFields(parallelTransaction.body())).toEqual(
      sharedBodyFields(collateralizedTransaction.body()),
    );
    expect(parallelTransaction.body().collateral_inputs()?.len() ?? 0).toBe(0);
    expect(parallelTransaction.body().collateral_return()).toBeUndefined();
    expect(parallelTransaction.body().total_collateral()).toBeUndefined();
    expect(optionalCborHex(parallelTransaction.witness_set().redeemers())).toBe(
      optionalCborHex(collateralizedTransaction.witness_set().redeemers()),
    );
    expect(
      optionalCollectionItemCborHexes(
        parallelTransaction.witness_set().plutus_v3_scripts(),
      ),
    ).toEqual(
      optionalCollectionItemCborHexes(
        collateralizedTransaction.witness_set().plutus_v3_scripts(),
      ),
    );
    expect(
      optionalCollectionItemCborHexes(
        parallelTransaction.witness_set().vkeywitnesses(),
      ),
    ).not.toEqual(
      optionalCollectionItemCborHexes(
        collateralizedTransaction.witness_set().vkeywitnesses(),
      ),
    );

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            redeemerCollateralSchemaFeasibilityDiagnosticV1: {
              cardanoSignedBytes: collateralized.signedBytes,
              cardanoByteMargin:
                CARDANO_BOUNDARY_MAX_TX_SIZE_V1 - collateralized.signedBytes,
              sharedSpendOutRefs: fullSpendOutRefs,
              collateralInputOutRefs: collateralized.collateralInputOutRefs,
              collateralReturnCborHex:
                collateralized.collateralReturnCborHex ?? null,
              totalCollateral:
                collateralized.totalCollateral?.toString() ?? null,
              fee: collateralized.fee.toString(),
              scriptDataHashHex: collateralized.scriptDataHashHex,
              redeemersCborHex: collateralized.redeemersCborHex,
              redeemerDataCborHexes: collateralized.redeemerDataCborHexes,
              executionMemory: collateralized.executionMemory.toString(),
              executionSteps: collateralized.executionSteps.toString(),
              productionCollateralRejection: collateralRejection,
              parallelSignedBytes: parallel.cborHex.length / 2,
              exactSharedBodyFields: true,
              exactRedeemerCborMatch: true,
              cardanoEmulatorResult: "PASS",
            },
          },
          null,
          2,
        ),
      );
    }

    const parallelNativeCbor = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
      Buffer.from(parallel.cborHex, "hex"),
    );
    const parallelNative =
      decodeMidgardNativeTxFullV1FromCanonicalCbor(parallelNativeCbor);
    expect(
      parallelNative.witnessSet.redeemerTxWitsPreimageCbor.toString("hex"),
    ).toBe(expectedMidgardRedeemersCbor.toString("hex"));
    const reconstructedCardano = CML.Transaction.from_cbor_bytes(
      midgardNativeTxFullToCardanoTxEncoding(parallelNative),
    );
    const reconstructedRedeemers = reconstructedCardano
      .witness_set()
      .redeemers();
    expect(
      reconstructedRedeemers?.as_map_redeemer_key_to_redeemer_val(),
    ).toBeDefined();
    const reconstructedFlatRedeemers = reconstructedRedeemers!.to_flat_format();
    expect(reconstructedFlatRedeemers.len()).toBe(1);
    const reconstructedRedeemer = reconstructedFlatRedeemers.get(0);
    expect({
      tag: reconstructedRedeemer.tag(),
      index: reconstructedRedeemer.index(),
      dataCborHex: reconstructedRedeemer.data().to_canonical_cbor_hex(),
      memory: reconstructedRedeemer.ex_units().mem(),
      steps: reconstructedRedeemer.ex_units().steps(),
    }).toEqual({
      tag: collateralizedRedeemer.tag(),
      index: collateralizedRedeemer.index(),
      dataCborHex: collateralizedRedeemer.data().to_canonical_cbor_hex(),
      memory: collateralizedRedeemer.ex_units().mem(),
      steps: collateralizedRedeemer.ex_units().steps(),
    });
    const redeemerField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: parallel.cborHex,
      fieldIndex: 8,
    });
    expect(redeemerField.itemCount).toBe(1);
    expect(redeemerField.revealStepCount).toBe(1);
    expect(redeemerField.fieldBytes).toBe(expectedMidgardRedeemersCbor.length);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            redeemerCollateralSchemaFeasibilityV1: {
              cardanoSignedBytes: collateralized.signedBytes,
              cardanoByteMargin:
                CARDANO_BOUNDARY_MAX_TX_SIZE_V1 - collateralized.signedBytes,
              inputCount: collateralized.inputCount,
              outputCount: collateralized.outputCount,
              fee: collateralized.fee.toString(),
              collateralInputOutRefs: collateralized.collateralInputOutRefs,
              collateralReturnCborHex:
                collateralized.collateralReturnCborHex ?? null,
              totalCollateral:
                collateralized.totalCollateral?.toString() ?? null,
              scriptDataHashHex: collateralized.scriptDataHashHex,
              vkeyWitnessCount: collateralized.vkeyWitnessCount,
              plutusV3ScriptCount: collateralized.plutusV3ScriptCount,
              redeemerCount: collateralized.redeemerCount,
              redeemersCborHex: collateralized.redeemersCborHex,
              normalizedMidgardRedeemersCborHex:
                expectedMidgardRedeemersCbor.toString("hex"),
              redeemerDataCborHexes: collateralized.redeemerDataCborHexes,
              executionMemory: collateralized.executionMemory.toString(),
              executionSteps: collateralized.executionSteps.toString(),
              productionCollateralRejection: collateralRejection,
              parallelSignedBytes: parallel.cborHex.length / 2,
              exactRedeemerCborMatch: true,
              field8Bytes: redeemerField.fieldBytes,
              field8Items: redeemerField.itemCount,
              field8RevealSteps: redeemerField.revealStepCount,
              field8MaxChunkBytes: redeemerField.maxChunkBytes,
              field8MaxRevealBytes: redeemerField.maxRevealBytes,
              completeFoldSteps: redeemerField.completeFoldStepCount,
              emulatorResult: "PASS",
            },
          },
          null,
          2,
        ),
      );
    }
  }, 300_000);
});
