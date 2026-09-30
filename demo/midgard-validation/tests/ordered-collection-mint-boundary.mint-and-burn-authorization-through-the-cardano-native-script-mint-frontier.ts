import "./ordered-collection-mint-boundary.canonical-v1-mint-cardano-boundary.js";

import { CML, Emulator, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  CARDANO_BOUNDARY_MINT_ASSET_NAME,
  deterministicCardanoBoundaryPrivateKey,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
} from "./helpers/ordered-collection-boundary.js";

// C49 (#540). Mirrors the Aiken
// `mint_and_burn_authorization_rejects_unbacked_policy` selector's frontier
// membership check at the Cardano layer this package actually submits
// through: a mint step may only move value for a policy a native script in
// the SAME transaction's witness set actually witnesses (Emulator's
// `checkAndConsumeHash` walks every `body.mint()` policy id and throws
// "Missing script witness" for any that lack one) -- the direct analogue of
// the authenticated mint frontier's leaf-membership check.
//
// ATTRIBUTION: all four legs mint or burn the identical asset under the
// identical policy id, with the identical value-conservation shape (mint
// quantity exactly settles the value delta between funding input and
// output). The sole field that differs between a passing and a failing leg
// is whether `backedScript` is present in the witness set's native scripts --
// the policy's real authorization, stripped out.
describe("mint and burn authorization through the Cardano native-script mint frontier", () => {
  it("authorizes mint and burn for a native-script-backed policy, and rejects both once the same policy's witnessing native script is withheld", async () => {
    const signingKey = deterministicCardanoBoundaryPrivateKey(7);
    const signerHash = signingKey.to_public().hash();
    const address = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(signerHash),
    )
      .to_address()
      .to_bech32();

    const backedScript = CML.NativeScript.new_script_pubkey(signerHash);
    const backedPolicyId = backedScript.hash();
    const unit = toUnit(
      backedPolicyId.to_hex(),
      CARDANO_BOUNDARY_MINT_ASSET_NAME.toString("hex"),
    );

    const genesisLovelace = 1_000_000_000_000n;
    const emulator = new Emulator(
      [
        // 0: backed mint funding (no pre-existing units).
        {
          seedPhrase: "",
          privateKey: signingKey.to_bech32(),
          address,
          assets: { lovelace: genesisLovelace },
        },
        // 1: backed burn funding (already holds the 3 units it will burn).
        {
          seedPhrase: "",
          privateKey: signingKey.to_bech32(),
          address,
          assets: { lovelace: genesisLovelace, [unit]: 3n },
        },
        // 2: unbacked mint funding.
        {
          seedPhrase: "",
          privateKey: signingKey.to_bech32(),
          address,
          assets: { lovelace: genesisLovelace },
        },
        // 3: unbacked burn funding.
        {
          seedPhrase: "",
          privateKey: signingKey.to_bech32(),
          address,
          assets: { lovelace: genesisLovelace, [unit]: 3n },
        },
      ],
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
    );
    const utxos = await emulator.getUtxos(address);
    expect(utxos).toHaveLength(4);

    const buildMintOrBurnTx = ({
      fundingInput,
      mintQuantity,
      includeMintScript,
    }: {
      readonly fundingInput: (typeof utxos)[number];
      readonly mintQuantity: bigint;
      readonly includeMintScript: boolean;
    }): string => {
      const fee = 200_000n;
      const inputs = CML.TransactionInputList.new();
      inputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(fundingInput.txHash),
          BigInt(fundingInput.outputIndex),
        ),
      );
      const remainingAssetQuantity =
        (fundingInput.assets[unit] ?? 0n) + mintQuantity;
      const outputAssets = CML.MultiAsset.new();
      if (remainingAssetQuantity > 0n) {
        const perAsset = CML.MapAssetNameToCoin.new();
        perAsset.insert(
          CML.AssetName.from_raw_bytes(CARDANO_BOUNDARY_MINT_ASSET_NAME),
          remainingAssetQuantity,
        );
        outputAssets.insert_assets(backedPolicyId, perAsset);
      }
      const outputs = CML.TransactionOutputList.new();
      outputs.add(
        CML.TransactionOutputBuilder.new()
          .with_address(CML.Address.from_bech32(address))
          .next()
          .with_value(
            CML.Value.new(
              (fundingInput.assets.lovelace ?? 0n) - fee,
              outputAssets,
            ),
          )
          .build()
          .output(),
      );
      const mint = CML.Mint.new();
      const mintAssets = CML.MapAssetNameToNonZeroInt64.new();
      mintAssets.insert(
        CML.AssetName.from_raw_bytes(CARDANO_BOUNDARY_MINT_ASSET_NAME),
        mintQuantity,
      );
      mint.insert_assets(backedPolicyId, mintAssets);
      const body = CML.TransactionBody.new(inputs, outputs, fee);
      body.set_mint(mint);
      const vkeyWitnesses = CML.VkeywitnessList.new();
      vkeyWitnesses.add(
        CML.make_vkey_witness(CML.hash_transaction(body), signingKey),
      );
      const witnessSet = CML.TransactionWitnessSet.new();
      witnessSet.set_vkeywitnesses(vkeyWitnesses);
      if (includeMintScript) {
        const nativeScripts = CML.NativeScriptList.new();
        nativeScripts.add(backedScript);
        witnessSet.set_native_scripts(nativeScripts);
      }
      return CML.Transaction.new(
        body,
        witnessSet,
        true,
        undefined,
      ).to_cbor_hex();
    };

    // Positive controls: the backed policy mints and burns.
    const mintTxHash = await emulator.submitTx(
      buildMintOrBurnTx({
        fundingInput: utxos[0]!,
        mintQuantity: 3n,
        includeMintScript: true,
      }),
    );
    await expect(emulator.awaitTx(mintTxHash)).resolves.toBe(true);

    const burnTxHash = await emulator.submitTx(
      buildMintOrBurnTx({
        fundingInput: utxos[1]!,
        mintQuantity: -3n,
        includeMintScript: true,
      }),
    );
    await expect(emulator.awaitTx(burnTxHash)).resolves.toBe(true);

    // The same steps under the same policy, minus its witnessing native
    // script, are unprovable: mint and burn.
    expect(() =>
      emulator.submitTx(
        buildMintOrBurnTx({
          fundingInput: utxos[2]!,
          mintQuantity: 3n,
          includeMintScript: false,
        }),
      ),
    ).toThrow("Missing script witness");

    expect(() =>
      emulator.submitTx(
        buildMintOrBurnTx({
          fundingInput: utxos[3]!,
          mintQuantity: -3n,
          includeMintScript: false,
        }),
      ),
    ).toThrow("Missing script witness");
  }, 60_000);
});
