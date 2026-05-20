import fs from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  cardanoTxBytesToMidgardNativeTxFullBytes,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import {
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  type QueuedTx,
} from "@al-ft/midgard-validation";
import {
  makeCardanoTxOutput,
  makeMidgardTxOutput,
} from "./midgard-output-helpers.js";

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(__dirname, "./txs/txs_0.json");
const txFixtures = JSON.parse(
  fs.readFileSync(fixturePath, "utf8"),
) as readonly TxFixture[];

const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase1_midgard",
} as const;

const TEST_ADDRESS =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

const makePubKeyOutput = (
  keyHash: InstanceType<typeof CML.Ed25519KeyHash>,
  value: InstanceType<typeof CML.Value>,
): Buffer =>
  Buffer.from(
    makeMidgardTxOutput(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(keyHash),
      ).to_address(),
      value,
    ).to_cbor_bytes(),
  );

describe("phase-a converted fixture signature bridge", () => {
  it("rejects converted Cardano witnesses that only sign the original Cardano body hash", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex("11".repeat(32)),
        0n,
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      makeCardanoTxOutput(
        CML.EnterpriseAddress.new(
          0,
          CML.Credential.new_pub_key(signerKey.to_public().hash()),
        ).to_address(),
        CML.Value.from_coin(3_000_000n),
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, 0n);
    const witnessSet = CML.TransactionWitnessSet.new();
    const vkeyWitnesses = CML.VkeywitnessList.new();
    vkeyWitnesses.add(
      CML.make_vkey_witness(CML.hash_transaction(body), signerKey),
    );
    witnessSet.set_vkeywitnesses(vkeyWitnesses);
    const cardanoBytes = Buffer.from(
      CML.Transaction.new(body, witnessSet, true, undefined).to_cbor_bytes(),
    );
    const nativeBytes = cardanoTxBytesToMidgardNativeTxFullBytes(cardanoBytes);
    const nativeTx = decodeMidgardNativeTxFull(nativeBytes);
    const txId = computeMidgardNativeTxId(nativeTx);
    const queued: QueuedTx = {
      txId,
      txCbor: nativeBytes,
      arrivalSeq: 0n,
      createdAt: new Date(0),
    };
    const result = await Effect.runPromise(
      runPhaseAValidation([queued], phaseAConfig),
    );
    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.InvalidSignature);
  });

  it("accepts converted fixture bytes only when witnesses sign the Midgard-native body hash", async () => {
    const signerKey = CML.PrivateKey.generate_ed25519();
    const mintScript = CML.NativeScript.new_script_pubkey(
      signerKey.to_public().hash(),
    );
    const policyId = mintScript.hash();
    const assetName = CML.AssetName.from_raw_bytes(Buffer.from("0c", "hex"));

    const inputs = CML.TransactionInputList.new();
    const input = CML.TransactionInput.new(
      CML.TransactionHash.from_hex("11".repeat(32)),
      0n,
    );
    inputs.add(input);

    const mintAssets = CML.MapAssetNameToCoin.new();
    mintAssets.insert(assetName, 1n);
    const multiasset = CML.MultiAsset.new();
    multiasset.insert_assets(policyId, mintAssets);
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      makeCardanoTxOutput(
        CML.Address.from_bech32(TEST_ADDRESS),
        CML.Value.new(3_000_000n, multiasset),
      ),
    );

    const body = CML.TransactionBody.new(inputs, outputs, 0n);
    const mint = CML.Mint.new();
    const mintPolicyAssets = CML.MapAssetNameToNonZeroInt64.new();
    mintPolicyAssets.insert(assetName, 1n);
    mint.insert_assets(policyId, mintPolicyAssets);
    body.set_mint(mint);

    const witnessSet = CML.TransactionWitnessSet.new();
    const unsignedCardanoTx = CML.Transaction.new(
      body,
      witnessSet,
      true,
      undefined,
    );
    const unsignedNativeBytes = cardanoTxBytesToMidgardNativeTxFullBytes(
      Buffer.from(unsignedCardanoTx.to_cbor_bytes()),
    );
    const nativeTx = decodeMidgardNativeTxFull(unsignedNativeBytes);
    const vkeyWitnesses = CML.VkeywitnessList.new();
    vkeyWitnesses.add(
      CML.make_vkey_witness(
        CML.TransactionHash.from_raw_bytes(computeMidgardNativeTxId(nativeTx)),
        signerKey,
      ),
    );
    witnessSet.set_vkeywitnesses(vkeyWitnesses);
    const nativeScripts = CML.NativeScriptList.new();
    nativeScripts.add(mintScript);
    witnessSet.set_native_scripts(nativeScripts);

    const cardanoTx = CML.Transaction.new(body, witnessSet, true, undefined);
    const nativeBytes = cardanoTxBytesToMidgardNativeTxFullBytes(
      Buffer.from(cardanoTx.to_cbor_bytes()),
    );
    const converted = decodeMidgardNativeTxFull(nativeBytes);
    const txId = computeMidgardNativeTxId(converted);

    const queued: QueuedTx = {
      txId,
      txCbor: nativeBytes,
      arrivalSeq: 0n,
      createdAt: new Date(0),
    };
    const phaseA = await Effect.runPromise(
      runPhaseAValidation([queued], phaseAConfig),
    );
    expect(phaseA.rejected).toHaveLength(0);
    expect(phaseA.accepted).toHaveLength(1);

    const preState = new Map<string, Buffer>([
      [
        Buffer.from(input.to_cbor_bytes()).toString("hex"),
        makePubKeyOutput(
          signerKey.to_public().hash(),
          CML.Value.from_coin(3_000_000n),
        ),
      ],
    ]);
    const { accepted, rejected } = await Effect.runPromise(
      runPhaseBValidationWithPatch(phaseA.accepted, preState, {
        nowCardanoSlotNo: 0n,
        bucketConcurrency: 1,
      }),
    );
    expect(rejected).toHaveLength(0);
    expect(accepted).toHaveLength(1);
  });
});
