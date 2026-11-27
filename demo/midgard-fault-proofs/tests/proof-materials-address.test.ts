/**
 * The prover's two addresses. Its money sits at the plain enterprise address
 * of its key (the funding address), where every pure-Ada coin without a
 * reference script is funding, a refund's tag datum included. Its in-flight
 * proof materials sit at the enterprise address of the native script
 * `sig(key)` (the proof-materials address), which only that key spends. A read of either by its full
 * address never returns an output of the other, and the workflow's wallet
 * offers only the funding address's coins to coin selection and collateral.
 * A workflow transaction is signed only in the workflow's shape: it pays no
 * other address of a key, posts collateral from, and returns it to, the
 * funding address only, and embeds our proof-materials lock only to spend a
 * proof material.
 */
import * as SDK from "@al-ft/midgard-sdk/fraud-proof";
import {
  CML,
  Constr,
  credentialToAddress,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { isPlainFundingOutput } from "../src/funding/plain-funding-output.js";
import {
  proofMaterialsAddress,
  proofMaterialsAddressOfFundingAddress,
  proofMaterialsScript,
} from "../src/funding/proof-materials-address.js";
import { resolveProverSigner } from "../src/runtime.js";
import { assertManifestBoundWorkflowSigner } from "../src/workflow/deployment-manifest-binding.assert-deployment-info-matches-manifest.js";
import { restrictWorkflowWallet } from "../src/workflow/workflow-wallet.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import { signingScene } from "./support/workflow-transaction-shape.js";

const KEY = "a7".repeat(28);
/** A Close refund's tag: the OutputReference of the bond it refunds. */
const TAG = Data.to(new Constr(0, ["ab".repeat(32), 0n]));
const coin = (output: Partial<UTxO>): UTxO => ({
  txHash: "61".repeat(32),
  outputIndex: 0,
  address: credentialToAddress("Preview", { type: "Key", hash: KEY }),
  assets: { lovelace: 5_000_000n },
  ...output,
});

describe("the proof-materials address", () => {
  it("is the enterprise address of the key's sig native script, on the funding address's network", () => {
    for (const network of ["Preview", "Mainnet"] as const) {
      const funding = credentialToAddress(network, { type: "Key", hash: KEY });
      const materials = proofMaterialsAddress(network, KEY);
      const details = getAddressDetails(materials);
      expect(details.type).toBe("Enterprise");
      expect(details.networkId).toBe(getAddressDetails(funding).networkId);
      expect(details.paymentCredential).toEqual({
        type: "Script",
        hash: validatorToScriptHash(proofMaterialsScript(KEY)),
      });
      expect(details.stakeCredential).toBeUndefined();
      // As long as the funding address, so publications keep their size.
      expect(Buffer.from(details.address.hex, "hex").length).toBe(
        Buffer.from(getAddressDetails(funding).address.hex, "hex").length,
      );
      expect(materials).toBe(SDK.midgardCekStagingAddress(network, KEY));
      expect(materials).not.toBe(funding);
      expect(proofMaterialsAddressOfFundingAddress(funding)).toBe(materials);
    }
    const other = credentialToAddress("Preview", {
      type: "Key",
      hash: "a8".repeat(28),
    });
    expect(proofMaterialsAddressOfFundingAddress(other)).not.toBe(
      proofMaterialsAddress("Preview", KEY),
    );
  });

  it("is derived only from a plain enterprise key address", () => {
    for (const address of [
      proofMaterialsAddress("Preview", KEY),
      credentialToAddress("Preview", { type: "Script", hash: KEY }),
    ])
      expect(() => proofMaterialsAddressOfFundingAddress(address)).toThrow(
        "the funding address must be a plain enterprise key address",
      );
  });

  it("binds a workflow signer to both of its key's manifest-network addresses", () => {
    const address = credentialToAddress("Preview", { type: "Key", hash: KEY });
    const signer = {
      address,
      paymentKeyHash: KEY,
      proofMaterialsAddress: proofMaterialsAddress("Preview", KEY),
    };
    expect(() =>
      assertManifestBoundWorkflowSigner({ network: "Preview", ...signer }),
    ).not.toThrow();
    expect(() =>
      assertManifestBoundWorkflowSigner({ network: "Mainnet", ...signer }),
    ).toThrow("manifest-network enterprise address");
    for (const substituted of [
      address,
      proofMaterialsAddress("Preview", "a8".repeat(28)),
      credentialToAddress(
        "Preview",
        { type: "Key", hash: KEY },
        { type: "Key", hash: "a8".repeat(28) },
      ),
    ])
      expect(() =>
        assertManifestBoundWorkflowSigner({
          network: "Preview",
          ...signer,
          proofMaterialsAddress: substituted,
        }),
      ).toThrow("proof-materials address");
  });
});

describe("a funding coin", () => {
  it("is any pure-Ada output without a reference script, a datum or datum hash included", () => {
    expect(isPlainFundingOutput(coin({}))).toBe(true);
    expect(isPlainFundingOutput(coin({ datum: TAG }))).toBe(true);
    expect(isPlainFundingOutput(coin({ datumHash: "cd".repeat(32) }))).toBe(
      true,
    );
  });

  it("is never an output holding a token or a reference script", () => {
    expect(
      isPlainFundingOutput(
        coin({
          assets: { lovelace: 5_000_000n, [`${"aa".repeat(28)}00`]: 1n },
        }),
      ),
    ).toBe(false);
    expect(
      isPlainFundingOutput(
        coin({ scriptRef: { type: "PlutusV3", script: "46010000222601" } }),
      ),
    ).toBe(false);
  });
});

describe("the funding and proof-materials reads", () => {
  it("never return each other's outputs, and coin selection offers only funding coins, a tagged refund included", async () => {
    const prover = generateEmulatorAccount({});
    const signer = resolveProverSigner({
      network: "Custom",
      walletSeedPhrase: prover.seedPhrase,
    });
    const genesis = (address: string, lovelace: bigint, datum?: string) => ({
      ...prover,
      address,
      assets: { lovelace },
      ...(datum === undefined ? {} : { outputData: { inline: datum } }),
    });
    const emulator = new Emulator([
      genesis(signer.address, 40_000_000n),
      genesis(signer.address, 60_000_000n, TAG),
      genesis(signer.proofMaterialsAddress, 80_000_000n, Data.to("cafe")),
    ]);
    const [plain, refund, material] = (
      await emulator.getUtxosByOutRef(
        [0, 1, 2].map((outputIndex) => ({
          txHash: "00".repeat(32),
          outputIndex,
        })),
      )
    ).sort((left, right) => left.outputIndex - right.outputIndex);
    const label = ({ txHash, outputIndex }: UTxO) =>
      `${txHash}#${outputIndex.toString()}`;
    const byLabel = (utxos: readonly UTxO[]) => utxos.map(label).sort();

    const lucid = await Lucid(emulator, "Custom");
    expect(byLabel(await lucid.utxosAt(signer.address))).toEqual(
      byLabel([plain!, refund!]),
    );
    expect(byLabel(await lucid.utxosAt(signer.proofMaterialsAddress))).toEqual([
      label(material!),
    ]);

    // The workflow's restricted wallet funds and posts collateral from the
    // funding coins alone.
    const process = createTestWorkflowProcess();
    restrictWorkflowWallet({
      signer,
      objectiveWallet: process.objectiveWallet({
        category: "doubleSpend",
        headerHash: "c1".repeat(32),
      }),
    }).selectWallet(lucid);
    expect(byLabel(await lucid.wallet().getUtxos())).toEqual(
      byLabel([plain!, refund!]),
    );
  });
});

describe("a workflow transaction at signing", () => {
  it("pays no other address of a key", async () => {
    const scene = await signingScene();
    const stranger = credentialToAddress("Custom", {
      type: "Key",
      hash: "52".repeat(28),
    });
    // The funding key staked to another key is not ours either.
    const restaked = credentialToAddress(
      "Custom",
      { type: "Key", hash: scene.signer.paymentKeyHash },
      { type: "Key", hash: "a8".repeat(28) },
    );
    for (const address of [
      scene.signer.address,
      scene.signer.proofMaterialsAddress,
      scene.contract,
    ])
      await expect(
        scene.sign({ inputs: [scene.coin], outputs: [address] }),
      ).resolves.toBeDefined();
    for (const address of [stranger, restaked])
      await expect(
        scene.sign({ inputs: [scene.coin], outputs: [address] }),
      ).rejects.toThrow(
        "workflow transaction output escapes the governed contract roster",
      );
  });

  it("posts collateral from, and returns it to, the funding address only", async () => {
    const scene = await signingScene();
    const shape = (collateral: string, returnAddress: string) => ({
      inputs: [scene.coin],
      outputs: [scene.contract],
      collateral: { inputs: [collateral], returnAddress },
    });
    await expect(
      scene.sign(shape(scene.collateral, scene.signer.address)),
    ).resolves.toBeDefined();
    await expect(
      scene.sign(shape(scene.material, scene.signer.address)),
    ).rejects.toThrow("workflow collateral is not our wallet's");
    await expect(
      scene.sign(shape(scene.collateral, scene.signer.proofMaterialsAddress)),
    ).rejects.toThrow("workflow collateral return escapes our wallet");
  });

  it("embeds our proof-materials lock only to spend a proof material", async () => {
    const scene = await signingScene();
    const lock = (key: string) =>
      CML.NativeScript.new_script_pubkey(CML.Ed25519KeyHash.from_hex(key));
    const ours = lock(scene.signer.paymentKeyHash);
    await expect(
      scene.sign({
        inputs: [scene.material],
        outputs: [scene.signer.address],
        nativeScripts: [ours],
      }),
    ).resolves.toBeDefined();
    await expect(
      scene.sign({
        inputs: [scene.coin],
        outputs: [scene.signer.address],
        nativeScripts: [ours],
      }),
    ).rejects.toThrow(
      "workflow transaction embeds our proof-materials script without spending a proof material",
    );
    for (const nativeScripts of [[lock("a8".repeat(28))], [ours, ours]])
      await expect(
        scene.sign({
          inputs: [scene.material],
          outputs: [scene.signer.address],
          nativeScripts,
        }),
      ).rejects.toThrow(
        "workflow transaction embeds an executable script witness off the roster",
      );
  });

  it("keeps a fee our coins pay within the protocol's per-transaction ceiling, and leaves a fee no coin of ours pays to the protocol", async () => {
    const scene = await signingScene();
    const over = scene.maximumFee + 1n;
    await expect(
      scene.sign({ inputs: [scene.coin], outputs: [scene.contract] }),
    ).resolves.toBeDefined();
    for (const inputs of [[scene.coin], [scene.material]])
      await expect(
        scene.sign({
          inputs,
          outputs: [scene.contract],
          fee: over,
          ...(inputs[0] === scene.material
            ? {
                nativeScripts: [
                  CML.NativeScript.new_script_pubkey(
                    CML.Ed25519KeyHash.from_hex(scene.signer.paymentKeyHash),
                  ),
                ],
              }
            : {}),
        }),
      ).rejects.toThrow("workflow transaction fee is outside the policy bound");
    // A slash's fee is its slashed bond's: no input of ours pays it.
    await expect(
      scene.sign({
        inputs: [scene.foreign],
        outputs: [scene.contract],
        fee: over,
        collateral: {
          inputs: [scene.collateral],
          returnAddress: scene.signer.address,
        },
      }),
    ).resolves.toBeDefined();
  });
});
