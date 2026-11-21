/**
 * A prover's restricted workflow wallet, held to a deployment's transaction
 * shape, over an emulator with two funding coins, one proof material and one
 * coin of somebody else: `sign` hand-builds a transaction of a given shape
 * over them and has the wallet sign it.
 */
import {
  CML,
  credentialToAddress,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { resolveProverSigner } from "../../src/runtime.js";
import { validationTraceMoveFeeCeiling } from "../../src/validation-trace-dispute/move-fee.js";
import { restrictWorkflowWallet } from "../../src/workflow/workflow-wallet.js";
import { createTestWorkflowProcess } from "./workflow-process.js";

/** A governed contract of the deployment. */
const CONTRACT_HASH = "c0".repeat(28);

export type SigningShape = Readonly<{
  inputs: readonly string[];
  outputs: readonly string[];
  collateral?: Readonly<{ inputs: readonly string[]; returnAddress: string }>;
  nativeScripts?: readonly CML.NativeScript[];
  fee?: bigint;
}>;

const inputList = (outRefs: readonly string[]): CML.TransactionInputList => {
  const list = CML.TransactionInputList.new();
  for (const outRef of outRefs) {
    const [txHash, index] = outRef.split("#");
    list.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(txHash!),
        BigInt(index!),
      ),
    );
  }
  return list;
};

const output = (address: string): CML.TransactionOutput =>
  CML.TransactionOutput.new(
    CML.Address.from_bech32(address),
    CML.Value.from_coin(2_000_000n),
  );

export const signingScene = async () => {
  const prover = generateEmulatorAccount({});
  const signer = resolveProverSigner({
    network: "Custom",
    walletSeedPhrase: prover.seedPhrase,
  });
  const foreignAddress = credentialToAddress("Custom", {
    type: "Key",
    hash: "52".repeat(28),
  });
  const genesis = (address: string, lovelace: bigint) => ({
    ...prover,
    address,
    assets: { lovelace },
  });
  const emulator = new Emulator([
    genesis(signer.address, 40_000_000n),
    genesis(signer.address, 10_000_000n),
    genesis(signer.proofMaterialsAddress, 20_000_000n),
    genesis(foreignAddress, 30_000_000n),
  ]);
  const [coin, collateral, material, foreign] = [0, 1, 2, 3].map(
    (index) => `${"00".repeat(32)}#${index.toString()}`,
  ) as [string, string, string, string];
  const contract = credentialToAddress(
    "Custom",
    scriptHashToCredential(CONTRACT_HASH),
  );
  const lucid = await Lucid(emulator, "Custom");
  restrictWorkflowWallet({
    signer,
    objectiveWallet: createTestWorkflowProcess().objectiveWallet({
      category: "doubleSpend",
      headerHash: "c1".repeat(32),
    }),
    roster: { contractAddresses: [contract], scriptHashes: [CONTRACT_HASH] },
  }).selectWallet(lucid);

  const sign = async (
    shape: SigningShape,
  ): Promise<CML.TransactionWitnessSet> => {
    // A build lists the wallet first: its coins are known as ours.
    await lucid.wallet().getUtxos();
    const outputs = CML.TransactionOutputList.new();
    for (const address of shape.outputs) outputs.add(output(address));
    const body = CML.TransactionBody.new(
      inputList(shape.inputs),
      outputs,
      shape.fee ?? 200_000n,
    );
    if (shape.collateral !== undefined) {
      body.set_collateral_inputs(inputList(shape.collateral.inputs));
      body.set_total_collateral(1_000_000n);
      body.set_collateral_return(output(shape.collateral.returnAddress));
    }
    const witnesses = CML.TransactionWitnessSet.new();
    if (shape.nativeScripts !== undefined) {
      const scripts = CML.NativeScriptList.new();
      for (const script of shape.nativeScripts) scripts.add(script);
      witnesses.set_native_scripts(scripts);
    }
    return await lucid
      .wallet()
      .signTx(CML.Transaction.new(body, witnesses, true));
  };

  return {
    signer,
    coin,
    collateral,
    material,
    foreign,
    contract,
    maximumFee: validationTraceMoveFeeCeiling(lucid),
    sign,
  };
};
