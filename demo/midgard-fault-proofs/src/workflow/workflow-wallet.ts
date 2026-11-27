import {
  assetsToValue,
  CML,
  type LucidEvolution,
  type UTxO,
  utxoToCore,
  type WalletApi,
} from "@lucid-evolution/lucid";

import { isPlainFundingOutput } from "../funding/plain-funding-output.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { validationTraceMoveFeeCeiling } from "../validation-trace-dispute/move-fee.js";
import { proofMaterialsViewFor } from "./proof-materials-view.js";
import {
  assertWorkflowTransactionShape,
  workflowTransactionParts,
  type WorkflowTransactionRoster,
} from "./transaction-shape.js";
import type { WorkflowObjectiveWallet } from "./workflow-run.js";

export type { WorkflowTransactionRoster } from "./transaction-shape.js";

const outRefLabel = ({ txHash, outputIndex }: UTxO): string =>
  `${txHash}#${outputIndex.toString()}`;

/** The summed value of `utxos`, as a wallet's `getBalance` reports it. */
export const balanceCbor = (utxos: readonly UTxO[]): string => {
  const assets: Record<string, bigint> = {};
  for (const utxo of utxos)
    for (const [unit, quantity] of Object.entries(utxo.assets))
      assets[unit] = (assets[unit] ?? 0n) + quantity;
  return assetsToValue(assets).to_cbor_hex();
};

/**
 * Returns a signer whose wallet offers the plain-ada coins of the prover's
 * funding address, one pool offered both to coin selection and as
 * collateral. Each read lists the wallet afresh. Signing and submission
 * delegate to the original wallet.
 *
 * The Lucid a workflow is handed reads the ledger at the node's tip with the
 * node's mempool applied, in every read a build makes (the wallet listing,
 * `utxosByOutRef`, the proof-materials finds): a coin a mempool transaction
 * spends is never offered, and the change a mempool transaction makes is.
 * Nothing here holds a coin or remembers a transaction.
 *
 * Given the deployment's `roster`, the wallet signs no transaction outside
 * the workflow transaction shape (`transaction-shape.ts`): an input the
 * wallet listed last is placed at our wallet, and any other input is read
 * from the chain only where the shape turns on it.
 */
export const restrictWorkflowWallet = ({
  signer,
  objectiveWallet,
  roster,
}: {
  readonly signer: ResolvedProverSigner;
  readonly objectiveWallet: WorkflowObjectiveWallet;
  readonly roster?: WorkflowTransactionRoster;
}): ResolvedProverSigner => {
  if (signer.proofMaterials !== undefined)
    objectiveWallet.installProofMaterialsView(signer.proofMaterials);
  const assertShape = async (
    lucid: LucidEvolution,
    tx: string,
    governed: WorkflowTransactionRoster,
    walletCoins: ReadonlySet<string>,
  ): Promise<void> => {
    const parts = workflowTransactionParts(tx);
    const placed = new Map<string, string>();
    for (const outRef of [...parts.inputs, ...parts.collateralInputs])
      if (walletCoins.has(outRef)) placed.set(outRef, signer.address);
    const maximumFeeLovelace = validationTraceMoveFeeCeiling(lucid);
    // Every other input the shape turns on is read: those of a transaction
    // that embeds a script (our lock spends a proof material) or whose fee
    // passes the ceiling (our coins must not pay it), and collateral.
    const unplaced = [
      ...(parts.embeddedScriptHashes.length > 0 ||
      parts.fee > maximumFeeLovelace
        ? parts.inputs
        : []),
      ...parts.collateralInputs,
    ].filter((outRef) => !placed.has(outRef));
    if (unplaced.length > 0)
      for (const utxo of await lucid.utxosByOutRef(
        [...new Set(unplaced)].map((outRef) => {
          const [txHash, outputIndex] = outRef.split("#");
          return { txHash: txHash!, outputIndex: Number(outputIndex) };
        }),
      ))
        placed.set(outRefLabel(utxo), utxo.address);
    assertWorkflowTransactionShape({
      parts,
      shape: {
        roster: governed,
        walletAddress: signer.address,
        proofMaterialsAddress: signer.proofMaterialsAddress,
        paymentKeyHash: signer.paymentKeyHash,
        maximumFeeLovelace,
      },
      addressOf: (outRef) => placed.get(outRef),
    });
  };
  return Object.freeze({
    ...signer,
    selectWallet: (lucid: LucidEvolution): void => {
      signer.selectWallet(lucid);
      const original = lucid.wallet();
      let walletCoins: ReadonlySet<string> = new Set<string>();
      const selectable = async (): Promise<readonly UTxO[]> => {
        const utxos = (await original.getUtxos()).filter(
          (utxo) => utxo.address === signer.address,
        );
        walletCoins = new Set(utxos.map(outRefLabel));
        return utxos.filter(isPlainFundingOutput);
      };
      const addressHex = CML.Address.from_bech32(signer.address).to_hex();
      const api: WalletApi = Object.freeze({
        getNetworkId: async () =>
          CML.Address.from_bech32(signer.address).to_raw_bytes()[0]! & 0x0f,
        getUtxos: async () =>
          (await selectable()).map((utxo) => utxoToCore(utxo).to_cbor_hex()),
        getBalance: async () => balanceCbor(await selectable()),
        getUsedAddresses: async () => [addressHex],
        getUnusedAddresses: async () => [],
        getChangeAddress: async () => addressHex,
        getRewardAddresses: async () => [],
        signTx: async (tx) => {
          if (roster !== undefined)
            await assertShape(lucid, tx, roster, walletCoins);
          return (
            await original.signTx(CML.Transaction.from_cbor_hex(tx))
          ).to_cbor_hex();
        },
        signData: async (address, payload) =>
          await original.signMessage(
            CML.Address.from_hex(address).to_bech32(),
            payload,
          ),
        submitTx: async (tx) => await original.submitTx(tx),
        getCollateral: async () =>
          (await selectable()).map((utxo) => utxoToCore(utxo).to_cbor_hex()),
        experimental: Object.freeze({
          getCollateral: async () =>
            (await selectable()).map((utxo) => utxoToCore(utxo).to_cbor_hex()),
          on: () => undefined,
          off: () => undefined,
        }),
      });
      lucid.selectWallet.fromAPI(api);
      const view = proofMaterialsViewFor({
        view: signer.proofMaterials,
        lucid,
        address: signer.proofMaterialsAddress,
      });
      objectiveWallet.installProofMaterialsView(view);
      objectiveWallet.installSweepWallet(
        Object.freeze({ lucid, fundingAddress: signer.address, view }),
      );
    },
  });
};
