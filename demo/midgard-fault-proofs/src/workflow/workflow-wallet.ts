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
import type { WorkflowSends } from "./in-flight.js";
import { proofMaterialsViewFor } from "./proof-materials-view.js";

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
 * funding address that no transaction or action of this process holds
 * (`in-flight.ts`), one pool offered both to coin selection and as
 * collateral. Each read lists the wallet afresh and notes every coin it
 * lists as a wallet coin of the objective's sends, so the runner can tell a
 * transaction spending only wallet coins. Signing and submission delegate to
 * the original wallet.
 *
 * While the next build of an action must spend the wallet coins of its
 * expired predecessor (`WorkflowSends.requiredWalletInputs`), every
 * transaction the wallet's Lucid builds collects those coins explicitly, and
 * coin selection tops up from the rest of the pool.
 */
export const restrictWorkflowWallet = ({
  signer,
  sends,
}: {
  readonly signer: ResolvedProverSigner;
  readonly sends: WorkflowSends;
}): ResolvedProverSigner => {
  if (signer.proofMaterials !== undefined)
    sends.installProofMaterialsView(signer.proofMaterials);
  return Object.freeze({
    ...signer,
    selectWallet: (lucid: LucidEvolution): void => {
      signer.selectWallet(lucid);
      const original = lucid.wallet();
      const listed = async (): Promise<readonly UTxO[]> => {
        const utxos = (await original.getUtxos()).filter(
          (utxo) => utxo.address === signer.address,
        );
        sends.noteWalletCoins(utxos.map(outRefLabel));
        return utxos;
      };
      const selectable = async (): Promise<readonly UTxO[]> =>
        (await listed()).filter(
          (utxo) =>
            isPlainFundingOutput(utxo) && !sends.isHeld(outRefLabel(utxo)),
        );
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
        signTx: async (tx) =>
          (
            await original.signTx(CML.Transaction.from_cbor_hex(tx))
          ).to_cbor_hex(),
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
      sends.installProofMaterialsView(view);
      sends.installSweepWallet(
        Object.freeze({ lucid, fundingAddress: signer.address, view }),
      );
      walletListings.set(lucid, listed);
      collectRequiredWalletInputs(lucid);
    },
  });
};

/** Lists the wallet coins of the Lucid a restricted wallet is installed
 * on, noting them as the objective's wallet coins. */
const walletListings = new WeakMap<
  LucidEvolution,
  () => Promise<readonly UTxO[]>
>();

/** The coins every transaction a Lucid builds must spend now. */
const requiredInputs = new WeakMap<LucidEvolution, readonly UTxO[]>();

/**
 * Runs `build` while every transaction built on the Lucid the objective's
 * restricted wallet is installed on spends those of `outRefs` still at the
 * wallet. A coin already gone is skipped: the runner has checked that the
 * transaction that spent it before cannot land.
 */
export const withRequiredWalletInputs = async <Result>(
  sends: WorkflowSends,
  outRefs: readonly string[],
  build: () => Promise<Result>,
): Promise<Result> => {
  const lucid = sends.sweepWallet()?.lucid;
  const list = lucid === undefined ? undefined : walletListings.get(lucid);
  if (outRefs.length === 0 || lucid === undefined || list === undefined)
    return await build();
  const wanted = new Set(outRefs);
  const present = (await list()).filter((utxo) =>
    wanted.has(outRefLabel(utxo)),
  );
  if (present.length === 0) return await build();
  requiredInputs.set(lucid, present);
  try {
    return await build();
  } finally {
    requiredInputs.delete(lucid);
  }
};

const collecting = new WeakSet<LucidEvolution>();

/**
 * Coin selection spends wallet inputs largest first and stops once it is
 * covered, so offering a predecessor's coin does not make a replacement
 * spend it. Every transaction built while required coins are set collects
 * them explicitly.
 */
const collectRequiredWalletInputs = (lucid: LucidEvolution): void => {
  if (collecting.has(lucid)) return;
  collecting.add(lucid);
  const newTx = lucid.newTx;
  lucid.newTx = () => {
    const builder = newTx();
    const required = requiredInputs.get(lucid) ?? [];
    if (required.length === 0) return builder;
    const forced = new Set(required.map(outRefLabel));
    const collectFrom = builder.collectFrom;
    // A builder that collects one of these itself must not spend it twice.
    builder.collectFrom = (utxos, redeemer) => {
      if (redeemer !== undefined) return collectFrom(utxos, redeemer);
      const rest = utxos.filter((utxo) => !forced.has(outRefLabel(utxo)));
      return rest.length === 0 && utxos.length !== 0
        ? builder
        : collectFrom(rest);
    };
    return collectFrom([...required]);
  };
};

/**
 * Broadcasts the exact signed bytes again through the objective's wallet.
 * False when no restricted wallet is installed, so there is nowhere to send
 * them.
 */
export const resubmitWorkflowTransaction = async (
  sends: WorkflowSends,
  signedTransactionCborHex: string,
): Promise<boolean> => {
  const wallet = sends.sweepWallet();
  if (wallet === undefined) return false;
  await wallet.lucid.wallet().submitTx(signedTransactionCborHex);
  return true;
};
