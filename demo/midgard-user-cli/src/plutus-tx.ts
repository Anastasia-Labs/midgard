/**
 * `midgard-user plutus-tx`: a normal L2 transaction that runs a Plutus
 * script. It mints one token under an always-succeeds PlutusV3 policy from
 * the wallet's first L2 output, has the node evaluate the redeemer's budget,
 * submits it, and waits for the node's verdict (lucid-midgard's default
 * targets and bound).
 */
import { type UserCliDeps } from "./deps.js";
import { type L2CommandOptions, openL2Wallet, prepareL2Command } from "./l2.js";
import {
  ALWAYS_SUCCEEDS_PLUTUS_V3_HASH,
  buildPlutusMint,
} from "./l2-shapes.js";
import { loadUserDeployment } from "./manifest.js";
import { refuse } from "./options.js";

export type PlutusTxOptions = L2CommandOptions;

export type PlutusTxResult = {
  readonly l2TxId: string;
  /** The node's status: accepted or committed. */
  readonly status: string;
  readonly policyId: string;
};

export const runPlutusTx = async (
  options: PlutusTxOptions,
  deps: UserCliDeps,
): Promise<PlutusTxResult> => {
  const { deployment, wallet } = await prepareL2Command(
    options,
    deps,
    loadUserDeployment,
  );
  const l2 = await openL2Wallet(deps, options.nodeEndpoint, deployment, wallet);
  if (l2.shapeWallet.utxos.length === 0)
    refuse(`The node holds no L2 output at ${wallet.address}.`);
  const built = await buildPlutusMint(l2.midgard, l2.shapeWallet).complete();
  const signed = await built.sign.withWallet().complete();
  const status = await (await signed.submit()).awaitStatus();
  if (status.kind === "rejected")
    refuse(
      `The node rejected L2 transaction ${status.txId}: ${status.code}${status.detail === null ? "" : ` (${status.detail})`}.`,
    );
  return {
    l2TxId: signed.txIdHex,
    status: status.kind,
    policyId: ALWAYS_SUCCEEDS_PLUTUS_V3_HASH,
  };
};

export const formatPlutusTx = (result: PlutusTxResult): string =>
  `plutus tx ${result.status}: L2 tx ${result.l2TxId}, minted under ${result.policyId}\n`;
