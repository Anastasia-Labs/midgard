/**
 * `midgard-user withdraw`: withdraw one committed L2 output to L1.
 *
 * The first run reads the output from the node: the node must serve this
 * deployment, the output must exist, its producing L2 transaction must be
 * committed (a deposit or genesis output has none and reads `not_found`), and
 * its owner must be the signing wallet's key. The output's value is recorded
 * as CBOR in the intent, so a rerun rebuilds the identical signed body without
 * contacting the node.
 */
import { decodeMidgardAddressText } from "@al-ft/midgard-core/codec";
import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type OutRef } from "@lucid-evolution/lucid";

import { type UserCliDeps } from "./deps.js";
import { resolveL1Access } from "./l1-view.js";
import { loadUserDeployment } from "./manifest.js";
import { type UserNode } from "./node.js";
import {
  cardanoDatumCbor,
  parseAddress,
  parseAddressData,
  parseL2OutRef,
  parseOptionalDataHex,
  parseOptionalLovelace,
  parseSubmissionId,
  refuse,
} from "./options.js";
import { readIntent } from "./record-store.js";
import { resolveSeedPhrase, walletOf } from "./seed.js";
import {
  existingIntent,
  manifestPathOf,
  recordDirectoryOf,
  runSubmission,
  type SubmitOptions,
} from "./submission.js";

export type WithdrawOptions = SubmitOptions & {
  readonly l2OutRef: string;
  readonly l1Address: string;
  readonly l1Datum?: string;
  readonly refundAddress?: string;
  readonly refundDatum?: string;
  readonly lovelace?: string;
  readonly nodeEndpoint: string;
};

const errorText = (cause: unknown) =>
  cause instanceof Error ? cause.message : String(cause);

/** The output's owning key hash; a script-owned output cannot be withdrawn
 * by a key signature. */
export const selectedOutputOwner = (address: string): string => {
  let credential: ReturnType<
    typeof decodeMidgardAddressText
  >["paymentCredential"];
  try {
    credential = decodeMidgardAddressText(address).paymentCredential;
  } catch (cause) {
    return refuse(`Selected L2 UTxO address is invalid: ${errorText(cause)}`);
  }
  return credential.kind === "PubKey"
    ? credential.hash.toString("hex")
    : refuse("Selected L2 UTxO must be owned by a key credential.");
};

/** A withdrawal is classified against the committed ledger. */
const requireCommittedProducer = (txHash: string, status: string): void => {
  if (status === "committed" || status === "not_found") return;
  refuse(
    `Producing L2 tx ${txHash} is ${status}, not committed; wait for the producing tx to commit before withdrawing its output.`,
  );
};

/** First contact: the node's view of the selected output, checked. */
const readSelectedOutput = async (
  node: UserNode,
  manifestId: string,
  outRef: OutRef,
  signerKeyHash: string,
): Promise<{ readonly owner: string; readonly valueCbor: string }> => {
  const nodeManifestId = await node.deploymentManifestId();
  if (nodeManifestId !== manifestId)
    refuse(
      `The node serves deployment ${nodeManifestId ?? "(none reported)"}, not this manifest's ${manifestId}.`,
    );
  const selected = await node.utxoByOutRef(outRef);
  if (selected === undefined)
    return refuse(
      `The node has no unspent L2 output ${outRef.txHash}#${outRef.outputIndex.toString()}.`,
    );
  requireCommittedProducer(outRef.txHash, await node.txStatus(outRef.txHash));
  const owner = selectedOutputOwner(selected.address);
  if (owner !== signerKeyHash)
    refuse(
      `Selected L2 UTxO is owned by ${owner}, not withdrawal signer ${signerKeyHash}.`,
    );
  return {
    owner,
    valueCbor: Data.to(SDK.assetsToValue({ ...selected.assets }), SDK.Value),
  };
};

const recordedValueCbor = (intent: Readonly<Record<string, unknown>>) => {
  const valueCbor = intent.l2ValueCbor;
  try {
    if (typeof valueCbor !== "string") throw new Error("missing");
    Data.from(valueCbor, SDK.Value);
    return valueCbor;
  } catch {
    return refuse("The recorded withdrawal intent has no valid l2ValueCbor.");
  }
};

export const runWithdraw = async (
  options: WithdrawOptions,
  deps: UserCliDeps,
): Promise<SDK.UserSubmissionOutcome> => {
  const submissionId = parseSubmissionId(options.submissionId);
  const l2OutRef = parseL2OutRef(options.l2OutRef);
  const l1Datum = parseOptionalDataHex(options.l1Datum, "--l1-datum");
  const refundDatum = parseOptionalDataHex(
    options.refundDatum,
    "--refund-datum",
  );
  const lovelace = parseOptionalLovelace(options.lovelace, "--lovelace");
  const seedPhrase = resolveSeedPhrase(
    {
      env: options.walletSeedPhraseEnv,
      file: options.walletSeedPhraseFile,
    },
    deps.env,
  );
  const deployment = await loadUserDeployment(manifestPathOf(options, deps));
  const { network } = deployment;
  const l1Address = parseAddress(options.l1Address, "--l1-address", network);
  const refundAddress =
    options.refundAddress === undefined
      ? l1Address
      : parseAddress(options.refundAddress, "--refund-address", network);
  const l1AddressData = parseAddressData(l1Address, "--l1-address");
  const refundAddressData = parseAddressData(refundAddress, "--refund-address");
  const access = resolveL1Access(options, deps.env, network);
  const wallet = walletOf(seedPhrase, network);
  const recordDirectory = recordDirectoryOf(
    options,
    deps,
    deployment.manifestId,
    "Withdrawal",
    submissionId,
  );

  const signed = (valueCbor: string) => {
    const placeholder: SDK.WithdrawalBody = {
      l2_outref: {
        transactionId: l2OutRef.txHash,
        outputIndex: BigInt(l2OutRef.outputIndex),
      },
      l2_owner: wallet.paymentKeyHash,
      l2_value: new Map(),
      l1_address: l1AddressData,
      l1_datum: "NoDatum",
    };
    const bodyCbor = replacePlutusConstrFieldCbor(
      replacePlutusConstrFieldCbor(
        Data.to(placeholder, SDK.WithdrawalBody),
        [2],
        valueCbor,
      ),
      [4],
      cardanoDatumCbor(l1Datum),
    );
    const config = {
      bodyCbor,
      signature: SDK.signWithdrawalBodyCbor(
        CML.PrivateKey.from_bech32(wallet.paymentKey),
        bodyCbor,
      ),
      refundAddress: refundAddressData,
      refundDatumCbor: cardanoDatumCbor(refundDatum),
      ...(lovelace === undefined ? {} : { lovelace }),
    };
    return {
      config,
      intentHash: SDK.userSubmissionIntentHash("Withdrawal", config),
    };
  };

  const recorded = await readIntent(recordDirectory);
  const valueCbor =
    recorded !== undefined
      ? recordedValueCbor(recorded.intent)
      : (
          await readSelectedOutput(
            await deps.connectNode(options.nodeEndpoint),
            deployment.manifestId,
            l2OutRef,
            wallet.paymentKeyHash,
          )
        ).valueCbor;
  const { config, intentHash } = signed(valueCbor);
  await existingIntent(recordDirectory, {
    kind: "Withdrawal",
    submissionId,
    manifestId: deployment.manifestId,
    walletAddress: wallet.address,
    intentHash,
  });
  return runSubmission(
    options,
    deps,
    {
      kind: "Withdrawal",
      submissionId,
      deployment,
      access,
      seedPhrase,
      walletAddress: wallet.address,
      intentHash,
      intent: {
        l2OutRef: `${l2OutRef.txHash}#${l2OutRef.outputIndex.toString()}`,
        l1Address,
        l1Datum,
        refundAddress,
        refundDatum,
        lovelace: lovelace?.toString() ?? null,
        l2Owner: wallet.paymentKeyHash,
        l2ValueCbor: valueCbor,
      },
      recordDirectory,
      config: (withdrawalMinting): SDK.SubmitWithdrawalConfig => ({
        ...config,
        referenceScripts: { withdrawalMinting },
      }),
    },
    SDK.submitWithdrawalProgram,
  );
};
