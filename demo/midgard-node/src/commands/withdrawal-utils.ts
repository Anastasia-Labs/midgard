import * as SDK from "@al-ft/midgard-sdk";
import {
  type Credential,
  credentialToAddress,
  Data as LucidData,
  type Network,
} from "@lucid-evolution/lucid";

import { parseHexBytes, parseTxOutRefLabel } from "./command-utils.js";

export const parseWithdrawalTxOutRefLabel = (
  value: unknown,
  fieldName = "txOutRef",
): {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly cbor: Buffer;
  readonly outputReference: SDK.OutputReference;
} => {
  const parsed = parseTxOutRefLabel(value, fieldName);
  return {
    ...parsed,
    outputReference: {
      transactionId: parsed.txHash,
      outputIndex: BigInt(parsed.outputIndex),
    },
  };
};

export const parseCardanoDatum = (
  value: string | undefined,
  fieldName = "datum",
): SDK.CardanoDatum => {
  if (value === undefined || value.trim().length === 0) {
    return "NoDatum";
  }
  const datumCbor = parseHexBytes(value, fieldName).toString("hex");
  try {
    return {
      InlineDatum: {
        data: LucidData.from(datumCbor),
      },
    } as SDK.CardanoDatum;
  } catch (cause) {
    throw new Error(
      `${fieldName} must decode as Plutus data CBOR: ${String(cause)}`,
    );
  }
};

export const addressDataToBech32 = (
  network: Network,
  address: SDK.AddressData,
): string => {
  const paymentCredential = credentialFromAddressData(
    address.paymentCredential,
  );
  if (address.stakeCredential === null) {
    return credentialToAddress(network, paymentCredential);
  }
  if ("Inline" in address.stakeCredential) {
    return credentialToAddress(
      network,
      paymentCredential,
      credentialFromAddressData(address.stakeCredential.Inline[0]),
    );
  }
  throw new Error("Pointer stake credentials are not supported by this CLI.");
};

const credentialFromAddressData = (credential: SDK.CredentialD): Credential => {
  if ("PublicKeyCredential" in credential) {
    return {
      type: "Key",
      hash: credential.PublicKeyCredential[0],
    };
  }
  return {
    type: "Script",
    hash: credential.ScriptCredential[0],
  };
};
