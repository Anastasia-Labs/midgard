import type { AddressData, CredentialD } from "@al-ft/midgard-sdk";
import {
  type Credential,
  credentialToAddress,
  type Network,
} from "@lucid-evolution/lucid";

/** The governed spending address is independent of the event minting policy. */
export const governedUserEventAddress = (
  network: Network,
  address: AddressData,
): string => {
  const credential = (value: CredentialD): Credential =>
    "PublicKeyCredential" in value
      ? { type: "Key", hash: value.PublicKeyCredential[0] }
      : { type: "Script", hash: value.ScriptCredential[0] };
  if (address.stakeCredential === null)
    return credentialToAddress(network, credential(address.paymentCredential));
  if ("Inline" in address.stakeCredential)
    return credentialToAddress(
      network,
      credential(address.paymentCredential),
      credential(address.stakeCredential.Inline[0]),
    );
  throw new Error(
    "Governed user-event pointer stake addresses are unsupported",
  );
};
