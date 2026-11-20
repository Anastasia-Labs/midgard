import {
  CML,
  type Network,
  type Script,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

const networkId = (network: Network): number => (network === "Mainnet" ? 1 : 0);

export const scriptRewardAddress = (
  network: Network,
  script: Script,
): string => {
  const credential = CML.Credential.new_script(
    CML.ScriptHash.from_hex(validatorToScriptHash(script)),
  );
  return CML.RewardAddress.new(networkId(network), credential)
    .to_address()
    .to_bech32();
};
