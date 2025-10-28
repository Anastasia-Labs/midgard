import { Network, ScriptHash } from "@lucid-evolution/lucid";

/*
  Getting the onchain parameters based on the provided network
*/
export function get_parameters(network: Network) {
  if (network === "Mainnet") {
    throw new Error("No parameters for mainnet defined");
  }

  const max_tokens_allowed_in_deposits: number = 10;
  const event_wait_duration: number = 60_000;
  const plutarch_phas_validator_hash: ScriptHash = "";
  const plutarch_pexcludes_validator_hash: ScriptHash = "";

  return {
    max_tokens_allowed_in_deposits,
    event_wait_duration,
    plutarch_phas_validator_hash,
    plutarch_pexcludes_validator_hash,
  };
}
