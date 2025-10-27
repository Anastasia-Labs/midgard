import { Address, PolicyId, ScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";

/*
    The onchain paratemets service, parameters are hardcoded for now.
    Meant to be used in the midgard node.
*/
export class Parameters extends Effect.Service<Parameters>()("Parameters", {
  effect: Effect.gen(function* () {
    const max_tokens_allowed_in_deposits: number = 10;
    const event_wait_duration: number = 60_000
    const l2_outbox_address: Address = "00000000000000000000000000000000000000000000000000000000";
    const l2_outbox_policy_id: PolicyId = "00000000000000000000000000000000000000000000000000000000";
    const l2_outbox_asset_name: string = "";
    const plutarch_phas_validator_hash: ScriptHash = "";
    const plutarch_pexcludes_validator_hash: ScriptHash = "";

    return {
        max_tokens_allowed_in_deposits,
        event_wait_duration,
        l2_outbox_address,
        l2_outbox_policy_id,
        l2_outbox_asset_name,
        plutarch_phas_validator_hash,
        plutarch_pexcludes_validator_hash
    };
  }),
}) {}
