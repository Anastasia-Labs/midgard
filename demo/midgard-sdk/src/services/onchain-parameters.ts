import { ScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";

/*
    The onchain paratemets service, parameters are hardcoded for now.
    Meant to be used in the midgard node.
*/
export class Parameters extends Effect.Service<Parameters>()("Parameters", {
  effect: Effect.gen(function* () {
    const max_tokens_allowed_in_deposits: number = 10;
    const event_wait_duration: number = 60_000
    const plutarch_phas_validator_hash: ScriptHash = "";
    const plutarch_pexcludes_validator_hash: ScriptHash = "";

    return {
        max_tokens_allowed_in_deposits,
        event_wait_duration,
        plutarch_phas_validator_hash,
        plutarch_pexcludes_validator_hash
    };
  }),
}) {}
