import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "../../blueprints/always-succeeds/plutus.json" with { type: "json" };
import "./always-succeeds.make-spending-validator.js";
import "./always-succeeds.make-always-succeeds-service.js";
import "./always-succeeds.always-succeeds-contract.js";
export { AlwaysSucceedsContract } from "./always-succeeds.always-succeeds-contract.js";
