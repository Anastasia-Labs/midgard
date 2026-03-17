import { Effect } from "effect";
import * as Services from "@/services/index.js";

const program = Effect.gen(function* () {
  const contracts = yield* Services.MidgardContracts;
  console.log("registered policy", contracts.registeredOperators.policyId);
  console.log("active policy", contracts.activeOperators.policyId);
  console.log("registered mint len", contracts.registeredOperators.mintingScriptCBOR.length);
  console.log("active mint len", contracts.activeOperators.mintingScriptCBOR.length);
  console.log("registered mint prefix", contracts.registeredOperators.mintingScriptCBOR.slice(0, 80));
  console.log("active mint prefix", contracts.activeOperators.mintingScriptCBOR.slice(0, 80));
});

Effect.runPromise(
  program.pipe(
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.MidgardContracts.Default),
  ),
).catch((e) => {
  console.error(e);
  process.exit(1);
});
