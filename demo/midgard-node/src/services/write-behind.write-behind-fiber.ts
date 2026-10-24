import "./write-behind.make-write-behind.js";

import { Effect } from "effect";

import { WriteBehind } from "./write-behind.summarize-write-behind-telemetry.js";

export const writeBehindFiber: Effect.Effect<void, never, WriteBehind> =
  Effect.gen(function* () {
    const service = yield* WriteBehind;
    yield* Effect.logInfo("📝 Write-behind writer fiber started.");
    yield* service.run;
  });
