import { Effect } from "effect";

import { makeAlwaysSucceedsService } from "./always-succeeds.make-always-succeeds-service.js";

export class AlwaysSucceedsContract extends Effect.Service<AlwaysSucceedsContract>()(
  "AlwaysSucceedsContract",
  {
    effect: makeAlwaysSucceedsService,
  },
) {}
