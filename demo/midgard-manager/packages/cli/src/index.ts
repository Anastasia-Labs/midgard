import { NodeContext, NodeRuntime } from '@effect/platform-node';
import { Effect } from 'effect';

import { run } from './Cli.js';

// Run the CLI
Effect.runPromise(
  run(process.argv).pipe(
    Effect.provide(NodeContext.layer),
    NodeRuntime.runMain({ disableErrorReporting: true })
  )
);
