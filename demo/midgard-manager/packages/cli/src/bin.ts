#!/usr/bin/env node

import * as NodeContext from '@effect/platform-node/NodeContext';
import * as NodeRuntime from '@effect/platform-node/NodeRuntime';
import * as Effect from 'effect/Effect';

import { run } from './Cli.js';
import { displayLogo } from './utils/logo.js';

// Display the Midgard logo when the CLI starts
displayLogo({ variant: 'full' });

run(process.argv).pipe(
  Effect.provide(NodeContext.layer),
  NodeRuntime.runMain({ disableErrorReporting: true })
);
