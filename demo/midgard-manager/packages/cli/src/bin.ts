#!/usr/bin/env node

import * as NodeContext from '@effect/platform-node/NodeContext';
import * as NodeRuntime from '@effect/platform-node/NodeRuntime';
import * as Effect from 'effect/Effect';

import { run } from './Cli.js';
import { displayLogo } from './utils/logo.js';

/**
 * CLI process entrypoint.
 *
 * It renders the shared logo/header, wires the Effect Node runtime, and then
 * hands off control to the root CLI command.
 */
displayLogo({ variant: 'full' });

run(process.argv).pipe(
  Effect.provide(NodeContext.layer),
  NodeRuntime.runMain({ disableErrorReporting: true })
);
