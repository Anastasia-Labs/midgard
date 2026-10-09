/**
 * Imported first by an emulator family suite: installs the follower
 * provider (`follower-emulator.ts`) on every emulator the suite creates, so
 * the node's provider is the provider the suite runs with, and checks at
 * the end that the suite's transactions went through it.
 */
import { afterAll, expect } from "vitest";

import {
  type FollowerEmulatorUse,
  installFollowerEmulator,
} from "./follower-emulator.js";

const installation = installFollowerEmulator();

afterAll(async () => {
  // One emulator at a time: each use moves the store to its chain.
  const uses: FollowerEmulatorUse[] = [];
  try {
    for (const host of installation.hosts()) uses.push(await host.use());
  } finally {
    await installation.restore();
  }
  const submitted = uses.flatMap(({ submitted }) => submitted);
  const confirmed = uses.flatMap(({ confirmed }) => confirmed);
  const stored = uses.flatMap(({ stored }) => stored);
  console.info(
    `follower provider: ${uses.length.toString()} emulators, ${uses
      .reduce((sum, { providerCalls }) => sum + providerCalls, 0)
      .toString()} provider calls, ${submitted.length.toString()} submitted, ${confirmed.length.toString()} confirmed, ${stored.length.toString()} in the follower store`,
  );
  // The suite submitted through the follower provider, and every
  // transaction the emulator confirmed reached the follower store.
  expect(confirmed.length).toBeGreaterThan(0);
  expect(stored).toEqual(confirmed);
});
