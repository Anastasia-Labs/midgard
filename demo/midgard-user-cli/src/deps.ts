/**
 * Everything `runUserCli` reaches outside its arguments, so a test can run the
 * real command against an emulator-backed view and a stub node.
 */
import { homedir } from "node:os";

import {
  MidgardNodeProvider,
  type MidgardProvider,
} from "@al-ft/lucid-midgard";
import { type UserL1View } from "@al-ft/midgard-sdk";

import { type OpenL1ViewRequest, openProviderL1View } from "./l1-view.js";
import { connectMidgardNode, type UserNode } from "./node.js";

export type UserCliDeps = {
  readonly env: Readonly<Record<string, string | undefined>>;
  readonly stdout: (text: string) => void;
  readonly stderr: (text: string) => void;
  readonly homeDir: string;
  readonly now: () => number;
  readonly sleep: (ms: number) => Promise<void>;
  /** Kupo's patterns and health, Blockfrost's latest block, the node's API. */
  readonly fetch: typeof fetch;
  readonly openL1View: (request: OpenL1ViewRequest) => Promise<UserL1View>;
  readonly connectNode: (endpoint: string) => Promise<UserNode>;
  /** The node's L2 API as lucid-midgard builds and submits against it. */
  readonly connectL2: (endpoint: string) => Promise<MidgardProvider>;
};

export const defaultUserCliDeps = (): UserCliDeps => ({
  env: process.env,
  stdout: (text) => process.stdout.write(text),
  stderr: (text) => process.stderr.write(text),
  homeDir: homedir(),
  now: Date.now,
  sleep: (ms) => new Promise((resolve) => setTimeout(resolve, ms)),
  fetch,
  openL1View: (request) => openProviderL1View(request, fetch),
  connectNode: (endpoint) => connectMidgardNode(endpoint, fetch),
  connectL2: (endpoint) => MidgardNodeProvider.create({ endpoint, fetch }),
});
